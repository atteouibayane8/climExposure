#' Importer des données climatiques
#'
#' @description
#' Importe des données climatiques depuis un fichier CSV ou NetCDF et les
#' structure dans un dataframe standardisé pour les analyses du package.
#'
#' @param file Chemin vers le fichier de données climatiques (CSV ou NetCDF).
#' @param format Format du fichier : \code{"csv"} (défaut) ou \code{"netcdf"}.
#' @param start_date Date de début au format "YYYY-MM-DD" (optionnel).
#' @param end_date Date de fin au format "YYYY-MM-DD" (optionnel).
#' @param variables Vecteur de variables à extraire. Par défaut :
#'   \code{c("tmax", "tmin", "prec")}.
#'
#' @return Un dataframe avec les colonnes :
#' \describe{
#'   \item{date}{Date au format Date}
#'   \item{year}{Année}
#'   \item{month}{Mois}
#'   \item{tmax}{Température maximale (°C)}
#'   \item{tmin}{Température minimale (°C)}
#'   \item{prec}{Précipitations (mm)}
#' }
#'
#' @examples
#' # Utilisation avec données d'exemple du package
#' data(climate_morocco)
#' head(climate_morocco)
#'
#' # Importation depuis un CSV
#' \dontrun{
#' df <- import_climate_data("donnees_climat.csv", format = "csv",
#'                           start_date = "2000-01-01",
#'                           end_date = "2020-12-31")
#' }
#'
#' @export
import_climate_data <- function(file,
                                 format = "csv",
                                 start_date = NULL,
                                 end_date = NULL,
                                 variables = c("tmax", "tmin", "prec")) {

  # V\u00e9rification du format
  format <- match.arg(format, c("csv", "netcdf"))

  if (!file.exists(file)) {
    stop("Le fichier '", file, "' n'existe pas.")
  }

  # Import CSV
  if (format == "csv") {
    df <- utils::read.csv(file, stringsAsFactors = FALSE)

    # Standardisation des noms de colonnes (minuscules)
    names(df) <- tolower(names(df))

    # D\u00e9tection de la colonne date
    date_col <- grep("date|jour|day", names(df), value = TRUE, ignore.case = TRUE)[1]
    if (is.na(date_col)) {
      stop("Aucune colonne 'date' trouv\u00e9e. Colonnes disponibles : ",
           paste(names(df), collapse = ", "))
    }

    # Conversion de la date
    df$date <- tryCatch(
      as.Date(df[[date_col]]),
      error = function(e) {
        tryCatch(lubridate::ymd(df[[date_col]]),
                 error = function(e2) lubridate::dmy(df[[date_col]]))
      }
    )
    if (date_col != "date") df[[date_col]] <- NULL

    # Ajout colonnes temporelles
    df$year  <- lubridate::year(df$date)
    df$month <- lubridate::month(df$date)

    # V\u00e9rification des variables requises
    missing_vars <- setdiff(variables, names(df))
    if (length(missing_vars) > 0) {
      warning("Variables manquantes dans le fichier : ",
              paste(missing_vars, collapse = ", "))
    }

    # Filtrage temporel
    if (!is.null(start_date)) {
      df <- df[df$date >= as.Date(start_date), ]
    }
    if (!is.null(end_date)) {
      df <- df[df$date <= as.Date(end_date), ]
    }

    # S\u00e9lection colonnes pertinentes
    cols_keep <- intersect(c("date", "year", "month", variables), names(df))
    df <- df[, cols_keep, drop = FALSE]

    message("Donn\u00e9es climatiques import\u00e9es : ", nrow(df), " observations.")
    return(df)

  } else if (format == "netcdf") {
    # Lecture NetCDF simplifi\u00e9e (n\u00e9cessite le package ncdf4)
    if (!requireNamespace("ncdf4", quietly = TRUE)) {
      stop("Le package 'ncdf4' est requis pour lire les fichiers NetCDF.\n",
           "Installez-le avec : install.packages('ncdf4')")
    }

    nc <- ncdf4::nc_open(file)
    on.exit(ncdf4::nc_close(nc))

    # Extraction des dimensions temporelles
    time_var <- nc$var[["time"]] %||% nc$dim[["time"]]
    dates <- as.Date(ncdf4::ncvar_get(nc, "time"), origin = "1970-01-01")

    df_list <- list(date = dates)

    # Extraction des variables climatiques
    var_map <- list(tmax = c("tmax", "tasmax", "tx"),
                    tmin = c("tmin", "tasmin", "tn"),
                    prec = c("prec", "pr", "precipitation"))

    for (v in variables) {
      candidates <- var_map[[v]]
      nc_var <- intersect(candidates, names(nc$var))[1]
      if (!is.na(nc_var)) {
        df_list[[v]] <- as.vector(ncdf4::ncvar_get(nc, nc_var))
      }
    }

    df <- as.data.frame(df_list)
    df$year  <- lubridate::year(df$date)
    df$month <- lubridate::month(df$date)

    if (!is.null(start_date)) df <- df[df$date >= as.Date(start_date), ]
    if (!is.null(end_date))   df <- df[df$date <= as.Date(end_date), ]

    message("Donn\u00e9es NetCDF import\u00e9es : ", nrow(df), " observations.")
    return(df)
  }
}

# Utilitaire interne : op\u00e9rateur null-coalesce
`%||%` <- function(a, b) if (!is.null(a)) a else b
