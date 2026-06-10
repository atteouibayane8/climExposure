#' Calculer les indices de sécheresse
#'
#' @description
#' Calcule des indices simples de sécheresse : le SPI (Standardized
#' Precipitation Index) simplifié et le déficit de précipitations par rapport
#' à une référence historique.
#'
#' @param data Dataframe climatique avec colonnes \code{year}, \code{month}
#'   et \code{prec}.
#' @param method Méthode de calcul : \code{"spi"} (SPI simplifié, défaut) ou
#'   \code{"deficit"} (déficit de précipitations).
#' @param period Période d'accumulation en mois pour le SPI. Par défaut : 3.
#' @param ref_period Vecteur de deux années définissant la période de référence
#'   pour le calcul des anomalies. Ex : \code{c(1981, 2010)}.
#'
#' @return Un dataframe avec les colonnes :
#' \describe{
#'   \item{year}{Année}
#'   \item{month}{Mois (si period = "monthly")}
#'   \item{drought_index}{Valeur de l'indice de sécheresse}
#'   \item{drought_class}{Classe de sécheresse : "normale", "modérée",
#'     "sévère", "extrême"}
#' }
#'
#' @examples
#' data(climate_morocco)
#' drought <- calculate_drought_index(climate_morocco, method = "spi")
#' head(drought)
#'
#' @export
calculate_drought_index <- function(data,
                                     method = "spi",
                                     period = 3,
                                     ref_period = NULL) {

  required_cols <- c("year", "month", "prec")
  missing <- setdiff(required_cols, names(data))
  if (length(missing) > 0) {
    stop("Colonnes manquantes : ", paste(missing, collapse = ", "))
  }

  method <- match.arg(method, c("spi", "deficit"))

  # Agr\u00e9gation mensuelle des pr\u00e9cipitations
  monthly_prec <- aggregate(prec ~ year + month, data = data, FUN = sum, na.rm = TRUE)
  monthly_prec <- monthly_prec[order(monthly_prec$year, monthly_prec$month), ]

  if (method == "spi") {
    # SPI simplifi\u00e9 : standardisation par mois
    monthly_prec$drought_index <- NA_real_

    for (m in 1:12) {
      idx <- monthly_prec$month == m

      # Utiliser la p\u00e9riode de r\u00e9f\u00e9rence si d\u00e9finie
      if (!is.null(ref_period)) {
        ref_idx <- idx & monthly_prec$year >= ref_period[1] &
                          monthly_prec$year <= ref_period[2]
        ref_mean <- mean(monthly_prec$prec[ref_idx], na.rm = TRUE)
        ref_sd   <- stats::sd(monthly_prec$prec[ref_idx], na.rm = TRUE)
      } else {
        ref_mean <- mean(monthly_prec$prec[idx], na.rm = TRUE)
        ref_sd   <- stats::sd(monthly_prec$prec[idx], na.rm = TRUE)
      }

      if (!is.na(ref_sd) && ref_sd > 0) {
        monthly_prec$drought_index[idx] <-
          (monthly_prec$prec[idx] - ref_mean) / ref_sd
      }
    }

  } else if (method == "deficit") {
    # D\u00e9ficit de pr\u00e9cipitations (anomalie en mm)
    monthly_prec$drought_index <- NA_real_

    for (m in 1:12) {
      idx <- monthly_prec$month == m
      ref_mean <- mean(monthly_prec$prec[idx], na.rm = TRUE)
      monthly_prec$drought_index[idx] <- monthly_prec$prec[idx] - ref_mean
    }
  }

  # Classification de la s\u00e9cheresse (bas\u00e9e sur SPI ou z-score)
  monthly_prec$drought_class <- cut(
    monthly_prec$drought_index,
    breaks = c(-Inf, -2, -1.5, -1, Inf),
    labels = c("extr\u00eame", "s\u00e9v\u00e8re", "mod\u00e9r\u00e9e", "normale"),
    right  = TRUE
  )

  row.names(monthly_prec) <- NULL
  message("Indice de s\u00e9cheresse calcul\u00e9 : m\u00e9thode = ", method,
          ", ", nrow(monthly_prec), " observations.")
  return(monthly_prec)
}
