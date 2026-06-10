#' Calculer les indices de vagues de chaleur
#'
#' @description
#' Calcule plusieurs indices thermiques permettant d'identifier et quantifier
#' les vagues de chaleur : nombre de jours chauds, durée des vagues de chaleur
#' et température maximale moyenne par période.
#'
#' @param data Dataframe climatique contenant au minimum les colonnes
#'   \code{date}, \code{year}, \code{month} et \code{tmax}.
#' @param threshold Seuil de température (°C) définissant un jour chaud.
#'   Par défaut : 35°C.
#' @param min_duration Nombre minimal de jours consécutifs pour définir une
#'   vague de chaleur. Par défaut : 3 jours.
#' @param period Période de calcul : \code{"annual"} (défaut) ou
#'   \code{"seasonal"}.
#' @param season Si \code{period = "seasonal"}, saison à analyser :
#'   \code{"summer"} (mois 6-8), \code{"spring"} (mois 3-5),
#'   \code{"autumn"} (mois 9-11), \code{"winter"} (mois 12,1,2).
#'
#' @return Un dataframe avec les colonnes :
#' \describe{
#'   \item{year}{Année}
#'   \item{n_hot_days}{Nombre de jours dépassant le seuil}
#'   \item{n_heatwaves}{Nombre de vagues de chaleur}
#'   \item{max_duration}{Durée maximale d'une vague (jours)}
#'   \item{mean_tmax}{Température maximale moyenne (°C)}
#' }
#'
#' @examples
#' data(climate_morocco)
#' hw <- calculate_heatwave_index(climate_morocco, threshold = 38)
#' head(hw)
#'
#' # Analyse saisonnière estivale
#' hw_summer <- calculate_heatwave_index(climate_morocco,
#'                                        threshold = 38,
#'                                        period = "seasonal",
#'                                        season = "summer")
#'
#' @export
calculate_heatwave_index <- function(data,
                                      threshold = 35,
                                      min_duration = 3,
                                      period = "annual",
                                      season = "summer") {

  # V\u00e9rifications
  required_cols <- c("date", "year", "month", "tmax")
  missing <- setdiff(required_cols, names(data))
  if (length(missing) > 0) {
    stop("Colonnes manquantes : ", paste(missing, collapse = ", "))
  }

  period <- match.arg(period, c("annual", "seasonal"))

  # Filtre saisonnier si n\u00e9cessaire
  if (period == "seasonal") {
    season_months <- switch(season,
      "summer" = 6:8,
      "spring" = 3:5,
      "autumn" = 9:11,
      "winter" = c(12, 1, 2),
      stop("Saison inconnue. Choisir parmi: summer, spring, autumn, winter")
    )
    data <- data[data$month %in% season_months, ]
  }

  # Groupement par ann\u00e9e
  years <- unique(data$year)

  results <- lapply(years, function(yr) {
    yr_data <- data[data$year == yr, ]
    yr_data <- yr_data[order(yr_data$date), ]

    # Jours chauds
    hot_days <- !is.na(yr_data$tmax) & yr_data$tmax >= threshold
    n_hot_days <- sum(hot_days, na.rm = TRUE)

    # D\u00e9tection des vagues de chaleur (s\u00e9quences cons\u00e9cutives)
    runs <- rle(hot_days)
    hw_lengths <- runs$lengths[runs$values == TRUE]
    hw_valid <- hw_lengths[hw_lengths >= min_duration]

    n_heatwaves <- length(hw_valid)
    max_duration <- if (length(hw_valid) > 0) max(hw_valid) else 0L

    data.frame(
      year         = yr,
      n_hot_days   = n_hot_days,
      n_heatwaves  = n_heatwaves,
      max_duration = max_duration,
      mean_tmax    = round(mean(yr_data$tmax, na.rm = TRUE), 2)
    )
  })

  result_df <- do.call(rbind, results)
  row.names(result_df) <- NULL

  message("Indices de chaleur calcul\u00e9s pour ", length(years),
          " ann\u00e9es (seuil = ", threshold, "\u00b0C).")
  return(result_df)
}
