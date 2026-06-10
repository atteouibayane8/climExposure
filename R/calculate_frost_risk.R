#' Calculer le risque de gel tardif
#'
#' @description
#' Analyse le risque de gel tardif printanier, particulièrement critique
#' pour les cultures sensibles comme la vigne, l'olivier et les céréales.
#'
#' @param data Dataframe climatique avec colonnes \code{date}, \code{year},
#'   \code{month} et \code{tmin}.
#' @param frost_threshold Température seuil (°C) définissant le gel.
#'   Par défaut : 0°C.
#' @param spring_months Mois de la période printanière sensible.
#'   Par défaut : \code{c(3, 4, 5)} (mars-mai).
#' @param crop Culture analysée pour adapter la période critique.
#'   Options : \code{"wheat"} (blé), \code{"maize"} (maïs),
#'   \code{"olive"} (olivier), \code{"vine"} (vigne).
#'
#' @return Un dataframe avec les colonnes :
#' \describe{
#'   \item{year}{Année}
#'   \item{n_frost_days}{Nombre de jours de gel printaniers}
#'   \item{last_frost_doy}{Dernier jour de gel (jour de l'année)}
#'   \item{frost_frequency}{Fréquence relative des années avec gel}
#'   \item{frost_risk}{Niveau de risque : "faible", "moyen", "élevé"}
#' }
#'
#' @examples
#' data(climate_morocco)
#' frost <- calculate_frost_risk(climate_morocco, crop = "vine")
#' head(frost)
#'
#' @export
calculate_frost_risk <- function(data,
                                  frost_threshold = 0,
                                  spring_months = c(3, 4, 5),
                                  crop = NULL) {

  required_cols <- c("date", "year", "month", "tmin")
  missing <- setdiff(required_cols, names(data))
  if (length(missing) > 0) {
    stop("Colonnes manquantes : ", paste(missing, collapse = ", "))
  }

  # Adaptation des mois critiques par culture
  if (!is.null(crop)) {
    crop_months <- list(
      wheat  = c(3, 4),       # bl\u00e9 : gel en floraison mars-avril
      maize  = c(4, 5),       # maïs : gel \u00e0 la lev\u00e9e avril-mai
      olive  = c(3, 4, 5),    # olivier : floraison printemps
      vine   = c(4, 5, 6)     # vigne : d\u00e9bourrement avril-juin
    )
    if (crop %in% names(crop_months)) {
      spring_months <- crop_months[[crop]]
      message("P\u00e9riode critique pour ", crop, " : mois ",
              paste(spring_months, collapse = ", "))
    }
  }

  # Filtrage sur la p\u00e9riode printani\u00e8re
  spring_data <- data[data$month %in% spring_months, ]

  years <- sort(unique(spring_data$year))

  results <- lapply(years, function(yr) {
    yr_data <- spring_data[spring_data$year == yr, ]
    yr_data <- yr_data[order(yr_data$date), ]

    frost_days <- !is.na(yr_data$tmin) & yr_data$tmin <= frost_threshold
    n_frost_days <- sum(frost_days, na.rm = TRUE)

    # Dernier jour de gel (jour de l'ann\u00e9e)
    if (n_frost_days > 0) {
      last_frost_date <- max(yr_data$date[frost_days], na.rm = TRUE)
      last_frost_doy  <- as.integer(format(last_frost_date, "%j"))
    } else {
      last_frost_doy <- 0L
    }

    data.frame(
      year         = yr,
      n_frost_days = n_frost_days,
      last_frost_doy = last_frost_doy
    )
  })

  result_df <- do.call(rbind, results)

  # Fr\u00e9quence des ann\u00e9es avec gel
  frost_freq <- mean(result_df$n_frost_days > 0, na.rm = TRUE)
  result_df$frost_frequency <- frost_freq

  # Niveau de risque
  result_df$frost_risk <- cut(
    result_df$n_frost_days,
    breaks = c(-Inf, 0, 3, Inf),
    labels = c("faible", "moyen", "\u00e9lev\u00e9"),
    right  = TRUE
  )

  row.names(result_df) <- NULL
  message("Risque de gel calcul\u00e9 pour ", length(years), " ann\u00e9es.")
  return(result_df)
}
