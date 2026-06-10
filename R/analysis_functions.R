#' Résumer les extrêmes climatiques
#'
#' @description
#' Produit un tableau de synthèse statistique des indices d'extrêmes
#' climatiques : fréquence, intensité, durée et anomalies.
#'
#' @param heatwave_df Dataframe résultat de \code{\link{calculate_heatwave_index}}.
#' @param drought_df Dataframe résultat de \code{\link{calculate_drought_index}}.
#' @param frost_df Dataframe résultat de \code{\link{calculate_frost_risk}}.
#' @param ref_period Vecteur de deux années pour la période de référence.
#'   Ex : \code{c(1981, 2010)}.
#'
#' @return Un dataframe de synthèse avec des statistiques clés par indice.
#'
#' @examples
#' data(climate_morocco)
#' hw <- calculate_heatwave_index(climate_morocco, threshold = 38)
#' dr <- calculate_drought_index(climate_morocco)
#' fr <- calculate_frost_risk(climate_morocco)
#' summary_df <- summarize_extremes(hw, dr, fr)
#' print(summary_df)
#'
#' @export
summarize_extremes <- function(heatwave_df = NULL,
                                drought_df  = NULL,
                                frost_df    = NULL,
                                ref_period  = NULL) {

  summary_list <- list()

  # R\u00e9sum\u00e9 chaleur
  if (!is.null(heatwave_df)) {
    hw_summary <- data.frame(
      indice        = "Chaleur",
      moyenne       = round(mean(heatwave_df$n_hot_days,   na.rm = TRUE), 1),
      maximum       = max(heatwave_df$n_hot_days,          na.rm = TRUE),
      sd            = round(stats::sd(heatwave_df$n_hot_days, na.rm = TRUE), 1),
      annees_risque = sum(heatwave_df$n_heatwaves > 0,     na.rm = TRUE),
      tendance      = .compute_trend(heatwave_df$year, heatwave_df$n_hot_days)
    )
    summary_list[["chaleur"]] <- hw_summary
  }

  # R\u00e9sum\u00e9 s\u00e9cheresse (nombre de mois en s\u00e9cheresse mod\u00e9r\u00e9e+)
  if (!is.null(drought_df) && "drought_class" %in% names(drought_df)) {
    dr_severe <- drought_df[drought_df$drought_class %in%
                              c("mod\u00e9r\u00e9e", "s\u00e9v\u00e8re", "extr\u00eame"), ]
    dr_annual <- aggregate(drought_index ~ year, data = drought_df,
                           FUN = function(x) mean(x, na.rm = TRUE))

    dr_summary <- data.frame(
      indice        = "S\u00e9cheresse",
      moyenne       = round(mean(dr_annual$drought_index, na.rm = TRUE), 2),
      maximum       = round(min(drought_df$drought_index, na.rm = TRUE), 2),
      sd            = round(stats::sd(dr_annual$drought_index, na.rm = TRUE), 2),
      annees_risque = length(unique(dr_severe$year)),
      tendance      = .compute_trend(dr_annual$year, dr_annual$drought_index)
    )
    summary_list[["secheresse"]] <- dr_summary
  }

  # R\u00e9sum\u00e9 gel
  if (!is.null(frost_df)) {
    fr_summary <- data.frame(
      indice        = "Gel tardif",
      moyenne       = round(mean(frost_df$n_frost_days,  na.rm = TRUE), 1),
      maximum       = max(frost_df$n_frost_days,          na.rm = TRUE),
      sd            = round(stats::sd(frost_df$n_frost_days, na.rm = TRUE), 1),
      annees_risque = sum(frost_df$n_frost_days > 0,      na.rm = TRUE),
      tendance      = .compute_trend(frost_df$year, frost_df$n_frost_days)
    )
    summary_list[["gel"]] <- fr_summary
  }

  if (length(summary_list) == 0) {
    stop("Aucun dataframe d'indice fourni.")
  }

  result <- do.call(rbind, summary_list)
  row.names(result) <- NULL
  return(result)
}

# Calcul interne de la tendance lin\u00e9aire (valeur de la pente)
.compute_trend <- function(x, y) {
  if (length(x) < 3 || all(is.na(y))) return(NA_real_)
  tryCatch({
    coef(stats::lm(y ~ x, na.action = na.omit))[2]
  }, error = function(e) NA_real_)
}


#' Analyser les tendances climatiques
#'
#' @description
#' Analyse les tendances temporelles des températures et de la sécheresse
#' par régression linéaire simple et calcul d'anomalies décennales.
#'
#' @param data Dataframe climatique avec colonnes \code{year} et variables
#'   climatiques (\code{tmax}, \code{tmin}, \code{prec}).
#' @param variable Variable à analyser : \code{"tmax"}, \code{"tmin"},
#'   \code{"prec"} ou \code{"tmean"}.
#' @param ref_period Vecteur \code{c(annee_debut, annee_fin)} pour les
#'   anomalies. Si \code{NULL}, utilise la moyenne globale.
#'
#' @return Une liste contenant :
#' \describe{
#'   \item{annual_values}{Valeurs annuelles de la variable}
#'   \item{trend}{Pente de la tendance (unité/an)}
#'   \item{trend_significance}{p-value du test de tendance}
#'   \item{anomalies}{Anomalies par rapport à la référence}
#'   \item{decadal}{Moyennes décennales}
#' }
#'
#' @examples
#' data(climate_morocco)
#' trend_tmax <- analyze_trends(climate_morocco, variable = "tmax",
#'                               ref_period = c(1981, 2010))
#' print(trend_tmax$trend)
#'
#' @export
analyze_trends <- function(data,
                            variable  = "tmax",
                            ref_period = NULL) {

  if (!variable %in% names(data) && variable != "tmean") {
    stop("Variable '", variable, "' introuvable dans les donn\u00e9es.")
  }

  # Calcul de tmean si demand\u00e9
  if (variable == "tmean") {
    if (!all(c("tmax", "tmin") %in% names(data))) {
      stop("tmax et tmin sont n\u00e9cessaires pour calculer tmean.")
    }
    data$tmean <- (data$tmax + data$tmin) / 2
  }

  # Agr\u00e9gation annuelle
  annual <- aggregate(data[[variable]] ~ data$year,
                      FUN = mean, na.rm = TRUE)
  names(annual) <- c("year", variable)

  # R\u00e9gression lin\u00e9aire pour la tendance
  lm_fit <- stats::lm(annual[[variable]] ~ annual$year, na.action = na.omit)
  trend_slope <- coef(lm_fit)[2]
  trend_pval  <- summary(lm_fit)$coefficients[2, 4]

  # Anomalies par rapport \u00e0 la r\u00e9f\u00e9rence
  if (!is.null(ref_period)) {
    ref_data <- annual[annual$year >= ref_period[1] &
                         annual$year <= ref_period[2], variable]
    ref_mean <- mean(ref_data, na.rm = TRUE)
  } else {
    ref_mean <- mean(annual[[variable]], na.rm = TRUE)
  }
  annual$anomaly <- annual[[variable]] - ref_mean

  # Moyennes d\u00e9cennales
  annual$decade <- floor(annual$year / 10) * 10
  decadal <- aggregate(annual[[variable]] ~ annual$decade,
                       FUN = mean, na.rm = TRUE)
  names(decadal) <- c("decade", paste0(variable, "_mean"))

  message("Tendance de ", variable, " : ",
          round(trend_slope * 10, 3), " \u00b0C/d\u00e9cennie",
          " (p = ", round(trend_pval, 4), ")")

  return(list(
    annual_values        = annual,
    trend                = round(trend_slope, 5),
    trend_per_decade     = round(trend_slope * 10, 3),
    trend_significance   = round(trend_pval, 4),
    reference_mean       = round(ref_mean, 2),
    decadal              = decadal
  ))
}

