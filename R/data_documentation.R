#' Données climatiques simulées pour le Maroc (1981–2020)
#'
#' @description
#' Jeu de données climatiques journalières simulées représentant les conditions
#' typiques du Maroc sur la période 1981–2020, avec une tendance de réchauffement
#' réaliste de +2.5°C sur la période.
#'
#' @format Un dataframe de 14 610 lignes et 6 colonnes :
#' \describe{
#'   \item{date}{Date au format Date (YYYY-MM-DD)}
#'   \item{year}{Année (1981 à 2020)}
#'   \item{month}{Mois (1 à 12)}
#'   \item{tmax}{Température maximale journalière (°C)}
#'   \item{tmin}{Température minimale journalière (°C)}
#'   \item{prec}{Précipitations journalières (mm)}
#' }
#'
#' @details
#' Les données sont simulées avec un signal saisonnier méditerranéen réaliste :
#' \itemize{
#'   \item Été chaud et sec (juin–septembre)
#'   \item Hiver doux et pluvieux (novembre–février)
#'   \item Tendance de réchauffement de +2.5°C sur 40 ans
#'   \item Régime pluviométrique avec saisonnalité marquée
#' }
#'
#' @source Données simulées à des fins pédagogiques.
#'   Pour des données réelles, consulter WorldClim (\url{https://worldclim.org})
#'   ou ERA5 (\url{https://cds.climate.copernicus.eu}).
#'
#' @examples
#' data(climate_morocco)
#' head(climate_morocco)
#' summary(climate_morocco[, c("tmax", "tmin", "prec")])
"climate_morocco"


#' Zones agricoles du Maroc avec scores de risque climatique
#'
#' @description
#' Dataframe des principales zones agricoles marocaines avec leurs coordonnées
#' géographiques, les cultures dominantes et les scores de risque climatique
#' calculés pour chaque type d'extrême.
#'
#' @format Un dataframe de 10 lignes et 9 colonnes :
#' \describe{
#'   \item{region}{Nom de la région agricole}
#'   \item{lon}{Longitude (degrés décimaux)}
#'   \item{lat}{Latitude (degrés décimaux)}
#'   \item{heat_score}{Score de risque chaleur (0 = faible, 1 = élevé)}
#'   \item{drought_score}{Score de risque sécheresse (0–1)}
#'   \item{frost_score}{Score de risque gel tardif (0–1)}
#'   \item{agri_area_kha}{Surface agricole utile (milliers d'hectares)}
#'   \item{main_crop}{Culture principale de la zone}
#'   \item{exposure_index}{Indice d'exposition composite (0–1)}
#' }
#'
#' @source Données simulées à des fins pédagogiques.
#'
#' @examples
#' data(agri_zones_morocco)
#' head(agri_zones_morocco)
"agri_zones_morocco"
