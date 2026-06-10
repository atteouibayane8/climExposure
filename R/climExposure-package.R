#' climExposure: Exposition des Systèmes Agricoles aux Extrêmes Climatiques
#'
#' @description
#' Le package \pkg{climExposure} fournit des outils pour analyser l'exposition
#' des cultures agricoles aux événements climatiques extrêmes. Il permet :
#' \itemize{
#'   \item D'importer et traiter des données climatiques (CSV, NetCDF)
#'   \item De calculer des indices climatiques (chaleur, sécheresse, gel)
#'   \item D'identifier les zones à risque
#'   \item De produire des cartes et rapports automatiques
#' }
#'
#' @section Fonctions principales:
#' \describe{
#'   \item{\code{\link{import_climate_data}}}{Importation des données climatiques}
#'   \item{\code{\link{calculate_heatwave_index}}}{Calcul des indices de chaleur}
#'   \item{\code{\link{calculate_drought_index}}}{Calcul des indices de sécheresse}
#'   \item{\code{\link{calculate_frost_risk}}}{Analyse du risque de gel tardif}
#'   \item{\code{\link{summarize_extremes}}}{Résumé statistique des extrêmes}
#'   \item{\code{\link{analyze_trends}}}{Analyse des tendances climatiques}
#'   \item{\code{\link{cluster_risk_regions}}}{Classification des régions à risque}
#'   \item{\code{\link{calculate_exposure_index}}}{Indice d'exposition climatique}
#'   \item{\code{\link{compare_scenarios}}}{Comparaison de scénarios climatiques}
#'   \item{\code{\link{plot_climate_risk_map}}}{Cartographie du risque climatique}
#'   \item{\code{\link{plot_climate_timeseries}}}{Visualisation temporelle}
#'   \item{\code{\link{generate_risk_atlas}}}{Génération d'atlas climatique}
#' }
#'
#' @docType package
#' @name climExposure-package
#' @aliases climExposure
"_PACKAGE"
#' @importFrom stats aggregate coef na.omit setNames lm sd median quantile kmeans
#' @importFrom rlang .data
NULL
