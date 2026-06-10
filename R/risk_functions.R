#' Classifier les régions selon le risque climatique
#'
#' @description
#' Utilise un clustering K-means pour regrouper les régions géographiques
#' selon leur niveau de risque climatique combiné (chaleur, sécheresse, gel).
#'
#' @param exposure_data Dataframe avec colonnes \code{region}, \code{heat_score},
#'   \code{drought_score} et \code{frost_score} (valeurs numériques 0-1).
#' @param n_clusters Nombre de groupes de vulnérabilité. Par défaut : 3.
#' @param seed Graine aléatoire pour la reproductibilité. Par défaut : 42.
#'
#' @return Un dataframe avec une colonne supplémentaire \code{risk_cluster}
#'   indiquant le groupe d'appartenance (1 = faible risque, n = risque élevé).
#'
#' @examples
#' # Données simulées
#' set.seed(42)
#' df <- data.frame(
#'   region = paste0("Region_", 1:20),
#'   heat_score    = runif(20, 0, 1),
#'   drought_score = runif(20, 0, 1),
#'   frost_score   = runif(20, 0, 0.5)
#' )
#' clustered <- cluster_risk_regions(df, n_clusters = 3)
#' table(clustered$risk_cluster)
#'
#' @export
cluster_risk_regions <- function(exposure_data,
                                  n_clusters = 3,
                                  seed = 42) {

  required_cols <- c("region", "heat_score", "drought_score", "frost_score")
  missing <- setdiff(required_cols, names(exposure_data))
  if (length(missing) > 0) {
    stop("Colonnes manquantes : ", paste(missing, collapse = ", "))
  }

  # Matrice de clustering (variables standardis\u00e9es)
  clust_matrix <- scale(exposure_data[, c("heat_score", "drought_score", "frost_score")])

  # K-means
  set.seed(seed)
  km <- stats::kmeans(clust_matrix, centers = n_clusters, nstart = 25)

  exposure_data$risk_cluster_raw <- km$cluster

  # R\u00e9ordonnancement : cluster 1 = risque le plus bas
  cluster_means <- tapply(
    rowSums(exposure_data[, c("heat_score", "drought_score", "frost_score")]),
    km$cluster,
    mean
  )
  cluster_order <- order(cluster_means)
  mapping <- setNames(seq_along(cluster_order), cluster_order)
  exposure_data$risk_cluster <- mapping[as.character(exposure_data$risk_cluster_raw)]
  exposure_data$risk_cluster_raw <- NULL

  # \u00c9tiquette de risque
  exposure_data$risk_label <- cut(
    exposure_data$risk_cluster,
    breaks = c(0, 1, 2, Inf),
    labels = c("faible", "moyen", "\u00e9lev\u00e9"),
    right  = TRUE
  )
  if (n_clusters > 3) {
    exposure_data$risk_label <- paste("Groupe", exposure_data$risk_cluster)
  }

  message("Clustering termin\u00e9 : ", n_clusters, " groupes identifi\u00e9s.")
  message("Distribution : ", paste(table(exposure_data$risk_cluster), collapse = " | "))
  return(exposure_data)
}


#' Calculer l'indice d'exposition climatique
#'
#' @description
#' Calcule un indice composite d'exposition climatique en combinant les scores
#' de chaleur, de sécheresse et de gel, pondérés par l'importance agricole.
#' Produit trois classes d'exposition : faible, moyen, élevé.
#'
#' @param heatwave_df Dataframe résultat de \code{\link{calculate_heatwave_index}}.
#' @param drought_df Dataframe résultat de \code{\link{calculate_drought_index}}.
#' @param frost_df Dataframe résultat de \code{\link{calculate_frost_risk}}.
#' @param w_heat Poids de l'indice chaleur (entre 0 et 1). Par défaut : 0.4.
#' @param w_drought Poids de l'indice sécheresse. Par défaut : 0.4.
#' @param w_frost Poids de l'indice gel. Par défaut : 0.2.
#' @param agri_importance Importance agricole de la zone (0-1). Par défaut : 1.
#'
#' @return Un dataframe avec les colonnes :
#' \describe{
#'   \item{year}{Année}
#'   \item{heat_score}{Score normalisé chaleur (0-1)}
#'   \item{drought_score}{Score normalisé sécheresse (0-1)}
#'   \item{frost_score}{Score normalisé gel (0-1)}
#'   \item{exposure_index}{Indice composite (0-1)}
#'   \item{exposure_class}{Classe : "faible", "moyen", "élevé"}
#' }
#'
#' @examples
#' data(climate_morocco)
#' hw <- calculate_heatwave_index(climate_morocco, threshold = 38)
#' dr <- calculate_drought_index(climate_morocco)
#' fr <- calculate_frost_risk(climate_morocco)
#' expo <- calculate_exposure_index(hw, dr, fr)
#' head(expo)
#'
#' @export
calculate_exposure_index <- function(heatwave_df,
                                      drought_df,
                                      frost_df,
                                      w_heat   = 0.4,
                                      w_drought = 0.4,
                                      w_frost  = 0.2,
                                      agri_importance = 1) {

  # V\u00e9rification des poids
  if (abs(w_heat + w_drought + w_frost - 1) > 0.001) {
    stop("Les poids doivent sommer \u00e0 1. Actuellement : ",
         w_heat + w_drought + w_frost)
  }

  # Normalisation min-max interne
  .normalize <- function(x) {
    rng <- range(x, na.rm = TRUE)
    if (diff(rng) == 0) return(rep(0.5, length(x)))
    (x - rng[1]) / diff(rng)
  }

  # Score chaleur : bas\u00e9 sur n_hot_days
  heat_annual <- heatwave_df[, c("year", "n_hot_days")]
  heat_annual$heat_score <- .normalize(heat_annual$n_hot_days)

  # Score s\u00e9cheresse : proportion de mois en s\u00e9cheresse mod\u00e9r\u00e9e+
  drought_df$is_dry <- drought_df$drought_class %in%
                         c("mod\u00e9r\u00e9e", "s\u00e9v\u00e8re", "extr\u00eame")
  drought_annual <- aggregate(is_dry ~ year, data = drought_df,
                              FUN = mean, na.rm = TRUE)
  names(drought_annual)[2] <- "drought_score"

  # Score gel : bas\u00e9 sur n_frost_days
  frost_annual <- frost_df[, c("year", "n_frost_days")]
  frost_annual$frost_score <- .normalize(frost_annual$n_frost_days)

  # Fusion par ann\u00e9e
  combined <- merge(heat_annual[, c("year", "heat_score")],
                    drought_annual, by = "year", all = TRUE)
  combined <- merge(combined,
                    frost_annual[, c("year", "frost_score")],
                    by = "year", all = TRUE)

  # Remplacer les NA par 0
  combined[is.na(combined)] <- 0

  # Indice composite pond\u00e9r\u00e9
  combined$exposure_index <- agri_importance * (
    w_heat    * combined$heat_score +
    w_drought * combined$drought_score +
    w_frost   * combined$frost_score
  )

  # Classification
  q33 <- stats::quantile(combined$exposure_index, 0.33, na.rm = TRUE)
  q67 <- stats::quantile(combined$exposure_index, 0.67, na.rm = TRUE)

  combined$exposure_class <- cut(
    combined$exposure_index,
    breaks = c(-Inf, q33, q67, Inf),
    labels = c("faible", "moyen", "\u00e9lev\u00e9"),
    right  = TRUE
  )

  combined <- combined[order(combined$year), ]
  row.names(combined) <- NULL

  message("Indice d'exposition calcul\u00e9 pour ", nrow(combined), " ann\u00e9es.")
  return(combined)
}
