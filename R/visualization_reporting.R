#' Cartographier le risque climatique
#'
#' @description
#' Produit une carte de chaleur (heatmap) du risque climatique a partir
#' d'un dataframe de regions avec leurs coordonnees et scores de risque.
#'
#' @param risk_data Dataframe avec colonnes \code{region}, \code{lon},
#'   \code{lat} et au moins une variable de risque.
#' @param variable Variable a cartographier. Par defaut : \code{"exposure_index"}.
#' @param title Titre de la carte.
#' @param output_file Chemin de sortie pour sauvegarder la carte (PNG ou PDF).
#'   Si \code{NULL}, le graphique est affiche sans sauvegarde.
#' @param palette Palette de couleurs : \code{"RdYlGn"} (defaut) ou \code{"Blues"}.
#'
#' @return Un objet ggplot2 (invisible).
#'
#' @examples
#' data(agri_zones_morocco)
#' p <- plot_climate_risk_map(agri_zones_morocco,
#'                             variable = "exposure_index",
#'                             title = "Exposition climatique au Maroc")
#' print(p)
#'
#' @importFrom rlang .data
#' @export
plot_climate_risk_map <- function(risk_data,
                                  variable = "exposure_index",
                                  title = "Carte du risque climatique",
                                  output_file = NULL,
                                  palette = "RdYlGn") {

  if (!variable %in% names(risk_data)) {
    stop("Variable '", variable, "' absente du dataframe.")
  }
  if (!all(c("lon", "lat") %in% names(risk_data))) {
    stop("Colonnes 'lon' et 'lat' requises pour la cartographie.")
  }

  p <- ggplot2::ggplot(risk_data,
                       ggplot2::aes(x = .data[["lon"]], y = .data[["lat"]],
                                    fill = .data[[variable]])) +
    ggplot2::geom_tile(width = 0.5, height = 0.5) +
    ggplot2::scale_fill_gradientn(
      colours = if (palette == "RdYlGn") {
        rev(c("#1a9641", "#a6d96a", "#ffffbf", "#fdae61", "#d7191c"))
      } else {
        c("#deebf7", "#9ecae1", "#3182bd")
      },
      name = variable,
      na.value = "grey80"
    ) +
    ggplot2::labs(
      title   = title,
      x       = "Longitude",
      y       = "Latitude",
      fill    = variable,
      caption = paste("climExposure package |", Sys.Date())
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(hjust = 0.5, face = "bold"),
      legend.position = "right"
    )

  if (!is.null(output_file)) {
    ggplot2::ggsave(output_file, plot = p, width = 10, height = 7, dpi = 150)
    message("Carte sauvegardee : ", output_file)
  } else {
    print(p)
  }

  invisible(p)
}


#' Visualiser les series temporelles climatiques
#'
#' @description
#' Produit des graphiques de tendances climatiques (temperatures, secheresse,
#' vagues de chaleur) avec ajustement de tendance lineaire.
#'
#' @param data Dataframe avec colonnes \code{year} et une colonne numerique
#'   de valeurs. La colonne de valeurs peut s'appeler \code{tmax},
#'   \code{n_hot_days}, \code{drought_index}, \code{exposure_index},
#'   ou n'importe quel autre nom numerique (la deuxieme colonne numerique
#'   est utilisee en dernier recours).
#' @param type Type de graphique : \code{"temperature"}, \code{"heatwave"},
#'   \code{"drought"} ou \code{"exposure"}.
#' @param title Titre du graphique.
#' @param show_trend Afficher la droite de tendance. Par defaut : \code{TRUE}.
#' @param output_file Chemin de sortie. Si \code{NULL}, affichage direct.
#'
#' @return Un objet ggplot2 (invisible).
#'
#' @examples
#' data(climate_morocco)
#' annual <- aggregate(tmax ~ year, data = climate_morocco, FUN = mean)
#' p <- plot_climate_timeseries(annual, type = "temperature",
#'                               title = "Evolution de Tmax au Maroc")
#'
#' @importFrom rlang .data
#' @export
plot_climate_timeseries <- function(data,
                                    type = "temperature",
                                    title = NULL,
                                    show_trend = TRUE,
                                    output_file = NULL) {

  type <- match.arg(type, c("temperature", "heatwave", "drought", "exposure"))

  # Labels et couleurs selon le type
  cfg <- list(
    temperature = list(
      candidates = c("tmax", "tmin", "tmean", "value"),
      ylabel     = "Temperature max (degC)",
      color      = "#d7191c"
    ),
    heatwave = list(
      candidates = c("n_hot_days", "n_heatwaves", "value"),
      ylabel     = "Nombre de jours chauds",
      color      = "#ff7f00"
    ),
    drought = list(
      candidates = c("drought_index", "spi", "value"),
      ylabel     = "Indice de secheresse (SPI)",
      color      = "#8c510a"
    ),
    exposure = list(
      candidates = c("exposure_index", "value"),
      ylabel     = "Indice d'exposition (0-1)",
      color      = "#7b2d8b"
    )
  )[[type]]

  # Trouver la colonne y : chercher dans les candidats, puis fallback numerique
  y_col <- intersect(cfg$candidates, names(data))[1]

  if (is.na(y_col)) {
    # Fallback : premiere colonne numerique qui n'est pas "year"
    num_cols <- names(data)[sapply(data, is.numeric)]
    num_cols <- num_cols[num_cols != "year"]
    if (length(num_cols) == 0) {
      stop("Aucune colonne numerique trouvee dans les donnees (hors 'year').")
    }
    y_col <- num_cols[1]
    message("Colonne y choisie automatiquement : '", y_col, "'")
  }

  if (!"year" %in% names(data)) {
    stop("Colonne 'year' introuvable dans les donnees.")
  }

  if (is.null(title)) title <- paste("Evolution :", cfg$ylabel)

  p <- ggplot2::ggplot(data,
                       ggplot2::aes(x = .data[["year"]],
                                    y = .data[[y_col]])) +
    ggplot2::geom_line(color  = cfg$color, linewidth = 0.8, alpha = 0.8) +
    ggplot2::geom_point(color = cfg$color, size = 1.5,      alpha = 0.6)

  if (show_trend) {
    p <- p + ggplot2::geom_smooth(
      method   = "lm",
      formula  = y ~ x,
      se       = TRUE,
      color    = "black",
      linewidth = 0.6,
      linetype = "dashed"
    )
  }

  p <- p +
    ggplot2::labs(
      title   = title,
      x       = "Annee",
      y       = cfg$ylabel,
      caption = paste("climExposure package |", Sys.Date())
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
    )

  if (!is.null(output_file)) {
    ggplot2::ggsave(output_file, plot = p, width = 9, height = 5, dpi = 150)
    message("Graphique sauvegarde : ", output_file)
  } else {
    print(p)
  }

  invisible(p)
}


#' Comparer des scenarios climatiques
#'
#' @description
#' Compare les caracteristiques climatiques entre deux periodes (historique
#' vs futur) et calcule les anomalies.
#'
#' @param data Dataframe climatique complet avec colonne \code{year}.
#' @param baseline Vecteur \code{c(annee_debut, annee_fin)} pour la periode
#'   de reference historique.
#' @param future Vecteur \code{c(annee_debut, annee_fin)} pour la periode future.
#' @param variables Variables climatiques a comparer. Par defaut :
#'   \code{c("tmax", "tmin", "prec")}.
#'
#' @return Un dataframe de synthese des anomalies entre les deux periodes.
#'
#' @examples
#' data(climate_morocco)
#' comp <- compare_scenarios(climate_morocco,
#'                            baseline = c(1981, 2000),
#'                            future   = c(2001, 2020))
#' print(comp)
#'
#' @export
compare_scenarios <- function(data,
                              baseline  = c(1981, 2010),
                              future    = c(2011, 2040),
                              variables = c("tmax", "tmin", "prec")) {

  vars_avail <- intersect(variables, names(data))
  if (length(vars_avail) == 0) {
    stop("Aucune variable valide dans les donnees. Variables demandees : ",
         paste(variables, collapse = ", "),
         ". Colonnes disponibles : ", paste(names(data), collapse = ", "))
  }

  base_data <- data[data$year >= baseline[1] & data$year <= baseline[2], ]
  fut_data  <- data[data$year >= future[1]   & data$year <= future[2],   ]

  if (nrow(base_data) == 0) stop("Aucune donnee pour la periode de reference.")
  if (nrow(fut_data)  == 0) stop("Aucune donnee pour la periode future.")

  results <- lapply(vars_avail, function(v) {
    base_mean  <- mean(base_data[[v]], na.rm = TRUE)
    fut_mean   <- mean(fut_data[[v]],  na.rm = TRUE)
    anomaly    <- fut_mean - base_mean
    pct_change <- if (!is.na(base_mean) && base_mean != 0)
      100 * anomaly / abs(base_mean) else NA_real_

    data.frame(
      variable    = v,
      baseline    = round(base_mean, 2),
      future_mean = round(fut_mean,  2),
      anomaly     = round(anomaly,   2),
      pct_change  = round(pct_change, 1)
    )
  })

  result_df <- do.call(rbind, results)
  row.names(result_df) <- NULL

  message("Comparaison scenarios : ",
          baseline[1], "-", baseline[2], " vs ",
          future[1], "-", future[2])
  return(result_df)
}


#' Generer un atlas climatique automatique
#'
#' @description
#' Genere un rapport HTML complet comprenant les indices climatiques,
#' les tendances, les cartes de risque et les recommandations d'adaptation.
#'
#' @param climate_data Dataframe climatique de base.
#' @param exposure_df Dataframe d'indice d'exposition (resultat de
#'   \code{\link{calculate_exposure_index}}).
#' @param output_file Nom du fichier HTML de sortie. Par defaut :
#'   \code{"climate_atlas.html"}.
#' @param title Titre du rapport.
#' @param region Nom de la region etudiee.
#' @param open_browser Ouvrir le rapport dans le navigateur apres generation.
#'
#' @return Chemin du fichier HTML genere (invisible).
#'
#' @examples
#' \dontrun{
#' data(climate_morocco)
#' hw   <- calculate_heatwave_index(climate_morocco)
#' dr   <- calculate_drought_index(climate_morocco)
#' fr   <- calculate_frost_risk(climate_morocco)
#' expo <- calculate_exposure_index(hw, dr, fr)
#' generate_risk_atlas(climate_morocco, expo,
#'                     output_file = "atlas_maroc.html",
#'                     region = "Maroc")
#' }
#'
#' @importFrom stats lm coef na.omit
#' @export
generate_risk_atlas <- function(climate_data,
                                exposure_df,
                                output_file  = "climate_atlas.html",
                                title        = "Atlas des Risques Climatiques Agricoles",
                                region       = "Zone d'etude",
                                open_browser = FALSE) {

  high_risk_yrs <- sum(exposure_df$exposure_class == "\u00e9lev\u00e9", na.rm = TRUE)
  pct_high  <- round(100 * high_risk_yrs / nrow(exposure_df), 1)
  mean_expo <- round(mean(exposure_df$exposure_index, na.rm = TRUE), 3)
  year_range <- paste(range(exposure_df$year, na.rm = TRUE), collapse = " - ")

  tmax_trend <- tryCatch({
    lm_t <- stats::lm(tmax ~ year, data = climate_data,
                      na.action = stats::na.omit)
    round(stats::coef(lm_t)[2] * 10, 2)
  }, error = function(e) NA)

  # Lignes du tableau
  table_rows <- paste(sapply(seq_len(nrow(exposure_df)), function(i) {
    row <- exposure_df[i, ]
    badge_class <- switch(as.character(row$exposure_class),
                          "\u00e9lev\u00e9"  = "badge-high",
                          "moyen"            = "badge-medium",
                          "faible"           = "badge-low",
                          "badge-low")
    paste0(
      "<tr>",
      "<td>", row$year, "</td>",
      "<td>", round(row$heat_score,     3), "</td>",
      "<td>", round(row$drought_score,  3), "</td>",
      "<td>", round(row$frost_score,    3), "</td>",
      "<td>", round(row$exposure_index, 3), "</td>",
      "<td><span class='badge ", badge_class, "'>",
      row$exposure_class, "</span></td>",
      "</tr>"
    )
  }), collapse = "\n")

  tmax_label <- if (is.na(tmax_trend)) "N/A" else paste0(tmax_trend, " deg C")

  html_content <- paste0(
"<!DOCTYPE html>
<html lang='fr'>
<head>
  <meta charset='UTF-8'>
  <title>", title, "</title>
  <style>
    body{font-family:'Segoe UI',Arial,sans-serif;margin:0;background:#f5f7fa;color:#333}
    header{background:linear-gradient(135deg,#1a6b3c,#2ecc71);color:white;padding:30px 40px}
    header h1{margin:0;font-size:2em}
    header p{margin:8px 0 0;opacity:.85}
    .container{max-width:1100px;margin:30px auto;padding:0 20px}
    .card{background:white;border-radius:10px;box-shadow:0 2px 12px rgba(0,0,0,.08);
          padding:25px 30px;margin-bottom:25px}
    .card h2{margin-top:0;color:#1a6b3c;border-bottom:2px solid #eee;padding-bottom:10px}
    .kpi-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(200px,1fr));gap:15px}
    .kpi{background:#f0f9f4;border-left:5px solid #2ecc71;padding:15px 20px;border-radius:6px}
    .kpi .value{font-size:2em;font-weight:bold;color:#1a6b3c}
    .kpi .label{font-size:.9em;color:#666;margin-top:4px}
    .risk-high{background:#fde8e8;border-left-color:#e74c3c}
    .risk-medium{background:#fef9e7;border-left-color:#f39c12}
    table{width:100%;border-collapse:collapse;font-size:.95em}
    th{background:#1a6b3c;color:white;padding:10px 14px;text-align:left}
    td{padding:9px 14px;border-bottom:1px solid #eee}
    tr:hover{background:#f8fffe}
    .badge{display:inline-block;padding:3px 10px;border-radius:12px;
           font-size:.82em;font-weight:bold}
    .badge-high{background:#fde8e8;color:#c0392b}
    .badge-medium{background:#fef9e7;color:#d35400}
    .badge-low{background:#eafaf1;color:#27ae60}
    footer{text-align:center;padding:20px;color:#999;font-size:.85em}
  </style>
</head>
<body>
<header>
  <h1>", title, "</h1>
  <p>", region, " &#8212; ", year_range, " &#8212; ",
    format(Sys.Date(), "%d/%m/%Y"), "</p>
</header>
<div class='container'>
  <div class='card'>
    <h2>Indicateurs Cles</h2>
    <div class='kpi-grid'>
      <div class='kpi risk-high'>
        <div class='value'>", pct_high, "%</div>
        <div class='label'>Annees a exposition elevee</div>
      </div>
      <div class='kpi risk-medium'>
        <div class='value'>", mean_expo, "</div>
        <div class='label'>Indice d'exposition moyen</div>
      </div>
      <div class='kpi'>
        <div class='value'>", tmax_label, "</div>
        <div class='label'>Tendance Tmax / decennie</div>
      </div>
      <div class='kpi'>
        <div class='value'>", nrow(exposure_df), "</div>
        <div class='label'>Annees analysees</div>
      </div>
    </div>
  </div>
  <div class='card'>
    <h2>Indice d'Exposition par Annee</h2>
    <table>
      <tr><th>Annee</th><th>Chaleur</th><th>Secheresse</th>
          <th>Gel</th><th>Exposition</th><th>Classe</th></tr>
", table_rows, "
    </table>
  </div>
  <div class='card'>
    <h2>Recommandations d'Adaptation</h2>
    <ul>
      <li><strong>Varietes resistantes :</strong> Adopter des varietes tolerant
          les fortes chaleurs (ble dur, mais a cycle court, vigne adaptee).</li>
      <li><strong>Gestion de l'eau :</strong> Installer des systemes d'irrigation
          economes (goutte-a-goutte).</li>
      <li><strong>Systemes d'alerte :</strong> Alertes precoces pour vagues de
          chaleur et gelees tardives.</li>
      <li><strong>Agroforesterie :</strong> Introduire des arbres pour attenuer
          les effets thermiques.</li>
      <li><strong>Diversification :</strong> Diversifier les cultures pour reduire
          la vulnerabilite globale.</li>
    </ul>
  </div>
</div>
<footer>Rapport genere par <strong>climExposure</strong> v0.1.0</footer>
</body></html>")

  writeLines(html_content, output_file)
  message("Atlas climatique genere : ", output_file)

  if (open_browser && interactive()) {
    utils::browseURL(output_file)
  }

  invisible(output_file)
}
