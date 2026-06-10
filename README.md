# climExposure <img src="man/figures/logo.png" align="right" height="100" alt=""/>

<!-- badges: start -->
[![R-CMD-check](https://github.com/atteouibayane8/climExposure/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/atteouibayane8/climExposure/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R version](https://img.shields.io/badge/R-%3E%3D%204.0.0-blue.svg)](https://cran.r-project.org/)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

> **Analyse de l'Exposition des Systèmes Agricoles aux Extrêmes Climatiques**

`climExposure` est un package R conçu pour analyser et quantifier l'exposition
des cultures agricoles aux événements climatiques extrêmes. Il fournit des outils
complets pour calculer des indices de risque (chaleur, sécheresse, gel tardif),
identifier les zones vulnérables, visualiser les tendances et générer des rapports
automatiques.

---

## Table des matières

- [Contexte scientifique](#contexte-scientifique)
- [Installation](#installation)
- [Structure du package](#structure-du-package)
- [Données incluses](#données-incluses)
- [Fonctions principales](#fonctions-principales)
- [Utilisation rapide](#utilisation-rapide)
- [Flux de travail complet](#flux-de-travail-complet)
- [Visualisations](#visualisations)
- [Tests unitaires](#tests-unitaires)
- [Sources de données recommandées](#sources-de-données-recommandées)
- [Références scientifiques](#références-scientifiques)
- [Licence](#licence)
- [Contact](#contact)

---

## Contexte scientifique

Le changement climatique expose les systèmes agricoles à des risques croissants :
vagues de chaleur plus fréquentes, sécheresses prolongées, et décalage des
périodes de gel tardif. Ce package permet d'évaluer ces risques de manière
quantitative et reproductible à partir de données climatiques journalières.

### Cultures supportées

| Culture | Code | Période critique de gel |
|---------|------|------------------------|
| Blé | `wheat` | Mars – Avril |
| Maïs | `maize` | Avril – Mai |
| Olivier | `olive` | Mars – Mai |
| Vigne | `vine` | Avril – Juin |

### Indices climatiques calculés

| Indice | Description | Méthode |
|--------|-------------|---------|
| Vagues de chaleur | Séquences de jours > seuil | Comptage + RLE |
| SPI | Standardized Precipitation Index | Standardisation mensuelle |
| Déficit précipitations | Anomalie par rapport à la moyenne | Écart à la moyenne |
| Gel tardif | Jours de gel en période sensible | Seuil Tmin ≤ 0°C |
| Exposition composite | Indice multi-risque (0–1) | Combinaison pondérée |

---

## Installation

### Depuis GitHub (recommandé)

```r
# Installer devtools si nécessaire
install.packages("devtools")

# Installer climExposure
devtools::install_github("atteouibayane8/climExposure")
```

### Depuis le code source (local)

```r
# Cloner le dépôt, puis depuis le dossier parent :
devtools::install("climExposure")
```

### Dépendances requises

```r
install.packages(c(
  "dplyr",      # Manipulation de données
  "ggplot2",    # Visualisations
  "lubridate",  # Gestion des dates
  "rlang",      # Évaluation non-standard
  "tidyr"       # Restructuration des données
))

# Optionnel — pour les données NetCDF (ERA5, CMIP6) :
install.packages("ncdf4")
```

---

## Structure du package

```
climExposure/
│
├── R/                                  # Code source des fonctions
│   ├── climExposure-package.R          # Documentation générale du package
│   ├── import_climate_data.R           # Import CSV / NetCDF
│   ├── calculate_heatwave_index.R      # Indices de vagues de chaleur
│   ├── calculate_drought_index.R       # Indices de sécheresse (SPI)
│   ├── calculate_frost_risk.R          # Risque de gel tardif
│   ├── analysis_functions.R            # Résumés et tendances climatiques
│   ├── risk_functions.R                # Clustering et indice d'exposition
│   ├── visualization_reporting.R       # Cartes, graphiques et atlas HTML
│   └── data_documentation.R            # Documentation des jeux de données
│
├── data/                               # Jeux de données compilés (.rda)
│   ├── climate_morocco.rda             # Données journalières 1981–2020
│   └── agri_zones_morocco.rda          # Zones agricoles avec scores de risque
│
├── data-raw/                           # Scripts de génération des données
│   └── generate_data.R
│
├── inst/extdata/                       # Données brutes accessibles
│   └── climate_morocco.csv             # Données au format CSV
│
├── man/                                # Documentation auto-générée (roxygen2)
│
├── tests/testthat/                     # Tests unitaires
│   └── test-climExposure.R             # 21 tests couvrant toutes les fonctions
│
├── vignettes/                          # Tutoriel reproductible
│   └── introduction-climExposure.Rmd
│
├── .github/workflows/                  # Intégration continue
│   └── R-CMD-check.yaml
│
├── DESCRIPTION                         # Métadonnées du package
├── NAMESPACE                           # Exports et imports
├── LICENSE                             # Licence MIT
└── README.md                           # Ce fichier
```

---

## Données incluses

### `climate_morocco` — Données climatiques journalières

Données simulées représentant le climat marocain sur 40 ans (1981–2020),
avec une tendance de réchauffement de +0.62°C/décennie.

```r
data("climate_morocco")
str(climate_morocco)
# 'data.frame': 14610 obs. of 6 variables:
#  $ date  : Date
#  $ year  : int  1981 1981 ...
#  $ month : int  1 1 ...
#  $ tmax  : num  température maximale (°C)
#  $ tmin  : num  température minimale (°C)
#  $ prec  : num  précipitations journalières (mm)

head(climate_morocco, 3)
#         date year month tmax  tmin prec
# 1 1981-01-01 1981     1  5.3  -3.4 55.6
# 2 1981-01-02 1981     1  6.3  -1.5 28.0
# 3 1981-01-03 1981     1 11.7   4.3  4.4
```

### `agri_zones_morocco` — Zones agricoles marocaines

10 régions agricoles avec coordonnées géographiques et scores de risque.

```r
data("agri_zones_morocco")
head(agri_zones_morocco, 3)
#        region   lon   lat heat_score drought_score frost_score agri_area_kha main_crop exposure_index
# 1 Souss-Massa  -9.5  30.3       0.89          0.72        0.12           342     olive          0.724
# 2       Haouz  -8.0  31.6       0.65          0.58        0.08           218        blé          0.504
# 3       Tadla  -6.5  32.5       0.54          0.44        0.15           445      maïs          0.414
```

---

## Fonctions principales

| Fonction | Description | Inputs | Output |
|----------|-------------|--------|--------|
| `import_climate_data()` | Import et structuration des données | CSV ou NetCDF | dataframe |
| `calculate_heatwave_index()` | Jours chauds, durée et fréquence des vagues | dataframe + seuil | dataframe |
| `calculate_drought_index()` | SPI ou déficit de précipitations | dataframe + méthode | dataframe |
| `calculate_frost_risk()` | Gel tardif par culture et par année | dataframe + culture | dataframe |
| `summarize_extremes()` | Tableau de synthèse multi-indices | 3 dataframes d'indices | dataframe |
| `analyze_trends()` | Tendance linéaire + anomalies décennales | dataframe + variable | liste |
| `cluster_risk_regions()` | Clustering K-means des zones à risque | dataframe régions | dataframe |
| `calculate_exposure_index()` | Indice composite d'exposition (0–1) | 3 indices pondérés | dataframe |
| `compare_scenarios()` | Anomalies entre deux périodes | dataframe + 2 périodes | dataframe |
| `plot_climate_risk_map()` | Carte de chaleur du risque spatial | dataframe régions | ggplot2 |
| `plot_climate_timeseries()` | Graphique de tendance temporelle | dataframe annuel | ggplot2 |
| `generate_risk_atlas()` | Rapport HTML complet automatique | données + exposition | fichier HTML |

---

## Utilisation rapide

```r
library(climExposure)

# Charger les données d'exemple
data("climate_morocco")

# Calculer les indices en 4 lignes
hw   <- calculate_heatwave_index(climate_morocco, threshold = 38)
dr   <- calculate_drought_index(climate_morocco, method = "spi")
fr   <- calculate_frost_risk(climate_morocco, crop = "vine")
expo <- calculate_exposure_index(hw, dr, fr)

# Résultat
tail(expo, 3)
#    year heat_score drought_score frost_score exposure_index exposure_class
# 38 2018      0.834         0.691       0.021          0.613          élevé
# 39 2019      0.891         0.724       0.018          0.651          élevé
# 40 2020      0.912         0.748       0.024          0.673          élevé
```

---

## Flux de travail complet

### Étape 1 — Import des données

```r
library(climExposure)

# Option A : utiliser les données d'exemple
data("climate_morocco")

# Option B : importer vos propres données CSV
df <- import_climate_data(
  file       = "mes_donnees.csv",
  format     = "csv",
  start_date = "2000-01-01",
  end_date   = "2020-12-31"
)
```

### Étape 2 — Calcul des indices climatiques

```r
# Indice de chaleur (seuil adapté au Maroc : 38°C)
hw_index <- calculate_heatwave_index(
  climate_morocco,
  threshold    = 38,
  min_duration = 3,        # minimum 3 jours consécutifs
  period       = "annual"
)

# Indice de sécheresse SPI
drought_index <- calculate_drought_index(
  climate_morocco,
  method     = "spi",
  ref_period = c(1981, 2010)
)

# Risque de gel (adapté pour la vigne)
frost_risk <- calculate_frost_risk(
  climate_morocco,
  frost_threshold = 0,
  crop            = "vine"
)
```

### Étape 3 — Analyse des tendances

```r
trend <- analyze_trends(
  climate_morocco,
  variable   = "tmax",
  ref_period = c(1981, 2010)
)

cat("Tendance Tmax :", trend$trend_per_decade, "°C / décennie\n")
# Tendance Tmax : 0.622 °C / décennie

cat("Significativité :", trend$trend_significance, "\n")
# Significativité : 0
```

### Étape 4 — Indice d'exposition composite

```r
# Pondération : chaleur 40%, sécheresse 40%, gel 20%
exposure <- calculate_exposure_index(
  heatwave_df = hw_index,
  drought_df  = drought_index,
  frost_df    = frost_risk,
  w_heat      = 0.4,
  w_drought   = 0.4,
  w_frost     = 0.2
)

# Distribution des classes de risque
table(exposure$exposure_class)
# faible  moyen  élevé
#     14     13     13
```

### Étape 5 — Comparaison de scénarios

```r
comparison <- compare_scenarios(
  climate_morocco,
  baseline = c(1981, 2000),
  future   = c(2001, 2020)
)
print(comparison)
#   variable baseline future_mean anomaly pct_change
# 1     tmax    27.05       28.31    1.26        4.7
# 2     tmin    19.01       20.24    1.23        6.5
# 3     prec     4.89        4.21   -0.68      -13.9
```

### Étape 6 — Synthèse des extrêmes

```r
summary_table <- summarize_extremes(
  heatwave_df = hw_index,
  drought_df  = drought_index,
  frost_df    = frost_risk
)
print(summary_table)
#       indice moyenne maximum   sd annees_risque tendance
# 1    Chaleur    24.3      67  12.1            40 1.62e+00
# 2 Secheresse    -0.1       0   1.0            22 -2.3e-03
# 3 Gel tardif     0.8       5   1.2            18 -1.1e-02
```

---

## Visualisations

### Tendance des températures

```r
annual_tmax <- aggregate(tmax ~ year, data = climate_morocco, FUN = mean)
plot_climate_timeseries(
  annual_tmax,
  type  = "temperature",
  title = "Evolution de la Tmax au Maroc (1981-2020)"
)
```

### Carte des risques par région

```r
data("agri_zones_morocco")
plot_climate_risk_map(
  agri_zones_morocco,
  variable = "exposure_index",
  title    = "Indice d'exposition climatique - Regions agricoles du Maroc",
  palette  = "RdYlGn"
)
```

### Atlas climatique automatique

```r
generate_risk_atlas(
  climate_data = climate_morocco,
  exposure_df  = exposure,
  output_file  = "atlas_climatique_maroc.html",
  title        = "Atlas des Risques Climatiques Agricoles - Maroc",
  region       = "Maroc",
  open_browser = TRUE     # Ouvre automatiquement dans le navigateur
)
```

---

## Tests unitaires

Le package inclut **21 tests unitaires** couvrant toutes les fonctions principales.

```r
devtools::test()

# ══ Results ══════════════════════════════
# [ FAIL 0 | WARN 0 | SKIP 0 | PASS 21 ]
```

### Couverture des tests

| Fonction testée | Nombre de tests |
|----------------|----------------|
| `import_climate_data()` | 4 |
| `calculate_heatwave_index()` | 5 |
| `calculate_drought_index()` | 2 |
| `calculate_frost_risk()` | 2 |
| `calculate_exposure_index()` | 2 |
| `analyze_trends()` | 3 |
| `compare_scenarios()` | 1 |
| Test personnalisé | 2 |

---

## Sources de données recommandées

Pour utiliser `climExposure` avec des données réelles :

| Source | Variables | Résolution | Accès |
|--------|-----------|------------|-------|
| [WorldClim 2.1](https://worldclim.org) | Tmax, Tmin, Précip | 1 km | Gratuit |
| [ERA5 (Copernicus)](https://cds.climate.copernicus.eu) | Toutes variables | 31 km | Gratuit (inscription) |
| [CHIRPS](https://chirps.ucsb.edu) | Précipitations | 5 km | Gratuit |
| [CMIP6](https://esgf-node.llnl.gov) | Projections futures | Variable | Gratuit |
| [DMN Maroc](http://www.marocmeteo.ma) | Données nationales | Stations | Sur demande |

### Exemple avec WorldClim

```r
# Installer le package geodata pour accéder à WorldClim
install.packages("geodata")

# Télécharger les données de température pour le Maroc
library(geodata)
tmax_wc <- worldclim_country("MAR", var = "tmax", res = 2.5)
```

---

## Références scientifiques

- **Klein Tank et al. (2009).** *Guidelines on Analysis of extremes in a changing climate in support of informed decisions for adaptation.* WMO-TD No. 1500.

- **McKee, T.B., Doesken, N.J. & Kleist, J. (1993).** *The relationship of drought frequency and duration to time scales.* 8th Conference on Applied Climatology, AMS.

- **Fick, S.E. & Hijmans, R.J. (2017).** *WorldClim 2: new 1-km spatial resolution climate surfaces for global land areas.* International Journal of Climatology, 37(12), 4302–4315.

- **Hersbach et al. (2020).** *The ERA5 global reanalysis.* Quarterly Journal of the Royal Meteorological Society, 146(730), 1999–2049.

- **IPCC (2021).** *Climate Change 2021: The Physical Science Basis.* Contribution of Working Group I to the Sixth Assessment Report.

---

## Vignette

Une vignette complète est disponible avec un flux de travail reproduisible de bout en bout :

```r
vignette("introduction-climExposure", package = "climExposure")
```

Elle couvre : import → indices → tendances → exposition → rapport.

---

## Licence

Ce package est distribué sous licence **MIT**.  
Voir le fichier [LICENSE](LICENSE) pour les détails complets.

---

## Citation

Si vous utilisez `climExposure` dans vos travaux, merci de le citer :

```
Bayane Atteouib (2026). climExposure: Exposition des Systèmes Agricoles
aux Extrêmes Climatiques. R package version 0.1.0.
https://github.com/atteouibayane8/climExposure
```

---

## Contact

- **Auteur** : Bayane Atteouib
- **GitHub** : [@atteouibayane8](https://github.com/atteouibayane8)
- **Issues** : [github.com/atteouibayane8/climExposure/issues](https://github.com/atteouibayane8/climExposure/issues)

---

<div align="center">
  <sub>Développé dans le cadre du cours de développement de packages R</sub>
</div>
