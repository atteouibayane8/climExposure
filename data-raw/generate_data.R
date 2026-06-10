# data-raw/generate_data.R
# Script de génération des jeux de données d'exemple
# Exécuter une seule fois pour créer les .rda dans data/

set.seed(123)
n_years <- 40
start_year <- 1981

# ── 1. climate_morocco : données climatiques journalières simulées ─────────
dates <- seq(as.Date(paste0(start_year, "-01-01")),
             as.Date(paste0(start_year + n_years - 1, "-12-31")), by = "day")

n <- length(dates)
years  <- as.integer(format(dates, "%Y"))
months <- as.integer(format(dates, "%m"))

# Tendances climatiques réalistes pour le Maroc
year_idx <- (years - start_year) / n_years   # 0 → 1

# Tmax : signal saisonnier + tendance de réchauffement + bruit
seasonal_tmax <- 20 + 15 * sin((months - 3) * pi / 6)
tmax <- seasonal_tmax + 2.5 * year_idx + rnorm(n, 0, 3)

# Tmin : suit tmax avec décalage
tmin <- tmax - (8 + rnorm(n, 0, 1.5))

# Précipitations : régime méditerranéen (hiver humide, été sec)
prec_seasonal <- pmax(0, 40 * sin((months - 9) * pi / 6)^2)
prec <- pmax(0, prec_seasonal * rgamma(n, shape = 0.8, rate = 1) -
               5 * year_idx + rnorm(n, 0, 5))
prec[months %in% 6:9] <- prec[months %in% 6:9] * 0.1   # été très sec

climate_morocco <- data.frame(
  date  = dates,
  year  = years,
  month = months,
  tmax  = round(tmax, 1),
  tmin  = round(tmin, 1),
  prec  = round(prec, 1)
)

# ── 2. agri_zones_morocco : zones agricoles avec coordonnées ──────────────
regions <- c("Souss-Massa", "Haouz", "Tadla", "Gharb", "Doukkala",
             "Moulouya", "Saiss", "Oriental", "Tensift", "Chaouia")
set.seed(42)

agri_zones_morocco <- data.frame(
  region         = regions,
  lon            = c(-9.5, -8.0, -6.5, -6.2, -8.5, -2.5, -5.2, -1.8, -8.3, -6.9),
  lat            = c( 30.3, 31.6, 32.5, 34.5, 32.6, 34.1, 34.0, 34.7, 31.5, 33.4),
  heat_score     = round(runif(10, 0.3, 1.0), 2),
  drought_score  = round(runif(10, 0.2, 0.9), 2),
  frost_score    = round(runif(10, 0.0, 0.5), 2),
  agri_area_kha  = round(runif(10, 50, 500), 0),
  main_crop      = sample(c("blé", "maïs", "olivier", "vigne"), 10, replace = TRUE)
)
agri_zones_morocco$exposure_index <- with(agri_zones_morocco,
  round(0.4 * heat_score + 0.4 * drought_score + 0.2 * frost_score, 3))

# ── 3. Sauvegarde ─────────────────────────────────────────────────────────
usethis::use_data(climate_morocco,    overwrite = TRUE)
usethis::use_data(agri_zones_morocco, overwrite = TRUE)

message("Jeux de données générés et sauvegardés dans data/")
