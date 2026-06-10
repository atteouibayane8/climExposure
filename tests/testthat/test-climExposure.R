# tests/testthat/test-import_climate_data.R
library(testthat)
library(climExposure)

# ─── Données de test ──────────────────────────────────────────────────────
tmp_csv <- tempfile(fileext = ".csv")
test_df <- data.frame(
  date = seq(as.Date("2000-01-01"), as.Date("2005-12-31"), by = "day"),
  tmax = rnorm(2192, 28, 8),
  tmin = rnorm(2192, 15, 6),
  prec = pmax(0, rnorm(2192, 5, 10))
)
write.csv(test_df, tmp_csv, row.names = FALSE)

# ─── Tests import_climate_data ────────────────────────────────────────────
test_that("import_climate_data retourne un dataframe", {
  df <- import_climate_data(tmp_csv, format = "csv")
  expect_s3_class(df, "data.frame")
})

test_that("import_climate_data contient les colonnes attendues", {
  df <- import_climate_data(tmp_csv)
  expect_true(all(c("date", "year", "month", "tmax") %in% names(df)))
})

test_that("import_climate_data applique le filtre temporel", {
  df <- import_climate_data(tmp_csv, start_date = "2003-01-01",
                             end_date = "2004-12-31")
  expect_true(min(df$year) >= 2003)
  expect_true(max(df$year) <= 2004)
})

test_that("import_climate_data lève une erreur si le fichier est absent", {
  expect_error(import_climate_data("fichier_inexistant.csv"), "n'existe pas")
})

# ─── Tests calculate_heatwave_index ───────────────────────────────────────
data("climate_morocco", package = "climExposure")

test_that("calculate_heatwave_index retourne un dataframe", {
  hw <- calculate_heatwave_index(climate_morocco, threshold = 38)
  expect_s3_class(hw, "data.frame")
})

test_that("calculate_heatwave_index contient les colonnes attendues", {
  hw <- calculate_heatwave_index(climate_morocco)
  expect_true(all(c("year", "n_hot_days", "n_heatwaves",
                    "max_duration", "mean_tmax") %in% names(hw)))
})

test_that("n_hot_days est non négatif", {
  hw <- calculate_heatwave_index(climate_morocco, threshold = 35)
  expect_true(all(hw$n_hot_days >= 0))
})

test_that("seuil élevé donne moins de jours chauds", {
  hw_low  <- calculate_heatwave_index(climate_morocco, threshold = 30)
  hw_high <- calculate_heatwave_index(climate_morocco, threshold = 45)
  expect_true(sum(hw_high$n_hot_days) <= sum(hw_low$n_hot_days))
})

test_that("erreur si colonnes manquantes", {
  bad_df <- climate_morocco[, c("year", "month")]
  expect_error(calculate_heatwave_index(bad_df), "Colonnes manquantes")
})

# ─── Tests calculate_drought_index ────────────────────────────────────────
test_that("calculate_drought_index retourne les colonnes attendues", {
  dr <- calculate_drought_index(climate_morocco)
  expect_true(all(c("year", "month", "drought_index", "drought_class") %in% names(dr)))
})

test_that("drought_class est un facteur valide", {
  dr <- calculate_drought_index(climate_morocco)
  expect_s3_class(dr$drought_class, "factor")
  valid_levels <- c("normale", "modérée", "sévère", "extrême")
  expect_true(all(levels(dr$drought_class) %in% valid_levels))
})

# ─── Tests calculate_frost_risk ───────────────────────────────────────────
test_that("calculate_frost_risk retourne un dataframe", {
  fr <- calculate_frost_risk(climate_morocco)
  expect_s3_class(fr, "data.frame")
})

test_that("n_frost_days est non négatif", {
  fr <- calculate_frost_risk(climate_morocco)
  expect_true(all(fr$n_frost_days >= 0))
})

# ─── Tests calculate_exposure_index ───────────────────────────────────────
test_that("calculate_exposure_index retourne exposure_index entre 0 et 1", {
  hw <- calculate_heatwave_index(climate_morocco)
  dr <- calculate_drought_index(climate_morocco)
  fr <- calculate_frost_risk(climate_morocco)
  expo <- calculate_exposure_index(hw, dr, fr)
  expect_true(all(expo$exposure_index >= 0 & expo$exposure_index <= 1))
})

test_that("erreur si poids ne somment pas à 1", {
  hw <- calculate_heatwave_index(climate_morocco)
  dr <- calculate_drought_index(climate_morocco)
  fr <- calculate_frost_risk(climate_morocco)
  expect_error(
    calculate_exposure_index(hw, dr, fr, w_heat = 0.5, w_drought = 0.5, w_frost = 0.5),
    "poids doivent sommer"
  )
})

# ─── Tests analyze_trends ─────────────────────────────────────────────────
test_that("analyze_trends retourne une liste avec les éléments attendus", {
  result <- analyze_trends(climate_morocco, variable = "tmax")
  expect_type(result, "list")
  expect_true(all(c("trend", "annual_values", "decadal") %in% names(result)))
})

test_that("tendance de réchauffement positive sur les données simulées", {
  result <- analyze_trends(climate_morocco, variable = "tmax")
  expect_true(result$trend > 0)
})

# ─── Tests compare_scenarios ─────────────────────────────────────────────
test_that("compare_scenarios retourne les colonnes attendues", {
  comp <- compare_scenarios(climate_morocco,
                             baseline = c(1981, 2000),
                             future   = c(2001, 2020))
  expect_true(all(c("variable", "baseline", "future_mean", "anomaly") %in% names(comp)))
})

# Nettoyage
file.remove(tmp_csv)
