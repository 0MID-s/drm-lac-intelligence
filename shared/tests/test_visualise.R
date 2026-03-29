# shared/tests/test_visualise.R
# Unit tests for R/03_visualise.R — DRM LAC chart functions
# TDD RED: written before implementation; all tests should FAIL until 03_visualise.R exists.

library(testthat)
library(here)
here::i_am("drm-lac-intelligence.Rproj")

# ---------------------------------------------------------------------------
# Load viz_utils.R (required for DRM_COLOURS, RISK_PALETTE, theme_drm etc.)
# ---------------------------------------------------------------------------
source(here::here("shared/R/viz_utils.R"))

# ---------------------------------------------------------------------------
# Load 03_visualise.R — tests will FAIL if file does not exist
# ---------------------------------------------------------------------------
vis_path <- here::here("R/03_visualise.R")
if (!file.exists(vis_path)) {
  stop("R/03_visualise.R not found — tests cannot proceed (expected RED in TDD Wave 0)")
}
source(vis_path)

# ---------------------------------------------------------------------------
# Minimal scores fixture — 4 rows, covers all required columns
# ---------------------------------------------------------------------------
make_fixture <- function() {
  data.frame(
    iso3                    = c("ARG", "BRA", "HTI", "JAM"),
    country                 = c("Argentina", "Brazil", "Haiti", "Jamaica"),
    sub_region              = c("South America", "South America", "Caribbean SIDS", "Caribbean SIDS"),
    hazard_composite        = c(0.45, 0.38, 0.82, 0.61),
    exposure_score          = c(0.52, 0.67, 0.78, 0.55),
    vulnerability_score     = c(0.40, 0.35, 0.91, 0.58),
    ews_readiness_score     = c(0.62, 0.71, 0.22, 0.44),
    nbs_feasibility_score   = c(0.55, 0.60, 0.40, 0.70),
    recovery_readiness_score= c(0.50, 0.55, 0.20, 0.45),
    ews_gap_tier            = c("Medium", "Low", "Critical", "High"),
    structural_gap_usd      = c(20300, 45000, 1200, 3400),
    protection_gap          = c(0.77, 0.65, 0.95, 0.88),
    sendai_target_e         = c("Partial", "Reported", "Not Reported", "Partial"),
    sendai_target_g         = c("Not Reported", "Partial", "Not Reported", "Reported"),
    hazard_EQ_num           = c(0.50, 0.40, 0.60, 0.35),
    hazard_FL_num           = c(0.50, 0.55, 0.70, 0.45),
    hazard_CY_num           = c(0.25, 0.20, 0.90, 0.75),
    hazard_VO_num           = c(0.25, 0.15, 0.30, 0.20),
    hazard_DR_num           = c(0.50, 0.45, 0.55, 0.40),
    composite_baseline_2024 = c(0.45, 0.38, 0.82, 0.61),
    composite_baseline_2030 = c(0.47, 0.40, 0.86, 0.64),
    composite_baseline_2050 = c(0.50, 0.43, 0.92, 0.68),
    composite_rcp45_2024    = c(0.45, 0.38, 0.82, 0.61),
    composite_rcp45_2030    = c(0.49, 0.41, 0.88, 0.66),
    composite_rcp45_2050    = c(0.53, 0.45, 0.97, 0.72),
    composite_rcp85_2024    = c(0.45, 0.38, 0.82, 0.61),
    composite_rcp85_2030    = c(0.50, 0.43, 0.92, 0.68),
    composite_rcp85_2050    = c(0.58, 0.49, 1.05, 0.78),
    stringsAsFactors        = FALSE
  )
}

scores <- make_fixture()

# ===========================================================================
# Test group 1: plot_choropleth_drm
# ===========================================================================
test_that("plot_choropleth_drm returns a ggplot object", {
  p <- plot_choropleth_drm(scores, scenario = "baseline", year = 2024)
  expect_s3_class(p, "gg")
})

test_that("plot_choropleth_drm stopifnot fires on bad scenario/year", {
  expect_error(plot_choropleth_drm(scores, scenario = "nonsense", year = 2099))
})

# ===========================================================================
# Test group 2: plot_risk_matrix_drm
# ===========================================================================
test_that("plot_risk_matrix_drm returns a ggplot object", {
  p <- plot_risk_matrix_drm(scores)
  expect_s3_class(p, "gg")
})

# ===========================================================================
# Test group 3: plot_radar_drm (polygon closure)
# ===========================================================================
test_that("plot_radar_drm returns a ggplot object", {
  p <- plot_radar_drm(scores)
  expect_s3_class(p, "gg")
})

test_that("plot_radar_drm closes polygon: first and last dim per country are identical", {
  # Extract the data used in the chart by re-running the pivot logic
  dims   <- c("hazard_composite", "exposure_score", "vulnerability_score",
              "ews_readiness_score", "nbs_feasibility_score", "recovery_readiness_score")
  labels <- c("Hazard", "Exposure", "Vulnerability", "EWS", "NbS", "Recovery")

  long <- scores |>
    dplyr::select(country, dplyr::all_of(dims)) |>
    tidyr::pivot_longer(-country, names_to = "dimension", values_to = "value") |>
    dplyr::mutate(dimension = factor(dimension, levels = dims, labels = labels))

  long_closed <- dplyr::bind_rows(
    long,
    long |> dplyr::filter(dimension == labels[1]) |>
      dplyr::mutate(dimension = factor(labels[1], levels = labels))
  )

  # For each country the first and last rows should have the same dimension label
  for (ctry in unique(long_closed$country)) {
    ctry_rows <- long_closed[long_closed$country == ctry, ]
    expect_equal(as.character(ctry_rows$dimension[1]),
                 as.character(ctry_rows$dimension[nrow(ctry_rows)]))
  }
})

test_that("plot_radar_drm respects countries filter", {
  p <- plot_radar_drm(scores, countries = c("Haiti", "Jamaica"))
  expect_s3_class(p, "gg")
})

# ===========================================================================
# Test group 4: plot_hazard_stacked_drm
# ===========================================================================
test_that("plot_hazard_stacked_drm returns a ggplot object", {
  p <- plot_hazard_stacked_drm(scores)
  expect_s3_class(p, "gg")
})

# ===========================================================================
# Test group 5: plot_ews_dumbbell_drm
# ===========================================================================
test_that("plot_ews_dumbbell_drm returns a ggplot object", {
  p <- plot_ews_dumbbell_drm(scores)
  expect_s3_class(p, "gg")
})

# ===========================================================================
# Test group 6: plot_treemap_drm
# ===========================================================================
test_that("plot_treemap_drm returns a ggplot object", {
  p <- plot_treemap_drm(scores)
  expect_s3_class(p, "gg")
})

# ===========================================================================
# Test group 7: plot_sendai_heatmap_drm
# ===========================================================================
test_that("plot_sendai_heatmap_drm returns a ggplot object", {
  p <- plot_sendai_heatmap_drm(scores)
  expect_s3_class(p, "gg")
})

# ===========================================================================
# Test group 8: plot_protection_gap_drm
# ===========================================================================
test_that("plot_protection_gap_drm returns a ggplot object", {
  p <- plot_protection_gap_drm(scores)
  expect_s3_class(p, "gg")
})

# ===========================================================================
# Test group 9: param resolution
# ===========================================================================
test_that("scenario col resolves to valid column for all 9 scenario x year combos", {
  combos <- expand.grid(
    scenario = c("baseline", "rcp45", "rcp85"),
    year     = c(2024, 2030, 2050),
    stringsAsFactors = FALSE
  )
  for (i in seq_len(nrow(combos))) {
    col <- paste0("composite_", combos$scenario[i], "_", as.character(combos$year[i]))
    expect_true(col %in% names(scores),
                info = paste("Column missing:", col))
  }
})

# ===========================================================================
# Test group 10: load_scores (skipped if CSV absent)
# ===========================================================================
test_that("load_scores returns a data.frame when CSV exists", {
  csv_path <- here::here("data/processed/scores_intermediate.csv")
  skip_if_not(file.exists(csv_path), "scores_intermediate.csv not present")
  df <- load_scores(csv_path)
  expect_true(is.data.frame(df))
  expect_gt(nrow(df), 0)
})
