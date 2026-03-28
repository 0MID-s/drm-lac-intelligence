# shared/tests/test_scoring.R
# Tests for Phase 3 scoring pipeline
# Covers: geo_mean_weighted() (hazard_utils.R) and score_hazard()/sensitivity_test() (03_score.R)

library(testthat)
library(here)
here::i_am("drm-lac-intelligence.Rproj")

# Load shared utilities (includes hazard_utils.R which will have geo_mean_weighted)
source(here::here("shared/R/hazard_utils.R"))

# Load scoring script for function definitions only (not the main pipeline)
# The sys.nframe() guard in 03_score.R prevents pipeline execution on source()
source(here::here("R/03_score.R"))

# ============================================================
# geo_mean_weighted() tests
# ============================================================

test_that("geo_mean_weighted returns positive numeric vector of correct length", {
  df <- data.frame(a = c(0.1, 0.5, 0.9), b = c(0.9, 0.5, 0.1))
  r  <- geo_mean_weighted(df, c("a", "b"), c(a = 0.5, b = 0.5))
  expect_true(is.numeric(r))
  expect_equal(length(r), 3L)
  expect_true(all(r > 0))
})

test_that("geo_mean_weighted handles VLO minimum (0.1) without returning 0 or -Inf", {
  df <- data.frame(x = c(0.1, 0.1), y = c(0.1, 0.9))
  r  <- geo_mean_weighted(df, c("x", "y"), c(x = 0.5, y = 0.5))
  expect_true(all(is.finite(r)))
  expect_true(all(r > 0))
})

test_that("geo_mean_weighted eps floor prevents log(0) = -Inf when value is exactly 0", {
  df <- data.frame(a = c(0.0, 0.5), b = c(0.9, 0.5))
  r  <- geo_mean_weighted(df, c("a", "b"), c(a = 0.5, b = 0.5))
  expect_true(all(is.finite(r)))
  expect_true(all(r > 0))
})

test_that("geo_mean_weighted aligns weights by name regardless of order", {
  df     <- data.frame(a = c(0.5), b = c(0.8))
  # Reversed order
  r1 <- geo_mean_weighted(df, c("a", "b"), c(b = 0.6, a = 0.4))
  r2 <- geo_mean_weighted(df, c("a", "b"), c(a = 0.4, b = 0.6))
  expect_equal(r1, r2)
})

test_that("geo_mean_weighted errors when cols not found in df", {
  df <- data.frame(a = c(0.5))
  expect_error(geo_mean_weighted(df, c("a", "missing"), c(a = 0.5, missing = 0.5)),
               regexp = "cols not found")
})

test_that("geo_mean_weighted errors when weight names do not match cols", {
  df <- data.frame(a = c(0.5), b = c(0.8))
  expect_error(geo_mean_weighted(df, c("a", "b"), c(a = 0.5, c = 0.5)),
               regexp = "weight names must match cols")
})

# ============================================================
# HAZ-01: HAZARD_ORDINAL_MAP constant
# ============================================================

test_that("HAZ-01: HAZARD_ORDINAL_MAP has correct values and VLO > 0", {
  expect_named(HAZARD_ORDINAL_MAP, c("VLO", "LOW", "MED", "HIG"), ignore.order = TRUE)
  expect_true(HAZARD_ORDINAL_MAP["VLO"] > 0,
              info = "VLO must be > 0 to prevent log(0) in geometric mean")
  expect_equal(HAZARD_ORDINAL_MAP[["VLO"]], 0.1)
  expect_equal(HAZARD_ORDINAL_MAP[["LOW"]], 0.25)
  expect_equal(HAZARD_ORDINAL_MAP[["MED"]], 0.50)
  expect_equal(HAZARD_ORDINAL_MAP[["HIG"]], 0.90)
})

# ============================================================
# HAZ-02: score_hazard() produces valid composite column
# ============================================================

test_that("HAZ-02: score_hazard() adds hazard_composite column in [0,1] with no NA", {
  lac <- readr::read_csv(here::here("data/processed/lac_master.csv"),
                         show_col_types = FALSE)
  result <- score_hazard(lac)
  expect_true("hazard_composite" %in% names(result))
  expect_equal(nrow(result), 27L)
  expect_true(all(!is.na(result$hazard_composite)))
  expect_true(all(result$hazard_composite >= 0 & result$hazard_composite <= 1))
})

# ============================================================
# HAZ-02b: Country validation anchors
# ============================================================

test_that("HAZ-02b: Haiti hazard_composite >= 0.85", {
  lac    <- readr::read_csv(here::here("data/processed/lac_master.csv"),
                            show_col_types = FALSE)
  result <- score_hazard(lac)
  hti    <- result$hazard_composite[result$iso3 == "HTI"]
  expect_true(hti >= 0.85,
              info = paste("Haiti hazard_composite =", round(hti, 4), "(expected >= 0.85)"))
})

test_that("HAZ-02b: Guatemala hazard_composite >= 0.80", {
  lac    <- readr::read_csv(here::here("data/processed/lac_master.csv"),
                            show_col_types = FALSE)
  result <- score_hazard(lac)
  gtm    <- result$hazard_composite[result$iso3 == "GTM"]
  expect_true(gtm >= 0.80,
              info = paste("Guatemala hazard_composite =", round(gtm, 4), "(expected >= 0.80)"))
})

# ============================================================
# HAZ-03: sensitivity_test() returns TRUE
# ============================================================

test_that("HAZ-03: sensitivity_test() returns TRUE (min Spearman rho >= 0.90)", {
  lac    <- readr::read_csv(here::here("data/processed/lac_master.csv"),
                            show_col_types = FALSE)
  result <- score_hazard(lac)
  passed <- sensitivity_test(result)
  expect_true(passed,
              info = "Sensitivity analysis failed: min Spearman rho < 0.90 under ±20% weight perturbation")
})

# ============================================================
# HAZ-04: All 27 countries have finite, non-NA hazard_composite
# ============================================================

test_that("HAZ-04: all 27 countries have hazard_composite values, no NA", {
  lac    <- readr::read_csv(here::here("data/processed/lac_master.csv"),
                            show_col_types = FALSE)
  result <- score_hazard(lac)
  expect_equal(nrow(result), 27L)
  expect_true(all(!is.na(result$hazard_composite)),
              info = "Some countries have NA hazard_composite")
  expect_true(all(is.finite(result$hazard_composite)),
              info = "Some countries have non-finite hazard_composite")
})
