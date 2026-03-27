library(testthat)
library(here)
here::i_am("drm-lac-intelligence.Rproj")

# ── Directory structure (FOUND-02) ─────────────────────────────────────────
test_that("required directory R/ exists", {
  expect_true(dir.exists(here::here("R")))
})

test_that("required directory data/raw/ exists", {
  expect_true(dir.exists(here::here("data/raw")))
})

test_that("required directory data/processed/ exists", {
  expect_true(dir.exists(here::here("data/processed")))
})

test_that("required directory report/ exists", {
  expect_true(dir.exists(here::here("report")))
})

test_that("required directory shared/R/ exists", {
  expect_true(dir.exists(here::here("shared/R")))
})

test_that("required directory shared/tests/ exists", {
  expect_true(dir.exists(here::here("shared/tests")))
})

# ── country_scores.csv existence (FOUND-04) ────────────────────────────────
test_that("country_scores.csv exists in data/processed/", {
  expect_true(file.exists(here::here("data/processed/country_scores.csv")))
})

# ── country_scores.csv schema — identifier columns (FOUND-04) ─────────────
test_that("country_scores.csv has required identifier columns", {
  skip_if_not(file.exists(here::here("data/processed/country_scores.csv")))
  library(readr)
  data <- readr::read_csv(here::here("data/processed/country_scores.csv"),
                          show_col_types = FALSE)
  expect_true(all(c("country", "iso3", "region") %in% names(data)))
})

# ── country_scores.csv schema — all 9 scenario×year columns (FOUND-04) ────
test_that("country_scores.csv has all 9 scenario-by-year columns", {
  skip_if_not(file.exists(here::here("data/processed/country_scores.csv")))
  library(readr)
  data <- readr::read_csv(here::here("data/processed/country_scores.csv"),
                          show_col_types = FALSE)
  required_cols <- c(
    "composite_baseline_2024", "composite_baseline_2030", "composite_baseline_2050",
    "composite_rcp45_2024",    "composite_rcp45_2030",    "composite_rcp45_2050",
    "composite_rcp85_2024",    "composite_rcp85_2030",    "composite_rcp85_2050"
  )
  expect_true(all(required_cols %in% names(data)))
})

# ── renv.lock existence (PROF-03) ──────────────────────────────────────────
test_that("renv.lock exists at repo root", {
  expect_true(file.exists(here::here("renv.lock")))
})
