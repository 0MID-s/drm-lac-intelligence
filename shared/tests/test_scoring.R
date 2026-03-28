# shared/tests/test_scoring.R
# Tests for Phase 3 analytical scoring modules.
# Run: Rscript -e "testthat::test_file(here::here('shared/tests/test_scoring.R'))"

library(testthat)
library(here)
here::i_am("drm-lac-intelligence.Rproj")

source(here::here("shared/R/utils.R"))
source(here::here("R/03_score.R"))

# ---------------------------------------------------------------------------
# Test helpers
# ---------------------------------------------------------------------------

make_lac_stub <- function() {
  data.frame(
    iso3 = c("HTI", "CHL", "JAM", "ARG", "BHS"),
    country = c("Haiti", "Chile", "Jamaica", "Argentina", "Bahamas"),
    gov_eff = c(-1.8, 1.2, -0.3, -0.4, -0.1),
    rule_of_law = c(-1.5, 1.3, -0.2, -0.3, 0.2),
    inform_risk_score = c(7.5, 2.1, 5.5, 3.4, 4.8),
    sids_reliability_flag = c(FALSE, FALSE, TRUE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# EWS-01: ews_readiness_score continuous [0,1], no NA
# ---------------------------------------------------------------------------

test_that("EWS-01: score_ews() adds ews_readiness_score column", {
  lac <- make_lac_stub()
  result <- score_ews(lac)
  expect_true("ews_readiness_score" %in% names(result),
              info = "EWS-01: ews_readiness_score column must be present")
})

test_that("EWS-01: ews_readiness_score values are in [0, 1]", {
  lac <- make_lac_stub()
  result <- score_ews(lac)
  scores <- result$ews_readiness_score
  expect_true(all(scores >= 0 & scores <= 1),
              info = "EWS-01: all ews_readiness_score values must be in [0, 1]")
})

test_that("EWS-01: ews_readiness_score has no NA values", {
  lac <- make_lac_stub()
  result <- score_ews(lac)
  expect_false(anyNA(result$ews_readiness_score),
               info = "EWS-01: ews_readiness_score must have no NA values")
})

test_that("EWS-01: score_ews() handles NA in gov_eff gracefully (coalesce to 0)", {
  lac <- make_lac_stub()
  lac$gov_eff[2] <- NA_real_
  lac$rule_of_law[2] <- NA_real_
  result <- score_ews(lac)
  expect_false(anyNA(result$ews_readiness_score),
               info = "EWS-01: NA inputs must be coalesced; no NA output")
})

test_that("EWS-01: ews_readiness_score for all 27 LAC countries returns no NA", {
  lac <- read.csv(here::here("data/processed/lac_master.csv"),
                  stringsAsFactors = FALSE)
  result <- score_ews(lac)
  expect_equal(nrow(result), nrow(lac))
  expect_false(anyNA(result$ews_readiness_score),
               info = "EWS-01: all 27 countries must have non-NA ews_readiness_score")
  expect_true(all(result$ews_readiness_score >= 0 & result$ews_readiness_score <= 1),
              info = "EWS-01: all 27 country scores must be in [0, 1]")
})

# ---------------------------------------------------------------------------
# EWS-02: ews_gap_tier ordinal classification
# ---------------------------------------------------------------------------

test_that("EWS-02: score_ews() adds ews_gap_tier column", {
  lac <- make_lac_stub()
  result <- score_ews(lac)
  expect_true("ews_gap_tier" %in% names(result),
              info = "EWS-02: ews_gap_tier column must be present")
})

test_that("EWS-02: ews_gap_tier contains only canonical tier strings", {
  lac <- make_lac_stub()
  result <- score_ews(lac)
  valid_tiers <- c("Critical", "High", "Moderate", "Low")
  expect_true(all(result$ews_gap_tier %in% valid_tiers),
              info = paste("EWS-02: ews_gap_tier must be one of:",
                           paste(valid_tiers, collapse = ", ")))
})

test_that("EWS-02: ews_gap_tier has no NA values", {
  lac <- make_lac_stub()
  result <- score_ews(lac)
  expect_false(anyNA(result$ews_gap_tier),
               info = "EWS-02: ews_gap_tier must have no NA values")
})

test_that("EWS-02: Haiti (HTI) classified as Critical", {
  lac <- make_lac_stub()
  result <- score_ews(lac)
  haiti_tier <- result$ews_gap_tier[result$iso3 == "HTI"]
  expect_equal(haiti_tier, "Critical",
               info = "EWS-02: Haiti must be classified as Critical tier")
})

test_that("EWS-02: Chile (CHL) classified as Low or Moderate (strong governance)", {
  lac <- make_lac_stub()
  result <- score_ews(lac)
  chile_tier <- result$ews_gap_tier[result$iso3 == "CHL"]
  expect_true(chile_tier %in% c("Low", "Moderate"),
              info = "EWS-02: Chile must be Low or Moderate (high governance)")
})

test_that("EWS-02: full LAC dataset gap tiers are all canonical strings", {
  lac <- read.csv(here::here("data/processed/lac_master.csv"),
                  stringsAsFactors = FALSE)
  result <- score_ews(lac)
  valid_tiers <- c("Critical", "High", "Moderate", "Low")
  expect_true(all(result$ews_gap_tier %in% valid_tiers),
              info = "EWS-02: all 27 countries must have canonical ews_gap_tier")
  expect_false(anyNA(result$ews_gap_tier),
               info = "EWS-02: no NA in ews_gap_tier for full LAC dataset")
})

test_that("EWS-02: Haiti in full LAC dataset classified as Critical", {
  lac <- read.csv(here::here("data/processed/lac_master.csv"),
                  stringsAsFactors = FALSE)
  result <- score_ews(lac)
  haiti_tier <- result$ews_gap_tier[result$iso3 == "HTI"]
  expect_equal(haiti_tier, "Critical",
               info = "EWS-02: Haiti must be Critical in full LAC dataset")
})
