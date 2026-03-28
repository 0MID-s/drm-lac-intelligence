library(testthat)
library(here)
here::i_am("drm-lac-intelligence.Rproj")
# Source only what scoring tests need — avoids pulling in viz dependencies (ggplot2)
# that are not yet installed. Full source(utils.R) is used by non-test scripts.
source(here::here("shared/R/hazard_utils.R"))
# 03_score.R: stub in Wave 0 (no scoring functions yet); defines functions after Wave 1+
# Wrap in tryCatch so the test file loads cleanly even while 03_score.R sources utils.R
tryCatch(
  source(here::here("R/03_score.R")),
  error = function(e) message("03_score.R source warning (expected in Wave 0): ", conditionMessage(e))
)

# ---------------------------------------------------------------------------
# Test fixtures — load data files with graceful fallback for Wave 0 state
# ---------------------------------------------------------------------------
scores_int_path     <- here::here("data/processed/scores_intermediate.csv")
country_scores_path <- here::here("data/processed/country_scores.csv")

lac <- readr::read_csv(here::here("data/processed/lac_master.csv"), show_col_types = FALSE)

# scores_intermediate.csv does not exist yet in Wave 0 — use empty tibble so
# that all column-presence and range assertions fail (RED) without a parse error.
scores_int <- if (file.exists(scores_int_path)) {
  readr::read_csv(scores_int_path, show_col_types = FALSE)
} else {
  tibble::tibble()  # empty placeholder — all column-presence tests will fail (RED)
}

country_scores <- readr::read_csv(country_scores_path, show_col_types = FALSE)

# ---------------------------------------------------------------------------
# HAZ-01: hazard_composite column present in scores_intermediate, values in [0,1]
# ---------------------------------------------------------------------------
test_that("HAZ-01: hazard_composite column present in scores_intermediate, values in [0,1]", {
  expect_true("hazard_composite" %in% names(scores_int))
  expect_true(all(scores_int$hazard_composite >= 0 & scores_int$hazard_composite <= 1, na.rm = TRUE))
})

# ---------------------------------------------------------------------------
# HAZ-02: geometric mean validation — Haiti >= 0.85, Guatemala >= 0.80
# ---------------------------------------------------------------------------
test_that("HAZ-02: Haiti hazard_composite >= 0.85 and Guatemala >= 0.80 (geometric mean validation)", {
  haiti     <- scores_int$hazard_composite[scores_int$iso3 == "HTI"]
  guatemala <- scores_int$hazard_composite[scores_int$iso3 == "GTM"]
  expect_true(length(haiti) == 1L && haiti >= 0.85)
  expect_true(length(guatemala) == 1L && guatemala >= 0.80)
})

# ---------------------------------------------------------------------------
# HAZ-02b: Dominican Republic and Colombia in high-risk band [0.60, 0.80]
# ---------------------------------------------------------------------------
test_that("HAZ-02b: Dominican Republic and Colombia hazard_composite in [0.60, 0.80]", {
  dom <- scores_int$hazard_composite[scores_int$iso3 == "DOM"]
  col <- scores_int$hazard_composite[scores_int$iso3 == "COL"]
  expect_true(length(dom) == 1L && dom >= 0.60 && dom <= 0.80)
  expect_true(length(col) == 1L && col >= 0.60 && col <= 0.80)
})

# ---------------------------------------------------------------------------
# HAZ-03: sensitivity_test() returns TRUE (min Spearman rho >= 0.90)
# ---------------------------------------------------------------------------
test_that("HAZ-03: sensitivity_test() returns TRUE (min Spearman rho >= 0.90)", {
  skip_if_not(exists("sensitivity_test"), "sensitivity_test() not yet defined (Wave 0 stub)")
  set.seed(42)
  result <- sensitivity_test(scores_int)
  expect_true(isTRUE(result))
})

# ---------------------------------------------------------------------------
# HAZ-04: scores_intermediate has 27 rows and correct sub_region labels
# ---------------------------------------------------------------------------
test_that("HAZ-04: scores_intermediate has 27 rows and correct sub_region labels", {
  expect_equal(nrow(scores_int), 27L)
  valid_regions <- c("South America", "Central America", "Caribbean SIDS", "Mexico")
  expect_true(all(scores_int$sub_region %in% valid_regions))
})

# ---------------------------------------------------------------------------
# EXP-01: exposure_score column present, no NA values
# ---------------------------------------------------------------------------
test_that("EXP-01: exposure_score column present, no NA values", {
  expect_true("exposure_score" %in% names(scores_int))
  expect_false(any(is.na(scores_int$exposure_score)))
})

# ---------------------------------------------------------------------------
# EXP-02: vulnerability_score column present, no NA values
# ---------------------------------------------------------------------------
test_that("EXP-02: vulnerability_score column present, no NA values", {
  expect_true("vulnerability_score" %in% names(scores_int))
  expect_false(any(is.na(scores_int$vulnerability_score)))
})

# ---------------------------------------------------------------------------
# EXP-03: country_scores.csv has exactly 12 columns and no NA in composite cols
# ---------------------------------------------------------------------------
test_that("EXP-03: country_scores.csv has exactly 12 columns and no NA in composite columns", {
  expect_equal(ncol(country_scores), 12L)
  composite_cols <- grep("^composite_", names(country_scores), value = TRUE)
  expect_equal(length(composite_cols), 9L)
  for (col in composite_cols) {
    expect_false(any(is.na(country_scores[[col]])), info = paste("NA found in", col))
  }
})

# ---------------------------------------------------------------------------
# EWS-01: ews_readiness_score present and in [0, 1]
# ---------------------------------------------------------------------------
test_that("EWS-01: ews_readiness_score present and in [0, 1]", {
  expect_true("ews_readiness_score" %in% names(scores_int))
  expect_true(all(scores_int$ews_readiness_score >= 0 & scores_int$ews_readiness_score <= 1, na.rm = TRUE))
})

# ---------------------------------------------------------------------------
# EWS-02: ews_gap_tier values are one of four canonical strings
# ---------------------------------------------------------------------------
test_that("EWS-02: ews_gap_tier values are one of four canonical strings", {
  expect_true("ews_gap_tier" %in% names(scores_int))
  valid_tiers <- c("Critical", "High", "Moderate", "Low")
  expect_true(all(scores_int$ews_gap_tier %in% valid_tiers))
})

# ---------------------------------------------------------------------------
# NBS-01: nbs_feasibility_score present; Brazil ranks highest (largest mangrove area)
# ---------------------------------------------------------------------------
test_that("NBS-01: nbs_feasibility_score present; Brazil ranks highest (largest mangrove area)", {
  expect_true("nbs_feasibility_score" %in% names(scores_int))
  brazil_score <- scores_int$nbs_feasibility_score[scores_int$iso3 == "BRA"]
  expect_equal(which.max(scores_int$nbs_feasibility_score), which(scores_int$iso3 == "BRA"))
})

# ---------------------------------------------------------------------------
# NBS-02: nbs_drought_potential column present, no NA
# ---------------------------------------------------------------------------
test_that("NBS-02: nbs_drought_potential column present, no NA", {
  expect_true("nbs_drought_potential" %in% names(scores_int))
  expect_false(any(is.na(scores_int$nbs_drought_potential)))
})

# ---------------------------------------------------------------------------
# NBS-03: nbs_flag is a character column with expected levels
# ---------------------------------------------------------------------------
test_that("NBS-03: nbs_flag is a character column with expected levels", {
  expect_true("nbs_flag" %in% names(scores_int))
  expect_true(is.character(scores_int$nbs_flag))
  valid_flags <- c("NbS Primary", "NbS Complement", "Structural Priority")
  expect_true(all(scores_int$nbs_flag %in% valid_flags))
})

# ---------------------------------------------------------------------------
# STRUCT-01: structural_vuln_score present and in [0, 1]
# ---------------------------------------------------------------------------
test_that("STRUCT-01: structural_vuln_score present and in [0, 1]", {
  expect_true("structural_vuln_score" %in% names(scores_int))
  expect_true(all(scores_int$structural_vuln_score >= 0 & scores_int$structural_vuln_score <= 1, na.rm = TRUE))
})

# ---------------------------------------------------------------------------
# STRUCT-02: structural_gap_usd present and all values > 0
# ---------------------------------------------------------------------------
test_that("STRUCT-02: structural_gap_usd present and all values > 0", {
  expect_true("structural_gap_usd" %in% names(scores_int))
  expect_true(all(scores_int$structural_gap_usd > 0, na.rm = TRUE))
})

# ---------------------------------------------------------------------------
# REC-01: recovery_readiness_score present and in [0, 1]
# ---------------------------------------------------------------------------
test_that("REC-01: recovery_readiness_score present and in [0, 1]", {
  expect_true("recovery_readiness_score" %in% names(scores_int))
  expect_true(all(scores_int$recovery_readiness_score >= 0 & scores_int$recovery_readiness_score <= 1, na.rm = TRUE))
})

# ---------------------------------------------------------------------------
# REC-02: GDP-weighted mean protection_gap is within [0.76, 0.86] (Swiss Re sigma target)
# ---------------------------------------------------------------------------
test_that("REC-02: GDP-weighted mean protection_gap is within [0.76, 0.86] (Swiss Re sigma target)", {
  expect_true("protection_gap" %in% names(scores_int))
  wt_mean <- weighted.mean(scores_int$protection_gap, scores_int$gdp_usd)
  expect_true(wt_mean >= 0.76 && wt_mean <= 0.86,
              info = paste("Weighted protection gap mean:", round(wt_mean, 3)))
})

# ---------------------------------------------------------------------------
# REC-03: parametric_flag column present and is character
# ---------------------------------------------------------------------------
test_that("REC-03: parametric_flag column present and is character", {
  expect_true("parametric_flag" %in% names(scores_int))
  expect_true(is.character(scores_int$parametric_flag))
})

# ---------------------------------------------------------------------------
# SENDAI-01: sendai_target_e and sendai_target_g columns present
# ---------------------------------------------------------------------------
test_that("SENDAI-01: sendai_target_e and sendai_target_g columns present in scores_intermediate", {
  expect_true("sendai_target_e" %in% names(scores_int))
  expect_true("sendai_target_g" %in% names(scores_int))
})

# ---------------------------------------------------------------------------
# SENDAI-02: Sendai target columns contain exactly the three ordinal strings
# ---------------------------------------------------------------------------
test_that("SENDAI-02: Sendai target columns contain exactly the three ordinal strings", {
  valid_ordinal <- c("Reported", "Partial", "Not Reported")
  expect_true(all(scores_int$sendai_target_e %in% valid_ordinal))
  expect_true(all(scores_int$sendai_target_g %in% valid_ordinal))
  expect_true(is.character(scores_int$sendai_target_e))
  expect_true(is.character(scores_int$sendai_target_g))
})

# ---------------------------------------------------------------------------
# EXP-01 (function-level): score_exposure() direct behavioural tests
# ---------------------------------------------------------------------------
test_that("EXP-01: score_exposure() adds exposure_score in [0,1] with no NA for all 27 countries", {
  result <- score_exposure(lac)
  expect_true("exposure_score" %in% names(result),
              info = "EXP-01: exposure_score column must be present")
  expect_equal(nrow(result), 27L)
  expect_false(any(is.na(result$exposure_score)),
               info = "EXP-01: NA values found in exposure_score")
  expect_true(all(result$exposure_score >= 0 & result$exposure_score <= 1),
              info = "EXP-01: exposure_score values outside [0,1]")
})

test_that("EXP-01: Brazil and Mexico have higher exposure than Caribbean SIDS average", {
  result    <- score_exposure(lac)
  bra_score <- result$exposure_score[result$iso3 == "BRA"]
  mex_score <- result$exposure_score[result$iso3 == "MEX"]
  sids_mean <- mean(result$exposure_score[result$sub_region == "Caribbean SIDS"],
                    na.rm = TRUE)
  expect_true(bra_score > sids_mean,
              info = paste("BRA", round(bra_score, 3), "not > SIDS mean", round(sids_mean, 3)))
  expect_true(mex_score > sids_mean,
              info = paste("MEX", round(mex_score, 3), "not > SIDS mean", round(sids_mean, 3)))
})

test_that("EXP-01: Haiti exposure_score > 0.05 (infrastructure proxy elevates exposure above small-GDP SIDS floor)", {
  result    <- score_exposure(lac)
  hti_score <- result$exposure_score[result$iso3 == "HTI"]
  expect_true(hti_score > 0.05,
              info = paste("Haiti exposure_score =", round(hti_score, 4), "(expected > 0.05)"))
})

# ---------------------------------------------------------------------------
# EXP-02 (function-level): score_vulnerability() direct behavioural tests
# ---------------------------------------------------------------------------
test_that("EXP-02: score_vulnerability() adds vulnerability_score in [0,1] with no NA", {
  result <- score_vulnerability(lac)
  expect_true("vulnerability_score" %in% names(result),
              info = "EXP-02: vulnerability_score column must be present")
  expect_equal(nrow(result), 27L)
  expect_false(any(is.na(result$vulnerability_score)),
               info = "EXP-02: NA values found in vulnerability_score")
  expect_true(all(result$vulnerability_score >= 0 & result$vulnerability_score <= 1),
              info = "EXP-02: vulnerability_score values outside [0,1]")
})

test_that("EXP-02: Haiti vulnerability_score > 0.70 (poor governance + low life_exp + high INFORM)", {
  result   <- score_vulnerability(lac)
  hti_vuln <- result$vulnerability_score[result$iso3 == "HTI"]
  expect_true(hti_vuln > 0.70,
              info = paste("Haiti vulnerability_score =", round(hti_vuln, 4), "(expected > 0.70)"))
})

test_that("EXP-02: governance inversion — Chile and Costa Rica lower vulnerability than Caribbean SIDS mean", {
  result    <- score_vulnerability(lac)
  chl_vuln  <- result$vulnerability_score[result$iso3 == "CHL"]
  cri_vuln  <- result$vulnerability_score[result$iso3 == "CRI"]
  sids_mean <- mean(result$vulnerability_score[result$sub_region == "Caribbean SIDS"],
                    na.rm = TRUE)
  expect_true(chl_vuln < sids_mean,
              info = paste("Chile", round(chl_vuln, 3), "not < SIDS mean", round(sids_mean, 3)))
  expect_true(cri_vuln < sids_mean,
              info = paste("Costa Rica", round(cri_vuln, 3), "not < SIDS mean", round(sids_mean, 3)))
})
