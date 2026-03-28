# R/03_score.R
# Phase 3: Analytical Scoring Pipeline
# Computes all scoring modules and writes country_scores.csv + scores_intermediate.csv
# Run from repo root: Rscript R/03_score.R
library(here)
source(here::here("shared/R/utils.R"))

# ============================================================
# CONSTANTS
# ============================================================

#' Ordinal hazard level → numeric score mapping
#' Source: INFORM Risk Index methodology; ThinkHazard classification alignment
#' CRITICAL: VLO = 0.1 (not 0.0) — geometric mean requires all values > 0
HAZARD_ORDINAL_MAP <- c(VLO = 0.1, LOW = 0.25, MED = 0.50, HIG = 0.90)

#' Default hazard weights (sum = 1.0)
#' Source: expert elicitation anchored to INFORM multi-hazard methodology
HAZARD_WEIGHTS <- c(
  hazard_EQ_num = 0.20,
  hazard_FL_num = 0.20,
  hazard_CY_num = 0.20,
  hazard_VO_num = 0.15,
  hazard_LS_num = 0.15,
  hazard_DR_num = 0.10
)

# ============================================================
# MODULE 1: HAZARD SCORING (HAZ-01, HAZ-02, HAZ-03, HAZ-04)
# ============================================================

#' Compute composite hazard index for all countries
#'
#' @param lac Data frame loaded from data/processed/lac_master.csv
#' @return lac with hazard_composite column added (numeric [0,1])
score_hazard <- function(lac) {
  hazard_cols <- c("hazard_EQ", "hazard_FL", "hazard_CY",
                   "hazard_VO", "hazard_LS", "hazard_DR")

  # Step 1: ordinal → numeric via HAZARD_ORDINAL_MAP
  for (col in hazard_cols) {
    lac[[paste0(col, "_num")]] <- HAZARD_ORDINAL_MAP[lac[[col]]]
  }

  num_cols <- paste0(hazard_cols, "_num")

  # Step 2: weighted geometric mean (geo_mean_weighted from hazard_utils.R)
  raw_scores <- geo_mean_weighted(lac, num_cols, HAZARD_WEIGHTS)

  # Step 3: percentile normalisation (prevents extreme outlier compression)
  lac$hazard_composite <- normalise_to_01(raw_scores, method = "percentile")

  message("score_hazard: completed — ",
          "Haiti=", round(lac$hazard_composite[lac$iso3 == "HTI"], 3),
          " Guatemala=", round(lac$hazard_composite[lac$iso3 == "GTM"], 3))
  lac
}

#' Sensitivity analysis: perturb hazard weights ±20%, check rank stability
#'
#' @param df Data frame with hazard_composite column (output of score_hazard)
#' @param base_weights Named numeric vector; defaults to HAZARD_WEIGHTS
#' @param n_perturb Number of perturbation iterations (default 100)
#' @return TRUE if min Spearman rho >= 0.90, FALSE otherwise
sensitivity_test <- function(df, base_weights = NULL, n_perturb = 100) {
  if (is.null(base_weights)) base_weights <- HAZARD_WEIGHTS
  set.seed(42)
  num_cols <- names(base_weights)
  base_ranks <- rank(-df$hazard_composite)
  cors <- replicate(n_perturb, {
    noise  <- stats::runif(length(base_weights), 0.8, 1.2)
    w_pert <- (base_weights * noise) / sum(base_weights * noise)
    perturbed_scores <- geo_mean_weighted(df, num_cols, w_pert)
    stats::cor(base_ranks, rank(-perturbed_scores), method = "spearman")
  })
  min_rho <- min(cors)
  message("sensitivity_test: min rho = ", round(min_rho, 3),
          "; mean rho = ", round(mean(cors), 3))
  min_rho >= 0.90
}

# ============================================================
# MODULE STUBS (filled in plans 03-02 through 03-07)
# ============================================================

score_exposure     <- function(lac) { message("score_exposure: stub"); lac }
score_vulnerability <- function(lac) { message("score_vulnerability: stub"); lac }
score_ews          <- function(lac) { message("score_ews: stub"); lac }
score_nbs          <- function(lac) { message("score_nbs: stub"); lac }
score_structural   <- function(lac) { message("score_structural: stub"); lac }
score_recovery     <- function(lac) { message("score_recovery: stub"); lac }
score_sendai       <- function(lac) { message("score_sendai: stub"); lac }
assemble_outputs   <- function(lac) { message("assemble_outputs: stub"); lac }

# ============================================================
# MAIN PIPELINE (run when sourced directly)
# ============================================================

if (sys.nframe() == 0L) {
  message("03_score.R: loading lac_master.csv")
  lac <- readr::read_csv(
    here::here("data/processed/lac_master.csv"),
    show_col_types = FALSE
  )

  lac <- score_hazard(lac)
  lac <- score_exposure(lac)
  lac <- score_vulnerability(lac)
  lac <- score_ews(lac)
  lac <- score_nbs(lac)
  lac <- score_structural(lac)
  lac <- score_recovery(lac)
  lac <- score_sendai(lac)
  lac <- assemble_outputs(lac)

  message("03_score.R: pipeline complete (stubs active for non-hazard modules)")
}
