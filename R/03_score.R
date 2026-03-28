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
# MODULE 2: EXPOSURE SCORING (EXP-01)
# ============================================================

#' Compute exposure index per country
#'
#' Weighted arithmetic mean of three normalised exposure components.
#' Exposure is additive (not multiplicative) — a country can have high
#' exposure from population OR GDP, unlike hazard which requires co-occurrence.
#'
#' @param lac Data frame from lac_master.csv
#' @return lac with exposure_score column added (numeric [0,1])
score_exposure <- function(lac) {
  # Component 1: GDP exposure (larger economy = higher absolute exposure)
  gdp_norm     <- normalise_to_01(lac$gdp_usd, method = "minmax")

  # Component 2: Population exposure
  pop_norm     <- normalise_to_01(lac$pop_total, method = "minmax")

  # Component 3: Infrastructure proxy — loss rate as % GDP
  # Higher losses_pct_gdp = more exposed infrastructure
  # Guard against NA with coalesce
  losses_pct   <- dplyr::coalesce(lac$losses_pct_gdp, 0)
  infra_norm   <- normalise_to_01(losses_pct, method = "minmax")

  # Weighted arithmetic mean (exposure is additive)
  lac$exposure_score <- 0.40 * gdp_norm + 0.40 * pop_norm + 0.20 * infra_norm

  message("score_exposure: completed — ",
          "Brazil=", round(lac$exposure_score[lac$iso3 == "BRA"], 3),
          " Haiti=", round(lac$exposure_score[lac$iso3 == "HTI"], 3))
  lac
}

# ============================================================
# MODULE 3: VULNERABILITY SCORING (EXP-02)
# ============================================================

#' Compute vulnerability index per country
#'
#' Combines HDI proxy (life expectancy), institutional capacity (WGI), and
#' INFORM Risk score. Governance indicators are INVERTED: high governance
#' capacity means lower vulnerability. Life expectancy is also inverted.
#'
#' @param lac Data frame from lac_master.csv
#' @return lac with vulnerability_score column added (numeric [0,1])
score_vulnerability <- function(lac) {
  # --- life expectancy: lower = higher vulnerability → invert after normalising ---
  life_norm   <- normalise_to_01(lac$life_exp, method = "minmax")
  life_vuln   <- 1 - life_norm  # inversion: low life_exp → high vulnerability

  # --- governance effectiveness: normalise from [-2.5, 2.5] to [0,1] then invert ---
  gov_clipped <- pmin(pmax(dplyr::coalesce(lac$gov_eff, 0), -2.5), 2.5)
  gov_norm    <- (gov_clipped + 2.5) / 5.0  # linear map [-2.5,2.5] → [0,1]
  gov_vuln    <- 1 - gov_norm  # inversion: high governance → low vulnerability

  # --- rule of law: same normalisation and inversion as gov_eff ---
  rl_clipped  <- pmin(pmax(dplyr::coalesce(lac$rule_of_law, 0), -2.5), 2.5)
  rl_norm     <- (rl_clipped + 2.5) / 5.0
  rl_vuln     <- 1 - rl_norm

  # --- INFORM risk score: already directional (higher = more vulnerable) ---
  inform_norm <- normalise_to_01(lac$inform_risk_score, method = "minmax")

  # Weighted mean: life_exp 30%, gov_eff 30%, rule_of_law 20%, INFORM 20%
  lac$vulnerability_score <- (0.30 * life_vuln) +
                             (0.30 * gov_vuln)  +
                             (0.20 * rl_vuln)   +
                             (0.20 * inform_norm)

  # Ensure output is strictly in [0,1] (floating point safety)
  lac$vulnerability_score <- pmin(pmax(lac$vulnerability_score, 0), 1)

  message("score_vulnerability: completed — ",
          "Haiti=", round(lac$vulnerability_score[lac$iso3 == "HTI"], 3),
          " Chile=", round(lac$vulnerability_score[lac$iso3 == "CHL"], 3))
  lac
}

# ============================================================
# MODULE 4: EWS READINESS SCORING (EWS-01, EWS-02)
# ============================================================

#' Compute EWS readiness score and gap tier per country
#'
#' Synthetic 4-dimension scoring using governance and INFORM proxies.
#' Gap tier identifies priority investment targets: countries with high
#' disaster risk exposure but low early warning system capacity.
#'
#' @param lac Data frame from lac_master.csv (after score_hazard() optionally run)
#' @return lac with ews_readiness_score [0,1] and ews_gap_tier character columns
score_ews <- function(lac) {
  # --- Normalise inputs ---
  gov_clipped  <- pmin(pmax(dplyr::coalesce(lac$gov_eff, 0), -2.5), 2.5)
  gov_norm     <- (gov_clipped + 2.5) / 5.0  # [0,1]; higher = better capacity

  rl_clipped   <- pmin(pmax(dplyr::coalesce(lac$rule_of_law, 0), -2.5), 2.5)
  rl_norm      <- (rl_clipped + 2.5) / 5.0

  inform_norm  <- normalise_to_01(lac$inform_risk_score, method = "minmax")
  # INFORM is inverted: lower INFORM risk -> better dissemination capacity
  inform_cap   <- 1 - inform_norm

  # SIDS reliability penalty: SIDS tend to have lower EWS infrastructure
  sids_penalty <- ifelse(lac$sids_reliability_flag, 0.10, 0.0)

  # --- 4 EWS dimensions ---
  # D1: Monitoring capacity (governance proxy)
  d1_monitoring     <- gov_norm

  # D2: Warning dissemination (INFORM capacity proxy)
  d2_dissemination  <- inform_cap

  # D3: Community preparedness (governance - SIDS penalty)
  d3_preparedness   <- pmax(gov_norm - sids_penalty, 0)

  # D4: Institutional coordination (avg of gov_eff + rule_of_law)
  d4_coordination   <- (gov_norm + rl_norm) / 2

  # Equal-weight average of 4 dimensions
  lac$ews_readiness_score <- (d1_monitoring + d2_dissemination +
                              d3_preparedness + d4_coordination) / 4

  # --- EWS gap tier: priority investment classification ---
  # Uses inform_risk_score as demand signal (hazard_composite not guaranteed
  # available when this module runs in isolation)
  lac$ews_gap_tier <- dplyr::case_when(
    lac$inform_risk_score >= 6.5 & lac$ews_readiness_score < 0.35 ~ "Critical",
    lac$inform_risk_score >= 5.0 & lac$ews_readiness_score < 0.50 ~ "High",
    lac$inform_risk_score >= 3.5 | lac$ews_readiness_score < 0.60 ~ "Moderate",
    TRUE                                                            ~ "Low"
  )

  message("score_ews: completed — ",
          "Haiti=", lac$ews_gap_tier[lac$iso3 == "HTI"],
          " Chile=", lac$ews_gap_tier[lac$iso3 == "CHL"])
  lac
}

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

  # Write scores_intermediate.csv — accumulates implemented scoring columns;
  # stubs leave columns absent (tests for unimplemented modules fail RED as designed)
  readr::write_csv(lac, here::here("data/processed/scores_intermediate.csv"))
  message("03_score.R: scores_intermediate.csv written (",
          nrow(lac), " rows, ", ncol(lac), " columns)")

  message("03_score.R: pipeline complete (stubs active for non-implemented modules)")
}
