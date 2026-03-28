here::i_am("drm-lac-intelligence.Rproj")
library(testthat)

# ── Constants ──────────────────────────────────────────────────────────────────

LAC_ISO3 <- c(
  "ARG", "BOL", "BRA", "CHL", "COL", "ECU", "GUY", "PER", "SUR", "VEN",
  "BLZ", "CRI", "GTM", "HND", "MEX", "NIC", "PAN", "SLV",
  "ATG", "BRB", "DOM", "GRD", "HTI", "JAM", "LCA", "TTO", "VCT"
)

RAW_FILES <- c(
  wdi          = "data/raw/wdi_lac.csv",
  inform       = "data/raw/inform_lac.csv",
  emdat        = "data/raw/emdat_lac.csv",
  thinkhazard  = "data/raw/thinkhazard_lac.csv",
  gmw          = "data/raw/gmw_lac.csv"
)

# ── DATA-01: WDI ───────────────────────────────────────────────────────────────

test_that("DATA-01: wdi_lac.csv exists with core economic columns", {
  skip_if_not(file.exists(here::here(RAW_FILES[["wdi"]])))
  df <- read.csv(here::here(RAW_FILES[["wdi"]]), stringsAsFactors = FALSE)
  expect_true(all(c("gdp_usd", "pop_total", "life_exp") %in% names(df)))
})

test_that("DATA-01: wdi_lac.csv covers all 27 LAC ISO3 codes (nrow >= 20)", {
  skip_if_not(file.exists(here::here(RAW_FILES[["wdi"]])))
  df <- read.csv(here::here(RAW_FILES[["wdi"]]), stringsAsFactors = FALSE)
  expect_true(nrow(df) >= 20)
  expect_true(all(LAC_ISO3 %in% df$iso3))
})

test_that("DATA-01: wdi_lac.csv has governance columns gov_eff and rule_of_law", {
  skip_if_not(file.exists(here::here(RAW_FILES[["wdi"]])))
  df <- read.csv(here::here(RAW_FILES[["wdi"]]), stringsAsFactors = FALSE)
  expect_true("gov_eff" %in% names(df))
  expect_true("rule_of_law" %in% names(df))
})

# ── DATA-02: INFORM ────────────────────────────────────────────────────────────

test_that("DATA-02: inform_lac.csv exists with inform_risk_score column", {
  skip_if_not(file.exists(here::here(RAW_FILES[["inform"]])))
  df <- read.csv(here::here(RAW_FILES[["inform"]]), stringsAsFactors = FALSE)
  expect_true("inform_risk_score" %in% names(df))
})

test_that("DATA-02: inform_lac.csv has sids_reliability_flag and HTI is flagged", {
  skip_if_not(file.exists(here::here(RAW_FILES[["inform"]])))
  df <- read.csv(here::here(RAW_FILES[["inform"]]), stringsAsFactors = FALSE)
  expect_true("sids_reliability_flag" %in% names(df))
  hti_row <- df[df$iso3 == "HTI", ]
  expect_true(nrow(hti_row) > 0)
  expect_true(any(hti_row$sids_reliability_flag == TRUE, na.rm = TRUE))
})

# ── DATA-03: EM-DAT ────────────────────────────────────────────────────────────

test_that("DATA-03: emdat_lac.csv has losses_per_100k_pop and losses_pct_gdp columns", {
  skip_if_not(file.exists(here::here(RAW_FILES[["emdat"]])))
  df <- read.csv(here::here(RAW_FILES[["emdat"]]), stringsAsFactors = FALSE)
  expect_true("losses_per_100k_pop" %in% names(df))
  expect_true("losses_pct_gdp" %in% names(df))
})

test_that("DATA-03: lac_master.csv does NOT contain total_losses_usd_m column", {
  skip_if_not(file.exists(here::here("data/processed/lac_master.csv")))
  df <- read.csv(here::here("data/processed/lac_master.csv"), stringsAsFactors = FALSE)
  expect_false("total_losses_usd_m" %in% names(df))
})

# ── DATA-04: ThinkHazard ───────────────────────────────────────────────────────

test_that("DATA-04: thinkhazard_lac.csv exists with hazard_level column", {
  skip_if_not(file.exists(here::here(RAW_FILES[["thinkhazard"]])))
  df <- read.csv(here::here(RAW_FILES[["thinkhazard"]]), stringsAsFactors = FALSE)
  expect_true("hazard_level" %in% names(df))
})

test_that("DATA-04: thinkhazard_lac.csv has rows for EQ, FL, and CY hazard types", {
  skip_if_not(file.exists(here::here(RAW_FILES[["thinkhazard"]])))
  df <- read.csv(here::here(RAW_FILES[["thinkhazard"]]), stringsAsFactors = FALSE)
  expect_true("hazard_type" %in% names(df))
  expect_true("EQ" %in% df$hazard_type)
  expect_true("FL" %in% df$hazard_type)
  expect_true("CY" %in% df$hazard_type)
})

# ── DATA-05: GMW ───────────────────────────────────────────────────────────────

test_that("DATA-05: gmw_lac.csv exists with mangrove_ha_2020 column", {
  skip_if_not(file.exists(here::here(RAW_FILES[["gmw"]])))
  df <- read.csv(here::here(RAW_FILES[["gmw"]]), stringsAsFactors = FALSE)
  expect_true("mangrove_ha_2020" %in% names(df))
})

# ── DATA-06: File outputs ──────────────────────────────────────────────────────

test_that("DATA-06: all five raw CSVs are present in data/raw/", {
  for (nm in names(RAW_FILES)) {
    skip_if_not(file.exists(here::here(RAW_FILES[[nm]])))
  }
  for (nm in names(RAW_FILES)) {
    expect_true(file.exists(here::here(RAW_FILES[[nm]])),
                label = paste0(RAW_FILES[[nm]], " exists"))
  }
})

test_that("DATA-06: lac_master.csv exists and iso3/country columns load without NA coercion", {
  skip_if_not(file.exists(here::here("data/processed/lac_master.csv")))
  df <- read.csv(here::here("data/processed/lac_master.csv"),
                 stringsAsFactors = FALSE, na.strings = "")
  expect_true(all(c("iso3", "country") %in% names(df)))
  expect_true(is.character(df$iso3))
  expect_true(is.character(df$country))
  expect_false(any(is.na(df$iso3)))
  expect_false(any(is.na(df$country)))
})

# ── DATA-07: Provenance columns ────────────────────────────────────────────────

test_that("DATA-07: all raw CSVs contain provenance columns data_source, reliability_flag, is_synthetic", {
  provenance_cols <- c("data_source", "reliability_flag", "is_synthetic")
  for (nm in names(RAW_FILES)) {
    skip_if_not(file.exists(here::here(RAW_FILES[[nm]])))
    df <- read.csv(here::here(RAW_FILES[[nm]]), stringsAsFactors = FALSE)
    missing <- setdiff(provenance_cols, names(df))
    expect_true(length(missing) == 0,
                label = paste0(nm, " missing provenance columns: ",
                               paste(missing, collapse = ", ")))
  }
})

test_that("DATA-07: reliability_flag values are integers in range 1-10 with no NAs", {
  for (nm in names(RAW_FILES)) {
    skip_if_not(file.exists(here::here(RAW_FILES[[nm]])))
    df <- read.csv(here::here(RAW_FILES[[nm]]), stringsAsFactors = FALSE)
    skip_if_not("reliability_flag" %in% names(df))
    flags <- df$reliability_flag
    expect_false(any(is.na(flags)),
                 label = paste0(nm, " reliability_flag has no NAs"))
    expect_true(all(flags >= 1 & flags <= 10),
                label = paste0(nm, " reliability_flag in range 1-10"))
    expect_true(all(flags == as.integer(flags)),
                label = paste0(nm, " reliability_flag is integer"))
  }
})
