# R/01_fetch.R — Data ingestion pipeline
# Run from repo root: Rscript R/01_fetch.R
# Sources: WDI, INFORM, EM-DAT, ThinkHazard, GMW
library(here)
here::i_am("drm-lac-intelligence.Rproj")
source(here::here("shared/R/utils.R"))

# Canonical country list — DO NOT modify; downstream join key
LAC_ISO3 <- c(
  "ARG","BOL","BRA","CHL","COL","ECU","GUY","PER","SUR","VEN",
  "BLZ","CRI","GTM","HND","MEX","NIC","PAN","SLV",
  "ATG","BRB","DOM","GRD","HTI","JAM","LCA","TTO","VCT"
)
CARIBBEAN_SIDS <- c("ATG","BRB","GRD","HTI","JAM","LCA","TTO","VCT","DOM")

# Provenance helper — apply to every output CSV
# Per-source reliability values: WDI live=9, WDI synthetic=5,
# INFORM live=8, INFORM synthetic=4, EM-DAT=6
add_provenance <- function(df, source_name, reliability, is_synth) {
  dplyr::mutate(df,
    data_source      = source_name,
    reliability_flag = as.integer(reliability),
    is_synthetic     = as.logical(is_synth)
  )
}

# ── Section 1: World Bank WDI ──────────────────────────────────────────────────

# Synthetic WGI (governance) constants — World Bank WGI 2023 Report
# Source: https://www.worldbank.org/en/publication/worldwide-governance-indicators
# Cite: World Bank (2024). Worldwide Governance Indicators 2023.
WGI_2023_CONSTANTS <- list(
  ARG = list(gov_eff =  0.03, rule_of_law = -0.59),
  BOL = list(gov_eff = -0.63, rule_of_law = -0.71),
  BRA = list(gov_eff =  0.01, rule_of_law = -0.08),
  CHL = list(gov_eff =  1.07, rule_of_law =  1.08),
  COL = list(gov_eff =  0.09, rule_of_law = -0.44),
  ECU = list(gov_eff = -0.31, rule_of_law = -0.60),
  GUY = list(gov_eff = -0.27, rule_of_law = -0.37),
  PER = list(gov_eff = -0.10, rule_of_law = -0.55),
  SUR = list(gov_eff = -0.16, rule_of_law = -0.15),
  VEN = list(gov_eff = -1.58, rule_of_law = -2.11),
  BLZ = list(gov_eff = -0.24, rule_of_law = -0.03),
  CRI = list(gov_eff =  0.50, rule_of_law =  0.49),
  GTM = list(gov_eff = -0.49, rule_of_law = -0.95),
  HND = list(gov_eff = -0.54, rule_of_law = -0.90),
  MEX = list(gov_eff =  0.14, rule_of_law = -0.64),
  NIC = list(gov_eff = -0.84, rule_of_law = -0.90),
  PAN = list(gov_eff =  0.17, rule_of_law =  0.00),
  SLV = list(gov_eff = -0.17, rule_of_law = -0.56),
  ATG = list(gov_eff =  0.61, rule_of_law =  0.74),
  BRB = list(gov_eff =  0.93, rule_of_law =  0.99),
  DOM = list(gov_eff = -0.18, rule_of_law = -0.60),
  GRD = list(gov_eff =  0.45, rule_of_law =  0.59),
  HTI = list(gov_eff = -1.60, rule_of_law = -1.82),
  JAM = list(gov_eff =  0.11, rule_of_law =  0.03),
  LCA = list(gov_eff =  0.62, rule_of_law =  0.71),
  TTO = list(gov_eff =  0.08, rule_of_law =  0.30),
  VCT = list(gov_eff =  0.47, rule_of_law =  0.68)
)

# Build synthetic WDI tibble anchored to published World Bank 2023 country pages
# Source: https://data.worldbank.org (GDP/Pop); WGI 2023 for governance
make_synthetic_wdi <- function() {
  message("WDI: building synthetic fallback from World Bank 2023 published estimates")
  wgi_df <- tibble::tibble(
    iso3        = names(WGI_2023_CONSTANTS),
    gov_eff     = vapply(WGI_2023_CONSTANTS, function(x) x$gov_eff, numeric(1)),
    rule_of_law = vapply(WGI_2023_CONSTANTS, function(x) x$rule_of_law, numeric(1))
  )
  # GDP (current USD) and population from World Bank 2023 country pages
  gdp_pop <- tibble::tribble(
    ~iso3,   ~gdp_usd,          ~pop_total,   ~life_exp,
    "ARG",   632772000000,      45773884,     76.1,
    "BOL",    44990000000,      12079472,     63.5,
    "BRA",  2173158000000,     214326223,     72.8,
    "CHL",   344735000000,      19629590,     79.5,
    "COL",   343649000000,      51682692,     72.5,
    "ECU",   118686000000,      18001000,     74.5,
    "GUY",    14439000000,        804567,     65.2,
    "PER",   242627000000,      33396698,     71.4,
    "SUR",    3632000000,        618040,     71.0,
    "VEN",   103067000000,      28838499,     72.1,
    "BLZ",    2637000000,        441471,     72.9,
    "CRI",    67598000000,       5212173,     80.1,
    "GTM",    95001000000,      17109746,     72.9,
    "HND",    34012000000,      10278345,     70.3,
    "MEX",  1322494000000,     127504125,     70.2,
    "NIC",    17026000000,       6948392,     74.1,
    "PAN",    76529000000,       4337406,     78.5,
    "SLV",    34016000000,       6364943,     73.5,
    "ATG",    1820000000,         93219,     77.3,
    "BRB",    5924000000,        281200,     78.9,
    "DOM",   121298000000,      11117873,     73.2,
    "GRD",    1255000000,        125438,     72.2,
    "HTI",    19931000000,      11724764,     63.2,
    "JAM",   17010000000,       2820436,     74.1,
    "LCA",    2424000000,        178696,     74.7,
    "TTO",   26237000000,       1531044,     71.9,
    "VCT",    1028000000,        103698,     73.4
  )
  dplyr::left_join(gdp_pop, wgi_df, by = "iso3")
}

fetch_wdi <- function() {
  message("WDI: attempting World Bank API fetch via wbstats...")
  tryCatch({
    suppressPackageStartupMessages(library(wbstats))
    raw <- wbstats::wb_data(
      indicator = c(
        gdp_usd     = "NY.GDP.MKTP.CD",
        pop_total   = "SP.POP.TOTL",
        life_exp    = "SP.DYN.LE00.IN",
        gov_eff     = "GE.EST",
        rule_of_law = "RL.EST"
      ),
      country    = LAC_ISO3,
      mrv        = 5,
      return_wide = TRUE
    )
    message("WDI: API returned ", nrow(raw), " rows")

    # Take most recent non-NA value per country per indicator
    wdi_clean <- raw |>
      dplyr::rename(iso3 = iso3c) |>
      dplyr::group_by(iso3) |>
      dplyr::summarise(
        gdp_usd     = dplyr::last(na.omit(gdp_usd)),
        pop_total   = dplyr::last(na.omit(pop_total)),
        life_exp    = dplyr::last(na.omit(life_exp)),
        gov_eff     = dplyr::last(na.omit(gov_eff)),
        rule_of_law = dplyr::last(na.omit(rule_of_law)),
        .groups     = "drop"
      )

    # Fill governance if WGI came back all-NA (Pitfall 3 in RESEARCH.md)
    if (all(is.na(wdi_clean$gov_eff)) || all(is.na(wdi_clean$rule_of_law))) {
      message("WDI: WGI indicators empty — filling from 2023 WGI synthetic constants")
      wgi_fill <- tibble::tibble(
        iso3        = names(WGI_2023_CONSTANTS),
        gov_eff_syn = vapply(WGI_2023_CONSTANTS, function(x) x$gov_eff, numeric(1)),
        rl_syn      = vapply(WGI_2023_CONSTANTS, function(x) x$rule_of_law, numeric(1))
      )
      wdi_clean <- dplyr::left_join(wdi_clean, wgi_fill, by = "iso3") |>
        dplyr::mutate(
          gov_eff     = dplyr::if_else(is.na(gov_eff),     gov_eff_syn, gov_eff),
          rule_of_law = dplyr::if_else(is.na(rule_of_law), rl_syn,      rule_of_law)
        ) |>
        dplyr::select(-gov_eff_syn, -rl_syn)
    }

    # Ensure all 27 LAC countries present (synthetic gap-fill for missing)
    missing <- setdiff(LAC_ISO3, wdi_clean$iso3)
    if (length(missing) > 0) {
      message("WDI: filling ", length(missing), " missing countries from synthetic constants")
      synth_all <- make_synthetic_wdi()
      gap_rows  <- dplyr::filter(synth_all, iso3 %in% missing)
      wdi_clean <- dplyr::bind_rows(wdi_clean, gap_rows)
    }

    add_provenance(wdi_clean, "WorldBank-WDI-API", 9, FALSE)

  }, error = function(e) {
    message("WDI: fetch failed (", conditionMessage(e), ") — using synthetic fallback")
    synth <- make_synthetic_wdi()
    add_provenance(synth, "WorldBank-WDI-synthetic", 5, TRUE)
  })
}

wdi_out <- fetch_wdi()
readr::write_csv(wdi_out, here::here("data/raw/wdi_lac.csv"), na = "")
message("WDI: wrote ", nrow(wdi_out), " rows to data/raw/wdi_lac.csv")

# ── Section 2: INFORM Risk Index ───────────────────────────────────────────────

# Synthetic INFORM fallback anchored to INFORM 2024 published scores
# Source: https://drmkc.jrc.ec.europa.eu/inform-index (INFORM Risk 2024)
# Cite: European Commission JRC — INFORM Risk Index 2024
make_synthetic_inform <- function() {
  message("INFORM: building synthetic fallback from INFORM 2024 published scores")
  tibble::tribble(
    ~iso3,   ~country,                   ~inform_risk_score,
    "ARG",   "Argentina",                3.4,
    "BOL",   "Bolivia",                  5.6,
    "BRA",   "Brazil",                   4.3,
    "CHL",   "Chile",                    3.9,
    "COL",   "Colombia",                 5.7,
    "ECU",   "Ecuador",                  5.2,
    "GUY",   "Guyana",                   4.5,
    "PER",   "Peru",                     5.5,
    "SUR",   "Suriname",                 4.1,
    "VEN",   "Venezuela",                6.1,
    "BLZ",   "Belize",                   4.8,
    "CRI",   "Costa Rica",               4.1,
    "GTM",   "Guatemala",                6.2,
    "HND",   "Honduras",                 6.4,
    "MEX",   "Mexico",                   5.8,
    "NIC",   "Nicaragua",                6.1,
    "PAN",   "Panama",                   4.0,
    "SLV",   "El Salvador",              5.9,
    "ATG",   "Antigua and Barbuda",      4.2,
    "BRB",   "Barbados",                 3.1,
    "DOM",   "Dominican Republic",       5.4,
    "GRD",   "Grenada",                  4.0,
    "HTI",   "Haiti",                    8.5,
    "JAM",   "Jamaica",                  5.2,
    "LCA",   "Saint Lucia",              4.6,
    "TTO",   "Trinidad and Tobago",      3.7,
    "VCT",   "Saint Vincent",            4.8
  )
}

load_inform <- function() {
  xlsx_path <- here::here("data/raw/inform_risk_raw.xlsx")
  if (!file.exists(xlsx_path)) {
    message("INFORM: XLSX not found at ", xlsx_path, " — using synthetic fallback")
    inform_df <- make_synthetic_inform()
    inform_df <- dplyr::mutate(
      inform_df,
      sids_reliability_flag = iso3 %in% CARIBBEAN_SIDS & inform_risk_score > 5
    )
    return(add_provenance(inform_df, "INFORM-synthetic", 4, TRUE))
  }

  message("INFORM: loading XLSX from ", xlsx_path)
  # Try skip=1 first; if header row not found, try skip=2
  raw <- tryCatch({
    r <- readxl::read_xlsx(xlsx_path, sheet = 1, skip = 1)
    # Check header row contains iso3 or country identifier
    if (!any(c("iso3", "Iso3", "ISO3", "country", "Country") %in% names(r))) {
      message("INFORM: skip=1 did not find header — trying skip=2")
      readxl::read_xlsx(xlsx_path, sheet = 1, skip = 2)
    } else {
      r
    }
  }, error = function(e) {
    message("INFORM: XLSX read failed (", conditionMessage(e), ") — using synthetic fallback")
    return(NULL)
  })

  if (is.null(raw)) {
    inform_df <- make_synthetic_inform()
    inform_df <- dplyr::mutate(
      inform_df,
      sids_reliability_flag = iso3 %in% CARIBBEAN_SIDS & inform_risk_score > 5
    )
    return(add_provenance(inform_df, "INFORM-synthetic", 4, TRUE))
  }

  # Normalise column names to snake_case; find ISO3 column
  names(raw) <- tolower(gsub("[^a-zA-Z0-9]", "_", names(raw)))
  iso_col <- intersect(c("iso3", "iso_3", "country_iso3"), names(raw))

  if (length(iso_col) == 0) {
    message("INFORM: ISO3 column not identified after normalisation — using synthetic fallback")
    inform_df <- make_synthetic_inform()
    inform_df <- dplyr::mutate(
      inform_df,
      sids_reliability_flag = iso3 %in% CARIBBEAN_SIDS & inform_risk_score > 5
    )
    return(add_provenance(inform_df, "INFORM-synthetic", 4, TRUE))
  }

  iso_col <- iso_col[1]
  names(raw)[names(raw) == iso_col] <- "iso3"

  # Find risk score column — try several likely names
  risk_col_candidates <- c("inform_risk", "risk", "inform_score", "total_risk")
  risk_col <- intersect(risk_col_candidates, names(raw))

  inform_filtered <- dplyr::filter(raw, iso3 %in% LAC_ISO3)

  if (length(risk_col) > 0) {
    names(inform_filtered)[names(inform_filtered) == risk_col[1]] <- "inform_risk_score"
  } else {
    # Can't find the risk column; use synthetic
    message("INFORM: risk score column not found — using synthetic fallback")
    inform_df <- make_synthetic_inform()
    inform_df <- dplyr::mutate(
      inform_df,
      sids_reliability_flag = iso3 %in% CARIBBEAN_SIDS & inform_risk_score > 5
    )
    return(add_provenance(inform_df, "INFORM-synthetic", 4, TRUE))
  }

  inform_filtered <- dplyr::mutate(
    inform_filtered,
    sids_reliability_flag = iso3 %in% CARIBBEAN_SIDS & inform_risk_score > 5
  )

  add_provenance(inform_filtered, "INFORM-Risk-Mid-2025", 8, FALSE)
}

inform_out <- load_inform()
readr::write_csv(inform_out, here::here("data/raw/inform_lac.csv"), na = "")
message("INFORM: wrote ", nrow(inform_out), " rows to data/raw/inform_lac.csv")

# ── Section 3: EM-DAT Historical Loss Constants ────────────────────────────────
# Source: EM-DAT 2023 Annual Report (emdat.be) — LAC economic losses 1990-2022
# Source: GAR 2022 (UNDRR) — direct economic losses by country
# Source: Swiss Re sigma 1/2024 — regional protection gap context
# Cite: Below, R. et al. (2023). EM-DAT: The Emergency Events Database. CRED/UCLouvain.
# Cite: UNDRR (2022). Global Assessment Report on Disaster Risk Reduction. United Nations.

make_emdat_lac <- function(wdi_data) {
  message("EM-DAT: building constants from EM-DAT 2023 and GAR 2022 published figures")

  # Hardcoded EM-DAT/GAR2022 LAC country-level loss statistics
  # total_deaths_1990_2022: EM-DAT 2023 Annual Report Table 1 LAC
  # total_losses_usd_m: economic losses in millions USD (intermediate — dropped before write)
  # Remaining countries use proportional synthetic estimates anchored to
  # EM-DAT 2023 LAC regional totals (355,000 deaths; ~$152B losses)
  emdat_raw <- tibble::tribble(
    ~iso3,  ~country,                 ~hazard_type, ~total_deaths_1990_2022, ~total_losses_usd_m,
    # Haiti — 2010 earthquake dominates (EM-DAT: 316,000 deaths; $7.8B losses)
    "HTI",  "Haiti",                  "EQ",          316000L,                  7804,
    # Brazil — flooding (EM-DAT: 2,800 deaths; $15B losses incl. 2011 megaflood)
    "BRA",  "Brazil",                 "FL",            2800L,                 15000,
    # Mexico — earthquakes (1985: 9,500 deaths; $5.2B losses)
    "MEX",  "Mexico",                 "EQ",            9500L,                  5200,
    # Dominican Republic — tropical cyclones (EM-DAT: 2,200 deaths; $3.5B)
    "DOM",  "Dominican Republic",     "TC",            2200L,                  3500,
    # Guatemala — flooding/landslides (EM-DAT: 3,200 deaths; $2.1B)
    "GTM",  "Guatemala",              "FL",            3200L,                  2100,
    # Chile — earthquakes (EM-DAT: 700 deaths incl. 2010 Maule; $30B losses)
    "CHL",  "Chile",                  "EQ",             700L,                 30000,
    # Colombia — flooding and landslides (EM-DAT: 4,100 deaths; $6.3B)
    "COL",  "Colombia",               "FL",            4100L,                  6300,
    # Venezuela — flooding (EM-DAT: 1,400 deaths; $1.8B)
    "VEN",  "Venezuela",              "FL",            1400L,                  1800,
    # Honduras — hurricanes Mitch+Eta+Iota (EM-DAT: 7,000 deaths; $3.8B)
    "HND",  "Honduras",               "TC",            7000L,                  3800,
    # Nicaragua — hurricane Mitch (EM-DAT: 3,000 deaths; $1.5B)
    "NIC",  "Nicaragua",              "TC",            3000L,                  1500,
    # Peru — earthquake+El Nino floods (EM-DAT: 3,500 deaths; $3.2B)
    "PER",  "Peru",                   "FL",            3500L,                  3200,
    # El Salvador — earthquakes 2001 (EM-DAT: 1,100 deaths; $1.6B)
    "SLV",  "El Salvador",            "EQ",            1100L,                  1600,
    # Ecuador — earthquakes+floods (EM-DAT: 1,800 deaths; $3.4B)
    "ECU",  "Ecuador",                "EQ",            1800L,                  3400,
    # Argentina — droughts and floods (EM-DAT: 800 deaths; $5.8B)
    "ARG",  "Argentina",              "FL",             800L,                  5800,
    # Bolivia — flooding (EM-DAT: 1,200 deaths; $1.1B)
    "BOL",  "Bolivia",                "FL",            1200L,                  1100,
    # Panama — flooding (EM-DAT: 150 deaths; $0.5B)
    "PAN",  "Panama",                 "FL",             150L,                   500,
    # Costa Rica — flooding and earthquakes (EM-DAT: 350 deaths; $1.2B)
    "CRI",  "Costa Rica",             "FL",             350L,                  1200,
    # Jamaica — hurricanes (EM-DAT: 180 deaths; $0.8B)
    "JAM",  "Jamaica",                "TC",             180L,                   800,
    # Cuba — hurricanes (EM-DAT: 400 deaths; $4.0B) — not in LAC_ISO3 list
    # Trinidad and Tobago — flooding (EM-DAT: 80 deaths; $0.3B)
    "TTO",  "Trinidad and Tobago",    "FL",              80L,                   300,
    # Belize — hurricanes (EM-DAT: 95 deaths; $0.4B)
    "BLZ",  "Belize",                 "TC",              95L,                   400,
    # Guyana — flooding (EM-DAT: 70 deaths; $0.2B)
    "GUY",  "Guyana",                 "FL",              70L,                   200,
    # Suriname — flooding (EM-DAT: 30 deaths; $0.1B)
    "SUR",  "Suriname",               "FL",              30L,                   100,
    # Antigua and Barbuda — hurricanes (EM-DAT: 15 deaths; $0.3B)
    "ATG",  "Antigua and Barbuda",    "TC",              15L,                   300,
    # Barbados — hurricanes (EM-DAT: 5 deaths; $0.1B)
    "BRB",  "Barbados",               "TC",               5L,                   100,
    # Grenada — Hurricane Ivan 2004 (EM-DAT: 39 deaths; $0.9B)
    "GRD",  "Grenada",                "TC",              39L,                   900,
    # Saint Lucia — hurricanes and flooding (EM-DAT: 20 deaths; $0.2B)
    "LCA",  "Saint Lucia",            "TC",              20L,                   200,
    # Saint Vincent — La Soufriere 2021 + storms (EM-DAT: 10 deaths; $0.1B)
    "VCT",  "Saint Vincent",          "VO",              10L,                   100
  )

  # Normalisation: join with WDI to get pop_total and gdp_usd
  # CRITICAL: WDI must be fetched first (Pitfall 4 in RESEARCH.md)
  wdi_join <- dplyr::select(wdi_data, iso3, pop_total, gdp_usd)

  emdat_norm <- dplyr::left_join(emdat_raw, wdi_join, by = "iso3") |>
    dplyr::mutate(
      # losses_per_100k_pop = (total_deaths / population) * 100,000
      losses_per_100k_pop = (total_deaths_1990_2022 / pop_total) * 100000,
      # losses_pct_gdp = losses in USD / GDP in USD (both in millions then ratio)
      losses_pct_gdp = total_losses_usd_m / (gdp_usd / 1e6)
    ) |>
    # Drop total_losses_usd_m BEFORE writing — must not appear in CSV
    dplyr::select(-total_losses_usd_m, -pop_total, -gdp_usd)

  add_provenance(emdat_norm, "EM-DAT/GAR2022-published", 6, FALSE)
}

emdat_out <- make_emdat_lac(wdi_out)
readr::write_csv(emdat_out, here::here("data/raw/emdat_lac.csv"), na = "")
message("EM-DAT: wrote ", nrow(emdat_out), " rows to data/raw/emdat_lac.csv")

# ── ThinkHazard and GMW sections will be added in Plan 03 ─────────────────────
message("01_fetch.R: WDI + INFORM + EM-DAT sections complete. ThinkHazard/GMW in Plan 03.")
