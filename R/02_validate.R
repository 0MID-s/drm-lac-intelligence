# R/02_validate.R — Data validation and quality checks
# Run from repo root: Rscript R/02_validate.R
# Reads: data/raw/*.csv | Writes: data/processed/lac_master.csv

library(here)
here::i_am("drm-lac-intelligence.Rproj")
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(tibble)
})

# ── Constants ──────────────────────────────────────────────────────────────────

LAC_ISO3 <- c(
  "ARG", "BOL", "BRA", "CHL", "COL", "ECU", "GUY", "PER", "SUR", "VEN",
  "BLZ", "CRI", "GTM", "HND", "MEX", "NIC", "PAN", "SLV",
  "ATG", "BRB", "DOM", "GRD", "HTI", "JAM", "LCA", "TTO", "VCT"
)

COUNTRY_NAMES <- c(
  ARG = "Argentina",      BOL = "Bolivia",          BRA = "Brazil",
  CHL = "Chile",          COL = "Colombia",         ECU = "Ecuador",
  GUY = "Guyana",         PER = "Peru",             SUR = "Suriname",
  VEN = "Venezuela",      BLZ = "Belize",           CRI = "Costa Rica",
  GTM = "Guatemala",      HND = "Honduras",         MEX = "Mexico",
  NIC = "Nicaragua",      PAN = "Panama",           SLV = "El Salvador",
  ATG = "Antigua and Barbuda", BRB = "Barbados",    DOM = "Dominican Republic",
  GRD = "Grenada",        HTI = "Haiti",            JAM = "Jamaica",
  LCA = "Saint Lucia",    TTO = "Trinidad and Tobago", VCT = "Saint Vincent and the Grenadines"
)

SUB_REGION_MAP <- c(
  ARG = "South America", BOL = "South America", BRA = "South America",
  CHL = "South America", COL = "South America", ECU = "South America",
  GUY = "South America", PER = "South America", SUR = "South America", VEN = "South America",
  BLZ = "Central America", CRI = "Central America", GTM = "Central America",
  HND = "Central America", MEX = "Mexico", NIC = "Central America",
  PAN = "Central America", SLV = "Central America",
  ATG = "Caribbean SIDS", BRB = "Caribbean SIDS", DOM = "Caribbean SIDS",
  GRD = "Caribbean SIDS", HTI = "Caribbean SIDS", JAM = "Caribbean SIDS",
  LCA = "Caribbean SIDS", TTO = "Caribbean SIDS", VCT = "Caribbean SIDS"
)

# ── Validation helper ──────────────────────────────────────────────────────────

validate_csv <- function(path, required_cols, label) {
  if (!file.exists(path)) {
    message("VALIDATION WARNING [", label, "]: file not found at ", path)
    return(NULL)
  }
  df <- readr::read_csv(path, show_col_types = FALSE)

  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    message("VALIDATION WARNING [", label, "]: missing columns: ",
            paste(missing_cols, collapse = ", "))
  }

  if ("iso3" %in% names(df)) {
    missing_countries <- setdiff(LAC_ISO3, df$iso3)
    if (length(missing_countries) > 0) {
      message("VALIDATION WARNING [", label, "]: missing countries: ",
              paste(missing_countries, collapse = ", "))
    }
  }

  # Check iso3 is character (not factor-coerced)
  if ("iso3" %in% names(df)) {
    if (!is.character(df$iso3)) {
      message("VALIDATION WARNING [", label, "]: iso3 column is not character type — coercion risk")
    }
  }

  # Check required numeric columns
  numeric_expected <- intersect(
    c("gdp_usd", "pop_total", "life_exp", "gov_eff", "rule_of_law",
      "inform_risk_score", "losses_per_100k_pop", "losses_pct_gdp",
      "mangrove_ha_2020", "change_ha", "reliability_flag"),
    names(df)
  )
  for (col in numeric_expected) {
    if (!is.numeric(df[[col]])) {
      message("VALIDATION WARNING [", label, "]: column '", col,
              "' expected numeric but is ", class(df[[col]]))
    }
  }

  # Log provenance
  synth_pct <- if ("is_synthetic" %in% names(df)) mean(df$is_synthetic, na.rm = TRUE) * 100 else NA
  message("VALIDATION [", label, "]: ", nrow(df), " rows | synthetic: ",
          round(synth_pct, 1), "% | reliability_flag range: ",
          if ("reliability_flag" %in% names(df)) {
            paste(range(df$reliability_flag, na.rm = TRUE), collapse = "-")
          } else {
            "N/A"
          })

  df
}

# ── Read and validate all 5 raw CSVs ──────────────────────────────────────────

message("02_validate.R: reading and validating raw CSVs...")

wdi <- validate_csv(
  here::here("data/raw/wdi_lac.csv"),
  c("iso3", "gdp_usd", "pop_total", "life_exp", "data_source", "reliability_flag", "is_synthetic"),
  "WDI"
)

inform <- validate_csv(
  here::here("data/raw/inform_lac.csv"),
  c("iso3", "inform_risk_score", "sids_reliability_flag", "data_source", "reliability_flag", "is_synthetic"),
  "INFORM"
)

emdat <- validate_csv(
  here::here("data/raw/emdat_lac.csv"),
  c("iso3", "losses_per_100k_pop", "losses_pct_gdp", "data_source", "reliability_flag", "is_synthetic"),
  "EM-DAT"
)

th <- validate_csv(
  here::here("data/raw/thinkhazard_lac.csv"),
  c("iso3", "hazard_type", "hazard_level", "data_source", "reliability_flag", "is_synthetic"),
  "ThinkHazard"
)

gmw <- validate_csv(
  here::here("data/raw/gmw_lac.csv"),
  c("iso3", "mangrove_ha_2020", "change_ha", "data_source", "reliability_flag", "is_synthetic"),
  "GMW"
)

# ── Aggregate EM-DAT: long-format -> one row per country ──────────────────────

emdat_agg <- if (!is.null(emdat)) {
  emdat |>
    dplyr::group_by(iso3) |>
    dplyr::summarise(
      losses_per_100k_pop = sum(losses_per_100k_pop, na.rm = TRUE),
      losses_pct_gdp      = sum(losses_pct_gdp, na.rm = TRUE),
      data_source         = dplyr::first(data_source),
      reliability_flag    = min(reliability_flag, na.rm = TRUE),
      is_synthetic        = any(is_synthetic),
      .groups             = "drop"
    )
} else {
  message("VALIDATION WARNING [EM-DAT]: NULL — emdat_agg will be empty")
  tibble::tibble(iso3 = character(), losses_per_100k_pop = numeric(),
                 losses_pct_gdp = numeric(), data_source = character(),
                 reliability_flag = integer(), is_synthetic = logical())
}

# ── Pivot ThinkHazard: long-format -> wide ────────────────────────────────────

th_wide <- if (!is.null(th)) {
  th |>
    dplyr::select(iso3, hazard_type, hazard_level) |>
    tidyr::pivot_wider(
      names_from    = hazard_type,
      values_from   = hazard_level,
      names_prefix  = "hazard_",
      values_fill   = "VLO"
    )
} else {
  message("VALIDATION WARNING [ThinkHazard]: NULL — th_wide will be empty")
  tibble::tibble(iso3 = character())
}

# ── Build joined lac_master ────────────────────────────────────────────────────

message("02_validate.R: building lac_master join chain...")

# Canonical country frame
base <- tibble::tibble(iso3 = LAC_ISO3) |>
  dplyr::mutate(
    country    = dplyr::recode(iso3, !!!COUNTRY_NAMES),
    sub_region = dplyr::recode(iso3, !!!SUB_REGION_MAP)
  )

# Strip duplicate provenance columns from per-source frames before joining
# (keep only the measurement columns; provenance is set globally on lac_master)
wdi_cols    <- if (!is.null(wdi))    dplyr::select(wdi,    -data_source, -reliability_flag, -is_synthetic) else NULL
inform_cols <- if (!is.null(inform)) dplyr::select(inform, -data_source, -reliability_flag, -is_synthetic, -country) else NULL
gmw_cols    <- if (!is.null(gmw))    dplyr::select(gmw,    -data_source, -reliability_flag, -is_synthetic, -country) else NULL

lac_master <- base

if (!is.null(wdi_cols)) {
  lac_master <- dplyr::left_join(lac_master, wdi_cols, by = "iso3")
}
if (!is.null(inform_cols)) {
  lac_master <- dplyr::left_join(lac_master, inform_cols, by = "iso3")
}
if (nrow(emdat_agg) > 0) {
  emdat_join <- dplyr::select(emdat_agg, iso3, losses_per_100k_pop, losses_pct_gdp)
  lac_master <- dplyr::left_join(lac_master, emdat_join, by = "iso3")
}
if (!is.null(th_wide)) {
  lac_master <- dplyr::left_join(lac_master, th_wide, by = "iso3")
}
if (!is.null(gmw_cols)) {
  gmw_join <- dplyr::select(gmw_cols, iso3, mangrove_ha_2020, change_ha)
  lac_master <- dplyr::left_join(lac_master, gmw_join, by = "iso3")
}

# Add pipeline-level provenance columns
lac_master <- lac_master |>
  dplyr::mutate(
    data_source      = "pipeline-join",
    reliability_flag = 6L,
    is_synthetic     = FALSE
  )

# ── Validate final output ──────────────────────────────────────────────────────

missing_final <- setdiff(LAC_ISO3, lac_master$iso3)
if (length(missing_final) > 0) {
  message("VALIDATION WARNING [lac_master]: ", length(missing_final),
          " countries missing from join: ", paste(missing_final, collapse = ", "))
} else {
  message("VALIDATION [lac_master]: all 27 LAC countries present")
}

# Verify iso3 and country are character (not factor-coerced)
if (!is.character(lac_master$iso3)) {
  message("VALIDATION WARNING [lac_master]: iso3 column is not character type")
}
if (!is.character(lac_master$country)) {
  message("VALIDATION WARNING [lac_master]: country column is not character type")
}

# Check no NA in iso3 or country
if (any(is.na(lac_master$iso3))) {
  message("VALIDATION WARNING [lac_master]: NA values found in iso3 column")
}
if (any(is.na(lac_master$country))) {
  message("VALIDATION WARNING [lac_master]: NA values found in country column")
}

message("lac_master: ", nrow(lac_master), " rows, ", ncol(lac_master),
        " columns — writing to data/processed/")

# ── Write output ───────────────────────────────────────────────────────────────

readr::write_csv(lac_master, here::here("data/processed/lac_master.csv"), na = "")
message("02_validate.R: complete — lac_master.csv written")
