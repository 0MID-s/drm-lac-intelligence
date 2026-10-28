# DRM Risk Intelligence Platform — LAC Region

An independent multi-hazard disaster risk management intelligence assessment covering 27 Latin America and Caribbean countries. Produced to IDB/GFDRR deliverable standards, this platform implements the Hazard-Exposure-Vulnerability (HEV) framework with climate scenario projections under baseline, RCP4.5, and RCP8.5 pathways across 2024, 2030, and 2050 time horizons.

The platform produces:
- A parameterised Quarto intelligence report (self-contained HTML, no server required)
- A Shiny dashboard with linked choropleth → risk matrix → radar views and scenario/year controls
- A Recovery Readiness Index — an original composite indicator bridging insurance penetration, fiscal space, social protection coverage, and DRM legal framework maturity
- A protection gap quantification anchored to Swiss Re sigma regional estimates (~81% for LAC)
- A Sendai Framework alignment scorecard mapping all outputs to Targets A–G

---

## Methods & Approach

- Constructed a geometric mean multi-hazard composite index across five hazard types (tropical cyclone, riverine flood, seismic, volcanic, drought/ENSO) with percentile normalisation and sensitivity analysis (rank correlation > 0.90 across ±20% weight variation)
- Implemented exposure scoring from GDP exposure, population in hazard zones, infrastructure proxies, and agricultural GDP share — per IDB HEV methodology
- Built a vulnerability module using HDI, World Governance Indicators, and adaptive capacity — calibrated against Caribbean SIDS reliability thresholds
- Developed an EWS readiness gap scoring framework across four Sendai Target G dimensions (monitoring, warning dissemination, community preparedness, institutional coordination) with priority investment tier classification
- Screened NbS feasibility using Global Mangrove Watch coastline-normalised coverage and watershed restoration proxies; flagged structural complements for highest-gap countries
- Estimated structural risk reduction investment gaps anchored to EM-DAT loss records and World Bank CDRP damage ratios
- Designed and implemented the Recovery Readiness Index — original composite integrating insurance penetration (CCRIF/Swiss Re sigma), social protection coverage, IMF fiscal space, and UNDRR DRM legal framework maturity
- Quantified LAC regional protection gap (~81%) and connected parametric insurance instruments (CCRIF, cat bonds) to DRM investment narrative, linking insurance risk transfer to development finance
- Mapped all outputs to Sendai Framework Targets A–G; produced qualitative scorecard for Targets E and G per UNDRR Sendai Monitor methodology
- Rendered publication-quality parameterised Quarto report (embed-resources: true) with 8 ggplot2 visualisations using custom DRM theme; OJS interactive explorer embedded
- Engineered full reproducible R pipeline with renv lockfile; all fetch scripts implement synthetic fallback paths; complete provenance schema with reliability flags per source

---

## Rendering

Requires: R ≥ 4.3, Quarto CLI ≥ 1.4, renv.

**Install dependencies (first time):**
```r
renv::restore()
```

**Render default scenario (baseline 2024):**
```bash
quarto render report/drm_intelligence_report.qmd
```

**Render specific scenario:**
```bash
quarto render report/drm_intelligence_report.qmd -P scenario:RCP4.5 -P year:2050
```

Valid scenario values: `baseline`, `RCP4.5`, `RCP8.5`
Valid year values: `2024`, `2030`, `2050`

Always render from the project root directory. Do not `cd report/` before rendering.

**Run the interactive dashboard:**
```r
shiny::runApp("dashboard")
```

**Run the test suite:**
```r
source("shared/tests/run_tests.R")
testthat::test_dir("dashboard/tests/testthat")
```

---

## Data Sources

| Source | Indicator | Coverage | Reliability |
|--------|-----------|----------|-------------|
| INFORM Risk Index (2024) | Composite humanitarian risk | 27 LAC countries | High |
| World Bank WDI (wbstats) | GDP, population, HDI, governance | 27 LAC countries | High |
| EM-DAT Historical Loss Database | Disaster losses by hazard type | LAC region summaries | High |
| ThinkHazard API (GFDRR) | Hazard level per country/type | 27 LAC countries | Medium (synthetic fallback active) |
| Global Mangrove Watch (JAXA) | Mangrove area by country | 27 LAC countries | High |
| Swiss Re sigma (2023) | LAC protection gap regional estimate | Regional aggregate | High |
| UNDRR Sendai Monitor | Targets E & G reporting status | 27 LAC countries | Medium |

---

## Project Structure

```
R/
├── 01_fetch.R           # Data ingestion (all sources with synthetic fallback)
├── 02_validate.R        # Data validation and lac_master.csv join
├── 03_score.R           # All 6 scoring modules + final assembly
└── 03_visualise.R       # 8 chart functions (ggplot2, theme_drm)

data/
├── raw/                 # One CSV per source with provenance columns
└── processed/           # lac_master.csv, country_scores.csv, scores_intermediate.csv

report/
├── drm_intelligence_report.qmd  # Parameterised Quarto report
└── styles.css                   # Custom IDB-style CSS

dashboard/
├── app.R                # Shiny app (leaflet choropleth, risk matrix, radar)
├── data/                # Self-contained data copy for shinyapps.io bundling
└── tests/               # testthat tests for data load, charts, deploy readiness

shared/
├── R/                   # Shared utilities (utils.R, viz_utils.R, hazard_utils.R, ...)
└── tests/               # testthat unit tests for all modules

renv.lock                # Reproducibility lockfile (all package versions pinned)
```

---

## Methodology Note

All composite indices use geometric mean aggregation to prevent high-hazard countries from being masked by low scores in other dimensions. Scores are normalised using percentile ranking (not min-max) to prevent outlier compression. Climate scenario projections apply scenario-specific multipliers to 2024 baseline values; multiplier assumptions are fully documented in the report's Methodology Appendix.

This is a static reproducible snapshot — not a live data pipeline. The synthetic fallback design is intentional: it ensures full reproducibility in offline environments and makes the methodology transparent.
