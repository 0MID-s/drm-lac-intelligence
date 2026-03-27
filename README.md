# DRM Risk Intelligence Platform — LAC Region

This project is an independent Disaster Risk Management (DRM) intelligence deliverable covering the Latin America and Caribbean (LAC) region. It demonstrates IDB/GFDRR-standard methodology for multi-hazard composite risk assessment, combining climate scenario projections (baseline, RCP4.5, RCP8.5) across 2024, 2030, and 2050 time horizons. The platform produces a parameterised Quarto intelligence report and an interactive Quarto OJS dashboard, enabling scenario-based risk comparison across LAC countries.

## Skills Demonstrated

- Multi-hazard composite indexing (flood, cyclone, drought, landslide, coastal inundation)
- Recovery readiness analysis using governance, fiscal capacity, and social protection indicators
- Protection gap quantification (insured vs. total economic loss ratio)
- Sendai Framework target mapping (A–G targets)
- Parameterised Quarto reporting with scenario and year selectors
- Interactive OJS visualisations embedded in Quarto (choropleth, risk matrix, radar charts)
- Geospatial analysis with sf and terra
- Reproducible R workflow with renv

## Data Sources

- World Bank World Development Indicators (WDI) — via `wbstats` R package
- INFORM Risk Index — composite humanitarian risk scores
- EM-DAT historical disaster losses — exposure and economic loss data
- ThinkHazard API — multi-hazard classification by country
- Global Mangrove Watch (JAXA) — nature-based solutions spatial data

## Render Instructions

To render the intelligence report:

```
quarto render report/drm_intelligence_report.qmd
```

To run the Shiny dashboard (if included):

```
Rscript dashboard/app.R
```

## Quarto CLI Version

Requires Quarto CLI 1.9.36 or later.

Check version: `quarto --version`

## Reproducibility

Restore the R package environment before running any scripts:

```
Rscript -e 'renv::restore()'
```

The `renv.lock` file pins all package versions. Run `renv::restore()` once after cloning to install the exact dependency set.

## Directory Structure

```
drm-lac-intelligence/
├── R/                    # Numbered pipeline scripts (01_fetch, 02_validate, 03_score)
├── data/
│   ├── raw/              # Source data (gitignored, populated by R/01_fetch.R)
│   └── processed/        # Derived outputs (country_scores.csv schema contract)
├── report/               # Quarto report (.qmd) and style assets
├── shared/
│   ├── R/                # Shared utility functions (utils.R bootstrapper)
│   └── tests/            # testthat unit tests for shared utilities
├── _quarto.yml           # Project-wide Quarto configuration
├── renv.lock             # Reproducible R dependency snapshot
└── drm-lac-intelligence.Rproj  # RStudio project anchor (required for here::i_am())
```
