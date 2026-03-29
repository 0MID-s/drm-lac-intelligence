# R/03_visualise.R
# Visualisation module — DRM LAC Intelligence Platform
# All functions accept a scores data frame and return ggplot2 objects.
# Source this file after sourcing shared/R/viz_utils.R.

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(scales)
  library(ggrepel)
  library(patchwork)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(treemapify)
})

# =============================================================================
# Data Loader
# =============================================================================

#' Load scores data for visualisation
#'
#' @param path character. Path to scores_intermediate.csv.
#'   Defaults to \code{here::here("data/processed/scores_intermediate.csv")}.
#' @return A data.frame with all scoring columns for 27 LAC countries.
load_scores <- function(path = here::here("data/processed/scores_intermediate.csv")) {
  readr::read_csv(path, show_col_types = FALSE)
}

# =============================================================================
# Chart 1: Choropleth Map
# =============================================================================

#' DRM choropleth: composite risk by country — LAC region
#'
#' Returns a ggplot2 map of the LAC region filled by the composite risk score
#' for a given scenario and year. Countries outside the scores data frame
#' are rendered in light grey (\code{na.value = "#E8E8E8"}).
#'
#' @param scores_df data.frame. scores_intermediate.csv loaded by caller.
#'   Must contain column \code{iso3} and the composite column for the
#'   requested scenario × year combination.
#' @param scenario  character. One of \code{"baseline"}, \code{"rcp45"},
#'   \code{"rcp85"}. Default \code{"baseline"}.
#' @param year      numeric or character. One of \code{2024}, \code{2030},
#'   \code{2050}. Default \code{2024}.
#' @return A ggplot2 object ready for \code{ggsave()} or a knitr chunk.
plot_choropleth_drm <- function(scores_df, scenario = "baseline", year = 2024) {
  col <- paste0("composite_", scenario, "_", as.character(year))
  stopifnot(col %in% names(scores_df))

  world    <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  map_data <- world |>
    dplyr::left_join(
      scores_df |> dplyr::select(iso3, dplyr::all_of(col)),
      by = c("iso_a3" = "iso3")
    )

  ggplot2::ggplot(map_data) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data[[col]]),
                     colour = "white", linewidth = 0.15) +
    ggplot2::scale_fill_gradientn(
      colours  = c(DRM_COLOURS$accent, DRM_COLOURS$warning, DRM_COLOURS$danger),
      na.value = "#E8E8E8",
      name     = "Composite Risk [0\u20131]",
      labels   = scales::label_number(accuracy = 0.01)
    ) +
    ggplot2::coord_sf(xlim = c(-120, -30), ylim = c(-58, 35), expand = FALSE) +
    theme_drm() +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_line(colour = "#D0D9E0", linewidth = 0.3),
      axis.text          = ggplot2::element_blank(),
      axis.title         = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title   = "Multi-Hazard Composite Risk \u2014 LAC Region",
      caption = paste0(
        "Source: INFORM Risk Index v2024, ThinkHazard API, World Bank WDI | ",
        "Scenario: ", scenario, " ", year
      )
    )
}

# =============================================================================
# Chart 2: Risk Matrix Scatter
# =============================================================================

#' DRM risk matrix: hazard vs vulnerability bubble chart
#'
#' Scatter chart with bubble size proportional to exposure score and colour
#' encoding sub-region. Country ISO3 labels are placed using
#' \code{ggrepel::geom_label_repel()} to avoid overlaps.
#'
#' @param scores_df data.frame. scores_intermediate.csv loaded by caller.
#'   Must contain columns: \code{hazard_composite}, \code{vulnerability_score},
#'   \code{exposure_score}, \code{sub_region}, \code{iso3}.
#' @param scenario  character. One of \code{"baseline"}, \code{"rcp45"},
#'   \code{"rcp85"}. Default \code{"baseline"} (not used in current version;
#'   retained for API consistency).
#' @param year      numeric or character. One of \code{2024}, \code{2030},
#'   \code{2050}. Default \code{2024} (not used in current version; retained
#'   for API consistency).
#' @return A ggplot2 object.
plot_risk_matrix_drm <- function(scores_df, scenario = "baseline", year = 2024) {
  ggplot2::ggplot(
    scores_df,
    ggplot2::aes(
      x      = hazard_composite,
      y      = vulnerability_score,
      size   = exposure_score,
      colour = sub_region,
      label  = iso3
    )
  ) +
    ggplot2::geom_point(alpha = 0.75) +
    ggrepel::geom_label_repel(
      size         = 2.5,
      max.overlaps = 20,
      show.legend  = FALSE,
      label.size   = 0.2
    ) +
    ggplot2::scale_size_continuous(range = c(3, 14), name = "Exposure") +
    ggplot2::scale_colour_manual(
      values = c(
        "South America"   = DRM_COLOURS$primary,
        "Central America" = DRM_COLOURS$accent,
        "Caribbean SIDS"  = DRM_COLOURS$warning,
        "Mexico"          = DRM_COLOURS$danger
      ),
      name = "Sub-region"
    ) +
    ggplot2::labs(
      title    = "Risk Matrix: Hazard \u00d7 Vulnerability",
      subtitle = "Bubble size proportional to Exposure Index",
      x        = "Hazard Composite [0\u20131]",
      y        = "Vulnerability Score [0\u20131]",
      caption  = "Source: Phase 3 scoring \u2014 DRM LAC Intelligence Platform"
    ) +
    theme_drm()
}

# =============================================================================
# Chart 3: Radar / Spider Chart (via coord_polar — no fmsb)
# =============================================================================

#' DRM radar chart: DRM dimension profile by country
#'
#' Produces a radar/spider chart comparing six DRM scoring dimensions across
#' countries using \code{coord_polar()}. The polygon is closed by appending a
#' duplicate of the first dimension row for each country (required by
#' coord_polar to draw a closed shape).
#'
#' @param scores_df data.frame. scores_intermediate.csv loaded by caller.
#'   Must contain columns: \code{hazard_composite}, \code{exposure_score},
#'   \code{vulnerability_score}, \code{ews_readiness_score},
#'   \code{nbs_feasibility_score}, \code{recovery_readiness_score},
#'   \code{country}.
#' @param countries character vector or \code{NULL}. If not \code{NULL},
#'   filters the data to the named countries before plotting. Default
#'   \code{NULL} (all 27 countries).
#' @return A ggplot2 object.
plot_radar_drm <- function(scores_df, countries = NULL) {
  dims   <- c("hazard_composite", "exposure_score", "vulnerability_score",
              "ews_readiness_score", "nbs_feasibility_score", "recovery_readiness_score")
  labels <- c("Hazard", "Exposure", "Vulnerability", "EWS", "NbS", "Recovery")

  if (!is.null(countries)) {
    scores_df <- dplyr::filter(scores_df, country %in% countries)
  }

  long <- scores_df |>
    dplyr::select(country, dplyr::all_of(dims)) |>
    tidyr::pivot_longer(-country, names_to = "dimension", values_to = "value") |>
    dplyr::mutate(
      dimension = factor(dimension, levels = dims, labels = labels)
    )

  # Close the polygon: duplicate first row per country so geom_polygon draws
  # a complete circuit back to the first dimension
  long_closed <- dplyr::bind_rows(
    long,
    long |>
      dplyr::filter(dimension == labels[1]) |>
      dplyr::mutate(dimension = factor(labels[1], levels = labels))
  )

  ggplot2::ggplot(
    long_closed,
    ggplot2::aes(x = dimension, y = value, group = country, colour = country)
  ) +
    ggplot2::geom_polygon(fill = NA, linewidth = 0.8) +
    ggplot2::coord_polar() +
    ggplot2::ylim(0, 1) +
    theme_drm_polar() +
    ggplot2::labs(
      title   = "DRM Dimension Profile \u2014 Selected Countries",
      caption = "Source: Phase 3 scoring \u2014 DRM LAC Intelligence Platform"
    )
}

# =============================================================================
# Chart 4: Stacked Bar — Hazard Composition
# =============================================================================

#' DRM stacked bar chart: hazard composition by country
#'
#' Displays the contribution of five hazard types to each country's hazard
#' profile. Countries are ordered by their composite hazard score (descending)
#' using \code{reorder()}. Hazard component columns are pivoted to long format
#' before plotting.
#'
#' @param scores_df data.frame. scores_intermediate.csv loaded by caller.
#'   Must contain columns: \code{country}, \code{hazard_composite},
#'   \code{hazard_EQ_num}, \code{hazard_FL_num}, \code{hazard_CY_num},
#'   \code{hazard_VO_num}, \code{hazard_DR_num}.
#' @return A ggplot2 object.
plot_hazard_stacked_drm <- function(scores_df) {
  hazard_long <- scores_df |>
    dplyr::select(
      country, hazard_composite,
      hazard_EQ_num, hazard_FL_num, hazard_CY_num, hazard_VO_num, hazard_DR_num
    ) |>
    tidyr::pivot_longer(
      cols      = c(hazard_EQ_num, hazard_FL_num, hazard_CY_num, hazard_VO_num, hazard_DR_num),
      names_to  = "hazard_type",
      values_to = "score"
    ) |>
    dplyr::mutate(
      hazard_type = dplyr::recode(
        hazard_type,
        "hazard_EQ_num" = "Seismic",
        "hazard_FL_num" = "Flood",
        "hazard_CY_num" = "Tropical Cyclone",
        "hazard_VO_num" = "Volcanic",
        "hazard_DR_num" = "Drought"
      ),
      country = reorder(country, hazard_composite)
    )

  hazard_colours <- c(
    "Seismic"          = DRM_COLOURS$primary,
    "Flood"            = DRM_COLOURS$accent,
    "Tropical Cyclone" = DRM_COLOURS$warning,
    "Volcanic"         = DRM_COLOURS$danger,
    "Drought"          = "#8E44AD"
  )

  ggplot2::ggplot(
    hazard_long,
    ggplot2::aes(x = score, y = country, fill = hazard_type)
  ) +
    ggplot2::geom_col(position = "stack") +
    ggplot2::scale_fill_manual(values = hazard_colours, name = "Hazard Type") +
    ggplot2::labs(
      title   = "Hazard Composition by Country",
      subtitle = "Stacked normalised hazard component scores [0\u20131]",
      x       = "Hazard Component Score",
      y       = NULL,
      caption = "Source: INFORM Risk Index v2024, ThinkHazard API | Note: hazard_CY = Tropical Cyclone"
    ) +
    theme_drm()
}

# =============================================================================
# Chart 5: Dumbbell Chart — EWS Readiness Gap
# =============================================================================

#' DRM dumbbell chart: EWS readiness gap
#'
#' Shows the gap between hazard composite score (red dot) and EWS readiness
#' score (teal dot) per country, connected by a segment. Countries are ordered
#' by EWS gap (hazard_composite - ews_readiness_score) descending, so the most
#' under-served countries appear at the top.
#'
#' @param scores_df data.frame. scores_intermediate.csv loaded by caller.
#'   Must contain columns: \code{country}, \code{hazard_composite},
#'   \code{ews_readiness_score}.
#' @return A ggplot2 object.
plot_ews_dumbbell_drm <- function(scores_df) {
  df <- scores_df |>
    dplyr::mutate(
      ews_gap = hazard_composite - ews_readiness_score,
      country = reorder(country, ews_gap)
    )

  ggplot2::ggplot(df) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x    = ews_readiness_score,
        xend = hazard_composite,
        y    = country,
        yend = country
      ),
      colour    = DRM_COLOURS$neutral %||% "#AAAAAA",
      linewidth = 1
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = hazard_composite, y = country),
      colour = DRM_COLOURS$danger,
      size   = 3
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = ews_readiness_score, y = country),
      colour = DRM_COLOURS$accent,
      size   = 3
    ) +
    ggplot2::labs(
      title   = "EWS Readiness Gap: Hazard vs EWS Readiness Score",
      subtitle = paste0(
        "\u25cf Hazard composite (red)  \u25cf EWS readiness (teal)"
      ),
      x       = "Score [0\u20131]",
      y       = NULL,
      caption = "Source: Phase 3 scoring \u2014 DRM LAC Intelligence Platform"
    ) +
    theme_drm()
}

# =============================================================================
# Chart 6: Treemap — Structural Protection Gap
# =============================================================================

#' DRM treemap: structural protection gap by country
#'
#' Treemap where tile area encodes \code{log1p(structural_gap_usd)} to reduce
#' extreme value compression. Tiles are coloured by sub-region. Labels show
#' ISO3 code and raw gap value in USD millions.
#'
#' @param scores_df data.frame. scores_intermediate.csv loaded by caller.
#'   Must contain columns: \code{iso3}, \code{sub_region},
#'   \code{structural_gap_usd}. Rows with \code{NA} structural_gap_usd are
#'   silently dropped.
#' @return A ggplot2 object.
plot_treemap_drm <- function(scores_df) {
  df <- scores_df |>
    dplyr::filter(!is.na(structural_gap_usd)) |>
    dplyr::mutate(log_gap = log1p(structural_gap_usd))

  region_colours <- c(
    "South America"   = DRM_COLOURS$primary,
    "Central America" = DRM_COLOURS$accent,
    "Caribbean SIDS"  = DRM_COLOURS$warning,
    "Mexico"          = DRM_COLOURS$danger
  )

  ggplot2::ggplot(df, ggplot2::aes(area = log_gap, fill = sub_region)) +
    treemapify::geom_treemap() +
    treemapify::geom_treemap_text(
      ggplot2::aes(
        label = paste0(iso3, "\n$", round(structural_gap_usd, 0), "M")
      ),
      colour   = "white",
      size     = 9,
      fontface = "bold"
    ) +
    ggplot2::scale_fill_manual(
      values = region_colours,
      name   = "Sub-region",
      na.value = "#AAAAAA"
    ) +
    theme_drm() +
    ggplot2::labs(
      title   = "Structural Protection Gap by Country",
      caption = paste0(
        "Area = log1p(structural gap USD M) \u2014 transformation applied to reduce ",
        "extreme value compression"
      )
    )
}

# =============================================================================
# Chart 7: Sendai Heatmap
# =============================================================================

#' DRM Sendai heatmap: Targets E and G reporting status
#'
#' Tile heatmap showing each country's Sendai Framework reporting status for
#' Target E (national and local DRR strategies) and Target G (EWS and
#' early action). Countries are ordered by \code{ews_readiness_score}
#' (ascending). Fill uses the DRM risk palette: Reported = low (teal),
#' Partial = medium (amber), Not Reported = high (red).
#'
#' @param scores_df data.frame. scores_intermediate.csv loaded by caller.
#'   Must contain columns: \code{country}, \code{sendai_target_e},
#'   \code{sendai_target_g}, \code{ews_readiness_score}.
#' @return A ggplot2 object.
plot_sendai_heatmap_drm <- function(scores_df) {
  long <- scores_df |>
    dplyr::select(country, sendai_target_e, sendai_target_g, ews_readiness_score) |>
    dplyr::mutate(
      `Target E\n(Strategies)` = dplyr::case_when(
        sendai_target_e == "Reported" ~ "Reported",
        sendai_target_e == "Partial"  ~ "Partial",
        TRUE                          ~ "Not Reported"
      ),
      `Target G\n(EWS/Coop)` = dplyr::case_when(
        sendai_target_g == "Reported" ~ "Reported",
        sendai_target_g == "Partial"  ~ "Partial",
        TRUE                          ~ "Not Reported"
      )
    ) |>
    tidyr::pivot_longer(
      cols      = c(`Target E\n(Strategies)`, `Target G\n(EWS/Coop)`),
      names_to  = "target",
      values_to = "status"
    ) |>
    dplyr::mutate(
      status  = factor(status, levels = c("Reported", "Partial", "Not Reported")),
      country = reorder(country, ews_readiness_score)
    )

  ggplot2::ggplot(long, ggplot2::aes(x = target, y = country, fill = status)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.4) +
    ggplot2::scale_fill_manual(
      values = c(
        "Reported"     = unname(RISK_PALETTE["low"]),
        "Partial"      = unname(RISK_PALETTE["medium"]),
        "Not Reported" = unname(RISK_PALETTE["high"])
      ),
      name = "Reporting Status"
    ) +
    ggplot2::labs(
      title   = "Sendai Framework Alignment \u2014 Targets E & G",
      x       = NULL,
      y       = NULL,
      caption = "Source: UNDRR Sendai Monitor \u2014 ordinal classification"
    ) +
    theme_drm()
}

# =============================================================================
# Chart 8: Protection Gap Projection
# =============================================================================

#' DRM protection gap projection line chart
#'
#' Projects protection gap (%) for the top-N countries by baseline protection
#' gap under three climate scenarios (baseline, rcp45, rcp85) across three
#' time horizons (2024, 2030, 2050). Projections are derived by applying
#' scenario-specific multipliers to 2024 baseline protection gap values.
#'
#' Methodology note: Projections derived by applying scenario-specific
#' multipliers to 2024 baseline protection gap values. Multiplier assumptions
#' documented in methodology appendix.
#'
#' @param scores_df data.frame. scores_intermediate.csv loaded by caller.
#'   Must contain columns: \code{country}, \code{protection_gap}.
#' @param top_n    integer. Number of highest-gap countries to display.
#'   Default \code{15}.
#' @return A ggplot2 object.
plot_protection_gap_drm <- function(scores_df, top_n = 15) {
  multipliers <- list(
    baseline_2024 = 1.00,
    baseline_2030 = 1.05,
    baseline_2050 = 1.12,
    rcp45_2024    = 1.00,
    rcp45_2030    = 1.08,
    rcp45_2050    = 1.18,
    rcp85_2024    = 1.00,
    rcp85_2030    = 1.12,
    rcp85_2050    = 1.28
  )

  top_countries <- scores_df |>
    dplyr::arrange(dplyr::desc(protection_gap)) |>
    dplyr::slice_head(n = top_n) |>
    dplyr::pull(country)

  df_top <- scores_df |>
    dplyr::filter(country %in% top_countries) |>
    dplyr::select(country, protection_gap)

  rows <- lapply(names(multipliers), function(key) {
    parts    <- strsplit(key, "_")[[1]]
    scenario <- parts[1]
    year     <- as.integer(parts[2])
    mult     <- multipliers[[key]]
    df_top |>
      dplyr::mutate(
        scenario       = scenario,
        year           = year,
        projected_gap  = pmin(protection_gap * mult, 1)
      )
  })

  long <- dplyr::bind_rows(rows)

  ggplot2::ggplot(
    long,
    ggplot2::aes(
      x        = year,
      y        = projected_gap,
      colour   = country,
      linetype = scenario,
      group    = interaction(country, scenario)
    )
  ) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::scale_x_continuous(breaks = c(2024, 2030, 2050)) +
    ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
    ggplot2::scale_colour_viridis_d(name = "Country", option = "turbo") +
    ggplot2::scale_linetype_manual(
      values = c("baseline" = "solid", "rcp45" = "dashed", "rcp85" = "dotted"),
      name   = "Scenario"
    ) +
    ggplot2::labs(
      title   = "Protection Gap Projection by Scenario",
      x       = "Year",
      y       = "Protection Gap (%)",
      caption = paste0(
        "Source: Phase 3 Recovery Readiness module; Swiss Re sigma anchor 81%. ",
        "Projections are scenario-multiplier estimates."
      )
    ) +
    theme_drm()
}

# =============================================================================
# Null-coalescing operator (used internally)
# =============================================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b
