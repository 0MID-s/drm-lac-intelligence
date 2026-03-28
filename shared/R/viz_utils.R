# shared/R/viz_utils.R
# DRM LAC Intelligence — shared ggplot2 visual theme and colour scales.
# Adapted from uk-climate-risk-portfolio/projects/utils/theme_climate_risk.R
# Requires packages: ggplot2

library(ggplot2)

# =============================================================================
# Colour Constants
# Locked DRM palette — reference these rather than hard-coding hex values.
# =============================================================================

#' DRM LAC Intelligence colour palette
#'
#' A named list of brand colours used across all DRM LAC charts and reports.
#' \describe{
#'   \item{primary}{Deep navy blue — titles, primary text, borders}
#'   \item{accent}{Mid teal — low risk, secondary highlights}
#'   \item{warning}{Amber — medium risk, caution indicators}
#'   \item{danger}{Risk red — high risk, critical alerts}
#'   \item{background}{Light off-white — panel and plot backgrounds}
#'   \item{text}{Dark grey — axis text, body copy}
#' }
DRM_COLOURS <- list(
  primary    = "#1B4F72",
  accent     = "#2E86AB",
  warning    = "#E8A838",
  danger     = "#C0392B",
  background = "#F8F9FA",
  text       = "#2C2C2C"
)

#' DRM 4-level risk palette
#'
#' Named character vector mapping risk tiers to hex colours.
#' Names are exactly: \code{low}, \code{medium}, \code{high}, \code{critical}.
#' Use with \code{scale_fill_drm_risk()} and \code{scale_color_drm_risk()}.
RISK_PALETTE <- c(
  low      = "#AED9E0",
  medium   = "#2E86AB",
  high     = "#E8A838",
  critical = "#C0392B"
)

# =============================================================================
# Core Theme
# =============================================================================

#' DRM LAC Intelligence ggplot2 theme
#'
#' A clean consulting-standard theme wrapping \code{theme_minimal()} with
#' DRM brand overrides: horizontal-only gridlines, light background (#F8F9FA),
#' bottom-centre legend, and navy title hierarchy.
#'
#' @param base_size   Numeric. Base font size in pt. Default 11.
#' @param base_family Character. Base font family. Default \code{"sans"}
#'   (system sans-serif; IBM Plex Sans if available via showtext).
#'
#' @return A ggplot2 theme object. Apply with \code{+ theme_drm()}.
#'
#' @examples
#' \dontrun{
#'   ggplot(df, aes(x, y)) + geom_col() + theme_drm()
#' }
theme_drm <- function(base_size = 11, base_family = "sans") {
  theme_minimal(base_family = base_family, base_size = base_size) +
    theme(
      # Gridlines: horizontal only — vertical gridlines add noise, not signal
      panel.grid.major.x   = element_blank(),
      panel.grid.minor     = element_blank(),
      panel.grid.major.y   = element_line(colour = "#D0D9E0", linewidth = 0.4),

      # Background: light off-white for clean print rendering
      panel.background     = element_rect(fill = "#F8F9FA", colour = NA),
      plot.background      = element_rect(fill = "#F8F9FA", colour = NA),

      # Legend: bottom centre — keeps chart area maximised
      legend.position      = "bottom",
      legend.justification = "center",

      # Title hierarchy: bold primary navy → muted subtitle → small grey caption
      plot.title           = element_text(face = "bold", colour = "#1B4F72"),
      plot.subtitle        = element_text(colour = "#555555"),
      plot.caption         = element_text(colour = "#888888", size = 8, hjust = 0),
      plot.caption.position = "plot",

      # Axis text: muted to keep chart data prominent
      axis.text            = element_text(colour = "#2C2C2C", size = 9),
      axis.title           = element_text(colour = "#2C2C2C", size = 10)
    )
}

# =============================================================================
# Polar / Radar Theme
# =============================================================================

#' DRM LAC Intelligence polar/radar ggplot2 theme
#'
#' Extends \code{theme_drm()} with modifications appropriate for
#' \code{coord_polar()} radar and spider charts. Removes axis text for a
#' cleaner circular look; enables both x and y major gridlines with light
#' styling for the circular grid.
#'
#' @param base_size   Numeric. Base font size in pt. Default 11.
#' @param base_family Character. Base font family. Default \code{"sans"}.
#'
#' @return A ggplot2 theme object suitable for use with \code{coord_polar()}.
#'
#' @examples
#' \dontrun{
#'   ggplot(df, aes(x = variable, y = value, group = country)) +
#'     geom_polygon(fill = NA, colour = "#2E86AB") +
#'     coord_polar() +
#'     theme_drm_polar()
#' }
theme_drm_polar <- function(base_size = 11, base_family = "sans") {
  theme_drm(base_size = base_size, base_family = base_family) +
    theme(
      # Enable circular grid lines for both axes (needed for radar chart grid)
      panel.grid.major.x = element_line(colour = "#D0D9E0", linewidth = 0.3,
                                        linetype = "solid"),
      panel.grid.major.y = element_line(colour = "#D0D9E0", linewidth = 0.3,
                                        linetype = "solid"),

      # Remove axis text for cleaner circular appearance
      axis.text.x        = element_blank(),
      axis.text.y        = element_blank(),
      axis.title.x       = element_blank(),
      axis.title.y       = element_blank(),

      # Remove axis ticks
      axis.ticks         = element_blank()
    )
}

# =============================================================================
# Risk Colour Scales
# =============================================================================

#' Discrete fill scale for DRM risk tiers
#'
#' Maps risk levels (low / medium / high / critical) to the locked
#' \code{RISK_PALETTE} colour scheme.
#'
#' @param ... Additional arguments passed to \code{scale_fill_manual()}.
#' @return A ggplot2 scale object.
#'
#' @examples
#' \dontrun{
#'   ggplot(df, aes(x = country, fill = risk_tier)) +
#'     geom_col() +
#'     scale_fill_drm_risk()
#' }
scale_fill_drm_risk <- function(...) {
  ggplot2::scale_fill_manual(values = RISK_PALETTE, ...)
}

#' Discrete colour scale for DRM risk tiers (geom_point / geom_line)
#'
#' Maps risk levels (low / medium / high / critical) to the locked
#' \code{RISK_PALETTE} colour scheme.
#'
#' @param ... Additional arguments passed to \code{scale_color_manual()}.
#' @return A ggplot2 scale object.
#'
#' @examples
#' \dontrun{
#'   ggplot(df, aes(x = year, y = index, colour = risk_tier)) +
#'     geom_line() +
#'     scale_color_drm_risk()
#' }
scale_color_drm_risk <- function(...) {
  ggplot2::scale_color_manual(values = RISK_PALETTE, ...)
}

# =============================================================================
# Phase 4 Chart Helper Stubs
# Implementations deferred to Phase 4 — stubs satisfy CONTEXT.md requirement.
# =============================================================================

#' DRM choropleth map helper (Phase 4)
#'
#' Produces a DRM-styled choropleth map of country-level risk scores.
#' \strong{Not implemented — Phase 4.}
#'
#' @param data     An \code{sf} object with country polygons.
#' @param fill_col Character. Name of the column to map to fill aesthetic.
#' @param title    Character. Plot title.
#' @return A ggplot2 object.
plot_choropleth_drm <- function(data, fill_col, title) {
  stop("Not implemented — Phase 4")
}

#' DRM radar/spider chart helper (Phase 4)
#'
#' Produces a DRM-styled radar chart comparing hazard dimensions across
#' countries.
#' \strong{Not implemented — Phase 4.}
#'
#' @param data        A data frame in long format.
#' @param country_col Character. Name of the country grouping column.
#' @param value_col   Character. Name of the numeric value column.
#' @param title       Character. Plot title.
#' @return A ggplot2 object.
plot_radar_drm <- function(data, country_col, value_col, title) {
  stop("Not implemented — Phase 4")
}
