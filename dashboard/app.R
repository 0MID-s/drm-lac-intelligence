# DRM Risk Intelligence Platform — Shiny Dashboard
# Full implementation: choropleth map, risk matrix scatter, radar chart
# Scenario/year controls with bidirectional country selection sync

# =============================================================================
# SECTION 1 — Library imports and constants
# =============================================================================

library(shiny)
library(bslib)
library(leaflet)
library(plotly)
library(dplyr)
library(readr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

RADAR_DIMS   <- c("hazard_composite", "exposure_score", "vulnerability_score",
                  "ews_readiness_score", "nbs_feasibility_score", "recovery_readiness_score")
RADAR_LABELS <- c("Hazard", "Exposure", "Vulnerability", "EWS Readiness", "NbS Feasibility", "Recovery")
IDB_NAVY     <- "#003366"
IDB_CYAN     <- "#00A1D6"

# =============================================================================
# SECTION 2 — Data load at startup (once)
# =============================================================================

# Resolve data path relative to app.R's own location so the file can be
# sourced from any working directory (e.g. testthat test runner with chdir=TRUE).
# When run via shiny::runApp("dashboard/"), Shiny sets cwd to dashboard/.
# When sourced with chdir=TRUE, cwd is also set to dashboard/.
# Therefore "data/scores_intermediate.csv" always resolves correctly.
.csv_path <- "data/scores_intermediate.csv"

scores_full <- readr::read_csv(.csv_path, show_col_types = FALSE)
world_sf    <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
lac_isos    <- unique(scores_full$iso3)

# =============================================================================
# SECTION 3 — Helper functions
# =============================================================================

composite_col <- function(scenario, year) {
  paste0("composite_", scenario, "_", year)
}

build_scatter_plot <- function(df) {
  # Sub-region colour map — single trace to avoid multi-trace restyle complexity
  marker_colors <- dplyr::recode(
    df$sub_region,
    "South America"   = "#1B4F72",
    "Central America" = "#2E86AB",
    "Caribbean SIDS"  = "#E8A838",
    "Mexico"          = "#C0392B",
    .default = "#888888"
  )

  sizeref <- 2 * max(df$exposure_score, na.rm = TRUE) / (14^2)

  plot_ly(
    data       = df,
    x          = ~hazard_composite,
    y          = ~vulnerability_score,
    type       = "scatter",
    mode       = "markers",
    marker     = list(
      size      = ~exposure_score,
      sizemode  = "area",
      sizeref   = sizeref,
      color     = marker_colors,
      line      = list(width = 0, color = "white")
    ),
    text       = ~paste0(
      country, "<br>",
      "Hazard: ", round(hazard_composite, 3), "<br>",
      "Vuln: ", round(vulnerability_score, 3), "<br>",
      "Sub-region: ", sub_region
    ),
    hoverinfo  = "text",
    customdata = ~iso3,
    showlegend = FALSE
  ) %>%
    layout(
      title  = list(text = "Risk Matrix", font = list(color = IDB_NAVY)),
      xaxis  = list(title = "Hazard Composite [0\u20131]", range = c(0, 1)),
      yaxis  = list(title = "Vulnerability [0\u20131]", range = c(0, 1)),
      showlegend = FALSE,
      margin = list(t = 30, b = 50, l = 60, r = 20)
    )
}

build_radar_plot <- function(df, selected_country, country2 = NULL) {
  # Compute LAC average trace
  lac_avg <- colMeans(df[, RADAR_DIMS], na.rm = TRUE)
  r_avg   <- c(lac_avg, lac_avg[1])
  theta   <- c(RADAR_LABELS, RADAR_LABELS[1])

  p <- plot_ly(
    type      = "scatterpolar",
    r         = r_avg,
    theta     = theta,
    fill      = "toself",
    fillcolor = "rgba(0,161,214,0.20)",
    line      = list(color = IDB_CYAN),
    name      = "LAC Average"
  )

  # Add selected country trace (country 1)
  if (!is.null(selected_country) && nchar(selected_country) > 0) {
    row1 <- df[df$country == selected_country, RADAR_DIMS]
    if (nrow(row1) > 0) {
      r1 <- c(unlist(row1[1, ]), unlist(row1[1, 1]))
      p  <- p %>% add_trace(
        type      = "scatterpolar",
        r         = r1,
        theta     = theta,
        fill      = "toself",
        fillcolor = "rgba(0,51,102,0.35)",
        line      = list(color = IDB_NAVY),
        name      = selected_country
      )
    }
  }

  # Add compare country trace (country 2)
  if (!is.null(country2) && nchar(country2) > 0) {
    row2 <- df[df$country == country2, RADAR_DIMS]
    if (nrow(row2) > 0) {
      r2 <- c(unlist(row2[1, ]), unlist(row2[1, 1]))
      p  <- p %>% add_trace(
        type      = "scatterpolar",
        r         = r2,
        theta     = theta,
        fill      = "toself",
        fillcolor = "rgba(232,168,56,0.35)",
        line      = list(color = "#E8A838"),
        name      = country2
      )
    }
  }

  p <- p %>%
    layout(
      polar      = list(radialaxis = list(visible = TRUE, range = c(0, 1))),
      showlegend = TRUE,
      margin     = list(t = 30)
    )

  # Materialise trace data so that result$x$data reflects actual traces
  # (plotly lazy-evaluates attrs into $x$data only after plotly_build).
  plotly_build(p)
}

# =============================================================================
# SECTION 4 — UI
# =============================================================================

ui <- bslib::page_navbar(
  title = "DRM Risk Intelligence \u2014 LAC",
  theme = bslib::bs_theme(
    version   = 5,
    primary   = IDB_NAVY,
    secondary = IDB_CYAN,
    bg        = "#FFFFFF",
    fg        = "#2C2C2C"
  ),

  bslib::nav_panel(
    "Dashboard",

    # Control bar — scenario, year, country, compare country
    bslib::card(
      bslib::card_body(
        fluidRow(
          column(3, selectInput(
            "scenario", "Scenario",
            choices  = c("Baseline" = "baseline", "RCP 4.5" = "rcp45", "RCP 8.5" = "rcp85"),
            selected = "baseline"
          )),
          column(3, selectInput(
            "year", "Year",
            choices  = c("2024", "2030", "2050"),
            selected = "2024"
          )),
          column(3, selectInput(
            "country_select", "Country",
            choices  = c("(None)" = "", sort(scores_full$country)),
            selected = ""
          )),
          column(3, selectInput(
            "country2_select", "Compare Country",
            choices  = c("(None)" = "", sort(scores_full$country)),
            selected = ""
          ))
        )
      )
    ),

    # Choropleth map
    bslib::card(
      leafletOutput("map", height = "500px")
    ),

    # Risk matrix scatter + radar chart
    bslib::card(
      bslib::layout_columns(
        col_widths = c(6, 6),
        plotlyOutput("scatter", height = "420px"),
        plotlyOutput("radar",   height = "420px")
      )
    )
  )
)

# =============================================================================
# SECTION 5 — Server
# =============================================================================

server <- function(input, output, session) {

  # --- Reactive: composite column name ---
  composite_col_r <- reactive({
    composite_col(input$scenario, input$year)
  })

  # --- Reactive: scores with active composite aliased as composite_selected ---
  scores_reactive <- reactive({
    col <- composite_col_r()
    dplyr::mutate(scores_full, composite_selected = .data[[col]])
  })

  # --- Map: initial render ---
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -75, lat = 5, zoom = 3)
  })

  # --- Map: update choropleth fill on scenario/year change ---
  observe({
    df  <- scores_reactive()
    pal <- colorNumeric(
      palette  = c(IDB_CYAN, IDB_NAVY),
      domain   = c(0, 1),
      na.color = "#E8E8E8"
    )
    joined <- dplyr::left_join(world_sf, df, by = c("iso_a3" = "iso3"))

    leafletProxy("map", session) %>%
      clearShapes() %>%
      addPolygons(
        data             = joined,
        fillColor        = ~pal(composite_selected),
        weight           = 0.5,
        color            = "white",
        fillOpacity      = 0.75,
        layerId          = ~iso_a3,
        highlightOptions = highlightOptions(weight = 2, color = IDB_NAVY, bringToFront = TRUE),
        popup            = ~ifelse(
          iso_a3 %in% lac_isos,
          paste0(
            "<strong>", name_long, "</strong><br/>",
            "Composite Risk: ", round(composite_selected, 3), "<br/>",
            "EWS Tier: ", ews_gap_tier, "<br/>",
            "Protection Gap: ", round(protection_gap * 100, 1), "%"
          ),
          paste0("<strong>", name_long, "</strong><br/>(Outside LAC scope)")
        )
      )
  })

  # --- Map click -> update Country dropdown ---
  observeEvent(input$map_shape_click, {
    clicked_iso <- input$map_shape_click$id
    if (!is.null(clicked_iso) && clicked_iso %in% lac_isos) {
      clicked_country <- scores_full$country[scores_full$iso3 == clicked_iso][1]
      updateSelectInput(session, "country_select", selected = clicked_country)
    }
  })

  # --- Country dropdown -> pan map to country centroid ---
  observeEvent(input$country_select, {
    req(input$country_select != "")
    row <- scores_full[scores_full$country == input$country_select, ]
    if (nrow(row) > 0) {
      iso        <- row$iso3[1]
      country_sf <- world_sf[world_sf$iso_a3 == iso, ]
      if (nrow(country_sf) > 0) {
        centroid <- sf::st_centroid(sf::st_geometry(country_sf))
        coords   <- sf::st_coordinates(centroid)
        leafletProxy("map", session) %>%
          setView(lng = coords[1], lat = coords[2], zoom = 5)
      }
    }
  })

  # --- Scatter: full render on scenario/year change ---
  output$scatter <- renderPlotly({
    build_scatter_plot(scores_reactive())
  })

  # --- Scatter: highlight selected country via restyle (no full redraw) ---
  observeEvent(input$country_select, {
    df      <- scores_reactive()
    sel_iso <- if (input$country_select == "") "" else
      scores_full$iso3[scores_full$country == input$country_select][1]
    widths  <- ifelse(!is.na(df$iso3) & df$iso3 == sel_iso, 3, 0)
    colors  <- ifelse(!is.na(df$iso3) & df$iso3 == sel_iso, IDB_NAVY, "white")
    plotlyProxy("scatter", session) %>%
      plotlyProxyInvoke(
        "restyle",
        list("marker.line.width" = list(widths),
             "marker.line.color" = list(colors)),
        list(0)
      )
  })

  # --- Radar: full redraw on country selection (scatterpolar, no proxy needed) ---
  output$radar <- renderPlotly({
    sel   <- if (input$country_select   == "") NULL else input$country_select
    comp2 <- if (input$country2_select  == "") NULL else input$country2_select
    build_radar_plot(scores_full, sel, comp2)
  })

}

# =============================================================================
# SECTION 6 — App call
# =============================================================================

# Guard shinyApp() so this file can be sourced for unit testing without
# starting a Shiny server. When run via shiny::runApp() or Rscript app.R,
# isRunning() is FALSE at source time but shinyApp() returns the app object
# (which runApp() picks up). The guard prevents auto-launch when sourced.
if (!isTRUE(getOption("shiny.testmode"))) {
  shinyApp(ui, server)
}
