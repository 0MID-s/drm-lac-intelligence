# DRM Risk Intelligence Platform — Shiny Dashboard Stub
#
# NOTE: The primary interactive interface for this project is the OJS explorer
# embedded directly in report/drm_intelligence_report.qmd (no server required,
# runs in any browser). This file is a minimal Shiny stub retained for
# compatibility with the original FOUND-03 requirement.
#
# To use the interactive dashboard, render the Quarto report:
#   quarto render report/drm_intelligence_report.qmd
#
# A standalone Shiny dashboard will be built in Phase 5 once country_scores.csv
# is populated with real scored data.

library(shiny)

ui <- fluidPage(
  titlePanel("DRM Risk Intelligence Platform — LAC Region"),
  p("Interactive analysis will be available in Phase 5."),
  p("For now, render the Quarto report for the full interactive OJS explorer:"),
  tags$code("quarto render report/drm_intelligence_report.qmd"),
  hr(),
  p(tags$em("Phase 5 will add: linked choropleth map, risk matrix scatter plot,
    and radar chart with scenario/year controls."))
)

server <- function(input, output, session) {}

shinyApp(ui, server)
