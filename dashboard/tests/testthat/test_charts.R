library(testthat)
library(plotly)
library(readr)

# Paths relative to test file location (dashboard/tests/testthat/)
# test_file() sets cwd to the directory containing the test file
app_r_path    <- "../../app.R"
csv_data_path <- "../../data/scores_intermediate.csv"

# Load scores data for chart tests
scores <- tryCatch(
  read_csv(csv_data_path, show_col_types = FALSE),
  error = function(e) NULL
)

# Source app.R to bring chart helper functions into scope.
# chdir = TRUE ensures app.R's relative paths (e.g. "data/scores_intermediate.csv")
# resolve correctly from dashboard/ rather than the test directory.
# shiny.testmode suppresses the shinyApp() call at the bottom of app.R.
options(shiny.testmode = TRUE)
tryCatch(
  source(app_r_path, chdir = TRUE),
  error = function(e) {
    message("Could not source dashboard/app.R: ", conditionMessage(e))
  }
)

# Test 1: build_scatter_plot() exists and returns a plotly object
# RED: app.R stub does not define build_scatter_plot — will fail with
# "could not find function" until Plan 02 implements it.
test_that("build_scatter_plot() exists and returns a plotly object", {
  skip_if(is.null(scores), "scores data not available")
  expect_true(exists("build_scatter_plot"),
              info = "build_scatter_plot() must be defined in app.R")
  result <- build_scatter_plot(scores)
  expect_s3_class(result, "plotly")
})

# Test 2: build_radar_plot() exists and returns a plotly object with >= 1 trace
# RED: app.R stub does not define build_radar_plot — will fail with
# "could not find function" until Plan 02 implements it.
test_that("build_radar_plot() exists and returns a plotly object with at least one trace", {
  skip_if(is.null(scores), "scores data not available")
  expect_true(exists("build_radar_plot"),
              info = "build_radar_plot() must be defined in app.R")
  result <- build_radar_plot(scores, selected_country = NULL)
  expect_s3_class(result, "plotly")
  # LAC average trace must always be present
  n_traces <- length(result$x$data)
  expect_gte(n_traces, 1L)
})
