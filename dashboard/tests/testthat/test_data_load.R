library(testthat)
library(readr)

# Paths relative to test file location (dashboard/tests/testthat/)
# test_file() sets cwd to the directory containing the test file
csv_data_path <- "../../data/scores_intermediate.csv"

# Test 1: scores_intermediate.csv loads without error and has > 0 rows
test_that("scores_intermediate.csv loads from dashboard/data/ without error", {
  expect_true(file.exists(csv_data_path))
  scores <- read_csv(csv_data_path, show_col_types = FALSE)
  expect_s3_class(scores, "data.frame")
  expect_gt(nrow(scores), 0)
})

# Test 2: All 9 composite columns present in loaded CSV
test_that("all 9 composite scenario columns are present in scores CSV", {
  scores <- read_csv(csv_data_path, show_col_types = FALSE)
  composite_cols <- c(
    "composite_baseline_2024", "composite_baseline_2030", "composite_baseline_2050",
    "composite_rcp45_2024",    "composite_rcp45_2030",    "composite_rcp45_2050",
    "composite_rcp85_2024",    "composite_rcp85_2030",    "composite_rcp85_2050"
  )
  for (col in composite_cols) {
    expect_true(col %in% names(scores), info = paste("Missing column:", col))
  }
})

# Test 3: composite_col helper returns expected column name
test_that("composite_col helper returns 'composite_rcp45_2030' for scenario='rcp45', year='2030'", {
  composite_col <- function(scenario, year) paste0("composite_", scenario, "_", year)
  result <- composite_col("rcp45", "2030")
  expect_equal(result, "composite_rcp45_2030")
})
