library(testthat)

# Paths relative to test file location (dashboard/tests/testthat/)
# test_file() sets cwd to the directory containing the test file
app_r_path   <- "../../app.R"
csv_data_path <- "../../data/scores_intermediate.csv"

# Read app.R as a character string for static analysis
app_code <- tryCatch(
  paste(readLines(app_r_path), collapse = "\n"),
  error = function(e) ""
)

# Test 1: app.R contains no relative paths outside dashboard/ (no "../")
# Paths starting with "../" would reference files outside dashboard/ and
# will 404 after rsconnect bundling — must be absent.
test_that("app.R contains no '../' relative paths that escape dashboard/", {
  expect_false(
    grepl("../", app_code, fixed = TRUE),
    label = "app.R must not contain '../' — all paths must be relative to dashboard/"
  )
})

# Test 2: app.R does not import terra (incompatible with shinyapps.io)
test_that("app.R does not call library(terra) or require(terra)", {
  expect_false(
    grepl("library(terra)", app_code, fixed = TRUE),
    label = "app.R must not import terra — confirmed incompatible with shinyapps.io"
  )
  expect_false(
    grepl("require(terra)", app_code, fixed = TRUE),
    label = "app.R must not require terra — confirmed incompatible with shinyapps.io"
  )
})

# Test 3: dashboard/data/scores_intermediate.csv exists (deploy self-containment)
test_that("dashboard/data/scores_intermediate.csv exists for self-contained deploy", {
  expect_true(
    file.exists(csv_data_path),
    label = "dashboard/data/scores_intermediate.csv must exist for rsconnect bundling"
  )
})
