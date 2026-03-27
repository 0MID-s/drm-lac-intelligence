library(testthat)
library(here)
here::i_am("drm-lac-intelligence.Rproj")
source(here::here("shared/R/hazard_utils.R"))
library(terra)

test_that("normalise_to_01 maps numeric vector to [0, 1]", {
  result <- normalise_to_01(c(10, 20, 30, 40, 50))
  expect_equal(min(result), 0)
  expect_equal(max(result), 1)
  expect_equal(length(result), 5L)
})

test_that("normalise_to_01 errors when all values are identical", {
  expect_error(normalise_to_01(c(5, 5, 5, 5)), regexp = "identical")
})

test_that("normalise_to_01 with method='percentile' returns values in [0, 1]", {
  result <- normalise_to_01(c(10, 20, 30, 40, 50), method = "percentile")
  expect_true(all(result >= 0 & result <= 1))
})

test_that("normalise_to_01 works on a SpatRaster", {
  r <- terra::rast(nrows = 3, ncols = 3, vals = c(10, 20, 30, 40, 50, 60, 70, 80, 90))
  result <- normalise_to_01(r)
  expect_s4_class(result, "SpatRaster")
  vals <- terra::values(result)
  expect_true(all(vals >= 0 & vals <= 1, na.rm = TRUE))
})

test_that("compute_composite_index produces weighted sum of layers", {
  r1 <- terra::rast(nrows = 3, ncols = 3, vals = rep(0.8, 9))
  r2 <- terra::rast(nrows = 3, ncols = 3, vals = rep(0.2, 9))
  layers  <- list(flood = r1, heat = r2)
  weights <- c(flood = 0.6, heat = 0.4)

  result <- compute_composite_index(layers, weights)
  expect_s4_class(result, "SpatRaster")
  expected_val <- 0.8 * 0.6 + 0.2 * 0.4  # 0.56
  expect_equal(round(mean(terra::values(result), na.rm = TRUE), 4), round(expected_val, 4))
})

test_that("compute_composite_index errors when names mismatch", {
  r1 <- terra::rast(nrows = 2, ncols = 2, vals = rep(0.5, 4))
  layers  <- list(flood = r1)
  weights <- c(heat = 1.0)  # wrong name
  expect_error(compute_composite_index(layers, weights))
})
