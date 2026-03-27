library(testthat)
library(here)
here::i_am("drm-lac-intelligence.Rproj")
source(here::here("shared/R/validation.R"))
library(terra)
library(sf)

test_that("assert_raster passes for SpatRaster", {
  r <- terra::rast(nrows = 2, ncols = 2, vals = 1:4)
  expect_invisible(assert_raster(r))
})

test_that("assert_raster errors with informative message for non-raster", {
  expect_error(assert_raster(list(a = 1), arg_name = "my_arg"),
               regexp = "SpatRaster")
})

test_that("assert_vector passes for sf object", {
  pt <- sf::st_sf(id = 1L, geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326))
  expect_invisible(assert_vector(pt))
})

test_that("assert_vector errors with informative message for non-sf", {
  expect_error(assert_vector(data.frame(x = 1), arg_name = "my_vec"),
               regexp = "sf object")
})

test_that("assert_numeric_range passes for values in [0, 1]", {
  expect_invisible(assert_numeric_range(c(0, 0.5, 1)))
})

test_that("assert_numeric_range errors for values out of range", {
  expect_error(assert_numeric_range(c(-0.1, 0.5, 1.1)), regexp = "\\[0")
})

test_that("assert_file_exists passes for a real file", {
  tmp <- tempfile()
  writeLines("test", tmp)
  on.exit(unlink(tmp))
  expect_invisible(assert_file_exists(tmp))
})

test_that("assert_file_exists errors for missing file", {
  expect_error(assert_file_exists("definitely_not_a_real_file.xyz"),
               regexp = "not found")
})
