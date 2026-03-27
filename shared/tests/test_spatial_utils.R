library(testthat)
library(here)
here::i_am("drm-lac-intelligence.Rproj")
source(here::here("shared/R/spatial_utils.R"))
library(terra)
library(sf)

test_that("reproject_to_wgs84 reprojects a SpatRaster to EPSG:4326", {
  r <- terra::rast(nrows = 4, ncols = 4, vals = 1:16, crs = "EPSG:32636")
  result <- reproject_to_wgs84(r)
  expect_s4_class(result, "SpatRaster")
  expect_true(grepl("4326", terra::crs(result, describe = TRUE)$code))
})

test_that("reproject_to_wgs84 reprojects an sf object to EPSG:4326", {
  pt <- sf::st_sf(
    id = 1L,
    geometry = sf::st_sfc(sf::st_point(c(500000, 4649776)), crs = 32636)
  )
  result <- reproject_to_wgs84(pt)
  expect_s3_class(result, "sf")
  expect_equal(sf::st_crs(result)$epsg, 4326L)
})

test_that("reproject_to_wgs84 errors for unsupported input types", {
  expect_error(reproject_to_wgs84(list(a = 1)))
})

test_that("resample_to_template returns SpatRaster matching template resolution", {
  source_r  <- terra::rast(nrows = 8, ncols = 8, vals = 1:64)
  template  <- terra::rast(nrows = 4, ncols = 4)
  terra::ext(template) <- terra::ext(source_r)
  result <- resample_to_template(source_r, template)
  expect_s4_class(result, "SpatRaster")
  expect_equal(terra::nrow(result), 4L)
  expect_equal(terra::ncol(result), 4L)
})
