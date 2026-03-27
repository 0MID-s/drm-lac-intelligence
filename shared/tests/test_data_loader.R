library(testthat)
library(here)
here::i_am("drm-lac-intelligence.Rproj")
source(here::here("shared/R/validation.R"))
source(here::here("shared/R/data_loader.R"))
library(terra)
library(sf)

test_that("load_raster returns a SpatRaster for a valid .tif", {
  skip_if_not_installed("terra")
  tmp <- terra::rast(nrows = 4, ncols = 4, vals = 1:16)
  tmp_path <- tempfile(fileext = ".tif")
  terra::writeRaster(tmp, tmp_path, overwrite = TRUE)
  on.exit(unlink(tmp_path))

  result <- load_raster(tmp_path)
  expect_s4_class(result, "SpatRaster")
})

test_that("load_raster errors with informative message on missing file", {
  expect_error(load_raster("nonexistent_file.tif"), regexp = "not found")
})

test_that("load_vector returns an sf object for a valid GeoPackage", {
  skip_if_not_installed("sf")
  tmp_sf <- sf::st_sf(
    id = 1:3,
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), sf::st_point(c(2, 2)),
      crs = 4326
    )
  )
  tmp_path <- tempfile(fileext = ".gpkg")
  sf::st_write(tmp_sf, tmp_path, quiet = TRUE)
  on.exit(unlink(tmp_path))

  result <- load_vector(tmp_path)
  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 3L)
})

test_that("load_vector errors with informative message on missing file", {
  expect_error(load_vector("nonexistent_file.gpkg"), regexp = "not found")
})
