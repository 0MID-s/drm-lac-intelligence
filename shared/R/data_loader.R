# shared/R/data_loader.R
# Typed data loading with error handling.
# Depends on: validation.R (assert_file_exists)
# Requires packages: terra, sf

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

.check_path <- function(path, caller) {
  if (!is.character(path) || length(path) != 1L || nchar(path) == 0L) {
    stop(caller, ": path must be a non-empty character string")
  }
  assert_file_exists(path, arg_name = paste0(caller, " path"))
}

# ---------------------------------------------------------------------------
# load_raster
# ---------------------------------------------------------------------------

#' Load a raster file and return a SpatRaster
#'
#' @param path Character. Path to a raster file (.tif, .nc, etc.).
#' @return A \code{SpatRaster} object (terra).
#' @examples
#' \dontrun{
#'   r <- load_raster(here::here("data/raw/flood_depth.tif"))
#' }
load_raster <- function(path) {
  .check_path(path, "load_raster")

  tryCatch(
    terra::rast(path),
    error = function(e) {
      stop("load_raster: failed to read '", path, "': ", conditionMessage(e))
    }
  )
}

# ---------------------------------------------------------------------------
# load_vector
# ---------------------------------------------------------------------------

#' Load a vector file and return an sf object
#'
#' Supported formats: .shp, .gpkg, .geojson (and any format readable by sf).
#'
#' @param path Character. Path to a vector file.
#' @return An \code{sf} object.
#' @examples
#' \dontrun{
#'   v <- load_vector(here::here("data/raw/admin_boundaries.gpkg"))
#' }
load_vector <- function(path) {
  .check_path(path, "load_vector")

  ext <- tolower(tools::file_ext(path))
  supported <- c("shp", "gpkg", "geojson", "json", "kml", "gml")
  if (!ext %in% supported) {
    stop(
      "load_vector: unrecognised file format '.", ext,
      "'. Supported: ", paste(supported, collapse = ", ")
    )
  }

  tryCatch(
    sf::st_read(path, quiet = TRUE),
    error = function(e) {
      stop("load_vector: failed to read '", path, "': ", conditionMessage(e))
    }
  )
}
