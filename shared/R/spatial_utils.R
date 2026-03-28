# shared/R/spatial_utils.R
# Spatial harmonisation utilities.
# Requires packages: terra, sf

# ---------------------------------------------------------------------------
# reproject_to_wgs84
# ---------------------------------------------------------------------------

#' Reproject a SpatRaster or sf object to WGS 84 (EPSG:4326)
#'
#' @param x A \code{SpatRaster} (terra) or \code{sf} object.
#' @return Reprojected object of the same class as \code{x}.
reproject_to_wgs84 <- function(x) {
  if (inherits(x, "SpatRaster")) {
    return(terra::project(x, "EPSG:4326"))
  }
  if (inherits(x, "sf")) {
    return(sf::st_transform(x, 4326))
  }
  stop(
    "reproject_to_wgs84: x must be a SpatRaster or sf object. Got: ",
    class(x)[1]
  )
}

# ---------------------------------------------------------------------------
# crop_to_extent
# ---------------------------------------------------------------------------

#' Crop a SpatRaster or sf object to a given extent
#'
#' @param x          A \code{SpatRaster} or \code{sf} object to crop.
#' @param extent_obj A \code{SpatExtent} (terra) or \code{sf} object defining the crop boundary.
#' @return Cropped object of the same class as \code{x}.
crop_to_extent <- function(x, extent_obj) {
  if (inherits(x, "SpatRaster")) {
    return(terra::crop(x, extent_obj))
  }
  if (inherits(x, "sf")) {
    # sf::st_crop accepts sf, bbox, or numeric extent
    if (inherits(extent_obj, "SpatExtent")) {
      bbox <- sf::st_bbox(
        c(
          xmin = terra::xmin(extent_obj),
          ymin = terra::ymin(extent_obj),
          xmax = terra::xmax(extent_obj),
          ymax = terra::ymax(extent_obj)
        ),
        crs = sf::st_crs(x)
      )
      return(sf::st_crop(x, bbox))
    }
    return(sf::st_crop(x, extent_obj))
  }
  stop(
    "crop_to_extent: x must be a SpatRaster or sf object. Got: ",
    class(x)[1]
  )
}

# ---------------------------------------------------------------------------
# resample_to_template
# ---------------------------------------------------------------------------

#' Resample a SpatRaster to match the resolution and extent of a template
#'
#' @param raster_obj     A \code{SpatRaster} to resample.
#' @param template_raster A \code{SpatRaster} defining the target grid.
#' @param method         Resampling method: \code{"bilinear"} (continuous data)
#'                       or \code{"near"} (categorical data). Default: \code{"bilinear"}.
#' @return Resampled \code{SpatRaster} matching \code{template_raster} grid.
resample_to_template <- function(raster_obj, template_raster,
                                  method = "bilinear") {
  valid_methods <- c("bilinear", "near", "cubic", "cubicspline", "lanczos",
                     "average", "mode", "max", "min", "med", "q1", "q3",
                     "sum", "rms")
  if (!method %in% valid_methods) {
    stop(
      "resample_to_template: invalid method '", method,
      "'. Use 'bilinear' (continuous) or 'near' (categorical)."
    )
  }
  terra::resample(raster_obj, template_raster, method = method)
}
