# shared/R/validation.R
# Defensive input checking functions for shared utility pipeline.
# All functions return invisible(TRUE) on success, stop() on failure.

# ---------------------------------------------------------------------------
# assert_raster
# ---------------------------------------------------------------------------

#' Assert that x is a SpatRaster (terra)
#'
#' @param x       Object to check.
#' @param arg_name Character string used in error messages (default "x").
#' @return invisible(TRUE) on success; stops on failure.
assert_raster <- function(x, arg_name = "x") {
  if (!inherits(x, "SpatRaster")) {
    stop(arg_name, " must be a SpatRaster (terra). Got: ", class(x)[1])
  }
  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# assert_vector
# ---------------------------------------------------------------------------

#' Assert that x is an sf object
#'
#' @param x       Object to check.
#' @param arg_name Character string used in error messages (default "x").
#' @return invisible(TRUE) on success; stops on failure.
assert_vector <- function(x, arg_name = "x") {
  if (!inherits(x, "sf")) {
    stop(arg_name, " must be an sf object. Got: ", class(x)[1])
  }
  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# assert_numeric_range
# ---------------------------------------------------------------------------

#' Assert that all values of x lie within [min_val, max_val]
#'
#' @param x        Numeric vector to check.
#' @param min_val  Lower bound (default 0).
#' @param max_val  Upper bound (default 1).
#' @param arg_name Character string used in error messages (default "x").
#' @return invisible(TRUE) on success; stops on failure.
assert_numeric_range <- function(x, min_val = 0, max_val = 1, arg_name = "x") {
  if (!all(x >= min_val & x <= max_val, na.rm = TRUE)) {
    stop(
      arg_name, " values must be in [", min_val, ", ", max_val, "]. ",
      "Range found: [",
      round(min(x, na.rm = TRUE), 4), ", ",
      round(max(x, na.rm = TRUE), 4), "]"
    )
  }
  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# assert_file_exists
# ---------------------------------------------------------------------------

#' Assert that a file path points to an existing file
#'
#' @param path     File path to check.
#' @param arg_name Character string used in error messages (default "path").
#' @return invisible(TRUE) on success; stops on failure.
assert_file_exists <- function(path, arg_name = "path") {
  if (!file.exists(path)) {
    stop(arg_name, ": file not found: ", path)
  }
  invisible(TRUE)
}
