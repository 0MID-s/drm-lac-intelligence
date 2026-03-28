# shared/R/hazard_utils.R
# Hazard scoring primitives for climate risk composite index computation.
# Requires packages: terra

# ---------------------------------------------------------------------------
# normalise_to_01
# ---------------------------------------------------------------------------

#' Normalise a numeric vector or SpatRaster to the [0, 1] range
#'
#' @param x      Numeric vector or \code{SpatRaster}.
#' @param method Normalisation method: \code{"minmax"} (default) or
#'               \code{"percentile"} (rank-based via \code{ecdf()}).
#' @return Object of the same type as \code{x}, with values in [0, 1].
normalise_to_01 <- function(x, method = "minmax") {
  method <- match.arg(method, c("minmax", "percentile"))

  if (inherits(x, "SpatRaster")) {
    vals <- terra::values(x, mat = FALSE)

    if (method == "minmax") {
      v_min <- min(vals, na.rm = TRUE)
      v_max <- max(vals, na.rm = TRUE)
      if (v_min == v_max) {
        stop("normalise_to_01: all values identical — cannot normalise")
      }
      norm_vals <- (vals - v_min) / (v_max - v_min)
    } else {
      non_na <- vals[!is.na(vals)]
      if (length(unique(non_na)) == 1L) {
        stop("normalise_to_01: all values identical — cannot normalise")
      }
      ecdf_fn <- ecdf(non_na)
      norm_vals <- ifelse(is.na(vals), NA_real_, ecdf_fn(vals))
    }

    result <- terra::setValues(x, norm_vals)
    return(result)
  }

  if (is.numeric(x)) {
    if (method == "minmax") {
      v_min <- min(x, na.rm = TRUE)
      v_max <- max(x, na.rm = TRUE)
      if (v_min == v_max) {
        stop("normalise_to_01: all values identical — cannot normalise")
      }
      return((x - v_min) / (v_max - v_min))
    } else {
      non_na <- x[!is.na(x)]
      if (length(unique(non_na)) == 1L) {
        stop("normalise_to_01: all values identical — cannot normalise")
      }
      ecdf_fn <- ecdf(non_na)
      return(ifelse(is.na(x), NA_real_, ecdf_fn(x)))
    }
  }

  stop(
    "normalise_to_01: x must be a numeric vector or SpatRaster. Got: ",
    class(x)[1]
  )
}

# ---------------------------------------------------------------------------
# compute_composite_index
# ---------------------------------------------------------------------------

#' Compute a weighted composite hazard index from multiple normalised layers
#'
#' All input layers must share identical extent, resolution, and CRS. Inputs
#' should already be normalised to [0, 1] (e.g., via \code{normalise_to_01()}).
#'
#' @param layers  Named list of \code{SpatRaster} objects.
#' @param weights Named numeric vector matching \code{names(layers)};
#'                values should sum to 1.
#' @return \code{SpatRaster} of weighted composite scores in [0, 1]
#'         (assuming normalised inputs).
compute_composite_index <- function(layers, weights) {
  if (!is.list(layers) || length(layers) == 0L) {
    stop("compute_composite_index: layers must be a non-empty named list of SpatRasters")
  }
  if (!is.numeric(weights) || length(weights) == 0L) {
    stop("compute_composite_index: weights must be a non-empty named numeric vector")
  }
  if (length(layers) != length(weights)) {
    stop(
      "compute_composite_index: length(layers) [", length(layers),
      "] != length(weights) [", length(weights), "]"
    )
  }

  layer_names  <- names(layers)
  weight_names <- names(weights)

  if (is.null(layer_names) || is.null(weight_names)) {
    stop("compute_composite_index: both layers and weights must be named")
  }
  if (!setequal(layer_names, weight_names)) {
    stop(
      "compute_composite_index: names mismatch. ",
      "Layer names: [", paste(sort(layer_names), collapse = ", "), "]. ",
      "Weight names: [", paste(sort(weight_names), collapse = ", "), "]"
    )
  }

  weight_sum <- sum(weights)
  if (abs(weight_sum - 1) > 0.001) {
    stop(
      "compute_composite_index: weights must sum to 1 (within 0.001 tolerance). ",
      "Sum found: ", round(weight_sum, 6)
    )
  }
  if (abs(weight_sum - 1) > .Machine$double.eps) {
    warning(
      "compute_composite_index: weights sum to ", round(weight_sum, 6),
      " (not exactly 1, but within tolerance). Proceeding."
    )
  }

  # Align weights to layer name order
  weights_aligned <- weights[layer_names]

  Reduce(
    "+",
    mapply(
      function(r, w) r * w,
      layers[layer_names],
      weights_aligned,
      SIMPLIFY = FALSE
    )
  )
}

# ---------------------------------------------------------------------------
# geo_mean_weighted
# ---------------------------------------------------------------------------

#' Compute weighted geometric mean across named columns of a data frame
#'
#' Tabular companion to \code{compute_composite_index()} for data frame
#' workflows (no SpatRaster required). Applies an epsilon floor before log
#' transformation to prevent \code{-Inf} from zero values.
#'
#' @param df      Data frame with one row per country.
#' @param cols    Character vector of column names (values normalised to [0, 1]).
#' @param weights Named numeric vector; names must match \code{cols}; sum to 1.
#' @return Numeric vector of length \code{nrow(df)}.
geo_mean_weighted <- function(df, cols, weights) {
  if (!all(cols %in% names(df))) {
    stop("geo_mean_weighted: cols not found in df: ",
         paste(setdiff(cols, names(df)), collapse = ", "))
  }
  if (!setequal(names(weights), cols)) {
    stop("geo_mean_weighted: weight names must match cols exactly")
  }
  eps <- 1e-6
  mat         <- as.matrix(df[, cols, drop = FALSE]) + eps
  log_mat     <- log(mat)
  w_aligned   <- weights[cols]
  drop(log_mat %*% w_aligned) |> exp()
}
