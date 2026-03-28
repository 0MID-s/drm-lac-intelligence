# shared/R/utils.R
# Bootstrap: load all shared utilities with one source() call.
# Usage in any project script: source(here::here("shared/R/utils.R"))

library(here)

# Anchor here() to the repo root (.Rproj file).
# This guards against working directory misdetection in complex layouts.
here::i_am("drm-lac-intelligence.Rproj")

source(here::here("shared/R/data_loader.R"))
source(here::here("shared/R/spatial_utils.R"))
source(here::here("shared/R/hazard_utils.R"))
source(here::here("shared/R/viz_utils.R"))
source(here::here("shared/R/validation.R"))

message("Shared utilities loaded: data_loader, spatial_utils, hazard_utils, viz_utils, validation")
