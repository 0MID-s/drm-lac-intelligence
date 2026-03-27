library(testthat)
library(here)
here::i_am("drm-lac-intelligence.Rproj")
results <- testthat::test_dir(here::here("shared/tests"), reporter = "progress")
