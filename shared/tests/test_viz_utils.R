library(testthat)
library(here)
here::i_am("drm-lac-intelligence.Rproj")
source(here::here("shared/R/viz_utils.R"))
library(ggplot2)

test_that("RISK_PALETTE exists with four named risk tiers", {
  expect_true(exists("RISK_PALETTE"))
  expect_named(RISK_PALETTE, c("low", "medium", "high", "critical"), ignore.order = TRUE)
})

test_that("RISK_PALETTE values are valid hex colour strings", {
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", RISK_PALETTE)))
})

test_that("theme_drm returns a ggplot2 theme object", {
  theme_obj <- theme_drm()
  expect_s3_class(theme_obj, "theme")
})

test_that("theme_drm can be added to a ggplot without error", {
  p <- ggplot(data.frame(x = 1:3, y = 1:3), aes(x, y)) +
    geom_point() +
    theme_drm()
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("scale_fill_drm_risk returns a ggplot2 Scale object", {
  scale_obj <- scale_fill_drm_risk()
  expect_true(inherits(scale_obj, "Scale"))
})
