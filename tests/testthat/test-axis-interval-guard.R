library(testthat)

source(app_test_path("helpers.R"))

test_that("axis_breaks_limited caps excessive tick counts", {
  br <- axis_breaks_limited(max_value = 100, interval = 0.0001, max_ticks = 40L)
  expect_true(is.numeric(br))
  expect_lte(length(br), 41L)
  expect_equal(max(br), 100)
})

test_that("axis_breaks_limited_range caps excessive tick counts", {
  br <- axis_breaks_limited_range(
    min_value = -1,
    max_value = 1,
    interval = 0.00001,
    max_ticks = 60L
  )
  expect_true(is.numeric(br))
  expect_lte(length(br), 61L)
  expect_equal(min(br), -1)
  expect_equal(max(br), 1)
})
