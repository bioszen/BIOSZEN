library(testthat)

root <- normalizePath(testthat::test_path("..", ".."))
source(file.path(root, "inst", "app", "helpers.R"))

test_that("curve_pointwise_fisher keeps per-point underflow p-values and returns a valid result", {
  d1 <- tibble::tibble(
    Time = c(0, 1, 2, 3, 4),
    Avg = c(1000, 1200, 1400, 1600, 1800),
    SD = c(1, 1, 1, 1, 1),
    N = c(100, 100, 100, 100, 100)
  )
  d2 <- tibble::tibble(
    Time = c(0, 1, 2, 3, 4),
    Avg = c(10, 20, 30, 40, 50),
    SD = c(1, 1, 1, 1, 1),
    N = c(100, 100, 100, 100, 100)
  )

  res <- curve_pointwise_fisher(d1, d2)

  expect_true(is.list(res))
  expect_true(is.finite(as.numeric(res$estimate)))
  expect_true(is.finite(as.numeric(res$p_value)))
  expect_true(as.numeric(res$p_value) >= 0 && as.numeric(res$p_value) <= 1)
  expect_true(as.numeric(res$n_points) >= 2)
})

test_that("curve_pointwise_fisher returns NA p-value when there are not enough overlapping points", {
  d1 <- tibble::tibble(
    Time = c(0, 1, 2),
    Avg = c(1, 2, 3),
    SD = c(0.2, 0.2, 0.2),
    N = c(3, 3, 3)
  )
  d2 <- tibble::tibble(
    Time = c(2, 3, 4),
    Avg = c(1.1, 2.1, 3.1),
    SD = c(0.2, 0.2, 0.2),
    N = c(3, 3, 3)
  )

  res <- curve_pointwise_fisher(d1, d2)

  expect_true(is.na(res$p_value))
  expect_true(as.numeric(res$n_points) < 2)
})
