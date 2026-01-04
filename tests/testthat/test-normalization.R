library(testthat)

root <- normalizePath(testthat::test_path("..", ".."))
source(file.path(root, "inst", "app", "helpers.R"))

make_norm_fixture <- function() {
  data.frame(
    Strain = c("S1", "S1", "S1"),
    Media  = c("Ctrl", "M1", "M2"),
    BiologicalReplicate = c(1, 1, 1),
    Growth = c(1, 2, 4),
    Yield  = c(0.5, 0.6, 0.7)
  )
}

test_that("normalize_params returns original data when normalization disabled", {
  df <- make_norm_fixture()
  res <- normalize_params(df, params = c("Growth", "Yield"), do_norm = FALSE, ctrl_medium = "Ctrl")
  expect_false(any(grepl("_Norm$", names(res))))
  expect_identical(res, df)
})

test_that("normalize_params normalizes available params and skips missing ones", {
  df <- make_norm_fixture()
  res <- normalize_params(df, params = c("Growth", "Missing"), do_norm = TRUE, ctrl_medium = "Ctrl")

  expect_true("Growth_Norm" %in% names(res))
  expect_false("Missing_Norm" %in% names(res))
  expect_equal(res$Growth_Norm[res$Media == "Ctrl"], 1)
  expect_equal(res$Growth_Norm[res$Media == "M1"], 2)
  expect_equal(res$Growth_Norm[res$Media == "M2"], 4)
})

test_that("normalize_params clones values when control medium is absent", {
  df <- make_norm_fixture()
  res <- normalize_params(df, params = c("Growth", "Yield"), do_norm = TRUE, ctrl_medium = "None")

  expect_equal(res$Growth_Norm, df$Growth)
  expect_equal(res$Yield_Norm, df$Yield)
  expect_true(isTRUE(attr(res, "norm_fallback")))
})

test_that("normalize_params falls back to raw when control is zero or missing", {
  df <- data.frame(
    Strain = c("S1", "S1"),
    Media  = c("Ctrl", "M1"),
    BiologicalReplicate = 1,
    Growth = c(0, 2)
  )
  res <- normalize_params(df, params = "Growth", do_norm = TRUE, ctrl_medium = "Ctrl")

  expect_equal(res$Growth_Norm, df$Growth)
  expect_true(isTRUE(attr(res, "norm_fallback")))
})
