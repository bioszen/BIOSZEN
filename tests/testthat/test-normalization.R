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

test_that("normalize_params marks normalized columns as unavailable when control medium is absent", {
  df <- make_norm_fixture()
  res <- normalize_params(df, params = c("Growth", "Yield"), do_norm = TRUE, ctrl_medium = "None")

  expect_true(all(is.na(res$Growth_Norm)))
  expect_true(all(is.na(res$Yield_Norm)))
  expect_true(isTRUE(attr(res, "norm_fallback")))
  expect_identical(attr(res, "norm_mode"), "unavailable")
})

test_that("normalize_params omits parameter when all replicate baselines are invalid", {
  df <- data.frame(
    Strain = c("S1", "S1"),
    Media  = c("Ctrl", "M1"),
    BiologicalReplicate = 1,
    Growth = c(0, 2)
  )
  res <- normalize_params(df, params = "Growth", do_norm = TRUE, ctrl_medium = "Ctrl")

  expect_true(all(is.na(res$Growth_Norm)))
  expect_true(isTRUE(attr(res, "norm_fallback")))
  expect_identical(attr(res, "norm_mode"), "unavailable")
  raw_params <- attr(res, "norm_raw_fallback_params")
  if (is.null(raw_params)) raw_params <- character(0)
  expect_length(raw_params, 0)
})

test_that("normalize_params only uses replicate-matched control baselines", {
  df <- data.frame(
    Strain = c("S1", "S1", "S1", "S1", "S1"),
    Media = c("Ctrl", "M1", "M1", "Ctrl", "M2"),
    BiologicalReplicate = c(1, 1, 2, 3, 2),
    Growth = c(2, 6, 8, 3, 12)
  )

  res <- normalize_params(df, params = "Growth", do_norm = TRUE, ctrl_medium = "Ctrl")

  expect_equal(res$Growth_Norm[df$Media == "Ctrl" & df$BiologicalReplicate == 1], 1)
  expect_equal(res$Growth_Norm[df$Media == "M1" & df$BiologicalReplicate == 1], 3)
  expect_true(is.na(res$Growth_Norm[df$Media == "M1" & df$BiologicalReplicate == 2]))
  expect_true(is.na(res$Growth_Norm[df$Media == "M2" & df$BiologicalReplicate == 2]))
  expect_equal(res$Growth_Norm[df$Media == "Ctrl" & df$BiologicalReplicate == 3], 1)
  expect_true(isTRUE(attr(res, "norm_fallback")))
  expect_identical(attr(res, "norm_mode"), "partial_skip")
  expect_true(isTRUE(attr(res, "norm_partial_omitted")))
})

test_that("normalize_params matches control medium case-insensitively", {
  df <- data.frame(
    Strain = c("S1", "S1", "S1"),
    Media  = c("mock", "DrugA", "DrugB"),
    BiologicalReplicate = c(1, 1, 1),
    Growth = c(2, 6, 4)
  )

  res <- normalize_params(df, params = "Growth", do_norm = TRUE, ctrl_medium = "Mock")
  expect_equal(res$Growth_Norm[df$Media == "mock"], 1)
  expect_equal(res$Growth_Norm[df$Media == "DrugA"], 3)
  expect_equal(res$Growth_Norm[df$Media == "DrugB"], 2)
})

test_that("normalize_params can mix valid replicates and omitted replicates by parameter", {
  df <- data.frame(
    Strain = c("S1", "S1", "S1", "S1"),
    Media  = c("Ctrl", "Treat", "Ctrl", "Treat"),
    BiologicalReplicate = c(1, 1, 2, 2),
    P_ok = c(2, 4, 3, 9),
    P_bad = c(0, 8, 0, 16)
  )

  res <- normalize_params(df, params = c("P_ok", "P_bad"), do_norm = TRUE, ctrl_medium = "Ctrl")

  expect_equal(res$P_ok_Norm[df$BiologicalReplicate == 1 & df$Media == "Treat"], 2)
  expect_equal(res$P_ok_Norm[df$BiologicalReplicate == 2 & df$Media == "Treat"], 3)
  expect_true(all(is.na(res$P_bad_Norm)))
  expect_true(isTRUE(attr(res, "norm_fallback")))
  expect_identical(attr(res, "norm_mode"), "unavailable")
  raw_params <- attr(res, "norm_raw_fallback_params")
  if (is.null(raw_params)) raw_params <- character(0)
  expect_length(raw_params, 0)
})
