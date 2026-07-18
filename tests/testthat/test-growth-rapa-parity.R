library(testthat)

root <- app_test_root()
source(app_test_path("server", "growth_module.R"))
source(app_test_path("params", "params_growth.R"))

growth_parity_fixture <- function(filename) {
  path <- testthat::test_path("fixtures", "growth", filename)
  if (!file.exists(path)) {
    stop(sprintf("Missing committed growth parity fixture: %s", filename))
  }
  path
}

test_that("growth fixture reproduces all parameters for all wells", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("gcplyr")
  skip_if_not_installed("dplyr")
  library(dplyr)

  curves_path <- growth_parity_fixture("Curvas_Test.xlsx")
  params_path <- growth_parity_fixture("Parametros_Test.xlsx")

  expect_identical(readxl::excel_sheets(curves_path), "Sheet1")

  prepared <- .bioszen_build_curves_sheet(
    curves_path,
    max_time = 48,
    time_interval = 0.5
  )
  expect_identical(prepared$format, "processed")
  expect_identical(names(prepared$new_data)[[1]], "Time")

  tidy_df <- gcplyr::trans_wide_to_tidy(prepared$new_data, id_cols = "Time")
  actual <- compute_growth_results_batch(tidy_df)
  expected <- readxl::read_excel(
    params_path,
    sheet = "Resultados Combinados",
    .name_repair = "minimal"
  )

  expected_columns <- .bioszen_growth_result_columns
  expect_identical(names(actual), expected_columns)
  expect_identical(names(expected), expected_columns)
  expect_identical(nrow(actual), 43L)
  expect_identical(nrow(expected), 43L)
  expect_identical(as.character(actual$Well), as.character(expected$Well))

  numeric_columns <- setdiff(expected_columns, "Well")
  expect_true(all(vapply(expected[numeric_columns], is.numeric, logical(1))))
  expect_true(all(vapply(actual[numeric_columns], function(x) all(is.finite(x)), logical(1))))

  stable_columns <- c("ODmax", "AUC", "max_time", "OD0")
  for (column in stable_columns) {
    expect_equal(
      unname(actual[[column]]),
      unname(expected[[column]]),
      tolerance = sqrt(.Machine$double.eps),
      info = sprintf("Growth parameter mismatch in column %s", column)
    )
  }

  # Flat curves have platform-sensitive near-zero fits; their phase timing is undefined.
  rate_column <- expected_columns[[2]]
  expected_rate <- expected[[rate_column]]
  actual_rate <- actual[[rate_column]]
  expected_growth <- is.finite(expected_rate) & expected_rate >= 0.05
  actual_growth <- is.finite(actual_rate) & actual_rate >= 0.05
  expect_identical(
    actual_growth,
    expected_growth,
    info = "The set of wells with a detected growth phase changed"
  )
  expect_true(
    all(abs(actual_rate[!expected_growth]) < 0.05),
    info = "A non-growing well produced a material growth rate"
  )

  phase_columns <- c(rate_column, "lag_time", "max_percap_time", "doub_time")
  for (column in phase_columns) {
    expect_equal(
      unname(actual[[column]][expected_growth]),
      unname(expected[[column]][expected_growth]),
      tolerance = sqrt(.Machine$double.eps),
      info = sprintf("Detected growth-phase parameter mismatch in column %s", column)
    )
  }
})
