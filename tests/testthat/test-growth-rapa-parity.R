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

  for (column in numeric_columns) {
    expect_equal(
      unname(actual[[column]]),
      unname(expected[[column]]),
      tolerance = sqrt(.Machine$double.eps),
      info = sprintf("Growth parameter mismatch in column %s", column)
    )
  }
})
