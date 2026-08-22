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

test_that("irregular curve fixture uses recorded time points for parameter extraction", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("gcplyr")
  skip_if_not_installed("dplyr")

  curves_path <- growth_parity_fixture("irregular_curve.xlsx")
  sheets <- readxl::excel_sheets(curves_path)
  expect_identical(sheets, "Magellan Sheet 1")

  source_curves <- readxl::read_excel(
    curves_path,
    sheet = sheets[[1]],
    skip = 2,
    .name_repair = "minimal"
  )
  expect_identical(names(source_curves)[1:2], c("Time", "Temperature"))

  recorded_time <- as.numeric(source_curves[["Time"]])
  expect_length(recorded_time, 97L)
  expect_true(all(is.finite(recorded_time)))
  expect_true(all(diff(recorded_time) > 0))
  expect_gt(length(unique(round(diff(recorded_time), 12))), 1L)
  expect_equal(
    head(recorded_time, 12L),
    c(0, 0.08, 0.5, 0.675, 1.5, 1.8, 2.5, 2.61, 3.5, 4.225, 5, 5.08),
    tolerance = sqrt(.Machine$double.eps)
  )
  expect_equal(tail(recorded_time, 1L), 47.5, tolerance = sqrt(.Machine$double.eps))

  prepared <- .bioszen_build_curves_sheet(
    curves_path,
    max_time = 1,
    time_interval = 1,
    time_mode = "irregular"
  )
  expect_identical(prepared$format, "raw")
  expect_identical(prepared$time_column, "Time")
  expect_equal(
    unname(prepared$new_data$Time),
    recorded_time,
    tolerance = sqrt(.Machine$double.eps)
  )

  well_names <- names(source_curves)[-(1:2)]
  expect_length(well_names, 48L)
  expect_identical(names(prepared$new_data)[-1], well_names)

  measurements <- as.data.frame(
    lapply(source_curves[well_names], as.numeric),
    check.names = FALSE
  )
  expect_true(all(vapply(measurements, function(x) all(is.finite(x)), logical(1))))

  tidy_df <- gcplyr::trans_wide_to_tidy(prepared$new_data, id_cols = "Time")
  actual <- compute_growth_results_batch(tidy_df)

  expect_identical(names(actual), .bioszen_growth_result_columns)
  expect_identical(nrow(actual), 48L)
  expect_identical(as.character(actual$Well), well_names)

  expected_od0 <- vapply(measurements, function(x) x[[1]], numeric(1))
  expected_odmax <- vapply(measurements, max, numeric(1))
  expected_peak_time <- vapply(
    measurements,
    function(x) recorded_time[[which.max(x)]],
    numeric(1)
  )
  expected_auc <- vapply(
    measurements,
    function(x) sum(diff(recorded_time) * (head(x, -1L) + tail(x, -1L)) / 2),
    numeric(1)
  )

  expect_equal(unname(actual$OD0), unname(expected_od0), tolerance = 1e-12)
  expect_equal(unname(actual$ODmax), unname(expected_odmax), tolerance = 1e-12)

  auc_rows <- is.finite(actual$AUC)
  expect_true(any(auc_rows))
  expect_equal(
    unname(actual$AUC[auc_rows]),
    unname(expected_auc[auc_rows]),
    tolerance = 1e-10
  )

  peak_rows <- is.finite(actual$max_time)
  expect_true(any(peak_rows))
  expect_equal(
    unname(actual$max_time[peak_rows]),
    unname(expected_peak_time[peak_rows]),
    tolerance = sqrt(.Machine$double.eps)
  )

  rate_column <- .bioszen_growth_result_columns[[2]]
  phase_rows <- is.finite(actual[[rate_column]]) & actual[[rate_column]] != 0
  expect_true(any(phase_rows))
  expect_equal(
    unname(actual$doub_time[phase_rows]),
    unname(log(2) / actual[[rate_column]][phase_rows]),
    tolerance = sqrt(.Machine$double.eps)
  )
  expect_true(all(actual$lag_time[phase_rows] >= min(recorded_time)))
  expect_true(all(actual$lag_time[phase_rows] <= max(recorded_time)))
  expect_true(all(actual$max_percap_time[phase_rows] >= min(recorded_time)))
  expect_true(all(actual$max_percap_time[phase_rows] <= max(recorded_time)))
})
