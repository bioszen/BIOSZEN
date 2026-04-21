library(testthat)

root <- app_test_root()
source(app_test_path( "server", "growth_module.R"))
source(app_test_path( "params", "params_growth.R"))

make_raw_curve_file <- function(path, n_points = 24L) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Sheet1")
  t <- seq_len(n_points)
  raw_data <- data.frame(
    Ignore1 = t,
    Ignore2 = t,
    W1 = exp(seq(0, by = 0.12, length.out = n_points)) * (1 + 0.01 * sin(t)),
    W2 = exp(seq(0, by = 0.09, length.out = n_points)) * (1 + 0.01 * cos(t)),
    W3 = exp(seq(0, by = 0.06, length.out = n_points)) * (1 + 0.012 * sin(t / 2)),
    check.names = FALSE
  )
  openxlsx::writeData(wb, "Sheet1", raw_data, startRow = 3, colNames = TRUE)
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}

test_that("raw and processed curve input formats are auto-detected and equivalent", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("gcplyr")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("writexl")
  skip_if_not_installed("openxlsx")

  raw_tmp <- tempfile(pattern = "growth_raw_", fileext = ".xlsx")
  processed_tmp <- tempfile(pattern = "growth_processed_", fileext = ".xlsx")
  make_raw_curve_file(raw_tmp, n_points = 24L)

  raw <- .bioszen_build_curves_sheet(raw_tmp, max_time = 23, time_interval = 1)
  writexl::write_xlsx(raw$new_data, processed_tmp)
  processed <- .bioszen_build_curves_sheet(processed_tmp, max_time = 23, time_interval = 1)

  expect_identical(raw$format, "raw")
  expect_identical(processed$format, "processed")
  expect_identical(names(raw$new_data), names(processed$new_data))

  tidy_raw <- gcplyr::trans_wide_to_tidy(raw$new_data, id_cols = "Time")
  tidy_processed <- gcplyr::trans_wide_to_tidy(processed$new_data, id_cols = "Time")

  out_raw <- compute_growth_results_batch(tidy_raw)
  out_processed <- compute_growth_results_batch(tidy_processed)

  expect_identical(names(out_raw), names(out_processed))
  expect_identical(as.character(out_raw$Well), as.character(out_processed$Well))

  numeric_cols <- setdiff(names(out_raw), "Well")
  for (col in numeric_cols) {
    expect_equal(
      unname(out_raw[[col]]),
      unname(out_processed[[col]]),
      tolerance = sqrt(.Machine$double.eps)
    )
  }
})

test_that("raw format detection rejects two leading index-like columns", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("readxl")

  raw_tmp <- tempfile(pattern = "growth_raw_detect_", fileext = ".xlsx")
  make_raw_curve_file(raw_tmp, n_points = 18L)

  built <- .bioszen_build_curves_sheet(raw_tmp, max_time = 17, time_interval = 1)

  expect_identical(built$format, "raw")
  expect_false(any(c("Ignore1", "Ignore2") %in% names(built$new_data)))
  expect_true(all(c("W1", "W2", "W3") %in% names(built$new_data)))
})

test_that("processed format is detected by structure even with custom first-column name", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("gcplyr")

  processed_custom <- data.frame(
    HorasMedicion = seq(0, 2, by = 0.5),
    A1 = c(0.100, 0.110, 0.125, 0.150, 0.180),
    A2 = c(0.105, 0.115, 0.130, 0.152, 0.185),
    check.names = FALSE
  )
  tmp <- tempfile(pattern = "custom_header_", fileext = ".xlsx")
  writexl::write_xlsx(processed_custom, tmp)

  built <- .bioszen_build_curves_sheet(tmp, max_time = 48, time_interval = 0.5)

  expect_identical(built$format, "processed")
  expect_identical(names(built$new_data), c("Time", "A1", "A2"))
  expect_equal(unname(built$new_data$Time), processed_custom$HorasMedicion)
})
