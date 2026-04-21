library(testthat)

root <- normalizePath(test_path("..", ".."))
load_malformed_excel_sources <- function() {
  app_dir <- app_test_path()
  old <- setwd(app_dir)
  on.exit(setwd(old), add = TRUE)
  source(file.path(app_dir, "global.R"))
}
load_malformed_excel_sources()

test_that("read_excel_tmp rejects non-Excel payloads with clear error", {
  bad <- tempfile("not_excel_", fileext = ".xlsx")
  writeLines(c("this", "is", "not", "xlsx"), bad, useBytes = TRUE)
  on.exit(unlink(bad), add = TRUE)

  expect_error(
    read_excel_tmp(bad, sheet = "Datos"),
    "no es un Excel válido|valid",
    ignore.case = TRUE
  )
})

test_that("load_curve_workbook fails gracefully for malformed curve sheets", {
  skip_if_not_installed("openxlsx")

  malformed <- tempfile("curve_bad_", fileext = ".xlsx")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Sheet1")
  openxlsx::writeData(
    wb,
    sheet = "Sheet1",
    x = data.frame(NotTime = 1:5, A1 = c(0.1, 0.2, 0.3, 0.4, 0.5)),
    colNames = TRUE
  )
  openxlsx::saveWorkbook(wb, malformed, overwrite = TRUE)
  on.exit(unlink(malformed), add = TRUE)

  out <- load_curve_workbook(malformed)
  expect_false(isTRUE(out$ok))
  expect_true(out$reason %in% c("invalid", "not_found"))
  expect_null(out$Sheet1)
  expect_null(out$Sheet2)
})

test_that("load_curve_workbook tolerates missing Sheet2 by auto-filling defaults", {
  skip_if_not_installed("openxlsx")

  partial <- tempfile("curve_partial_", fileext = ".xlsx")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Sheet1")
  openxlsx::writeData(
    wb,
    sheet = "Sheet1",
    x = data.frame(Time = 0:4, A1 = c(0.1, 0.2, 0.25, 0.31, 0.39)),
    colNames = TRUE
  )
  openxlsx::saveWorkbook(wb, partial, overwrite = TRUE)
  on.exit(unlink(partial), add = TRUE)

  out <- load_curve_workbook(partial)
  expect_true(isTRUE(out$ok))
  expect_true(is.data.frame(out$Sheet1))
  expect_true(is.data.frame(out$Sheet2))
  expect_true(all(c("X_Max", "Interval_X", "Y_Max", "Interval_Y", "X_Title", "Y_Title") %in% names(out$Sheet2)))
})
