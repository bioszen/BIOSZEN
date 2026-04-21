library(testthat)

test_that("grouped upload flow guards embedded curve parsing failures", {
  server_file <- app_test_path("server", "server_main.R")
  expect_true(file.exists(server_file))

  txt <- paste(readLines(server_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_true(grepl("curve_conv\\s*<-\\s*tryCatch\\(", txt, perl = TRUE))
  expect_true(grepl("load_curve_workbook\\(", txt, perl = TRUE))
  expect_true(grepl("curve_ok\\s*<-\\s*isTRUE\\(curve_conv\\$ok\\)", txt, perl = TRUE))
})
