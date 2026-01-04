library(testthat)

root <- normalizePath(testthat::test_path("..", ".."))
source(file.path(root, "inst", "app", "helpers.R"))

test_that("safe_file sanitizes names and keeps extension", {
  out <- safe_file("my report:1?.xlsx")
  expect_equal(out, "my_report_1_.xlsx")
  expect_equal(tools::file_ext(out), "xlsx")
})

test_that("safe_sheet strips invalid characters", {
  expect_equal(safe_sheet("Sheet 1/A:B"), "Sheet_1_A_B")
})

test_that("sanitize replaces forbidden filename characters", {
  expect_equal(sanitize("A/B:C*D?E\"F<G>H|I"), "A_B_C_D_E_F_G_H_I")
})
