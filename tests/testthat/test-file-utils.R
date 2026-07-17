library(testthat)

root <- app_test_root()
source(app_test_path( "helpers.R"))

test_that("safe_file sanitizes names and keeps extension", {
  out <- safe_file("my report:1?.xlsx")
  expect_equal(out, "my_report_1_.xlsx")
  expect_equal(tools::file_ext(out), "xlsx")
})

test_that("safe_sheet strips invalid characters", {
  expect_equal(safe_sheet("Sheet 1/A:B"), "Sheet_1_A_B")
})

test_that("safe_sheet_names keeps Excel sheet names unique within the 31-character limit", {
  params <- c(
    "cell_coloc_mito_vs_dapi_adj_manders",
    "cell_coloc_mito_vs_dapi_adj_manders_extra",
    "cell_coloc_mito_vs_dapi_adj_manders"
  )
  sheets <- safe_sheet_names(params)

  expect_length(sheets, length(params))
  expect_length(unique(sheets), length(params))
  expect_true(all(nchar(sheets, type = "chars") <= 31L))
  expect_equal(sheets[[1]], "cell_coloc_mito_vs_dapi_adj_man")
  expect_match(sheets[[2]], "_1$")
  expect_match(sheets[[3]], "_2$")
})

test_that("mirrored ZIP paths preserve repeated filenames in separate folders", {
  skip_if_not_installed("zip")

  root <- tempfile("bioszen_bundle_layout_")
  archive <- tempfile(fileext = ".zip")
  dir.create(file.path(root, "datasets", "one"), recursive = TRUE)
  dir.create(file.path(root, "versiones", "one"), recursive = TRUE)
  writeLines("dataset", file.path(root, "datasets", "one", "INFO.txt"))
  writeLines("version", file.path(root, "versiones", "one", "INFO.txt"))
  on.exit(unlink(c(root, archive), recursive = TRUE, force = TRUE), add = TRUE)

  old <- setwd(root)
  on.exit(setwd(old), add = TRUE)
  zip::zipr(
    zipfile = archive,
    files = list.files(".", recursive = TRUE),
    mode = "mirror"
  )

  listing <- utils::unzip(archive, list = TRUE)$Name
  expect_true("datasets/one/INFO.txt" %in% listing)
  expect_true("versiones/one/INFO.txt" %in% listing)
})

test_that("sanitize replaces forbidden filename characters", {
  expect_equal(sanitize("A/B:C*D?E\"F<G>H|I"), "A_B_C_D_E_F_G_H_I")
})

test_that("sanitize handles blank and NA filename parts", {
  expect_equal(sanitize(NA_character_), "")
  expect_equal(sanitize(c("A/B", ""), fallback = "file"), c("A_B", "file"))
})
