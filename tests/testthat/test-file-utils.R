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

test_that("mirrored ZIP paths preserve the flat format-based bundle layout", {
  skip_if_not_installed("zip")

  root <- tempfile("bioszen_bundle_layout_")
  archive <- tempfile(fileext = ".zip")
  extracted <- tempfile("bioszen_bundle_extracted_")
  dir.create(file.path(root, "datasets", "one"), recursive = TRUE)
  dir.create(file.path(root, "datasets", "one", "estadisticas"), recursive = TRUE)
  for (format in c("png", "pdf", "ppt", "metadata")) {
    dir.create(file.path(root, "versions", format), recursive = TRUE)
  }
  writeLines("dataset", file.path(root, "datasets", "one", "INFO.txt"))
  writeLines("statistics", file.path(root, "datasets", "one", "estadisticas", "estadisticas_1.xlsx"))
  writeLines("png", file.path(root, "versions", "png", "001_Boxplot.png"))
  writeLines("png-second", file.path(root, "versions", "png", "002_Boxplot.png"))
  writeLines("pdf", file.path(root, "versions", "pdf", "001_Boxplot.pdf"))
  writeLines("pdf-second", file.path(root, "versions", "pdf", "002_Boxplot.pdf"))
  writeLines("ppt", file.path(root, "versions", "ppt", "001_Boxplot.pptx"))
  writeLines("ppt-second", file.path(root, "versions", "ppt", "002_Boxplot.pptx"))
  writeLines("metadata", file.path(root, "versions", "metadata", "001_Boxplot.xlsx"))
  writeLines("metadata-second", file.path(root, "versions", "metadata", "002_Boxplot.xlsx"))
  writeLines("manifest", file.path(root, "versions", "manifest.csv"))
  on.exit(unlink(c(root, archive, extracted), recursive = TRUE, force = TRUE), add = TRUE)

  zip::zipr(
    zipfile = archive,
    files = list.files(root, recursive = TRUE),
    root = root,
    mode = "mirror"
  )

  listing <- utils::unzip(archive, list = TRUE)$Name
  expect_true("datasets/one/INFO.txt" %in% listing)
  expect_true("datasets/one/estadisticas/estadisticas_1.xlsx" %in% listing)
  expect_true("versions/png/001_Boxplot.png" %in% listing)
  expect_true("versions/png/002_Boxplot.png" %in% listing)
  expect_true("versions/pdf/001_Boxplot.pdf" %in% listing)
  expect_true("versions/ppt/001_Boxplot.pptx" %in% listing)
  expect_true("versions/metadata/001_Boxplot.xlsx" %in% listing)
  expect_true("versions/manifest.csv" %in% listing)
  expect_false(any(startsWith(listing, "versiones/")))
  expect_false(any(grepl("_INFO[.]txt$", listing)))
  expect_identical(anyDuplicated(tolower(listing)), 0L)

  dir.create(extracted, recursive = TRUE)
  utils::unzip(archive, exdir = extracted)
  expect_true(file.exists(file.path(
    extracted,
    "datasets", "one", "estadisticas", "estadisticas_1.xlsx"
  )))
  expect_identical(
    readLines(file.path(extracted, "versions", "png", "001_Boxplot.png")),
    "png"
  )
  expect_identical(
    readLines(file.path(extracted, "versions", "png", "002_Boxplot.png")),
    "png-second"
  )
})

test_that("sanitize replaces forbidden filename characters", {
  expect_equal(sanitize("A/B:C*D?E\"F<G>H|I"), "A_B_C_D_E_F_G_H_I")
})

test_that("sanitize handles blank and NA filename parts", {
  expect_equal(sanitize(NA_character_), "")
  expect_equal(sanitize(c("A/B", ""), fallback = "file"), c("A_B", "file"))
})
