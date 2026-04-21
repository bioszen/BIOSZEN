library(testthat)

root <- app_test_root()
app_dir <- app_test_path()
www_dir <- file.path(app_dir, "www")

load_app_sources_for_generation <- function() {
  source(file.path(app_dir, "helpers.R"))
  source(file.path(app_dir, "global.R"))
}

build_generation_fixture <- function() {
  datos <- expand.grid(
    Strain = c("S1", "S2"),
    Media = c("M1", "M2"),
    BiologicalReplicate = c("R1", "R2", "R3"),
    TechnicalReplicate = c("T1", "T2"),
    stringsAsFactors = FALSE
  )
  datos$Orden <- ifelse(datos$Media == "M1", 1L, 2L)
  datos$Well <- paste0("W", seq_len(nrow(datos)))

  rep_ix <- as.numeric(factor(datos$BiologicalReplicate, levels = c("R1", "R2", "R3")))
  media_ix <- as.numeric(factor(datos$Media, levels = c("M1", "M2")))
  strain_ix <- as.numeric(factor(datos$Strain, levels = c("S1", "S2")))
  tech_ix <- as.numeric(factor(datos$TechnicalReplicate, levels = c("T1", "T2")))

  datos$uMax <- 0.20 + 0.03 * strain_ix + 0.02 * media_ix + 0.01 * rep_ix + 0.005 * tech_ix
  datos$ODmax <- 0.70 + 0.15 * strain_ix + 0.08 * media_ix + 0.03 * rep_ix + 0.01 * tech_ix

  time_points <- c(0, 30, 60, 90)
  curves <- data.frame(Time = time_points)
  for (i in seq_len(nrow(datos))) {
    well <- datos$Well[[i]]
    curves[[well]] <- 0.05 * i + 0.002 * time_points
  }

  list(
    datos = datos,
    params = c("uMax", "ODmax"),
    curves = curves
  )
}

read_sheet_values <- function(path, sheet) {
  tbl <- readxl::read_excel(path, sheet = sheet, col_names = FALSE)
  as.data.frame(tbl, stringsAsFactors = FALSE)
}

test_that("manual PDFs are present and have valid PDF headers", {
  manuals <- c("MANUAL_EN.pdf", "MANUAL_ES.pdf")
  for (name in manuals) {
    path <- file.path(www_dir, name)
    if (!file.exists(path)) {
      fail(sprintf(
        "Missing manual file: %s. Possible cause: file was moved/removed from inst/app/www.",
        path
      ))
    }
    size <- file.info(path)$size
    if (is.na(size) || size <= 1024) {
      fail(sprintf(
        "Manual file is too small or unreadable: %s (size=%s). Possible cause: incomplete/corrupted PDF.",
        path,
        as.character(size)
      ))
    }

    header_raw <- readBin(path, what = "raw", n = 5)
    header <- rawToChar(header_raw)
    if (!identical(header, "%PDF-")) {
      fail(sprintf(
        "Invalid PDF header in %s (header='%s'). Possible cause: wrong file type or corrupted PDF.",
        path,
        header
      ))
    }
  }
  expect_equal(length(manuals), 2L)
})

test_that("reference workbook assets are present and readable", {
  skip_if_not_installed("readxl")

  ref_dir <- file.path(www_dir, "reference_files")
  expect_true(dir.exists(ref_dir))

  assets <- c(
    "Ejemplo_curvas.xlsx",
    "Ejemplo_input_summary_mean_sd.xlsx",
    "Ejemplo_parametros_agrupados.xlsx",
    "Ejemplo_platemap_parametros.xlsx"
  )

  for (name in assets) {
    path <- file.path(ref_dir, name)
    if (!file.exists(path)) {
      fail(sprintf(
        "Missing reference workbook: %s. Possible cause: file was moved or not packaged into www/reference_files.",
        path
      ))
    }
    size <- file.info(path)$size
    if (is.na(size) || size <= 0) {
      fail(sprintf(
        "Reference workbook has zero/unknown size: %s (size=%s). Possible cause: empty or corrupted file.",
        path,
        as.character(size)
      ))
    }

    sheets <- tryCatch(readxl::excel_sheets(path), error = function(e) e)
    if (inherits(sheets, "error")) {
      fail(sprintf(
        "Could not list sheets in %s. Possible cause: invalid XLSX format. Error: %s",
        path,
        conditionMessage(sheets)
      ))
    }
    if (!length(sheets)) {
      fail(sprintf(
        "Workbook has no sheets: %s. Possible cause: malformed XLSX or bad export.",
        path
      ))
    }

    first_sheet <- sheets[[1]]
    read_try <- tryCatch(
      readxl::read_excel(path, sheet = first_sheet, n_max = 5),
      error = function(e) e
    )
    if (inherits(read_try, "error")) {
      fail(sprintf(
        "Could not read first sheet '%s' in %s. Possible cause: corrupted workbook or invalid sheet content. Error: %s",
        first_sheet,
        path,
        conditionMessage(read_try)
      ))
    }
    expect_s3_class(read_try, "tbl_df")
  }
})

test_that("grouped workbook generation is deterministic for fixed inputs", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("readxl")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")

  old <- setwd(app_dir)
  on.exit(setwd(old), add = TRUE)
  load_app_sources_for_generation()

  fixture <- build_generation_fixture()
  out_a <- tempfile("bioszen_grouped_a_", fileext = ".xlsx")
  out_b <- tempfile("bioszen_grouped_b_", fileext = ".xlsx")
  on.exit(unlink(c(out_a, out_b)), add = TRUE)

  build_once <- function(path) {
    wb <- generate_summary_wb(fixture$datos, fixture$params)
    wb <- add_curves_by_group_sheet(
      wb = wb,
      curve_wide = fixture$curves,
      meta_df = fixture$datos
    )
    openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  }

  expect_no_error(build_once(out_a))
  expect_no_error(build_once(out_b))
  expect_true(file.exists(out_a))
  expect_true(file.exists(out_b))
  expect_gt(file.info(out_a)$size, 0)
  expect_gt(file.info(out_b)$size, 0)

  sheets_a <- openxlsx::getSheetNames(out_a)
  sheets_b <- openxlsx::getSheetNames(out_b)
  expect_equal(sheets_a, sheets_b)

  for (sheet in sheets_a) {
    expect_equal(
      read_sheet_values(out_a, sheet),
      read_sheet_values(out_b, sheet),
      info = sheet
    )
  }
})
