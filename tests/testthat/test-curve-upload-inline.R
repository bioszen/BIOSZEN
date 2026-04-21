library(testthat)

root <- app_test_root()
app_dir <- app_test_path()

load_app_sources <- function() {
  source(file.path(app_dir, "helpers.R"))
  source(file.path(app_dir, "global.R"))
}

test_that("data upload alternatives are accepted", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")

  old <- setwd(app_dir)
  on.exit(setwd(old), add = TRUE)
  load_app_sources()

  standard_path <- tempfile("bioszen_data_standard_", fileext = ".xlsx")
  grouped_path <- tempfile("bioszen_data_grouped_", fileext = ".xlsx")
  summary_path <- tempfile("bioszen_data_summary_", fileext = ".xlsx")
  on.exit(unlink(c(standard_path, grouped_path, summary_path)), add = TRUE)

  datos <- data.frame(
    Well = "A1",
    Strain = "S1",
    Media = "M1",
    Orden = 1,
    BiologicalReplicate = 1,
    TechnicalReplicate = 1,
    OD620 = 0.20
  )
  plot_settings <- data.frame(
    Parameter = "OD620",
    Y_Max = 1.0,
    Interval = 0.2,
    Y_Title = "OD620"
  )
  writexl::write_xlsx(list(Datos = datos, PlotSettings = plot_settings), path = standard_path)

  std_prep <- prepare_platemap(
    read_excel_tmp(standard_path, sheet = "Datos"),
    read_excel_tmp(standard_path, sheet = "PlotSettings")
  )
  expect_true(is.data.frame(std_prep$datos))
  expect_true(is.data.frame(std_prep$cfg))
  expect_true(nrow(std_prep$datos) > 0)
  expect_true("OD620" %in% names(std_prep$datos))

  grouped_param <- data.frame(
    C1 = c("Strain", "S1", "S1"),
    C2 = c("RepBiol", 1, 2),
    C3 = c("M1", 0.10, 0.12),
    C4 = c("M2", 0.08, 0.09)
  )
  writexl::write_xlsx(list(OD620 = grouped_param), path = grouped_path)

  grouped_conv <- suppressWarnings(build_platemap_from_summary(grouped_path))
  expect_false(is.null(grouped_conv))
  expect_true(all(c("Datos", "PlotSettings") %in% names(grouped_conv)))
  expect_true(nrow(grouped_conv$Datos) > 0)

  params_summary <- data.frame(
    Strain = c("S1", "S1"),
    Media = c("M1", "M1"),
    Parameter = c("OD620", "Lag"),
    Mean = c(0.22, 15),
    SD = c(0.02, 1),
    N = c(3, 3)
  )
  writexl::write_xlsx(list(Parameters_Summary = params_summary), path = summary_path)

  summary_conv <- build_platemap_from_mean_sd(summary_path)
  expect_false(is.null(summary_conv))
  expect_true(all(c("Datos", "PlotSettings", "SummaryMode") %in% names(summary_conv)))
  expect_true(isTRUE(summary_conv$SummaryMode))
})

test_that("load_curve_workbook parses standard curves workbook and builds defaults", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")

  old <- setwd(app_dir)
  on.exit(setwd(old), add = TRUE)
  load_app_sources()

  path <- tempfile("bioszen_curve_standard_", fileext = ".xlsx")
  on.exit(unlink(path), add = TRUE)

  sheet1 <- data.frame(
    Time = c(0, 60, 120),
    A1 = c(0.05, 0.11, 0.19),
    A2 = c(0.04, 0.10, 0.18)
  )
  writexl::write_xlsx(list(Sheet1 = sheet1), path = path)

  parsed <- load_curve_workbook(path)

  expect_true(isTRUE(parsed$ok))
  expect_equal(parsed$reason, "ok")
  expect_false(isTRUE(parsed$SummaryMode))
  expect_true("Time" %in% names(parsed$Sheet1))
  expect_true(all(c("X_Max", "Interval_X", "Y_Max", "Interval_Y") %in% names(parsed$Sheet2)))
})

test_that("load_curve_workbook parses wide curve CSV input", {
  old <- setwd(app_dir)
  on.exit(setwd(old), add = TRUE)
  load_app_sources()

  path <- tempfile("bioszen_curve_wide_", fileext = ".csv")
  on.exit(unlink(path), add = TRUE)

  sheet1 <- data.frame(
    Time = c(0, 30, 60, 90),
    A1 = c(0.03, 0.08, 0.14, 0.19),
    A2 = c(0.02, 0.07, 0.13, 0.18)
  )
  utils::write.csv(sheet1, file = path, row.names = FALSE)

  parsed <- load_curve_workbook(path, file_name = "curves.csv")

  expect_true(isTRUE(parsed$ok))
  expect_false(isTRUE(parsed$SummaryMode))
  expect_true(all(c("Time", "A1", "A2") %in% names(parsed$Sheet1)))
  expect_true(all(c("X_Max", "Interval_X", "Y_Max", "Interval_Y") %in% names(parsed$Sheet2)))
  expect_equal(parsed$Sheet2$X_Max[1], 90, tolerance = 1e-8)
  expect_equal(parsed$Sheet2$Interval_X[1], 22.5, tolerance = 1e-8)
  expect_equal(parsed$Sheet2$X_Title[1], "")
  expect_equal(parsed$Sheet2$Y_Title[1], "")
})

test_that("load_curve_workbook parses summary-style curve CSV input", {
  old <- setwd(app_dir)
  on.exit(setwd(old), add = TRUE)
  load_app_sources()

  path <- tempfile("bioszen_curve_summary_", fileext = ".csv")
  on.exit(unlink(path), add = TRUE)

  summary_csv <- data.frame(
    Time = c(0, 60, 120),
    Strain = c("S1", "S1", "S1"),
    Media = c("M1", "M1", "M1"),
    Mean = c(0.04, 0.11, 0.18),
    SD = c(0.01, 0.01, 0.02),
    N = c(3, 3, 3),
    Orden = c(1, 1, 1)
  )
  utils::write.csv(summary_csv, file = path, row.names = FALSE)

  parsed <- load_curve_workbook(path, file_name = "curves_summary.csv")

  expect_true(isTRUE(parsed$ok))
  expect_true(isTRUE(parsed$SummaryMode))
  expect_s3_class(parsed$Meta, "data.frame")
  expect_true(is.data.frame(parsed$Summary))
  expect_true(nrow(parsed$Summary) > 0)
  expect_equal(parsed$Sheet2$X_Max[1], 120, tolerance = 1e-8)
  expect_equal(parsed$Sheet2$Interval_X[1], 30, tolerance = 1e-8)
  expect_equal(parsed$Sheet2$Y_Max[1], 0.2, tolerance = 1e-8)
  expect_equal(parsed$Sheet2$Interval_Y[1], 0.05, tolerance = 1e-8)
  expect_equal(parsed$Sheet2$X_Title[1], "")
  expect_equal(parsed$Sheet2$Y_Title[1], "")
})

test_that("summary data workbook can carry embedded curves in same file", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")

  old <- setwd(app_dir)
  on.exit(setwd(old), add = TRUE)
  load_app_sources()

  path <- tempfile("bioszen_summary_with_curves_", fileext = ".xlsx")
  on.exit(unlink(path), add = TRUE)

  params_summary <- data.frame(
    Strain = c("S1", "S1"),
    Media = c("M1", "M1"),
    Parameter = c("OD620", "Lag"),
    Mean = c(0.23, 15.0),
    SD = c(0.03, 1.2),
    N = c(3, 3)
  )

  curves_summary <- data.frame(
    Time = c(0, 60, 120),
    Strain = c("S1", "S1", "S1"),
    Media = c("M1", "M1", "M1"),
    Mean = c(0.05, 0.11, 0.20),
    SD = c(0.01, 0.01, 0.02),
    N = c(3, 3, 3),
    Orden = c(1, 1, 1)
  )

  writexl::write_xlsx(
    list(
      Parameters_Summary = params_summary,
      Curves_Summary = curves_summary
    ),
    path = path
  )

  platemap <- build_platemap_from_mean_sd(path)
  parsed <- load_curve_workbook(path)

  expect_false(is.null(platemap))
  expect_true(isTRUE(parsed$ok))
  expect_true(isTRUE(parsed$SummaryMode))
  expect_s3_class(parsed$Meta, "data.frame")
  expect_true(nrow(parsed$Summary) > 0)
})

test_that("grouped data workbook can carry embedded curves in same file", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")

  old <- setwd(app_dir)
  on.exit(setwd(old), add = TRUE)
  load_app_sources()

  path <- tempfile("bioszen_grouped_with_curves_", fileext = ".xlsx")
  on.exit(unlink(path), add = TRUE)

  grouped_param <- data.frame(
    C1 = c("Strain", "S1", "S1"),
    C2 = c("RepBiol", 1, 2),
    C3 = c("M1", 0.10, 0.13),
    C4 = c("M2", 0.08, 0.09)
  )

  curves_summary <- data.frame(
    Time = c(0, 60, 120),
    Strain = c("S1", "S1", "S1"),
    Media = c("M1", "M1", "M1"),
    Mean = c(0.04, 0.10, 0.18),
    SD = c(0.01, 0.01, 0.02),
    N = c(2, 2, 2),
    Orden = c(1, 1, 1)
  )

  writexl::write_xlsx(
    list(
      OD620 = grouped_param,
      Curves_Summary = curves_summary
    ),
    path = path
  )

  platemap <- suppressWarnings(build_platemap_from_summary(path))
  parsed <- load_curve_workbook(path)

  expect_false(is.null(platemap))
  expect_true(isTRUE(parsed$ok))
  expect_true(isTRUE(parsed$SummaryMode))
})

test_that("load_curve_workbook reports missing and invalid embedded curves", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")

  old <- setwd(app_dir)
  on.exit(setwd(old), add = TRUE)
  load_app_sources()

  missing_path <- tempfile("bioszen_curve_missing_", fileext = ".xlsx")
  invalid_path <- tempfile("bioszen_curve_invalid_", fileext = ".xlsx")
  on.exit(unlink(c(missing_path, invalid_path)), add = TRUE)

  params_summary <- data.frame(
    Strain = "S1",
    Media = "M1",
    Parameter = "OD620",
    Mean = 0.20,
    SD = 0.03,
    N = 3
  )

  invalid_curves <- data.frame(
    Time = c(0, 60),
    Strain = c("S1", "S1"),
    Media = c("M1", "M1")
  )

  writexl::write_xlsx(list(Parameters_Summary = params_summary), path = missing_path)
  writexl::write_xlsx(list(Curves_Summary = invalid_curves), path = invalid_path)

  parsed_missing <- load_curve_workbook(missing_path)
  parsed_invalid <- load_curve_workbook(invalid_path)

  expect_false(isTRUE(parsed_missing$ok))
  expect_equal(parsed_missing$reason, "not_found")

  expect_false(isTRUE(parsed_invalid$ok))
  expect_equal(parsed_invalid$reason, "invalid")
})
