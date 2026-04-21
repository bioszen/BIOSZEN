library(testthat)

root <- normalizePath(test_path("..", ".."))
load_csv_sources <- function() {
  app_dir <- app_test_path()
  old <- setwd(app_dir)
  on.exit(setwd(old), add = TRUE)
  source(file.path(app_dir, "global.R"))
}
load_csv_sources()

test_that("read_csv_tmp reads semicolon-delimited CSV uploads", {
  csv_file <- tempfile("bioszen_csv_", fileext = ".csv")
  writeLines(
    c(
      "Cepa;Condicion;Parametro;Promedio",
      "WT;Control;ODmax;10,4",
      "WT;Drug;ODmax;12,1"
    ),
    csv_file,
    useBytes = TRUE
  )
  on.exit(unlink(csv_file), add = TRUE)

  df <- read_csv_tmp(csv_file)
  expect_true(all(c("Cepa", "Condicion", "Parametro", "Promedio") %in% names(df)))
  expect_equal(nrow(df), 2)
})

test_that("prepare_platemap uses CSV defaults with blank titles and integer intervals", {
  datos <- data.frame(
    Strain = c("WT", "WT", "KO"),
    Media = c("Control", "Drug", "Control"),
    ParamA = c(10.2, 12.6, 9.4),
    ParamB = c(101.1, 88.2, 95.4),
    stringsAsFactors = FALSE
  )

  prep <- prepare_platemap(datos, cfg = NULL, defaults_profile = "csv")
  cfg <- prep$cfg

  expect_true(all(cfg$Y_Title == ""))

  row_param_a <- cfg[cfg$Parameter == "ParamA", , drop = FALSE]
  expect_equal(row_param_a$Y_Max[[1]], 12.6, tolerance = 1e-8)
  expect_equal(row_param_a$Interval[[1]], round(12.6 / 5), tolerance = 1e-8)
  expect_equal(row_param_a$Interval[[1]], as.numeric(as.integer(row_param_a$Interval[[1]])))
})

test_that("CSV summary tables with mean values are converted to platemap format", {
  raw <- data.frame(
    Cepa = c("WT", "WT", "KO"),
    Condicion = c("Control", "Drug", "Control"),
    Parametro = c("ODmax", "ODmax", "ODmax"),
    Promedio = c("10,5", "12,0", "9,8"),
    Desviacion = c("0,5", "0,4", "0,3"),
    Orden = c(1, 2, 3),
    stringsAsFactors = FALSE
  )

  out <- build_platemap_from_mean_sd_data(raw, default_profile = "csv")
  expect_false(is.null(out))
  expect_true(is.data.frame(out$Datos))
  expect_true(is.data.frame(out$PlotSettings))
  expect_true("ODmax" %in% out$PlotSettings$Parameter)
  expect_true(all(out$PlotSettings$Y_Title == ""))
})
