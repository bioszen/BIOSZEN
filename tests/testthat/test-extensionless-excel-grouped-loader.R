library(testthat)

app_dir <- app_test_path()
old_wd <- setwd(app_dir)
on.exit(setwd(old_wd), add = TRUE)

source(app_test_path("helpers.R"))
source(app_test_path("global.R"))

test_that("grouped workbook parsing works from extensionless temp uploads", {
  src <- app_test_path("www", "reference_files", "Ejemplo_parametros_agrupados.xlsx")
  expect_true(file.exists(src))

  tmp_no_ext <- tempfile("grouped_upload_")
  on.exit(unlink(tmp_no_ext), add = TRUE)
  ok_copy <- file.copy(src, tmp_no_ext, overwrite = TRUE)
  expect_true(ok_copy)

  expect_no_error({
    conv_summary <- build_platemap_from_mean_sd(tmp_no_ext)
    expect_null(conv_summary)
  })

  conv_grouped <- NULL
  expect_no_warning(expect_no_error({
    conv_grouped <- build_platemap_from_summary(tmp_no_ext)
  }))
  expect_false(is.null(conv_grouped))
  expect_true(is.data.frame(conv_grouped$Datos))
  expect_true(is.data.frame(conv_grouped$PlotSettings))
  expect_gt(nrow(conv_grouped$Datos), 0)
  expect_gt(nrow(conv_grouped$PlotSettings), 0)
  expect_true(any(as.character(conv_grouped$PlotSettings$Parameter) == "Parametro_1"))
})
