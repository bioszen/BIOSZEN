library(testthat)

test_that("prepare_platemap keeps existing parameter columns even if numeric parsing is empty", {
  old <- setwd(app_test_path())
  on.exit(setwd(old), add = TRUE)

  source(app_test_path( "global.R"))

  datos <- data.frame(
    Well = c("A1", "A2"),
    Strain = c("WT", "WT"),
    Media = c("Mock", "Mock"),
    Orden = c(1, 1),
    BiologicalReplicate = c(1, 2),
    TechnicalReplicate = c(1, 1),
    ParamFormula = c("ND", "ND"),
    ParamNumeric = c("1.2", "2.4"),
    stringsAsFactors = FALSE
  )

  cfg <- data.frame(
    Parameter = c("ParamFormula", "ParamNumeric", "ParamMissing"),
    Y_Max = c(1, 3, 1),
    Interval = c(0.1, 0.5, 0.1),
    Y_Title = c("ParamFormula", "ParamNumeric", "ParamMissing"),
    stringsAsFactors = FALSE
  )

  prep <- prepare_platemap(datos, cfg)

  expect_equal(prep$cfg$Parameter, c("ParamFormula", "ParamNumeric"))
  expect_true("ParamMissing" %in% names(prep$datos))
  expect_true(all(is.na(prep$datos$ParamFormula)))
  expect_true(all(is.finite(prep$datos$ParamNumeric)))
})

test_that("prepare_platemap includes parameters present in Datos even when missing in PlotSettings", {
  old <- setwd(app_test_path())
  on.exit(setwd(old), add = TRUE)

  source(app_test_path( "global.R"))

  datos <- data.frame(
    Well = c("A1", "A2", "A3"),
    Strain = c("WA", "WA", "WA"),
    Media = c("Ctrl", "Ctrl", "Drug"),
    Orden = c(1, 1, 2),
    BiologicalReplicate = c(1, 2, 3),
    TechnicalReplicate = c("A", "A", "A"),
    YBR291C = c(10, NA, 12),
    `μMax` = c(0.9, 1.0, 1.1),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  cfg <- data.frame(
    Parameter = c("CTP1", "μMax"),
    Y_Max = c(500, 0.3),
    Interval = c(50, 0.1),
    Y_Title = c("Expression CTP1", "μMax"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  prep <- prepare_platemap(datos, cfg)

  expect_true("μMax" %in% prep$cfg$Parameter)
  expect_true("YBR291C" %in% prep$cfg$Parameter)
  expect_false("CTP1" %in% prep$cfg$Parameter)
})

test_that("prepare_platemap treats blank replicate as missing and keeps explicit zero", {
  old <- setwd(app_test_path())
  on.exit(setwd(old), add = TRUE)

  source(app_test_path( "global.R"))

  datos <- data.frame(
    Well = c("A1", "A2", "A3", "A4"),
    Strain = c("WA", "WA", "WA", "WA"),
    Media = c("Ctrl", "Ctrl", "Ctrl", "Ctrl"),
    Orden = c(1, 1, 1, 1),
    BiologicalReplicate = c("", "0", NA, "2"),
    TechnicalReplicate = c("A", "A", "A", "A"),
    ParamA = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )

  cfg <- data.frame(
    Parameter = "ParamA",
    Y_Max = 5,
    Interval = 1,
    Y_Title = "ParamA",
    stringsAsFactors = FALSE
  )

  prep <- prepare_platemap(datos, cfg)

  expect_true(is.na(prep$datos$BiologicalReplicate[1]))
  expect_equal(prep$datos$BiologicalReplicate[2], 0)
  expect_true(is.na(prep$datos$BiologicalReplicate[3]))
  expect_equal(prep$datos$BiologicalReplicate[4], 2)
})
