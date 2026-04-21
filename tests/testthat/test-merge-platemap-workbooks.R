library(testthat)

test_that("merge_platemap_workbooks preserves base order and merges parameters correctly", {
  old <- setwd(app_test_path())
  on.exit(setwd(old), add = TRUE)

  source(app_test_path( "global.R"))

  tmp_dir <- tempfile("platemap_merge_")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

  base_file <- file.path(tmp_dir, "base.xlsx")
  add_side_file <- file.path(tmp_dir, "add_side.xlsx")
  add_rep_file <- file.path(tmp_dir, "add_rep.xlsx")

  base_datos <- data.frame(
    Well = c("A1", "A2", "A3"),
    Strain = c("S1", "S1", "S2"),
    Media = c("M1", "M1", "M2"),
    Orden = c(1, 1, 2),
    Replicate = c(1, 2, 1),
    BiologicalReplicate = c(1, 2, 1),
    TechnicalReplicate = c("A", "A", "A"),
    ParamA = c(10, 11, 20),
    stringsAsFactors = FALSE
  )
  base_plot <- data.frame(
    Parameter = "ParamA",
    Y_Max = 30,
    Interval = 5,
    Y_Title = "ParamA",
    stringsAsFactors = FALSE
  )
  writexl::write_xlsx(list(Datos = base_datos, PlotSettings = base_plot), base_file)

  add_side_datos <- data.frame(
    Well = c("A1", "A2"),
    Strain = c("S1", "S3"),
    Media = c("M1", "M3"),
    Orden = c(5, 1),
    Replicate = c(1, 1),
    BiologicalReplicate = c(1, 1),
    TechnicalReplicate = c("A", "A"),
    ParamB = c(5, 30),
    stringsAsFactors = FALSE
  )
  add_side_plot <- data.frame(
    Parameter = "ParamB",
    Y_Max = 40,
    Interval = 8,
    Y_Title = "ParamB",
    stringsAsFactors = FALSE
  )
  writexl::write_xlsx(
    list(Datos = add_side_datos, PlotSettings = add_side_plot),
    add_side_file
  )

  add_rep_datos <- data.frame(
    Well = "A1",
    Strain = "S1",
    Media = "M1",
    Orden = 1,
    Replicate = 1,
    BiologicalReplicate = 1,
    TechnicalReplicate = "A",
    ParamA = 99,
    stringsAsFactors = FALSE
  )
  add_rep_plot <- data.frame(
    Parameter = "ParamA",
    Y_Max = 120,
    Interval = 20,
    Y_Title = "ParamA from additional",
    stringsAsFactors = FALSE
  )
  writexl::write_xlsx(
    list(Datos = add_rep_datos, PlotSettings = add_rep_plot),
    add_rep_file
  )

  res <- merge_platemap_workbooks(
    base_file = base_file,
    additional_files = c(add_side_file, add_rep_file)
  )

  datos <- res$Datos
  cfg <- res$PlotSettings

  s1 <- datos[datos$Strain == "S1" & datos$Media == "M1", , drop = FALSE]
  expect_equal(nrow(s1), 3)
  expect_equal(s1$ParamA, c(10, 11, 99))
  expect_equal(s1$ParamB, c(5, NA, NA))
  expect_equal(s1$Orden, c(1L, 1L, 1L))
  expect_equal(s1$Replicate, c(1L, 2L, 3L))
  expect_equal(s1$BiologicalReplicate, c(1L, 2L, 3L))
  expect_true(all(s1$TechnicalReplicate == "A"))

  s2 <- datos[datos$Strain == "S2" & datos$Media == "M2", , drop = FALSE]
  expect_equal(nrow(s2), 1)
  expect_equal(s2$ParamA, 20)
  expect_true(is.na(s2$ParamB))
  expect_equal(s2$Orden, 2L)

  s3 <- datos[datos$Strain == "S3" & datos$Media == "M3", , drop = FALSE]
  expect_equal(nrow(s3), 1)
  expect_true(is.na(s3$ParamA))
  expect_equal(s3$ParamB, 30)
  expect_equal(s3$Orden, 3L)

  expect_equal(datos$Well, merge_make_plate_wells(nrow(datos)))
  expect_true(all(c("ParamA", "ParamB") %in% cfg$Parameter))

  expect_equal(res$Info$base_file, basename(base_file))
  expect_equal(length(res$Info$extra_files), 2)
  expect_equal(res$Info$overlap_groups, 1)
  expect_equal(res$Info$groups_total, 3)
})

test_that("merge_platemap_workbooks preserves literal 'NA' labels in Strain/Media/Parameter", {
  old <- setwd(app_test_path())
  on.exit(setwd(old), add = TRUE)

  source(app_test_path( "global.R"))

  tmp_dir <- tempfile("platemap_merge_na_literal_")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

  base_file <- file.path(tmp_dir, "base_na.xlsx")
  add_file <- file.path(tmp_dir, "add_na.xlsx")

  base_datos <- data.frame(
    Well = c("A1", "A2"),
    Strain = c("NA", "WT"),
    Media = c("NA", "Mock"),
    Orden = c(1, 2),
    Replicate = c(1, 1),
    BiologicalReplicate = c(1, 1),
    TechnicalReplicate = c("A", "A"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  base_datos[["NA"]] <- c(10, 20)

  base_plot <- data.frame(
    Parameter = "NA",
    Y_Max = 30,
    Interval = 6,
    Y_Title = "NA",
    stringsAsFactors = FALSE
  )
  writexl::write_xlsx(list(Datos = base_datos, PlotSettings = base_plot), base_file)

  add_datos <- data.frame(
    Well = c("A1", "A2"),
    Strain = c("NA", "WT"),
    Media = c("NA", "Mock"),
    Orden = c(1, 2),
    Replicate = c(1, 1),
    BiologicalReplicate = c(1, 1),
    TechnicalReplicate = c("A", "A"),
    ParamB = c(99, 88),
    stringsAsFactors = FALSE
  )

  add_plot <- data.frame(
    Parameter = "ParamB",
    Y_Max = 120,
    Interval = 24,
    Y_Title = "ParamB",
    stringsAsFactors = FALSE
  )
  writexl::write_xlsx(list(Datos = add_datos, PlotSettings = add_plot), add_file)

  res <- merge_platemap_workbooks(
    base_file = base_file,
    additional_files = add_file
  )

  datos <- res$Datos
  cfg <- res$PlotSettings

  expect_true("NA" %in% names(datos))
  expect_true("NA" %in% cfg$Parameter)
  expect_equal(sum(is.na(datos$Strain)), 0)
  expect_equal(sum(is.na(datos$Media)), 0)

  na_group <- datos[datos$Strain == "NA" & datos$Media == "NA", , drop = FALSE]
  expect_equal(nrow(na_group), 1)
  expect_equal(na_group[["NA"]], 10)
  expect_equal(na_group$ParamB, 99)
})

test_that("merge_platemap_workbooks appends repeated parameters as new biological replicates while preserving technical replicates", {
  old <- setwd(app_test_path())
  on.exit(setwd(old), add = TRUE)

  source(app_test_path( "global.R"))

  tmp_dir <- tempfile("platemap_merge_reps_")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

  base_file <- file.path(tmp_dir, "base.xlsx")
  add_file <- file.path(tmp_dir, "add.xlsx")

  base_datos <- data.frame(
    Well = c("A1", "A2"),
    Strain = c("WT", "WT"),
    Media = c("Mock", "Mock"),
    Orden = c(1, 1),
    BiologicalReplicate = c(1, 1),
    TechnicalReplicate = c("A", "B"),
    Replicate = c(1, 1),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  base_datos[["\u00B5Max"]] <- c(0.30, 0.31)

  add_datos <- data.frame(
    Well = c("A1", "A2", "A3"),
    Strain = c("WT", "WT", "WT"),
    Media = c("Mock", "Mock", "Mock"),
    Orden = c(1, 1, 1),
    BiologicalReplicate = c(1, 1, 1),
    TechnicalReplicate = c("A", "B", "C"),
    Replicate = c(1, 1, 1),
    Zymosterol = c(8.1, 8.0, 7.9),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  add_datos[["\u00B5Max"]] <- c(0.29, 0.28, 0.27)

  writexl::write_xlsx(list(Datos = base_datos), base_file)
  writexl::write_xlsx(list(Datos = add_datos), add_file)

  res <- merge_platemap_workbooks(
    base_file = base_file,
    additional_files = add_file
  )

  datos <- res$Datos
  group <- datos[datos$Strain == "WT" & datos$Media == "Mock", , drop = FALSE]

  expect_equal(nrow(group), 5)
  expect_equal(group$BiologicalReplicate, c(1L, 1L, 2L, 2L, 2L))
  expect_equal(group$Replicate, c(1L, 1L, 2L, 2L, 2L))
  expect_equal(group$TechnicalReplicate, c("A", "B", "A", "B", "C"))
  expect_equal(group[["\u00B5Max"]], c(0.30, 0.31, 0.29, 0.28, 0.27))
  expect_equal(group$Zymosterol, c(NA, NA, 8.1, 8.0, 7.9))
  expect_equal(group$Well, c("A1", "A2", "A3", "A4", "A5"))
})

test_that("merge_platemap_workbooks keeps new-only parameters in parallel and expands rows only to max replicate count", {
  old <- setwd(app_test_path())
  on.exit(setwd(old), add = TRUE)

  source(app_test_path( "global.R"))

  tmp_dir <- tempfile("platemap_merge_parallel_")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

  base_file <- file.path(tmp_dir, "base_parallel.xlsx")
  add_file <- file.path(tmp_dir, "add_parallel.xlsx")

  base_datos <- data.frame(
    Well = c("A1", "A2"),
    Strain = c("WT", "WT"),
    Media = c("Mock", "Mock"),
    Orden = c(1, 1),
    BiologicalReplicate = c(1, 1),
    TechnicalReplicate = c("A", "B"),
    Replicate = c(1, 1),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  base_datos[["\u00B5Max"]] <- c(0.30, 0.31)

  add_datos <- data.frame(
    Well = c("A1", "A2", "A3"),
    Strain = c("WT", "WT", "WT"),
    Media = c("Mock", "Mock", "Mock"),
    Orden = c(1, 1, 1),
    BiologicalReplicate = c(1, 1, 1),
    TechnicalReplicate = c("A", "B", "C"),
    Replicate = c(1, 1, 1),
    Zymosterol = c(8.1, 8.0, 7.9),
    stringsAsFactors = FALSE
  )

  writexl::write_xlsx(list(Datos = base_datos), base_file)
  writexl::write_xlsx(list(Datos = add_datos), add_file)

  res <- merge_platemap_workbooks(
    base_file = base_file,
    additional_files = add_file
  )

  datos <- res$Datos
  group <- datos[datos$Strain == "WT" & datos$Media == "Mock", , drop = FALSE]

  expect_equal(nrow(group), 3)
  expect_equal(group$BiologicalReplicate, c(1L, 1L, 1L))
  expect_equal(group$Replicate, c(1L, 1L, 1L))
  expect_equal(group$TechnicalReplicate, c("A", "B", "C"))
  expect_equal(group[["\u00B5Max"]], c(0.30, 0.31, NA))
  expect_equal(group$Zymosterol, c(8.1, 8.0, 7.9))
  expect_equal(group$Well, c("A1", "A2", "A3"))
})

test_that("repeated parameters always remap wells for curves using appended biological replicates", {
  old <- setwd(app_test_path())
  on.exit(setwd(old), add = TRUE)

  source(app_test_path( "global.R"))

  tmp_dir <- tempfile("platemap_curve_merge_repeated_")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

  base_plate <- file.path(tmp_dir, "base_plate.xlsx")
  add_plate <- file.path(tmp_dir, "add_plate.xlsx")
  base_curve <- file.path(tmp_dir, "base_curve.xlsx")
  add_curve <- file.path(tmp_dir, "add_curve.xlsx")

  base_datos <- data.frame(
    Well = c("A1", "A2"),
    Strain = c("WT", "WT"),
    Media = c("Mock", "Mock"),
    Orden = c(1, 1),
    BiologicalReplicate = c(1, 1),
    TechnicalReplicate = c("A", "B"),
    Replicate = c(1, 1),
    uMax = c(0.30, 0.31),
    stringsAsFactors = FALSE
  )
  add_datos <- data.frame(
    Well = c("A1", "A2", "A3"),
    Strain = c("WT", "WT", "WT"),
    Media = c("Mock", "Mock", "Mock"),
    Orden = c(1, 1, 1),
    BiologicalReplicate = c(1, 1, 1),
    TechnicalReplicate = c("A", "B", "C"),
    Replicate = c(1, 1, 1),
    uMax = c(0.29, 0.28, 0.27),
    Zymosterol = c(8.1, 8.0, 7.9),
    stringsAsFactors = FALSE
  )

  writexl::write_xlsx(list(Datos = base_datos), base_plate)
  writexl::write_xlsx(list(Datos = add_datos), add_plate)

  merged <- merge_platemap_workbooks(
    base_file = base_plate,
    additional_files = add_plate
  )

  expect_equal(merged$Datos$Well, c("A1", "A2", "A3", "A4", "A5"))
  expect_equal(
    merged$WellMap[, c("SourceIndex", "SourceWell", "TargetWell")],
    data.frame(
      SourceIndex = c(1L, 1L, 2L, 2L, 2L),
      SourceWell = c("A1", "A2", "A1", "A2", "A3"),
      TargetWell = c("A1", "A2", "A3", "A4", "A5"),
      stringsAsFactors = FALSE
    )
  )

  base_curve_sheet1 <- data.frame(Time = c(0, 1), A1 = c(1, 2), A2 = c(3, 4), check.names = FALSE)
  add_curve_sheet1 <- data.frame(Time = c(0, 1), A1 = c(5, 6), A2 = c(7, 8), A3 = c(9, 10), check.names = FALSE)
  cfg <- data.frame(
    X_Max = 1, Interval_X = 1, Y_Max = 10, Interval_Y = 2,
    X_Title = "Time", Y_Title = "Signal",
    stringsAsFactors = FALSE
  )
  writexl::write_xlsx(list(Sheet1 = base_curve_sheet1, Sheet2 = cfg), base_curve)
  writexl::write_xlsx(list(Sheet1 = add_curve_sheet1, Sheet2 = cfg), add_curve)

  curve_merged <- merge_curve_workbooks_by_well_map(
    base_file = base_curve,
    additional_files = add_curve,
    well_map = merged$WellMap
  )

  expect_equal(names(curve_merged$Sheet1), c("Time", "A1", "A2", "A3", "A4", "A5"))
  expect_equal(curve_merged$Sheet1$A1, c(1, 2))
  expect_equal(curve_merged$Sheet1$A2, c(3, 4))
  expect_equal(curve_merged$Sheet1$A3, c(5, 6))
  expect_equal(curve_merged$Sheet1$A4, c(7, 8))
  expect_equal(curve_merged$Sheet1$A5, c(9, 10))
})
