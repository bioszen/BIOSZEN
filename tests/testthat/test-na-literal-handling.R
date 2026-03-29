library(testthat)

test_that("prepare_platemap keeps literal 'NA' strain/media and parameter names", {
  old <- setwd(testthat::test_path("..", "..", "inst", "app"))
  on.exit(setwd(old), add = TRUE)

  source(testthat::test_path("..", "..", "inst", "app", "global.R"))

  datos <- data.frame(
    Well = c("A1", "A2"),
    Strain = c("NA", "WT"),
    Media = c("Mock", "NA"),
    Orden = c(1, 2),
    BiologicalReplicate = c(1, 1),
    TechnicalReplicate = c("A", "A"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  datos[["NA"]] <- c(10, 20)
  datos[["ParamB"]] <- c(1.1, 2.2)

  cfg <- data.frame(
    Parameter = c("NA", "ParamB"),
    Y_Max = c(30, 5),
    Interval = c(5, 1),
    Y_Title = c("NA", "ParamB"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  prep <- prepare_platemap(datos, cfg)

  expect_true("NA" %in% names(prep$datos))
  expect_true("NA" %in% prep$cfg$Parameter)
  expect_true("NA" %in% prep$datos$Strain)
  expect_true("NA" %in% prep$datos$Media)
})

test_that("server does not treat literal 'NA' labels as missing for params/media", {
  server_file <- test_path("..", "..", "inst", "app", "server", "server_main.R")
  expect_true(file.exists(server_file))

  txt <- paste(readLines(server_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_false(grepl("identical\\(toupper\\(p_chr\\), \"NA\"\\)", txt, perl = TRUE))
  expect_false(grepl("nzchar\\(ctrl_chr\\) && toupper\\(ctrl_chr\\) != \"NA\"", txt, perl = TRUE))
  expect_false(grepl("opts <- opts\\[!is\\.na\\(opts\\) & nzchar\\(opts\\) & toupper\\(opts\\) != \"NA\"\\]", txt, perl = TRUE))
  expect_false(grepl("valid_media <- !is\\.na\\(media_chr\\) & nzchar\\(media_chr\\) & toupper\\(media_chr\\) != \"NA\"", txt, perl = TRUE))
  expect_false(grepl("dplyr::filter\\(!is\\.na\\(Media\\), nzchar\\(Media\\), toupper\\(Media\\) != \"NA\"\\)", txt, perl = TRUE))
  expect_false(grepl("dplyr::filter\\(!is\\.na\\(Group\\), nzchar\\(Group\\), toupper\\(Group\\) != \"NA-NA\"\\)", txt, perl = TRUE))
  expect_false(grepl("toupper\\(BiologicalReplicate\\) != \"NA\"", txt, perl = TRUE))
  expect_false(grepl("vals <- vals\\[!is\\.na\\(vals\\) & nzchar\\(vals\\) & toupper\\(vals\\) != \"NA\"\\]", txt, perl = TRUE))
})

test_that("helpers do not treat literal 'NA' labels as missing", {
  helpers_file <- test_path("..", "..", "inst", "app", "helpers.R")
  expect_true(file.exists(helpers_file))

  txt <- paste(readLines(helpers_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_false(grepl("identical\\(toupper\\(ctrl_medium\\), \"NA\"\\)", txt, perl = TRUE))
  expect_false(grepl("toupper\\(Replicate\\) != \"NA\"", txt, perl = TRUE))
  expect_false(grepl("vals <- vals\\[!is\\.na\\(vals\\) & nzchar\\(vals\\) & toupper\\(vals\\) != \"NA\"\\]", txt, perl = TRUE))
  expect_false(grepl("toupper\\(rep_chr\\) != \"NA\"", txt, perl = TRUE))
})
