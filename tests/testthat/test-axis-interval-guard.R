library(testthat)

source(app_test_path("helpers.R"))

test_that("axis_breaks_limited caps excessive tick counts", {
  br <- axis_breaks_limited(max_value = 100, interval = 0.0001, max_ticks = 40L)
  expect_true(is.numeric(br))
  expect_lte(length(br), 41L)
  expect_equal(max(br), 100)
})

test_that("axis_breaks_limited_range caps excessive tick counts", {
  br <- axis_breaks_limited_range(
    min_value = -1,
    max_value = 1,
    interval = 0.00001,
    max_ticks = 60L
  )
  expect_true(is.numeric(br))
  expect_lte(length(br), 61L)
  expect_equal(min(br), -1)
  expect_equal(max(br), 1)
})

test_that("axis interval guard does not overwrite numeric inputs while typing", {
  server_txt <- paste(
    readLines(app_test_path("server", "server_main.R"), warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )

  expect_false(grepl("enforce_axis_break_limit", server_txt, fixed = TRUE))
  expect_false(grepl(
    "updateNumericInput\\(session, break_id",
    server_txt,
    perl = TRUE
  ))
})

test_that("Y-axis limits are not overwritten when switching parameters", {
  server_txt <- paste(
    readLines(app_test_path("server", "server_main.R"), warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )

  expect_match(server_txt, "begin_axis_input_sync\\s*<-\\s*function", perl = TRUE)
  expect_match(
    server_txt,
    "observeEvent\\(\\s*list\\(input\\$ymax, input\\$ybreak\\)",
    perl = TRUE
  )
  expect_false(grepl(
    "list\\(input\\$ymax, input\\$ybreak, input\\$param\\)",
    server_txt,
    perl = TRUE
  ))
  expect_match(server_txt, "tgt <- safe_param\\(\\)", perl = TRUE)
  expect_match(server_txt, "dplyr::filter\\(Parameter == raw_param\\)", perl = TRUE)
  expect_match(server_txt, "paste0\\(raw_param, \"_Norm\"\\)", perl = TRUE)
  expect_match(server_txt, "stored_lims <- ylims\\[\\[tgt\\]\\]", perl = TRUE)
  expect_match(
    server_txt,
    "begin_axis_input_sync\\(\\)[\\s\\S]{0,250}updateNumericInput\\(session, \"ymax\"",
    perl = TRUE
  )
})
