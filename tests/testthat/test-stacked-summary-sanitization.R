library(testthat)

source(app_test_path("helpers.R"))

test_that("sanitize_stack_summary replaces non-finite stacked stats", {
  df <- data.frame(
    Mean = c(1, NaN, Inf, -Inf, NA),
    SD = c(0.5, NaN, Inf, -1, NA),
    stringsAsFactors = FALSE
  )

  out <- sanitize_stack_summary(df)

  expect_equal(out$Mean, c(1, 0, 0, 0, 0))
  expect_equal(out$SD, c(0.5, 0, 0, 0, 0))
})

test_that("stacked plotting pipelines call sanitize_stack_summary", {
  gg_file <- app_test_path("graficos", "graficos_apilados.R")
  server_file <- app_test_path("server", "server_main.R")

  gg_txt <- paste(readLines(gg_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  server_txt <- paste(readLines(server_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_true(grepl("df_long <- sanitize_stack_summary\\(df_long\\)", gg_txt, perl = TRUE))
  expect_true(grepl("df_long <- sanitize_stack_summary\\(df_long\\)", server_txt, perl = TRUE))
})
