test_that("correlation plot guards insufficient data without validate errors", {
  server_file <- test_path("..", "..", "inst", "app", "server", "server_main.R")
  expect_true(file.exists(server_file))

  txt <- paste(readLines(server_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_true(grepl('if \\(tipo == "Correlacion"\\)', txt, perl = TRUE))
  expect_true(grepl("corr_placeholder <- function\\(msg\\)", txt, perl = TRUE))
  expect_true(grepl("if \\(nrow\\(df\\) < 3\\) \\{\\s*return\\(corr_placeholder\\(", txt, perl = TRUE))
  expect_false(grepl("validate\\(need\\(\\s*nrow\\(df\\) >= 3", txt, perl = TRUE))
})

test_that("plot_base suppresses noisy notifications for transient expected states", {
  server_file <- test_path("..", "..", "inst", "app", "server", "server_main.R")
  txt <- paste(readLines(server_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_true(grepl('inherits\\(e, "shiny\\.silent\\.error"\\)', txt, perl = TRUE))
  expect_true(grepl("is_expected_transient", txt, fixed = TRUE))
  expect_true(grepl("if \\(!is_expected_transient\\)", txt, perl = TRUE))
})
