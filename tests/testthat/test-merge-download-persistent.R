test_that("UI includes persistent merged-plate download slot", {
  ui_file <- app_test_path( "ui", "ui_main.R")
  expect_true(file.exists(ui_file))

  txt <- paste(readLines(ui_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_true(grepl("uiOutput\\(\"mergedPlatemapDownloadUI\"\\)", txt, perl = TRUE))
})

test_that("server exposes persistent merged-plate download handler", {
  server_file <- app_test_path( "server", "server_main.R")
  expect_true(file.exists(server_file))

  txt <- paste(readLines(server_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_true(grepl("output\\$mergedPlatemapDownloadUI <- renderUI\\(", txt, perl = TRUE))
  expect_true(grepl("output\\$downloadMergedPlatemap <- downloadHandler\\(", txt, perl = TRUE))
  expect_true(grepl("output\\$downloadMergedPlatemapLatest <- downloadHandler\\(", txt, perl = TRUE))
  expect_true(grepl("copy_merged_platemap <- function\\(file\\)", txt, perl = TRUE))
})

test_that("merge action uses latest merged file as cumulative base when available", {
  server_file <- app_test_path( "server", "server_main.R")
  expect_true(file.exists(server_file))

  txt <- paste(readLines(server_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_true(grepl("latest_merged <- merged_platemap_path\\(\\)", txt, perl = TRUE))
  expect_true(grepl("base_file_for_merge <- if \\(!is\\.null\\(latest_merged\\)", txt, perl = TRUE))
  expect_true(grepl("base_file = base_file_for_merge", txt, perl = TRUE))
})
