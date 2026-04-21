test_that("plot loading overlay is wired in UI and server", {
  ui_file <- app_test_path( "ui", "ui_main.R")
  server_file <- app_test_path( "server", "server_main.R")

  expect_true(file.exists(ui_file))
  expect_true(file.exists(server_file))

  ui_txt <- paste(readLines(ui_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  server_txt <- paste(readLines(server_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_match(ui_txt, "plot-loading-wrap", fixed = TRUE)
  expect_match(ui_txt, "plot-loading-indicator", fixed = TRUE)
  expect_match(ui_txt, "shiny:outputinvalidated", fixed = TRUE)
  expect_match(ui_txt, "plotInteractivo", fixed = TRUE)

  expect_match(server_txt, "id = \"plot-loading-wrap\"", fixed = TRUE)
  expect_match(server_txt, "plot-loading-indicator", fixed = TRUE)
})

test_that("heatmap and correlation payload cache helpers exist", {
  server_file <- app_test_path( "server", "server_main.R")
  expect_true(file.exists(server_file))
  txt <- paste(readLines(server_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_match(txt, "plot_payload_cache_get\\s*<-\\s*function", perl = TRUE)
  expect_match(txt, "plot_payload_cache_set\\s*<-\\s*function", perl = TRUE)
  expect_match(txt, "get_heatmap_payload_cached\\s*<-\\s*function", perl = TRUE)
  expect_match(txt, "get_corrm_payload_cached\\s*<-\\s*function", perl = TRUE)
  expect_match(txt, "should_abort\\s*=\\s*is_session_closing", perl = TRUE)
})
