library(testthat)

load_dpi_config <- function() {
  env <- new.env(parent = baseenv())
  sys.source(app_test_path("config.R"), envir = env)
  env
}

read_dpi_app_file <- function(...) {
  paste(readLines(app_test_path(...), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

test_that("DPI defaults and supported range are centralized", {
  env <- load_dpi_config()

  expect_equal(env$BIOSZEN_DEFAULT_DPI, 300)
  expect_equal(env$BIOSZEN_MIN_DPI, 72)
  expect_equal(env$BIOSZEN_MAX_DPI, 600)
  expect_equal(env$BIOSZEN_CSS_DPI, 96)
})

test_that("valid user DPI values are preserved", {
  env <- load_dpi_config()

  for (dpi in c(72, 96, 150, 192, 300, 450, 600)) {
    status <- env$bioszen_validate_dpi(dpi)
    expect_true(status$valid, info = paste("DPI", dpi))
    expect_equal(status$value, dpi)
    expect_identical(status$reason, "valid")
  }
})

test_that("missing invalid and unsupported DPI values fall back to 300", {
  env <- load_dpi_config()
  invalid <- list(NULL, numeric(0), NA, "", "not-a-number", NaN, Inf, 0, -1, 71, 601, 9999)

  for (value in invalid) {
    status <- env$bioszen_validate_dpi(value)
    expect_false(status$valid, info = paste("Value", paste(value, collapse = ",")))
    expect_equal(status$value, 300)
  }
  expect_identical(env$bioszen_validate_dpi(NULL)$reason, "missing")
  expect_identical(env$bioszen_validate_dpi("bad")$reason, "non_numeric")
  expect_identical(env$bioszen_validate_dpi(0)$reason, "unsupported")
})

test_that("DPI changes raster pixels without changing logical dimensions", {
  env <- load_dpi_config()
  width_px <- 600
  height_px <- 420
  width_in <- width_px / env$BIOSZEN_CSS_DPI
  height_in <- height_px / env$BIOSZEN_CSS_DPI

  pixels_at <- function(dpi) c(width_in, height_in) * env$bioszen_effective_dpi(dpi)
  expect_equal(pixels_at(192), c(1200, 840))
  expect_equal(pixels_at(300), c(1875, 1312.5))
  expect_equal(width_in / height_in, width_px / height_px)
})

test_that("all raster export paths use effective DPI while vectors remain independent", {
  server_txt <- read_dpi_app_file("server", "server_main.R")
  panel_txt <- read_dpi_app_file("server", "panel_module.R")
  global_txt <- read_dpi_app_file("global.R")
  ui_txt <- read_dpi_app_file("ui", "ui_main.R")

  expect_match(global_txt, "export_dpi = BIOSZEN_DEFAULT_DPI", fixed = TRUE)
  expect_match(global_txt, "min = BIOSZEN_MIN_DPI", fixed = TRUE)
  expect_match(global_txt, "max = BIOSZEN_MAX_DPI", fixed = TRUE)
  expect_match(ui_txt, "bioszen_visual_defaults$export_dpi", fixed = TRUE)
  expect_match(ui_txt, "min = BIOSZEN_MIN_DPI", fixed = TRUE)
  expect_match(ui_txt, "max = BIOSZEN_MAX_DPI", fixed = TRUE)

  expect_match(server_txt, "export_dpi <- bioszen_effective_dpi(input$export_dpi)", fixed = TRUE)
  expect_match(server_txt, "export_scale <- export_dpi / BIOSZEN_CSS_DPI", fixed = TRUE)
  expect_match(server_txt, "scale   = bioszen_effective_dpi(input$export_dpi) / BIOSZEN_CSS_DPI", fixed = TRUE)
  expect_match(server_txt, "output$downloadPlotly_png <- individual_plot_png_download()", fixed = TRUE)
  expect_match(server_txt, "export_dpi     = bioszen_effective_dpi(input$export_dpi)", fixed = TRUE)
  expect_match(server_txt, "export_dpi <- bioszen_effective_dpi(input$combo_export_dpi)", fixed = TRUE)
  expect_match(server_txt, "raster_dpi <- bioszen_effective_dpi(input$combo_export_dpi)", fixed = TRUE)
  expect_match(server_txt, "downloadId = \"dl_combo_png\"", fixed = TRUE)
  expect_match(panel_txt, "combo_export_dpi  = bioszen_effective_dpi(input$combo_export_dpi)", fixed = TRUE)
  expect_match(ui_txt, "var downloadHref = downloadNode && downloadNode.href ? downloadNode.href : '';", fixed = TRUE)
  expect_match(ui_txt, "var src = downloadHref || img.src;", fixed = TRUE)

  expect_match(server_txt, "device = grDevices::cairo_pdf", fixed = TRUE)
  expect_false(grepl("cairo_pdf[\\s\\S]{0,200}dpi\\s*=", server_txt, perl = TRUE))
})

test_that("metadata restoration has explicit legacy and invalid DPI fallbacks", {
  server_txt <- read_dpi_app_file("server", "server_main.R")
  panel_txt <- read_dpi_app_file("server", "panel_module.R")

  expect_match(server_txt, "restore_dpi_metadata(get_val_allow_blank(\"export_dpi\"))", fixed = TRUE)
  expect_match(server_txt, "status <- bioszen_validate_dpi(value)", fixed = TRUE)
  expect_match(panel_txt, "restore_combo_dpi_metadata(gv('combo_export_dpi'))", fixed = TRUE)
  expect_match(panel_txt, "status <- bioszen_validate_dpi(value)", fixed = TRUE)
  expect_match(server_txt, "dpi_metadata_invalid_fallback", fixed = TRUE)
  expect_match(panel_txt, "dpi_metadata_invalid_fallback", fixed = TRUE)
})
