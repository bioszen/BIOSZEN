library(testthat)

read_preview_app_file <- function(...) {
  paste(readLines(app_test_path(...), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

test_that("interactive plot preview keeps the configured canvas and fits only its display", {
  ui_txt <- read_preview_app_file("ui", "ui_main.R")
  server_txt <- read_preview_app_file("server", "server_main.R")

  expect_match(server_txt, 'class = "bioszen-plot-preview-viewport"', fixed = TRUE)
  expect_match(server_txt, 'class = "bioszen-plot-preview-stage"', fixed = TRUE)
  expect_match(server_txt, '`data-plot-width` = format(plot_w', fixed = TRUE)
  expect_match(server_txt, '`data-plot-height` = format(plot_h', fixed = TRUE)
  expect_match(server_txt, 'width  = "100%"', fixed = TRUE)
  expect_match(server_txt, 'height = "100%"', fixed = TRUE)

  expect_match(ui_txt, "function fitPlotPreview(viewport)", fixed = TRUE)
  expect_match(ui_txt, "var scale = Math.min(1, widthScale, heightScale);", fixed = TRUE)
  expect_match(ui_txt, "window.innerHeight - Math.max(0, rect.top) - 12", fixed = TRUE)
  expect_match(ui_txt, "stage.style.zoom = String(scale);", fixed = TRUE)
  expect_match(ui_txt, "window.CSS.supports('zoom', '0.5')", fixed = TRUE)
  expect_match(ui_txt, "stage.style.transform = 'none';", fixed = TRUE)
  expect_match(ui_txt, "window.BIOSZEN_fitPlotPreviews = fitPlotPreviews", fixed = TRUE)
})

test_that("preview fitting preserves hover geometry and uses an aligned fallback", {
  ui_txt <- read_preview_app_file("ui", "ui_main.R")

  expect_match(ui_txt, "function resizePlotlyFallback(stage, width, height)", fixed = TRUE)
  expect_match(ui_txt, "Plotly.Plots.resize(graph)", fixed = TRUE)
  expect_match(ui_txt, "data-fallback-width", fixed = TRUE)
  expect_false(grepl("style.transform = 'scale(", ui_txt, fixed = TRUE))
  expect_false(grepl("translate3d(", ui_txt, fixed = TRUE))
})

test_that("preview fitting cannot change plot defaults or export dimensions", {
  ui_txt <- read_preview_app_file("ui", "ui_main.R")
  server_txt <- read_preview_app_file("server", "server_main.R")

  expect_match(ui_txt, "numericInput('plot_w', tr(\"plot_width\"), 1000", fixed = TRUE)
  expect_match(ui_txt, "numericInput('plot_h', tr(\"plot_height\"),  700", fixed = TRUE)
  expect_match(server_txt, "width <- input$plot_w %||% 900", fixed = TRUE)
  expect_match(server_txt, "height <- input$plot_h %||% 700", fixed = TRUE)
  expect_false(grepl("setInputValue\\(.*plot_[wh]", ui_txt, perl = TRUE))
  expect_false(grepl("Plotly\\.relayout", ui_txt, perl = TRUE))
})

test_that("panel dimension changes still update the intrinsic preview canvas", {
  server_txt <- read_preview_app_file("server", "server_main.R")

  expect_match(server_txt, "base_w <- as.numeric(input$plot_w %||% 1000)", fixed = TRUE)
  expect_match(server_txt, "base_h <- as.numeric(input$plot_h %||% 700)", fixed = TRUE)
  expect_match(server_txt, "plot_w <- if (use_effective) effective_plot_width(base_w) else base_w", fixed = TRUE)
  expect_match(server_txt, "plot_h <- if (use_effective) effective_plot_height(base_h) else base_h", fixed = TRUE)
  expect_match(server_txt, "style = sprintf(\"width:%spx; height:%spx;\", plot_w, plot_h)", fixed = TRUE)
  expect_match(server_txt, "`data-plot-width` = format(plot_w", fixed = TRUE)
  expect_match(server_txt, "`data-plot-height` = format(plot_h", fixed = TRUE)
})

test_that("preview fitting is refreshed after browser and pane changes", {
  ui_txt <- read_preview_app_file("ui", "ui_main.R")

  expect_match(ui_txt, "window.addEventListener('resize'", fixed = TRUE)
  expect_match(ui_txt, "window.addEventListener('orientationchange'", fixed = TRUE)
  expect_match(ui_txt, "name === 'plotInteractivoUI' || name === 'plotInteractivo'", fixed = TRUE)
  expect_match(ui_txt, "$(document).on('plotly_afterplot'", fixed = TRUE)
  expect_match(ui_txt, "window.BIOSZEN_fitPlotPreviews();", fixed = TRUE)
})
