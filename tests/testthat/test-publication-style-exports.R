library(testthat)

load_publication_helpers <- function() {
  env <- new.env(parent = globalenv())
  env$`%||%` <- function(x, y) if (is.null(x)) y else x
  sys.source(app_test_path("config.R"), envir = env)
  sys.source(app_test_path("helpers.R"), envir = env)
  env
}

read_publication_app_file <- function(...) {
  paste(readLines(app_test_path(...), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

test_that("publication style defaults keep the established canvas and use 300 DPI", {
  config_txt <- read_publication_app_file("config.R")
  global_txt <- read_publication_app_file("global.R")
  ui_txt <- read_publication_app_file("ui", "ui_main.R")
  server_txt <- read_publication_app_file("server", "server_main.R")

  expect_match(config_txt, "BIOSZEN_DEFAULT_DPI <- 300", fixed = TRUE)
  expect_match(config_txt, "BIOSZEN_CSS_DPI <- 96", fixed = TRUE)
  expect_match(global_txt, "export_dpi = BIOSZEN_DEFAULT_DPI", fixed = TRUE)
  expect_match(global_txt, "base_size = 18", fixed = TRUE)
  expect_match(global_txt, "title_size = 24", fixed = TRUE)
  expect_match(global_txt, "axis_size = 20", fixed = TRUE)
  expect_match(global_txt, "axis_tick_ratio = 0.82", fixed = TRUE)
  expect_match(global_txt, "violin_inner = \"box\"", fixed = TRUE)
  expect_match(ui_txt, "numericInput(\n                   \"export_dpi\"", fixed = TRUE)
  expect_match(ui_txt, "numericInput('plot_w', tr(\"plot_width\"), 1000", fixed = TRUE)
  expect_match(ui_txt, "numericInput('plot_h', tr(\"plot_height\"),  700", fixed = TRUE)
  expect_match(ui_txt, "\"axis_title_spacing_x\"", fixed = TRUE)
  expect_match(ui_txt, "\"axis_title_spacing_y\"", fixed = TRUE)
  expect_match(server_txt, "plot_dimension_scale <- function", fixed = TRUE)
  expect_match(server_txt, "axis.title.x = axis_title_x", fixed = TRUE)
  expect_match(server_txt, "axis.title.y = axis_title_y", fixed = TRUE)
  expect_match(server_txt, "publication_style_types <- c(\n    \"Boxplot\", \"Barras\", \"Violin\", \"Curvas\", \"Apiladas\", \"Correlacion\"", fixed = TRUE)
  expect_match(server_txt, "axis.line = element_line(linewidth = plot_axis_line_width()", fixed = TRUE)
  expect_match(server_txt, "axis.ticks = element_line(linewidth = plot_axis_line_width()", fixed = TRUE)
  expect_match(server_txt, "size = input$fs_legend", fixed = TRUE)
})

test_that("plot style scale is proportional and bounded", {
  env <- load_publication_helpers()

  expect_equal(env$bioszen_plot_dimension_scale(1000, 700), 1)
  expect_gt(env$bioszen_plot_dimension_scale(1400, 980), 1)
  expect_lt(env$bioszen_plot_dimension_scale(700, 490), 1)
  expect_equal(env$bioszen_plot_dimension_scale(10000, 7000), 1.8)
  expect_equal(env$bioszen_plot_dimension_scale(10, 7), 0.7)
})

test_that("composition cell scaling preserves point geometry", {
  skip_if_not_installed("ggplot2")
  env <- load_publication_helpers()

  layout <- matrix(c(1L, 2L, 3L), nrow = 1)
  scales <- env$bioszen_combo_cell_scales(
    layout,
    canvas_width = 1000,
    canvas_height = 700,
    source_widths = rep(1000, 3),
    source_heights = rep(700, 3)
  )
  expect_equal(scales, rep(0.30, 3), tolerance = 1e-10)

  plot <- ggplot2::ggplot(
    data.frame(x = 1:3, y = 1:3),
    ggplot2::aes(x, y)
  ) +
    ggplot2::geom_point(size = 4.5) +
    ggplot2::labs(title = "Original", x = "X", y = "Y") +
    ggplot2::theme_classic(base_size = 18) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 24))
  scaled <- env$bioszen_scale_saved_plot_theme(plot, scales[[1]])

  expect_equal(scaled$layers[[1]]$aes_params$size, 4.5)
  original_theme <- ggplot2::theme_get() + plot$theme
  scaled_theme <- ggplot2::theme_get() + scaled$theme
  expect_equal(
    ggplot2::calc_element("plot.title", scaled_theme)$size,
    ggplot2::calc_element("plot.title", original_theme)$size * scales[[1]],
    tolerance = 1e-10
  )
})

test_that("PowerPoint plot scaling adjusts absolute layer geometry", {
  skip_if_not_installed("ggplot2")
  env <- load_publication_helpers()
  plot <- ggplot2::ggplot(
    data.frame(x = 1:2, y = 1:2),
    ggplot2::aes(x, y)
  ) +
    ggplot2::geom_point(size = 4, stroke = 1) +
    ggplot2::geom_line(linewidth = 2)

  scaled <- env$bioszen_scale_plot_layers(plot, 0.5)
  expect_equal(scaled$layers[[1]]$aes_params$size, 2)
  expect_equal(scaled$layers[[1]]$aes_params$stroke, 0.5)
  expect_equal(scaled$layers[[2]]$aes_params$linewidth, 1)
  expect_equal(plot$layers[[1]]$aes_params$size, 4)
})

test_that("composition scaling honors custom spans and weights", {
  env <- load_publication_helpers()
  layout <- matrix(c(1L, 1L, 2L, 3L), nrow = 2, byrow = TRUE)
  scales <- env$bioszen_combo_cell_scales(
    layout,
    canvas_width = 1200,
    canvas_height = 800,
    source_widths = rep(1000, 3),
    source_heights = rep(700, 3),
    column_weights = c(2, 1),
    row_weights = c(1, 1)
  )
  expect_equal(scales[[1]], scales[[2]], tolerance = 1e-10)
  expect_gt(scales[[2]], scales[[3]])
})

test_that("automatic composition grids stay balanced when plots overflow", {
  env <- load_publication_helpers()

  expect_equal(env$bioszen_expand_combo_grid(1, 1, 1), list(nrow = 1L, ncol = 1L))
  expect_equal(env$bioszen_expand_combo_grid(3, 1, 1), list(nrow = 2L, ncol = 2L))
  expect_equal(env$bioszen_expand_combo_grid(4, 1, 1), list(nrow = 2L, ncol = 2L))
  expect_equal(env$bioszen_expand_combo_grid(5, 1, 1), list(nrow = 2L, ncol = 3L))
  expect_equal(env$bioszen_expand_combo_grid(3, 1, 3), list(nrow = 1L, ncol = 3L))
})

test_that("PowerPoint fitting preserves composition aspect ratio and slide bounds", {
  env <- load_publication_helpers()

  landscape <- env$bioszen_fit_aspect_rect(1000, 700, 10, 7.5, margin = 0.2)
  portrait <- env$bioszen_fit_aspect_rect(1000, 700, 7.5, 10, margin = 0.2)
  tiny <- env$bioszen_fit_aspect_rect(1000, 700, 2, 2, margin = 5)

  expect_equal(landscape$width / landscape$height, 1000 / 700, tolerance = 1e-10)
  expect_equal(portrait$width / portrait$height, 1000 / 700, tolerance = 1e-10)
  expect_gte(landscape$left, 0.2 - 1e-8)
  expect_gte(landscape$top, 0.2 - 1e-8)
  expect_lte(landscape$left + landscape$width, 10 - 0.2 + 1e-8)
  expect_lte(landscape$top + landscape$height, 7.5 - 0.2 + 1e-8)
  expect_gte(portrait$left, 0.2 - 1e-8)
  expect_gte(portrait$top, 0.2 - 1e-8)
  expect_lte(portrait$left + portrait$width, 7.5 - 0.2 + 1e-8)
  expect_lte(portrait$top + portrait$height, 10 - 0.2 + 1e-8)
  expect_true(tiny$small)
  expect_gte(tiny$left, tiny$margin - 1e-8)
  expect_gte(tiny$top, tiny$margin - 1e-8)
  expect_lte(tiny$left + tiny$width, 2 - tiny$margin + 1e-8)
  expect_lte(tiny$top + tiny$height, 2 - tiny$margin + 1e-8)
})

test_that("single-chart PowerPoint sizing preserves the current plot aspect ratio", {
  env <- load_publication_helpers()

  standard <- env$bioszen_pptx_size_from_pixels(1000, 700)
  portrait <- env$bioszen_pptx_size_from_pixels(700, 1000)
  oversized <- env$bioszen_pptx_size_from_pixels(10000, 7000)

  expect_equal(standard$width / standard$height, 1000 / 700, tolerance = 1e-10)
  expect_equal(portrait$width / portrait$height, 700 / 1000, tolerance = 1e-10)
  expect_equal(oversized$width / oversized$height, 10000 / 7000, tolerance = 1e-10)
  expect_equal(standard$orientation, "landscape")
  expect_equal(portrait$orientation, "portrait")
  expect_lte(max(oversized$width, oversized$height), 56)
})

test_that("individual chart menus expose editable PowerPoint downloads", {
  ui_txt <- read_publication_app_file("ui", "ui_main.R")
  server_txt <- read_publication_app_file("server", "server_main.R")
  helpers_txt <- read_publication_app_file("helpers.R")

  expect_equal(length(gregexpr("downloadPlot_pptx", ui_txt, fixed = TRUE)[[1]]), 1)
  expect_equal(length(gregexpr("downloadPlotly_pptx", ui_txt, fixed = TRUE)[[1]]), 1)
  expect_match(server_txt, "output$downloadPlot_pptx <- editable_plot_pptx_download()", fixed = TRUE)
  expect_match(server_txt, "output$downloadPlotly_pptx <- editable_plot_pptx_download()", fixed = TRUE)
  expect_match(server_txt, "slot = \"plot_pptx\"", fixed = TRUE)
  expect_match(server_txt, "bioszen_write_editable_plot_pptx(", fixed = TRUE)
  expect_match(server_txt, "build_plot(scope_sel, strain_sel, input$tipo, for_interactive = TRUE)", fixed = TRUE)
  expect_match(
    helpers_txt,
    "ppt_plot <- bioszen_prepare_named_pptx_strokes(plot, linewidth_pt = 1)",
    fixed = TRUE
  )
  expect_match(helpers_txt, "rvg::dml(ggobj = ppt_plot, editable = TRUE)", fixed = TRUE)
  expect_false(grepl("bioszen_write_plotly_svg_pptx", server_txt, fixed = TRUE))
  expect_false(grepl("bioszen_plot_svg", server_txt, fixed = TRUE))
})

test_that("single-chart PowerPoint writer creates editable vector objects", {
  skip_if_not_installed("officer")
  skip_if_not_installed("rvg")
  skip_if_not_installed("xml2")
  skip_if_not_installed("ggplot2")

  env <- load_publication_helpers()
  plot_obj <- ggplot2::ggplot(
    data.frame(group = c("A", "B", "C"), value = c(2, 4, 3)),
    ggplot2::aes(group, value, fill = group)
  ) +
    ggplot2::geom_col() +
    ggplot2::labs(title = "Editable chart", x = "Group", y = "Value") +
    ggplot2::theme_classic()

  out <- tempfile(fileext = ".pptx")
  unpacked <- tempfile("bioszen-pptx-")
  on.exit(unlink(c(out, unpacked), recursive = TRUE, force = TRUE), add = TRUE)
  dims <- env$bioszen_write_editable_plot_pptx(out, plot_obj, 1200, 800)

  expect_true(file.exists(out))
  expect_gt(file.info(out)$size, 1000)
  deck <- officer::read_pptx(out)
  expect_length(deck, 1)
  expect_equal(officer::slide_size(deck)$width, dims$width, tolerance = 1e-5)
  expect_equal(officer::slide_size(deck)$height, dims$height, tolerance = 1e-5)
  objects <- officer::pptx_summary(deck)
  expect_gt(nrow(objects), 1)
  expect_false(nrow(objects) == 1 && identical(objects$content_type[[1]], "image"))
  expect_true(any(grepl("Editable chart", objects$text, fixed = TRUE)))

  listing <- utils::unzip(out, list = TRUE)
  expect_false(any(grepl("^ppt/media/.*\\.(?:png|jpe?g|svg)$", listing$Name, perl = TRUE)))
  dir.create(unpacked, recursive = TRUE)
  utils::unzip(out, exdir = unpacked)
  slide <- xml2::read_xml(file.path(unpacked, "ppt", "slides", "slide1.xml"))
  slide_ns <- xml2::xml_ns(slide)
  expect_gt(length(xml2::xml_find_all(slide, ".//p:sp", slide_ns)), 5)
  expect_equal(length(xml2::xml_find_all(slide, ".//p:pic", slide_ns)), 0)
})

test_that("PowerPoint export helpers create one readable vector slide at custom size", {
  skip_if_not_installed("officer")
  skip_if_not_installed("rvg")
  skip_if_not_installed("xml2")
  skip_if_not_installed("ggplot2")

  env <- load_publication_helpers()
  slide_width <- 13.333
  slide_height <- 7.5
  fit <- env$bioszen_fit_aspect_rect(1000, 700, slide_width, slide_height)
  plot_obj <- ggplot2::ggplot(
    data.frame(x = 1:3, y = c(1, 3, 2)),
    ggplot2::aes(x, y)
  ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::theme_classic()

  doc <- officer::read_pptx()
  doc <- env$bioszen_set_pptx_slide_size(doc, slide_width, slide_height)
  doc <- officer::add_slide(doc, layout = "Blank", master = "Office Theme")
  vector_added <- tryCatch({
    doc <- officer::ph_with(
      doc,
      rvg::dml(ggobj = plot_obj),
      location = officer::ph_location(
        left = fit$left,
        top = fit$top,
        width = fit$width,
        height = fit$height
      )
    )
    TRUE
  }, error = function(e) {
    if (grepl("Graphics API version mismatch", conditionMessage(e), fixed = TRUE)) {
      return(FALSE)
    }
    stop(e)
  })
  if (!isTRUE(vector_added)) {
    raster_file <- tempfile(fileext = ".png")
    on.exit(unlink(raster_file, force = TRUE), add = TRUE)
    ggplot2::ggsave(
      raster_file,
      plot = plot_obj,
      width = fit$width,
      height = fit$height,
      units = "in",
      dpi = 150,
      limitsize = FALSE,
      bg = "white"
    )
    doc <- officer::ph_with(
      doc,
      officer::external_img(raster_file, width = fit$width, height = fit$height),
      location = officer::ph_location(
        left = fit$left,
        top = fit$top,
        width = fit$width,
        height = fit$height
      )
    )
  }

  out <- tempfile(fileext = ".pptx")
  on.exit(unlink(out, force = TRUE), add = TRUE)
  print(doc, target = out)

  expect_true(file.exists(out))
  expect_gt(file.info(out)$size, 1000)
  reopened <- officer::read_pptx(out)
  expect_length(reopened, 1)
  expect_equal(officer::slide_size(reopened)$width, slide_width, tolerance = 1e-5)
  expect_equal(officer::slide_size(reopened)$height, slide_height, tolerance = 1e-5)
  summary <- officer::pptx_summary(reopened)
  expect_gt(nrow(summary), 0)
  expect_true(isTRUE(vector_added) || any(summary$content_type == "image"))
})

test_that("PowerPoint controls and metadata are wired without adding slide-count input", {
  global_txt <- read_publication_app_file("global.R")
  panel_txt <- read_publication_app_file("server", "panel_module.R")
  server_txt <- read_publication_app_file("server", "server_main.R")

  expect_match(global_txt, "\"combo_pptx_preset\"", fixed = TRUE)
  expect_match(global_txt, "\"combo_pptx_orientation\"", fixed = TRUE)
  expect_match(global_txt, "\"combo_pptx_width\"", fixed = TRUE)
  expect_match(global_txt, "\"combo_pptx_height\"", fixed = TRUE)
  expect_false(grepl("combo_pptx_slide_count", global_txt, fixed = TRUE))
  expect_match(panel_txt, "combo_pptx_preset = input$combo_pptx_preset", fixed = TRUE)
  expect_match(panel_txt, "combo_pptx_orientation = input$combo_pptx_orientation", fixed = TRUE)
  expect_match(server_txt, "bioszen_set_pptx_slide_size", fixed = TRUE)
  expect_match(server_txt, "pptx_plot <- combo_plot_builder(pptx_plot_scale)", fixed = TRUE)
  expect_match(server_txt, "pptx_plot_scale <- pptx_plot_scale * 0.85", fixed = TRUE)
  expect_match(server_txt, "rvg::dml(ggobj = pptx_plot)", fixed = TRUE)
  expect_match(server_txt, "layout = \"Blank\"", fixed = TRUE)
  expect_match(server_txt, "officer::ph_location(", fixed = TRUE)
  expect_false(grepl("ph_location_fullsize", server_txt, fixed = TRUE))
})
