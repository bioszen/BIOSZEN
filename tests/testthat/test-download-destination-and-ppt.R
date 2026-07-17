library(testthat)

test_that("single-chart PowerPoint stores editable preview-derived DrawingML shapes", {
  skip_if_not_installed("officer")
  skip_if_not_installed("rvg")
  skip_if_not_installed("xml2")
  skip_if_not_installed("ggplot2")

  env <- new.env(parent = globalenv())
  env$`%||%` <- function(x, y) if (is.null(x)) y else x
  env$BIOSZEN_CSS_DPI <- 96
  sys.source(app_test_path("helpers.R"), envir = env)

  plot_obj <- ggplot2::ggplot(
    data.frame(group = c("A", "B", "C"), value = c(2, 4, 3)),
    ggplot2::aes(group, value, group = 1)
  ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 4, colour = "#2f6fb0") +
    ggplot2::labs(title = "Editable preview", x = "Group", y = "Value") +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA)
    )

  target <- tempfile(fileext = ".pptx")
  unpacked <- tempfile("bioszen-pptx-")
  on.exit(unlink(c(target, unpacked), recursive = TRUE, force = TRUE), add = TRUE)

  dims <- env$bioszen_write_editable_plot_pptx(target, plot_obj, 400, 300)
  listing <- utils::unzip(target, list = TRUE)
  deck <- officer::read_pptx(target)
  objects <- officer::pptx_summary(deck)
  dir.create(unpacked, recursive = TRUE)
  utils::unzip(target, exdir = unpacked)
  slide <- xml2::read_xml(file.path(unpacked, "ppt", "slides", "slide1.xml"))
  slide_ns <- xml2::xml_ns(slide)

  expect_true(file.exists(target))
  expect_gt(file.info(target)$size, 0)
  expect_equal(dims$aspect_ratio, 4 / 3)
  expect_gt(nrow(objects), 5)
  expect_true(any(grepl("Editable preview", objects$text, fixed = TRUE)))
  expect_gt(length(xml2::xml_find_all(slide, ".//p:sp", slide_ns)), 5)
  expect_equal(length(xml2::xml_find_all(slide, ".//p:pic", slide_ns)), 0)
  expect_equal(
    length(xml2::xml_find_all(
      slide,
      ".//p:sp/p:spPr/a:solidFill/a:srgbClr[translate(@val, 'abcdef', 'ABCDEF')='FFFFFF']",
      slide_ns
    )),
    0,
    info = "Editable PPT must not contain a white background shape."
  )
  expect_false(any(grepl("^ppt/media/.*\\.(?:png|jpe?g|svg)$", listing$Name, perl = TRUE)))
})

test_that("editable PPT applies PDF-aligned units without mutating the source plot", {
  skip_if_not_installed("officer")
  skip_if_not_installed("rvg")
  skip_if_not_installed("xml2")
  skip_if_not_installed("ggplot2")

  env <- new.env(parent = globalenv())
  env$`%||%` <- function(x, y) if (is.null(x)) y else x
  env$BIOSZEN_CSS_DPI <- 96
  sys.source(app_test_path("helpers.R"), envir = env)

  source_plot <- ggplot2::ggplot(
    data.frame(group = rep(c("A", "B"), each = 3), value = c(2, 3, 4, 5, 6, 7)),
    ggplot2::aes(group, value)
  ) +
    ggplot2::geom_boxplot(width = 0.8, linewidth = 0.6) +
    ggplot2::geom_point(size = 3) +
    ggplot2::labs(title = "PDF-aligned editable plot", y = "Value") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 24, face = "bold"),
      axis.line = ggplot2::element_line(linewidth = 1.3, colour = "black"),
      axis.ticks = ggplot2::element_line(linewidth = 1.3, colour = "black")
    )
  page <- env$bioszen_pdf_page_size_from_pixels(1000, 700)
  prepared <- env$bioszen_prepare_editable_plotly_plot(
    source_plot,
    plot_type = "Boxplot",
    content_width_px = 1000,
    content_height_px = 700,
    slide_width_px = page$width_px,
    slide_height_px = page$height_px
  )

  expect_equal(page$width_px, 1056)
  expect_equal(page$height_px, 816)
  layer_width <- function(layer) layer$geom_params$width %||% layer$aes_params$width
  expect_equal(layer_width(source_plot$layers[[1]]), 0.8)
  expect_equal(layer_width(prepared$layers[[1]]), 0.5)
  original_theme <- ggplot2::theme_get() + source_plot$theme
  prepared_theme <- ggplot2::theme_get() + prepared$theme
  expect_equal(
    ggplot2::calc_element("plot.title", prepared_theme)$size,
    ggplot2::calc_element("plot.title", original_theme)$size * 0.75
  )
  expect_equal(
    ggplot2::calc_element("axis.line", prepared_theme)$linewidth,
    ggplot2::calc_element("axis.line", original_theme)$linewidth / 2.834646,
    tolerance = 1e-7
  )
  expect_equal(source_plot$layers[[2]]$aes_params$size, 3)
  expect_equal(prepared$layers[[2]]$aes_params$size, 3 * 1.2)

  target <- tempfile(fileext = ".pptx")
  on.exit(unlink(target, force = TRUE), add = TRUE)
  dims <- env$bioszen_write_editable_plot_pptx(
    target,
    prepared,
    width_px = 1000,
    height_px = 700,
    slide_width_px = page$width_px,
    slide_height_px = page$height_px
  )
  deck <- officer::read_pptx(target)
  slide <- officer::slide_size(deck)
  objects <- officer::pptx_summary(deck)

  expect_equal(slide$width, 11, tolerance = 1e-6)
  expect_equal(slide$height, 8.5, tolerance = 1e-6)
  expect_equal(dims$content_aspect_ratio, 1000 / 700)
  expect_true(any(grepl("PDF-aligned editable plot", objects$text, fixed = TRUE)))
})

test_that("editable PPT fixes only axes, ticks, and significance bars at one point", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("officer")
  skip_if_not_installed("rvg")
  skip_if_not_installed("xml2")

  env <- new.env(parent = globalenv())
  env$`%||%` <- function(x, y) if (is.null(x)) y else x
  env$BIOSZEN_CSS_DPI <- 96
  sys.source(app_test_path("helpers.R"), envir = env)

  regular_segment <- data.frame(x = 1, xend = 2, y = 2, yend = 3)
  significance_segment <- data.frame(
    x = 1, xend = 2, y = 4, yend = 4, .sig_layer = TRUE
  )
  source_plot <- ggplot2::ggplot(
    data.frame(group = c("A", "B"), value = c(2, 3)),
    ggplot2::aes(group, value)
  ) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_segment(
      data = regular_segment,
      ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
      inherit.aes = FALSE,
      linewidth = 1.7
    ) +
    ggplot2::geom_segment(
      data = significance_segment,
      ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
      inherit.aes = FALSE,
      linewidth = 2.4
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.line = ggplot2::element_line(linewidth = 1.3, colour = "#123456"),
      axis.ticks = ggplot2::element_line(linewidth = 0.2, colour = "#654321"),
      panel.grid.major = ggplot2::element_line(linewidth = 0.9, colour = "grey70")
    )

  prepared <- env$bioszen_prepare_named_pptx_strokes(source_plot)
  target_mm <- env$bioszen_pptx_linewidth_mm(1)
  source_theme <- ggplot2::theme_get() + source_plot$theme
  prepared_theme <- ggplot2::theme_get() + prepared$theme

  for (name in c("axis.line", "axis.line.x", "axis.line.y",
                 "axis.ticks", "axis.ticks.x", "axis.ticks.y")) {
    expect_equal(
      ggplot2::calc_element(name, prepared_theme)$linewidth,
      target_mm,
      tolerance = 1e-9
    )
  }
  expect_equal(
    ggplot2::calc_element("panel.grid.major", prepared_theme)$linewidth,
    ggplot2::calc_element("panel.grid.major", source_theme)$linewidth
  )
  expect_equal(prepared$layers[[2]]$aes_params$linewidth, 1.7)
  expect_equal(prepared$layers[[3]]$aes_params$linewidth, target_mm, tolerance = 1e-9)
  expect_equal(source_plot$layers[[3]]$aes_params$linewidth, 2.4)
  expect_equal(ggplot2::calc_element("axis.line", source_theme)$linewidth, 1.3)

  target <- tempfile(fileext = ".pptx")
  unpacked <- tempfile("bioszen-one-point-pptx-")
  on.exit(unlink(c(target, unpacked), recursive = TRUE, force = TRUE), add = TRUE)
  env$bioszen_write_editable_plot_pptx(target, source_plot, 800, 600)
  dir.create(unpacked, recursive = TRUE)
  utils::unzip(target, exdir = unpacked)
  slide <- xml2::read_xml(file.path(unpacked, "ppt", "slides", "slide1.xml"))
  line_widths <- suppressWarnings(as.integer(xml2::xml_attr(
    xml2::xml_find_all(slide, ".//a:ln[@w]", xml2::xml_ns(slide)),
    "w"
  )))
  expect_true(12700L %in% line_widths)
})

test_that("editable PPT preserves configured significance text size", {
  skip_if_not_installed("ggplot2")

  env <- new.env(parent = globalenv())
  env$`%||%` <- function(x, y) if (is.null(x)) y else x
  env$BIOSZEN_CSS_DPI <- 96
  sys.source(app_test_path("helpers.R"), envir = env)

  regular_text <- data.frame(x = 1, y = 2, label = "regular")
  significance_text <- data.frame(
    x = 1.5, y = 3, label = "****", .sig_layer = TRUE
  )
  source_plot <- ggplot2::ggplot() +
    ggplot2::geom_text(
      data = regular_text,
      ggplot2::aes(x, y, label = label),
      size = 8
    ) +
    ggplot2::geom_text(
      data = significance_text,
      ggplot2::aes(x, y, label = label),
      size = 8
    )
  page <- env$bioszen_pdf_page_size_from_pixels(1000, 700)

  prepared <- env$bioszen_prepare_editable_plotly_plot(
    source_plot,
    plot_type = "Boxplot",
    content_width_px = 1000,
    content_height_px = 700,
    slide_width_px = page$width_px,
    slide_height_px = page$height_px
  )

  expect_equal(prepared$layers[[1]]$aes_params$size, 8 * 72 / 96)
  expect_equal(prepared$layers[[2]]$aes_params$size, 8)
  expect_equal(source_plot$layers[[2]]$aes_params$size, 8)
})

test_that("editable PPT point proportions are consistent across Plotly chart families", {
  skip_if_not_installed("ggplot2")

  env <- new.env(parent = globalenv())
  env$`%||%` <- function(x, y) if (is.null(x)) y else x
  env$BIOSZEN_CSS_DPI <- 96
  sys.source(app_test_path("helpers.R"), envir = env)

  source_plot <- ggplot2::ggplot(
    data.frame(group = rep(c("A", "B"), each = 3), value = c(2, 3, 4, 5, 6, 7)),
    ggplot2::aes(group, value)
  ) +
    ggplot2::geom_point(size = 3, stroke = 1) +
    ggplot2::theme_minimal()
  page <- env$bioszen_pdf_page_size_from_pixels(1000, 700)
  plotly_types <- c("Boxplot", "Barras", "Violin", "Apiladas", "Correlacion")

  for (plot_type in plotly_types) {
    prepared <- env$bioszen_prepare_editable_plotly_plot(
      source_plot,
      plot_type = plot_type,
      content_width_px = 1000,
      content_height_px = 700,
      slide_width_px = page$width_px,
      slide_height_px = page$height_px
    )
    expect_equal(
      prepared$layers[[1]]$aes_params$size,
      3 * 1.2,
      info = paste("PPT marker scale for", plot_type)
    )
    expect_identical(
      class(prepared$layers[[1]]$geom),
      class(source_plot$layers[[1]]$geom),
      info = paste("PPT geometry for", plot_type)
    )
  }

  curves <- env$bioszen_prepare_editable_plotly_plot(
    source_plot,
    plot_type = "Curvas",
    content_width_px = 1000,
    content_height_px = 700,
    slide_width_px = 1000,
    slide_height_px = 700
  )
  expect_identical(curves, source_plot)
  expect_equal(source_plot$layers[[1]]$aes_params$size, 3)
})

test_that("stacked PPT converts legacy error-bar size without changing its source plot", {
  skip_if_not_installed("ggplot2")

  env <- new.env(parent = globalenv())
  env$`%||%` <- function(x, y) if (is.null(x)) y else x
  env$BIOSZEN_CSS_DPI <- 96
  sys.source(app_test_path("helpers.R"), envir = env)

  source_plot <- suppressWarnings(
    ggplot2::ggplot(
      data.frame(group = c("A", "B"), value = c(2, 3), low = c(1, 2), high = c(3, 4)),
      ggplot2::aes(group, value)
    ) +
      ggplot2::geom_col() +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = low, ymax = high), size = 0.8)
  )
  page <- env$bioszen_pdf_page_size_from_pixels(1000, 700)
  prepared <- env$bioszen_prepare_editable_plotly_plot(
    source_plot,
    plot_type = "Apiladas",
    content_width_px = 1000,
    content_height_px = 700,
    slide_width_px = page$width_px,
    slide_height_px = page$height_px
  )

  expect_equal(source_plot$layers[[2]]$aes_params$size, 0.8)
  expect_null(source_plot$layers[[2]]$aes_params$linewidth)
  expect_null(prepared$layers[[2]]$aes_params$size)
  expect_equal(prepared$layers[[2]]$aes_params$linewidth, 0.8)

  old_warn <- getOption("warn")
  on.exit(options(warn = old_warn), add = TRUE)
  options(warn = 2)
  expect_silent(ggplot2::ggplot_build(prepared))
})

test_that("expected ggplot drawing warnings cannot abort editable PPT export", {
  skip_if_not_installed("officer")
  skip_if_not_installed("rvg")
  skip_if_not_installed("ggplot2")

  env <- new.env(parent = globalenv())
  env$`%||%` <- function(x, y) if (is.null(x)) y else x
  env$BIOSZEN_CSS_DPI <- 96
  sys.source(app_test_path("helpers.R"), envir = env)

  warning_plot <- ggplot2::ggplot(
    data.frame(group = c("A", "B"), value = c(1, 3)),
    ggplot2::aes(group, value)
  ) +
    ggplot2::geom_col() +
    ggplot2::scale_y_continuous(limits = c(0, 2))
  target <- tempfile(fileext = ".pptx")
  on.exit(unlink(target, force = TRUE), add = TRUE)
  old_warn <- getOption("warn")
  on.exit(options(warn = old_warn), add = TRUE)
  options(warn = 2)

  expect_silent(env$bioszen_write_editable_plot_pptx(target, warning_plot, 1000, 700))
  expect_true(file.exists(target))
  expect_true(file.info(target)$size > 1000)
})

test_that("heatmap PPT uses an editable vector colour bar without changing the source plot", {
  skip_if_not_installed("officer")
  skip_if_not_installed("rvg")
  skip_if_not_installed("ggplot2")

  env <- new.env(parent = globalenv())
  env$`%||%` <- function(x, y) if (is.null(x)) y else x
  env$BIOSZEN_CSS_DPI <- 96
  sys.source(app_test_path("helpers.R"), envir = env)

  heat_data <- expand.grid(x = letters[1:3], y = LETTERS[1:3])
  heat_data$value <- seq(-1, 1, length.out = nrow(heat_data))
  source_plot <- ggplot2::ggplot(
    heat_data,
    ggplot2::aes(x, y, fill = value)
  ) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
    ggplot2::theme_minimal()
  source_guides <- source_plot$guides
  page <- env$bioszen_pdf_page_size_from_pixels(1000, 700)
  prepared <- env$bioszen_prepare_editable_plotly_plot(
    source_plot,
    plot_type = "Heatmap",
    content_width_px = 1000,
    content_height_px = 700,
    slide_width_px = page$width_px,
    slide_height_px = page$height_px
  )

  target <- tempfile(fileext = ".pptx")
  on.exit(unlink(target, force = TRUE), add = TRUE)
  env$bioszen_write_editable_plot_pptx(
    target,
    prepared,
    width_px = 1000,
    height_px = 700,
    slide_width_px = page$width_px,
    slide_height_px = page$height_px
  )
  listing <- utils::unzip(target, list = TRUE)
  objects <- officer::pptx_summary(officer::read_pptx(target))

  expect_identical(source_plot$guides, source_guides)
  expect_true(nrow(objects) > nrow(heat_data))
  expect_false(any(grepl("^ppt/media/.*\\.(?:png|jpe?g|svg)$", listing$Name, perl = TRUE)))
})

test_that("download UI uses editable preview-aligned PPT and prompts for destinations", {
  ui_txt <- paste(readLines(app_test_path("ui", "ui_main.R"), warn = FALSE), collapse = "\n")
  server_txt <- paste(readLines(app_test_path("server", "server_main.R"), warn = FALSE), collapse = "\n")
  helpers_txt <- paste(readLines(app_test_path("helpers.R"), warn = FALSE), collapse = "\n")
  stacked_txt <- paste(readLines(app_test_path("graficos", "graficos_apilados.R"), warn = FALSE), collapse = "\n")

  expect_false(grepl("PPTX (editable)", ui_txt, fixed = TRUE))
  expect_gte(lengths(regmatches(ui_txt, gregexpr('downloadLink\\("downloadPlot(?:ly)?_pptx", "PPT"', ui_txt, perl = TRUE))), 1)
  expect_match(ui_txt, "window.showSaveFilePicker", fixed = TRUE)
  expect_match(ui_txt, "function canUseSaveFilePicker()", fixed = TRUE)
  expect_match(ui_txt, "window.self === window.top", fixed = TRUE)
  expect_match(ui_txt, "!window.isSecureContext", fixed = TRUE)
  expect_match(ui_txt, "function shouldUseNativeDownload(error)", fixed = TRUE)
  expect_match(ui_txt, "name === 'SecurityError'", fixed = TRUE)
  expect_match(ui_txt, "name === 'NotAllowedError'", fixed = TRUE)
  expect_match(ui_txt, "triggerNativeDownload(link);", fixed = TRUE)
  expect_match(ui_txt, "var canPick = canUseSaveFilePicker();", fixed = TRUE)
  expect_match(ui_txt, "function fetchDownloadBlob(link, attempts)", fixed = TRUE)
  expect_match(ui_txt, "fetchDownloadBlob(link, 3)", fixed = TRUE)
  expect_match(ui_txt, "cache:'no-store'", fixed = TRUE)
  expect_match(ui_txt, "blob.size <= 0", fixed = TRUE)
  expect_match(ui_txt, "writer.abort", fixed = TRUE)
  expect_false(grepl("window.BIOSZEN_preparePptSvg", ui_txt, fixed = TRUE))
  expect_match(ui_txt, "event.target.closest('a.shiny-download-link')", fixed = TRUE)
  expect_match(ui_txt, "function translateShinyFiles(lang)", fixed = TRUE)
  expect_match(ui_txt, "bioszenGrowthFolderCreated', function(msg)", fixed = TRUE)
  expect_match(ui_txt, "+ '\\\\n' + error.message", fixed = TRUE)
  expect_match(ui_txt, "contentTarget.textContent = lookupTranslation('shinyfiles_no_selection'", fixed = TRUE)
  expect_false(grepl('`aria-label` = as.character(tr("merge_open"))', ui_txt, fixed = TRUE))
  expect_match(server_txt, "build_plot(scope_sel, strain_sel, input$tipo, for_interactive = TRUE)", fixed = TRUE)
  expect_match(server_txt, "bioszen_prepare_editable_plotly_plot(", fixed = TRUE)
  expect_match(server_txt, "bioszen_pdf_page_size_from_pixels(", fixed = TRUE)
  expect_match(server_txt, "bioszen_write_editable_plot_pptx(", fixed = TRUE)
  expect_false(grepl("bioszen_write_plotly_svg_pptx", server_txt, fixed = TRUE))
  expect_false(grepl("bioszen_plot_svg", server_txt, fixed = TRUE))
  expect_false(grepl("current_plotly_svg", server_txt, fixed = TRUE))
  expect_match(
    helpers_txt,
    "ppt_plot <- bioszen_prepare_named_pptx_strokes(plot, linewidth_pt = 1)",
    fixed = TRUE
  )
  expect_match(helpers_txt, "rvg::dml(ggobj = ppt_plot, editable = TRUE)", fixed = TRUE)
  expect_false(grepl("size = input$errbar_size", stacked_txt, fixed = TRUE))
  expect_gte(
    lengths(regmatches(stacked_txt, gregexpr("linewidth = input$errbar_size", stacked_txt, fixed = TRUE))),
    4
  )
  expect_match(server_txt, "output$downloadPlotly_png <- individual_plot_png_download()", fixed = TRUE)
  expect_match(server_txt, "output$downloadPlotly_pdf <- individual_plot_pdf_download()", fixed = TRUE)
  expect_match(server_txt, 'plot_pptx_raw <- tryCatch(', fixed = TRUE)
  expect_match(server_txt, 'plot_pptx_name <- paste0("grafico_", type_label, version_suffix, ".pptx")', fixed = TRUE)
  expect_match(server_txt, 'writeBin(v$plot_pptx_raw, file.path(ver_dir, pptx_name))', fixed = TRUE)
  expect_match(server_txt, 'as.character(length(v$plot_pptx_raw %||% raw(0)))', fixed = TRUE)
  expect_match(server_txt, "write_validated_download_raw <- function", fixed = TRUE)
  expect_gte(
    lengths(regmatches(server_txt, gregexpr("write_validated_download_raw(raw, file", server_txt, fixed = TRUE))),
    7
  )
})

test_that("new download and shinyFiles labels exist in both languages", {
  en <- utils::read.csv(app_test_path("i18n", "translation_en.csv"), stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  es <- utils::read.csv(app_test_path("i18n", "translation_es.csv"), stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  required <- c(
    "download_save_failed", "download_file_description", "shinyfiles_create_folder",
    "shinyfiles_sort_content", "shinyfiles_directories", "shinyfiles_content",
    "shinyfiles_cancel", "shinyfiles_select", "shinyfiles_no_selection", "growth_folder_created",
    "growth_folder_create_error"
  )

  expect_setequal(intersect(required, en$key), required)
  expect_setequal(intersect(required, es$key), required)
  expect_true(all(nzchar(en$en[match(required, en$key)])))
  expect_true(all(nzchar(es$es[match(required, es$key)])))
})
