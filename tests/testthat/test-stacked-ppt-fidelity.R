library(testthat)

test_that("stacked PPT matching preserves source data and aligns Plotly appearance", {
  skip_if_not_installed("ggplot2")

  env <- new.env(parent = globalenv())
  env$`%||%` <- function(x, y) if (is.null(x)) y else x
  env$BIOSZEN_CSS_DPI <- 96
  sys.source(app_test_path("helpers.R"), envir = env)

  stack_levels <- c("First", "Second")
  original_colours <- c(First = "#3B82F6", Second = "#22C55E")
  softened_colours <- c(First = "#6CA1F8", Second = "#59D284")
  values <- data.frame(
    group = rep(c("A", "B"), each = 2),
    parameter = factor(rep(stack_levels, 2), levels = stack_levels),
    value = c(2, 5, 3, 7),
    low = c(1.8, 4.7, 2.7, 6.5),
    high = c(2.2, 5.3, 3.3, 7.5)
  )
  source_plot <- ggplot2::ggplot(
    values,
    ggplot2::aes(group, value, fill = parameter)
  ) +
    ggplot2::geom_col(
      position = "stack",
      width = 0.7,
      alpha = 0.85,
      colour = "black",
      linewidth = 0.6
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = low, ymax = high),
      linewidth = 0.4,
      width = 0.2
    ) +
    ggplot2::scale_fill_manual(values = softened_colours) +
    ggplot2::labs(title = "Stacked preview") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0),
      legend.position = "right",
      legend.justification = "center"
    )

  prepared <- env$bioszen_prepare_stacked_ppt_plot(
    source_plot,
    stack_levels = stack_levels,
    colours = original_colours
  )
  source_build <- ggplot2::ggplot_build(source_plot)$data[[1]]
  prepared_build <- ggplot2::ggplot_build(prepared)$data[[1]]

  expect_equal(source_plot$layers[[1]]$aes_params$alpha, 0.85)
  expect_equal(source_plot$layers[[1]]$aes_params$width, 0.7)
  expect_equal(prepared$layers[[1]]$aes_params$alpha, 1)
  expect_equal(prepared$layers[[1]]$geom_params$width, 0.8)
  expect_equal(prepared$layers[[1]]$aes_params$linewidth, 72 / 96)
  expect_true(all(abs((prepared_build$xmax - prepared_build$xmin) - 0.8) < 1e-8))
  expect_setequal(unique(prepared_build$fill), unname(original_colours))
  expect_setequal(unique(source_build$fill), unname(softened_colours))

  first_group <- prepared_build[prepared_build$x == min(prepared_build$x), , drop = FALSE]
  first_group <- first_group[order(first_group$ymin), , drop = FALSE]
  expect_identical(first_group$fill[[1]], unname(original_colours[["First"]]))
  expect_equal(first_group$ymin[[1]], 0)
  expect_identical(prepared$layers[[2]]$aes_params, source_plot$layers[[2]]$aes_params)
  expect_identical(prepared$layers[[2]]$geom_params, source_plot$layers[[2]]$geom_params)

  prepared_theme <- ggplot2::theme_get() + prepared$theme
  expect_equal(ggplot2::calc_element("plot.title", prepared_theme)$hjust, 0.5)
  expect_identical(prepared_theme$plot.title.position, "plot")
  expect_identical(prepared_theme$legend.position, "right")
  expect_equal(prepared_theme$legend.justification, c(0, 1))
  expect_identical(prepared_theme$legend.box.just, "top")
  expect_equal(
    grid::convertUnit(prepared_theme$legend.box.spacing, "pt", valueOnly = TRUE),
    18
  )
  expect_equal(
    grid::convertUnit(prepared_theme$legend.key.size, "pt", valueOnly = TRUE),
    11
  )
  expect_equal(
    grid::convertUnit(prepared_theme$legend.key.width, "pt", valueOnly = TRUE),
    11
  )
  expect_equal(
    grid::convertUnit(prepared_theme$legend.key.height, "pt", valueOnly = TRUE),
    11
  )
  expect_equal(
    grid::convertUnit(prepared_theme$legend.key.spacing.x, "pt", valueOnly = TRUE),
    4
  )
  expect_equal(
    grid::convertUnit(prepared_theme$legend.key.spacing.y, "pt", valueOnly = TRUE),
    7
  )
  expect_equal(ggplot2::calc_element("legend.text", prepared_theme)$hjust, 0)
  expect_equal(
    grid::convertUnit(
      ggplot2::calc_element("legend.text", prepared_theme)$margin[4],
      "pt",
      valueOnly = TRUE
    ),
    6
  )
  fill_guide <- prepared$scales$get_scales("fill")$guide
  expect_s3_class(fill_guide, "GuideLegend")
  expect_equal(
    grid::convertUnit(
      fill_guide$params$theme$legend.key.width,
      "pt",
      valueOnly = TRUE
    ),
    11
  )
  expect_equal(
    grid::convertUnit(
      fill_guide$params$theme$legend.key.height,
      "pt",
      valueOnly = TRUE
    ),
    11
  )
  expect_equal(fill_guide$params$theme$legend.text$hjust, 0)
  expect_identical(fill_guide$params$theme$legend.text.position, "right")
  expect_equal(
    grid::convertUnit(prepared_theme$legend.margin[1], "pt", valueOnly = TRUE),
    12
  )

  x_scale <- prepared$scales$get_scales("x")
  expect_s3_class(x_scale, "ScaleDiscretePosition")
  expect_equal(x_scale$expand, ggplot2::expansion(add = 0.5))
})

test_that("stacked PPT matching shortens only Plotly-style horizontal SD caps", {
  skip_if_not_installed("ggplot2")

  env <- new.env(parent = globalenv())
  env$`%||%` <- function(x, y) if (is.null(x)) y else x
  env$BIOSZEN_CSS_DPI <- 96
  sys.source(app_test_path("helpers.R"), envir = env)

  values <- data.frame(
    group = factor(c("A", "B")),
    parameter = factor(c("First", "First"), levels = "First"),
    value = c(2, 3),
    xnum = c(1, 2),
    ystart = c(2, 3),
    yend = c(2.4, 3.5)
  )
  source_plot <- ggplot2::ggplot(
    values,
    ggplot2::aes(group, value, fill = parameter)
  ) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_segment(
      data = values,
      ggplot2::aes(x = xnum, xend = xnum, y = ystart, yend = yend),
      linewidth = 0.5
    ) +
    ggplot2::geom_segment(
      data = values,
      ggplot2::aes(
        x = xnum - 0.35,
        xend = xnum + 0.35,
        y = yend,
        yend = yend
      ),
      linewidth = 0.5
    )

  prepared <- env$bioszen_prepare_stacked_ppt_plot(
    source_plot,
    stack_levels = "First",
    colours = c(First = "#3B82F6")
  )

  expect_false(".bioszen_cap_xmin" %in% names(source_plot$layers[[3]]$data))
  expect_false(".bioszen_cap_xmin" %in% names(prepared$layers[[2]]$data))
  expect_true(".bioszen_cap_xmin" %in% names(prepared$layers[[3]]$data))
  expected_cap_width <- 2 * (20 * 2 / (1000 - 72 - 115))
  expect_equal(
    prepared$layers[[3]]$data$.bioszen_cap_xmax -
      prepared$layers[[3]]$data$.bioszen_cap_xmin,
    rep(expected_cap_width, 2)
  )
  expected_linewidth <- 0.5 * 1.6 * (72 / 96)
  expect_equal(prepared$layers[[2]]$aes_params$linewidth, expected_linewidth)
  expect_equal(prepared$layers[[3]]$aes_params$linewidth, expected_linewidth)

  narrower <- env$bioszen_prepare_stacked_ppt_plot(
    source_plot,
    stack_levels = "First",
    colours = c(First = "#3B82F6"),
    content_width_px = 800
  )
  expect_gt(
    narrower$layers[[3]]$data$.bioszen_cap_xmax[[1]] -
      narrower$layers[[3]]$data$.bioszen_cap_xmin[[1]],
    prepared$layers[[3]]$data$.bioszen_cap_xmax[[1]] -
      prepared$layers[[3]]$data$.bioszen_cap_xmin[[1]]
  )
  expect_identical(rlang::get_expr(prepared$layers[[3]]$mapping$x), quote(.bioszen_cap_xmin))
  expect_identical(rlang::get_expr(prepared$layers[[3]]$mapping$xend), quote(.bioszen_cap_xmax))
})

test_that("stacked PPT matching is data-driven for flipped charts", {
  skip_if_not_installed("ggplot2")

  env <- new.env(parent = globalenv())
  env$`%||%` <- function(x, y) if (is.null(x)) y else x
  env$BIOSZEN_CSS_DPI <- 96
  sys.source(app_test_path("helpers.R"), envir = env)

  values <- data.frame(
    group = factor(c("A", "B", "C")),
    parameter = factor(rep("First", 3), levels = "First"),
    value = c(2, 3, 4),
    xnum = c(1, 2, 3),
    yend = c(2.4, 3.5, 4.3)
  )
  source_plot <- ggplot2::ggplot(
    values,
    ggplot2::aes(group, value, fill = parameter)
  ) +
    ggplot2::geom_col(width = 0.7, colour = "black") +
    ggplot2::geom_segment(
      data = values,
      ggplot2::aes(
        x = xnum - 0.35,
        xend = xnum + 0.35,
        y = yend,
        yend = yend
      ),
      linewidth = 0.5
    ) +
    ggplot2::coord_flip()

  prepared <- env$bioszen_prepare_stacked_ppt_plot(
    source_plot,
    stack_levels = "First",
    colours = c(First = "#3B82F6"),
    content_width_px = 1000,
    content_height_px = 700,
    flipped = TRUE
  )
  expected_cap_width <- 2 * (20 * 3 / (700 - 68 - 52))
  prepared_theme <- ggplot2::theme_get() + prepared$theme

  expect_true(inherits(prepared$coordinates, "CoordFlip"))
  expect_equal(
    prepared$layers[[2]]$data$.bioszen_cap_xmax -
      prepared$layers[[2]]$data$.bioszen_cap_xmin,
    rep(expected_cap_width, 3)
  )
  expect_equal(
    grid::convertUnit(prepared_theme$legend.key.spacing.y, "pt", valueOnly = TRUE),
    7
  )
  expect_setequal(
    unique(ggplot2::ggplot_build(prepared)$data[[1]]$fill),
    "#3B82F6"
  )
})

test_that("stacked editable PPT uses the Plotly stacked-panel geometry", {
  skip_if_not_installed("ggplot2")

  env <- new.env(parent = globalenv())
  env$`%||%` <- function(x, y) if (is.null(x)) y else x
  env$BIOSZEN_CSS_DPI <- 96
  sys.source(app_test_path("helpers.R"), envir = env)

  plot <- ggplot2::ggplot(
    data.frame(group = c("A", "B"), value = c(2, 3)),
    ggplot2::aes(group, value)
  ) +
    ggplot2::geom_col() +
    ggplot2::theme_classic()
  prepared <- env$bioszen_prepare_editable_plotly_plot(
    plot,
    plot_type = "Apiladas",
    content_width_px = 1000,
    content_height_px = 700,
    slide_width_px = 1000,
    slide_height_px = 700
  )
  bounds <- env$bioszen_measure_plot_panel(prepared, 1000 / 96, 700 / 96)

  expect_false(is.null(bounds))
  expect_equal(unname(bounds[["left"]] * 96), 72, tolerance = 6)
  expect_equal(unname(bounds[["right"]] * 96), 885, tolerance = 6)
  expect_equal(unname(700 - bounds[["top"]] * 96), 68, tolerance = 6)
  expect_equal(unname(bounds[["bottom"]] * 96), 52, tolerance = 6)
})

test_that("stacked PPT correction is isolated from every other export path", {
  server_text <- paste(
    readLines(app_test_path("server", "server_main.R"), warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )

  expect_match(
    server_text,
    paste0(
      "if \\(identical\\(input\\$tipo %\\|\\|% \\\"\\\", \\\"Apiladas\\\"\\)\\) \\{",
      "[\\s\\S]{0,800}bioszen_prepare_stacked_ppt_plot\\("
    ),
    perl = TRUE
  )
  expect_match(
    server_text,
    "flipped = isTRUE(input$plot_flip %||% FALSE)",
    fixed = TRUE
  )
  expect_equal(
    lengths(regmatches(
      server_text,
      gregexpr("bioszen_prepare_stacked_ppt_plot(", server_text, fixed = TRUE)
    )),
    1L
  )
  expect_match(server_text, "write_current_plot_png <- function", fixed = TRUE)
  expect_match(server_text, "write_current_plot_pdf <- function", fixed = TRUE)
  expect_match(
    server_text,
    "(input$tipo %||% \"\") %in% c(\"Curvas\", \"Apiladas\")",
    fixed = TRUE
  )
})

test_that("stacked editable PPT writes square legend swatches with label spacing", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("officer")
  skip_if_not_installed("rvg")
  skip_if_not_installed("xml2")

  cache_dir <- tempfile("bioszen-gdtools-")
  dir.create(cache_dir, recursive = TRUE)
  previous_cache <- Sys.getenv("GDTOOLS_CACHE_DIR", unset = NA_character_)
  on.exit({
    if (is.na(previous_cache)) {
      Sys.unsetenv("GDTOOLS_CACHE_DIR")
    } else {
      Sys.setenv(GDTOOLS_CACHE_DIR = previous_cache)
    }
    unlink(cache_dir, recursive = TRUE, force = TRUE)
  }, add = TRUE)
  Sys.setenv(GDTOOLS_CACHE_DIR = cache_dir)

  env <- new.env(parent = globalenv())
  env$`%||%` <- function(x, y) if (is.null(x)) y else x
  env$BIOSZEN_CSS_DPI <- 96
  sys.source(app_test_path("helpers.R"), envir = env)

  stack_levels <- c("uMax", "ODmax", "AUC", "OD")
  colours <- c(
    uMax = "#3B82F6",
    ODmax = "#A855F7",
    AUC = "#22A6B3",
    OD = "#65C466"
  )
  values <- data.frame(
    group = rep(c("Control", "PQ"), each = length(stack_levels)),
    parameter = factor(rep(stack_levels, 2), levels = stack_levels),
    value = c(0.4, 0.8, 38, 1.2, 0.3, 0.7, 28, 1.1)
  )
  plot <- ggplot2::ggplot(
    values,
    ggplot2::aes(group, value, fill = parameter)
  ) +
    ggplot2::geom_col(position = "stack", colour = "black") +
    ggplot2::scale_fill_manual(values = colours) +
    ggplot2::theme_classic()
  plot <- env$bioszen_prepare_stacked_ppt_plot(
    plot,
    stack_levels = stack_levels,
    colours = colours,
    content_width_px = 1000,
    content_height_px = 700
  )
  plot <- env$bioszen_prepare_editable_plotly_plot(
    plot,
    plot_type = "Apiladas",
    content_width_px = 1000,
    content_height_px = 700,
    slide_width_px = 1000,
    slide_height_px = 700
  )

  target <- tempfile(fileext = ".pptx")
  unpacked <- tempfile("bioszen-stacked-ppt-")
  on.exit(unlink(c(target, unpacked), recursive = TRUE, force = TRUE), add = TRUE)
  env$bioszen_write_editable_plot_pptx(
    target,
    plot,
    width_px = 1000,
    height_px = 700,
    slide_width_px = 1000,
    slide_height_px = 700
  )
  dir.create(unpacked, recursive = TRUE)
  utils::unzip(target, exdir = unpacked)
  slide <- xml2::read_xml(file.path(unpacked, "ppt", "slides", "slide1.xml"))
  slide_ns <- xml2::xml_ns(slide)

  key_shapes <- xml2::xml_find_all(
    slide,
    paste0(
      ".//p:sp[p:spPr/a:solidFill/a:srgbClr[",
      "translate(@val, 'abcdef', 'ABCDEF')='3B82F6']]"
    ),
    slide_ns
  )
  key_x <- as.numeric(xml2::xml_attr(
    xml2::xml_find_first(key_shapes, ".//p:spPr/a:xfrm/a:off", slide_ns),
    "x"
  ))
  legend_key <- key_shapes[[which.max(key_x)]]
  key_off <- xml2::xml_find_first(legend_key, ".//p:spPr/a:xfrm/a:off", slide_ns)
  key_ext <- xml2::xml_find_first(legend_key, ".//p:spPr/a:xfrm/a:ext", slide_ns)
  key_right <- as.numeric(xml2::xml_attr(key_off, "x")) +
    as.numeric(xml2::xml_attr(key_ext, "cx"))
  label_shape <- xml2::xml_find_first(
    slide,
    ".//a:t[text()='uMax']/ancestor::p:sp[1]",
    slide_ns
  )
  label_off <- xml2::xml_find_first(label_shape, ".//p:spPr/a:xfrm/a:off", slide_ns)
  label_gap_pt <- (
    as.numeric(xml2::xml_attr(label_off, "x")) - key_right
  ) / 12700

  key_width <- as.numeric(xml2::xml_attr(key_ext, "cx"))
  key_height <- as.numeric(xml2::xml_attr(key_ext, "cy"))
  expect_equal(key_width, key_height, tolerance = 1)
  expect_gte(key_width / 12700, 8)
  expect_gte(label_gap_pt, 4)
})
