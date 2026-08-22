library(testthat)

load_ppt_legend_helpers <- function() {
  env <- new.env(parent = globalenv())
  env$`%||%` <- function(x, y) if (is.null(x)) y else x
  env$BIOSZEN_CSS_DPI <- 96
  sys.source(app_test_path("helpers.R"), envir = env)
  env
}

make_discrete_legend_plot <- function(plot_type, visible = TRUE) {
  values <- data.frame(
    group = factor(rep(c("Control", "Treatment"), each = 3)),
    value = c(2, 2.2, 2.1, 3, 3.2, 3.1)
  )
  base <- ggplot2::ggplot(
    values,
    ggplot2::aes(group, value, fill = group)
  )
  layer <- if (identical(plot_type, "Boxplot")) {
    ggplot2::geom_boxplot(
      colour = "black",
      alpha = 0.5,
      show.legend = visible,
      key_glyph = "rect"
    )
  } else {
    ggplot2::geom_col(
      colour = "black",
      alpha = 0.7,
      show.legend = visible,
      key_glyph = "rect"
    )
  }

  base +
    layer +
    ggplot2::scale_fill_manual(
      values = c(Control = "#4A90E2", Treatment = "#E15759")
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = if (visible) "right" else "none")
}

test_that("bar and boxplot PPT legends use compact separated outlined squares", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("scales")

  env <- load_ppt_legend_helpers()

  for (plot_type in c("Boxplot", "Barras")) {
    source_plot <- make_discrete_legend_plot(plot_type, visible = TRUE)
    expected_alpha <- if (identical(plot_type, "Boxplot")) 0.5 else 0.7
    source_key <- source_plot$layers[[1]]$geom$draw_key
    prepared <- env$bioszen_prepare_discrete_ppt_legend(source_plot, plot_type)
    prepared_theme <- ggplot2::theme_get() + prepared$theme
    key <- prepared$layers[[1]]$geom$draw_key(
      data.frame(
        fill = "#4A90E2",
        colour = "#000000",
        alpha = expected_alpha,
        linewidth = 0.6,
        linetype = 1
      ),
      params = list(),
      size = 11
    )

    expect_identical(source_plot$layers[[1]]$geom$draw_key, source_key)
    expect_false(identical(prepared$layers[[1]]$geom$draw_key, source_key))
    expect_error(ggplot2::ggplot_build(prepared), NA)
    expect_s3_class(key, "rect")
    expect_equal(grid::convertWidth(key$width, "pt", valueOnly = TRUE), 10.8)
    expect_equal(grid::convertHeight(key$height, "pt", valueOnly = TRUE), 10.8)
    expect_identical(key$gp$col, "#000000")
    expect_equal(key$gp$lwd, 1.28)
    expect_identical(key$gp$fill, scales::alpha("#4A90E2", expected_alpha))
    expect_equal(
      grid::convertUnit(prepared_theme$legend.key.width, "pt", valueOnly = TRUE),
      10.8
    )
    expect_equal(
      grid::convertUnit(prepared_theme$legend.key.height, "pt", valueOnly = TRUE),
      10.8
    )
    expect_equal(
      grid::convertUnit(prepared_theme$legend.key.spacing.y, "pt", valueOnly = TRUE),
      14.5
    )
    expect_equal(ggplot2::calc_element("legend.text", prepared_theme)$hjust, 0)
    expect_equal(
      grid::convertUnit(
        ggplot2::calc_element("legend.text", prepared_theme)$margin[4],
        "pt",
        valueOnly = TRUE
      ),
      8
    )
  }
})

test_that("PPT legend correction is inactive when the legend option is off", {
  skip_if_not_installed("ggplot2")

  env <- load_ppt_legend_helpers()

  for (plot_type in c("Boxplot", "Barras")) {
    source_plot <- make_discrete_legend_plot(plot_type, visible = FALSE)
    prepared <- env$bioszen_prepare_discrete_ppt_legend(source_plot, plot_type)

    expect_identical(prepared, source_plot)
  }
})

test_that("direct and packaged PPT exports use the same prepared writer", {
  helpers_text <- paste(
    readLines(app_test_path("helpers.R"), warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )
  server_text <- paste(
    readLines(app_test_path("server", "server_main.R"), warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )

  expect_match(
    helpers_text,
    "out <- bioszen_prepare_discrete_ppt_legend(",
    fixed = TRUE
  )
  writer_call <- "writer = function(tmp) write_current_plot_pptx(tmp, width = width, height = height)"
  matches <- gregexpr(writer_call, server_text, fixed = TRUE)[[1]]
  expect_gte(sum(matches > 0), 2L)
})
