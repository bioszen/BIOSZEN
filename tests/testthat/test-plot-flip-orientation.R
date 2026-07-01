library(testthat)

read_app_file <- function(...) {
  parts <- as.character(c(...))
  if (length(parts) >= 2 && identical(parts[1:2], c("inst", "app"))) {
    parts <- parts[-c(1, 2)]
  }
  path <- do.call(app_test_path, as.list(parts))
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

test_that("flip orientation control is scoped to supported plot types and metadata wiring", {
  ui_txt <- read_app_file("inst", "app", "ui", "ui_main.R")
  srv_txt <- read_app_file("inst", "app", "server", "server_main.R")

  expect_true(grepl(
    "condition\\s*=\\s*\"\\['Boxplot','Barras','Violin','Apiladas'\\]\\.indexOf\\(input\\.tipo\\) >= 0\"",
    ui_txt,
    perl = TRUE
  ))
  expect_true(grepl(
    "checkboxInput\\(\\s*\"plot_flip\"\\s*,\\s*tr\\(\"plot_flip\"\\)\\s*,\\s*FALSE\\s*\\)",
    ui_txt,
    perl = TRUE
  ))
  expect_true(grepl(
    "uiOutput\\(\\s*\"errbarStatUI\"\\s*\\)",
    ui_txt,
    perl = TRUE
  ))
  expect_true(grepl(
    "output\\$errbarStatUI\\s*<-\\s*renderUI",
    srv_txt,
    perl = TRUE
  ))
  expect_true(grepl(
    "errbar_stat_minmax",
    srv_txt,
    fixed = TRUE
  ))
  expect_true(grepl(
    "default_errorbar_stat_for_plot\\(tipo_sel",
    srv_txt,
    perl = TRUE
  ))
  expect_true(grepl(
    "Campo\\s*=\\s*\"errbar_stat\"",
    srv_txt,
    perl = TRUE
  ))
  expect_true(grepl(
    "default_errorbar_stat_for_plot\\(\\s*input\\$tipo",
    srv_txt,
    perl = TRUE
  ))
  expect_true(grepl(
    "is_norm\\s*<-\\s*isTRUE\\(input\\$doNorm\\)\\s*&&\\s*has_ctrl_selected\\(\\)",
    srv_txt,
    perl = TRUE
  ))

  expect_true(grepl(
    "Campo\\s*=\\s*c\\(\"pt_size\",\\s*\"x_angle\",\\s*\"plot_flip\",\\s*\"x_wrap\",\\s*\"x_wrap_lines\"\\)",
    srv_txt,
    perl = TRUE
  ))
  expect_true(grepl(
    "updateCheckboxInput\\(session,\\s*\"plot_flip\"",
    srv_txt,
    perl = TRUE
  ))
  expect_true(grepl(
    "if \\(isTRUE\\(input\\$plot_flip\\)\\)\\s*\\{\\s*p_stack <- build_plot\\(scope_sel, strain_sel, \"Apiladas\"",
    srv_txt,
    perl = TRUE
  ))
  expect_true(grepl(
    "(?s)build_current_plotly_for_export\\s*<-\\s*function\\([^)]*\\).*identical\\(input\\$tipo, \"Apiladas\"\\).*isTRUE\\(input\\$plot_flip.*p_stack <- build_plot\\(scope_sel, strain_sel, \"Apiladas\"",
    srv_txt,
    perl = TRUE
  ))
})

test_that("flip orientation and error-bar labels exist in both translation files", {
  en <- read_app_file("inst", "app", "i18n", "translation_en.csv")
  es <- read_app_file("inst", "app", "i18n", "translation_es.csv")

  expect_true(grepl("(^|\\n)plot_flip,", en, perl = TRUE))
  expect_true(grepl("(^|\\n)plot_flip,", es, perl = TRUE))
  expect_true(grepl("(^|\\n)errbar_stat_minmax,", en, perl = TRUE))
  expect_true(grepl("(^|\\n)errbar_stat_minmax,", es, perl = TRUE))
})

test_that("stacked significance labels use visible stack parameters", {
  ui_txt <- read_app_file("inst", "app", "ui", "ui_main.R")
  srv_txt <- read_app_file("inst", "app", "server", "server_main.R")

  expect_true(grepl(
    "selectizeInput\\(\\s*'sig_param'",
    ui_txt,
    perl = TRUE
  ))
  expect_true(grepl(
    "(?s)update_selectize_adaptive\\s*<-\\s*function\\([^)]*server\\s*=\\s*NULL.*selected\\s*=\\s*normalized_selected",
    srv_txt,
    perl = TRUE
  ))
  expect_true(grepl(
    "(?s)resolve_sig_stack_params\\s*<-\\s*function\\(selected\\s*=\\s*NULL\\).*scoped_plot_df\\(\\).*resolve_stack_params\\(df\\s*=\\s*scope_df,\\s*selected\\s*=\\s*selected\\)",
    srv_txt,
    perl = TRUE
  ))
  expect_true(grepl(
    "(?s)update_selectize_adaptive\\(\\s*\"sig_param\".*choices\\s*=\\s*params.*selected\\s*=\\s*current.*server\\s*=\\s*FALSE",
    srv_txt,
    perl = TRUE
  ))
  expect_true(grepl(
    "select_sig_stack_param\\(input\\$sig_param,\\s*resolve_sig_stack_params\\(\\)\\)",
    srv_txt,
    perl = TRUE
  ))
})

test_that("distribution plot builders toggle CoordFlip only when requested", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")
  skip_if_not_installed("plotly")

  suppressPackageStartupMessages(library(ggplot2))
  suppressPackageStartupMessages(library(dplyr))
  suppressPackageStartupMessages(library(tidyr))
  suppressPackageStartupMessages(library(rlang))

  `%||%` <- function(x, y) if (is.null(x)) y else x

  make_dist_ctx <- function(flip = FALSE, errbar_stat = "SD") {
    df <- data.frame(
      Label = c("A", "A", "B", "B"),
      Strain = c("S1", "S1", "S2", "S2"),
      Media = c("M1", "M1", "M2", "M2"),
      ParamA = c(1.2, 1.8, 2.5, 2.9),
      stringsAsFactors = FALSE
    )
    list(
      scope = "Combinado",
      scope_df = df,
      param_sel = "ParamA",
      input = list(
        x_wrap = FALSE,
        x_wrap_lines = 2,
        x_angle = 0,
        colorMode = "Default",
        pt_jit = 0,
        pt_size = 2,
        errbar_size = 0.6,
        plotTitle = "Demo",
        base_size = 12,
        labelMode = FALSE,
        plot_h = 500,
        plot_flip = flip,
        errbar_stat = errbar_stat,
        box_w = 0.7,
        violin_width = 0.5,
        violin_linewidth = 0.6
      ),
      msg_no_data_sel = "No data",
      ylab = "Value",
      ymax = 5,
      ybreak = 1,
      fs_title = 12,
      fs_axis = 10,
      axis_size = 0.8,
      colourMode = "Default",
      box_coef = 1e6,
      for_interactive = FALSE,
      box_stats = NULL,
      wrap_label = function(x, lines = 2) x,
      palette_for_labels = function(df_labels, levels) {
        setNames(rep("#1f77b4", length(levels)), levels)
      },
      palette_for_levels = function(levels) {
        setNames(rep("#1f77b4", length(levels)), levels)
      },
      get_x_angle = function(n, angle_input) if (is.na(angle_input)) 0 else angle_input,
      get_bottom_margin = function(angle, wrap = FALSE, lines = 2) 30,
      margin_adj = function(top, right, bottom, left) ggplot2::margin(top, right, bottom, left, unit = "pt"),
      apply_sig_layers = function(p, ...) p,
      apply_square_legend_right = function(p, ...) p,
      legend_right_enabled = function(color_mode) FALSE,
      add_black_t_errorbar = function(p, ...) p,
      add_whisker_caps = function(p, ...) p,
      downsample_points_by_group = function(df, group_col, cap_total = 7000L) df,
      is_summary_mode = function() FALSE,
      resolve_prefixed_param_col = function(df, prefix, param_name) NULL
    )
  }

  source(app_test_path("helpers.R"))
  source(app_test_path( "graficos", "graficos_barras.R"))
  source(app_test_path( "graficos", "graficos_boxplot.R"))
  source(app_test_path( "graficos", "graficos_violin.R"))

  p_barras_v <- build_barras_plot_impl(make_dist_ctx(flip = FALSE))
  p_barras_h <- build_barras_plot_impl(make_dist_ctx(flip = TRUE))
  expect_false(inherits(p_barras_v$coordinates, "CoordFlip"))
  expect_true(inherits(p_barras_h$coordinates, "CoordFlip"))

  black_calls <- 0L
  whisker_calls <- 0L
  whisker_stats <- NULL
  box_ctx <- make_dist_ctx(flip = FALSE)
  box_ctx$add_black_t_errorbar <- function(p, ...) {
    black_calls <<- black_calls + 1L
    p
  }
  box_ctx$add_whisker_caps <- function(p, stats_df, ...) {
    whisker_calls <<- whisker_calls + 1L
    whisker_stats <<- stats_df
    p
  }
  p_box_v <- build_boxplot_plot_impl(box_ctx)
  p_box_h <- build_boxplot_plot_impl(make_dist_ctx(flip = TRUE))
  expect_false(inherits(p_box_v$coordinates, "CoordFlip"))
  expect_true(inherits(p_box_h$coordinates, "CoordFlip"))
  expect_equal(black_calls, 0L)
  expect_equal(whisker_calls, 1L)
  expect_true(all(c("Mean", "SD", "lower", "upper") %in% names(whisker_stats)))
  expect_equal(whisker_stats$lower, whisker_stats$Mean - whisker_stats$SD)
  expect_equal(whisker_stats$upper, whisker_stats$Mean + whisker_stats$SD)
  expect_error(ggplot2::ggplot_build(p_box_v), NA)
  expect_error(plotly::ggplotly(p_box_v, originalData = TRUE), NA)

  minmax_stats <- NULL
  minmax_ctx <- make_dist_ctx(flip = FALSE, errbar_stat = "MINMAX")
  minmax_ctx$add_whisker_caps <- function(p, stats_df, ...) {
    minmax_stats <<- stats_df
    p
  }
  expect_error(build_boxplot_plot_impl(minmax_ctx), NA)
  expect_equal(minmax_stats$lower, minmax_stats$min_val)
  expect_equal(minmax_stats$upper, minmax_stats$max_val)

  default_stats <- NULL
  default_ctx <- make_dist_ctx(flip = FALSE, errbar_stat = NULL)
  default_ctx$add_whisker_caps <- function(p, stats_df, ...) {
    default_stats <<- stats_df
    p
  }
  expect_error(build_boxplot_plot_impl(default_ctx), NA)
  expect_equal(default_stats$lower, default_stats$min_val)
  expect_equal(default_stats$upper, default_stats$max_val)

  p_violin_v <- build_violin_plot_impl(make_dist_ctx(flip = FALSE))
  p_violin_h <- build_violin_plot_impl(make_dist_ctx(flip = TRUE))
  expect_false(inherits(p_violin_v$coordinates, "CoordFlip"))
  expect_true(inherits(p_violin_h$coordinates, "CoordFlip"))

  sig_layers <- function(p, ...) {
    p + geom_text(
      data = data.frame(x = 1, y = 4.5, label = "*", .sig_layer = TRUE),
      inherit.aes = FALSE,
      aes(x = x, y = y, label = label),
      size = 4
    )
  }
  legend_called <- FALSE
  flipped_elements_ctx <- make_dist_ctx(flip = TRUE)
  flipped_elements_ctx$apply_sig_layers <- sig_layers
  flipped_elements_ctx$legend_right_enabled <- function(color_mode) TRUE
  flipped_elements_ctx$apply_square_legend_right <- function(p, ...) {
    legend_called <<- TRUE
    p + theme(legend.position = "right")
  }
  p_elements <- build_boxplot_plot_impl(flipped_elements_ctx)
  expect_true(inherits(p_elements$coordinates, "CoordFlip"))
  expect_true(legend_called)
  expect_true(any(vapply(p_elements$layers, function(layer) {
    is.data.frame(layer$data) &&
      ".sig_layer" %in% names(layer$data) &&
      any(layer$data$.sig_layer %in% TRUE)
  }, logical(1))))
  expect_identical(p_elements$theme$legend.position, "right")
  expect_error(ggplot2::ggplot_build(p_elements), NA)
  expect_error(plotly::ggplotly(p_elements, originalData = TRUE), NA)
})

test_that("stacked ggplot builder toggles CoordFlip only when requested", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")

  suppressPackageStartupMessages(library(ggplot2))
  suppressPackageStartupMessages(library(dplyr))
  suppressPackageStartupMessages(library(tidyr))
  suppressPackageStartupMessages(library(rlang))

  `%||%` <- function(x, y) if (is.null(x)) y else x

  make_stacked_ctx <- function(flip = FALSE, sig_param = NULL) {
    df <- data.frame(
      Label = c("A", "A", "B", "B"),
      Strain = c("S1", "S1", "S2", "S2"),
      Media = c("M1", "M1", "M2", "M2"),
      ParamA = c(1, 2, 3, 4),
      ParamB = c(2, 1, 2, 1),
      stringsAsFactors = FALSE
    )
    list(
      scope = "Combinado",
      scope_df = df,
      input = list(
        stackParams = c("ParamA", "ParamB"),
        orderStack = "",
        labelMode = FALSE,
        x_wrap = FALSE,
        x_wrap_lines = 2,
        x_angle = 0,
        fs_axis = 10,
        stack_outline_only = FALSE,
        showErrBars = FALSE,
        errbar_param_color = FALSE,
        errbar_size = 0.6,
        ymax = 8,
        ybreak = 1,
        base_size = 12,
        plotTitle = "Stacked",
        yLab = "",
        plot_h = 500,
        sig_param = sig_param,
        plot_flip = flip
      ),
      lang = "en",
      ps = list(Y_Title = "Value"),
      fs_title = 12,
      fs_axis = 10,
      fs_legend = 9,
      axis_size = 0.8,
      tr_text = function(key, lang) key,
      is_summary_mode = function() FALSE,
      resolve_prefixed_param_col = function(df, prefix, param_name) NULL,
      wrap_label = function(x, lines = 2) x,
      get_x_angle = function(n, angle_input) if (is.na(angle_input)) 0 else angle_input,
      get_bottom_margin = function(angle, wrap = FALSE, lines = 2) 30,
      palette_for_levels = function(levels) {
        setNames(rep("#1f77b4", length(levels)), levels)
      },
      margin_adj = function(top, right, bottom, left) ggplot2::margin(top, right, bottom, left, unit = "pt"),
      apply_sig_layers = function(p, group_tops = NULL, default_param = NULL, ...) {
        attr(p, "sig_default_param") <- default_param
        attr(p, "sig_label_params") <- as.character(unique(group_tops$param))
        p
      }
    )
  }

  source(app_test_path("helpers.R"))
  source(app_test_path( "graficos", "graficos_apilados.R"))

  p_stack_v <- build_apiladas_plot_impl(make_stacked_ctx(flip = FALSE))
  p_stack_h <- build_apiladas_plot_impl(make_stacked_ctx(flip = TRUE))
  p_stack_sig <- build_apiladas_plot_impl(make_stacked_ctx(flip = FALSE, sig_param = "ParamB"))
  expect_false(inherits(p_stack_v$coordinates, "CoordFlip"))
  expect_true(inherits(p_stack_h$coordinates, "CoordFlip"))
  expect_identical(attr(p_stack_sig, "sig_default_param"), "ParamB")
  expect_setequal(attr(p_stack_sig, "sig_label_params"), c("ParamA", "ParamB"))
  expect_error(ggplot2::ggplot_build(p_stack_h), NA)
})
