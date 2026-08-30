library(testthat)

load_critical_plot_env <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)
    suppressPackageStartupMessages({
      library(dplyr)
      library(ggplot2)
      library(rlang)
      library(shiny)
      library(tidyr)
    })
    cached <<- app_test_source_env(
      paths = c(
        app_test_path("helpers.R"),
        app_test_path("stats", "stats_upgrades.R"),
        app_test_path("stats", "stats_correlation.R"),
        app_test_path("graficos", "graficos_correlacion.R"),
        app_test_path("graficos", "graficos_barras.R"),
        app_test_path("graficos", "graficos_violin.R"),
        app_test_path("graficos", "graficos_curvas.R")
      ),
      required = c(
        "correlation_pair_with_p",
        "build_correlation_plot_impl",
        "build_barras_plot_impl",
        "build_violin_plot_impl",
        "build_curvas_plot_impl"
      )
    )
    cached
  }
})

make_distribution_edge_ctx <- function(values = c(1, 2, 3, 4),
                                       labels = c("A", "A", "B", "B"),
                                       errbar_stat = "SD",
                                       summary_mode = FALSE) {
  n <- length(values)
  labels <- rep(labels, length.out = n)
  df <- data.frame(
    Label = labels,
    Strain = ifelse(labels == "A", "S1", "S2"),
    Media = ifelse(labels == "A", "M1", "M2"),
    ParamA = values,
    SD_ParamA = ifelse(labels == "A", 2, 4),
    N_ParamA = 4,
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
      plotTitle = "Edge-case plot",
      base_size = 12,
      labelMode = FALSE,
      plot_h = 500,
      plot_flip = FALSE,
      errbar_stat = errbar_stat,
      violin_width = 0.5,
      violin_linewidth = 0.6,
      violin_inner = "points"
    ),
    msg_no_data_sel = "No finite data",
    ylab = "Value",
    ymax = 30,
    ybreak = 5,
    fs_title = 12,
    fs_axis = 10,
    axis_size = 0.8,
    colourMode = "Default",
    for_interactive = FALSE,
    box_stats = NULL,
    wrap_label = function(x, lines = 2) x,
    palette_for_labels = function(df_labels, levels) {
      stats::setNames(rep("#1f77b4", length(levels)), levels)
    },
    palette_for_levels = function(levels) {
      stats::setNames(rep("#1f77b4", length(levels)), levels)
    },
    get_x_angle = function(n, angle_input) if (is.na(angle_input)) 0 else angle_input,
    get_bottom_margin = function(angle, wrap = FALSE, lines = 2) 30,
    margin_adj = function(top, right, bottom, left) {
      ggplot2::margin(top, right, bottom, left, unit = "pt")
    },
    apply_sig_layers = function(p, ...) p,
    apply_square_legend_right = function(p, ...) p,
    legend_right_enabled = function(color_mode) FALSE,
    add_black_t_errorbar = function(p, ...) p,
    downsample_points_by_group = function(df, group_col, cap_total = 7000L) df,
    is_summary_mode = function() isTRUE(summary_mode),
    resolve_prefixed_param_col = function(df, prefix, param_name) {
      candidate <- paste0(prefix, param_name)
      if (candidate %in% names(df)) candidate else NULL
    }
  )
}

test_that("correlation calculations execute with finite-pair filtering and known results", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("shiny")
  env <- load_critical_plot_env()

  pair <- env$correlation_pair_with_p(
    x = c(1:6, NA, Inf),
    y = c(3, 5, 7, 9, 11, 13, 99, -1),
    method = "pearson"
  )
  expect_equal(pair$n, 6L)
  expect_equal(pair$r, 1, tolerance = 1e-12)
  expect_lte(pair$p.value, 1e-10)

  constant <- env$correlation_pair_with_p(
    x = rep(1, 5),
    y = 1:5,
    method = "pearson"
  )
  expect_equal(constant$n, 5L)
  expect_true(is.na(constant$r))
  expect_true(is.na(constant$p.value))

  df <- data.frame(
    Strain = rep("S1", 10),
    Media = rep(paste0("M", 1:5), each = 2),
    ParamX = rep(1:5, each = 2),
    ParamY = rep(2 * (1:5) + 1, each = 2),
    stringsAsFactors = FALSE
  )
  input <- list(
    corr_param_x = "ParamX",
    corr_param_y = "ParamY",
    corr_norm_target = "both",
    doNorm = FALSE,
    corr_method = "pearson",
    corr_show_line = TRUE,
    corr_show_ci = TRUE,
    corr_show_eq = TRUE,
    corr_show_r = TRUE,
    corr_show_p = TRUE,
    corr_show_r2 = TRUE,
    corr_show_labels = TRUE,
    corr_label_size = 3,
    corr_ci_level = 0.95,
    corr_ci_style = "band",
    xmin_corr = 0,
    xmax_corr = 6,
    ymin_corr = 0,
    ymax_corr = 12,
    xbreak_corr = 1,
    ybreak_corr = 2,
    corr_xlab = "",
    corr_ylab = "",
    plotTitle = "Known correlation",
    base_size = 12,
    fs_title = 12,
    fs_axis = 10,
    axis_line_size = 0.7,
    labelMode = FALSE,
    app_lang = "en"
  )

  plot <- env$build_correlation_plot_impl(
    scope = "Por Cepa",
    scope_df = df,
    input = input,
    lang = "en",
    has_ctrl_selected = function() FALSE,
    corr_adv_last_pair = function() NULL,
    tr_text = function(key, lang) key,
    margin_adj = function(top, right, bottom, left) {
      ggplot2::margin(top, right, bottom, left, unit = "pt")
    }
  )

  expect_s3_class(plot, "ggplot")
  expect_equal(plot$data$X, 1:5)
  expect_equal(plot$data$Y, 2 * (1:5) + 1)
  expect_no_error(ggplot2::ggplot_build(plot))
  annotation_labels <- unlist(lapply(plot$layers, function(layer) {
    if (is.data.frame(layer$data) && "label" %in% names(layer$data)) {
      as.character(layer$data$label)
    } else {
      character(0)
    }
  }), use.names = FALSE)
  expect_true(any(grepl("r = 1.000", annotation_labels, fixed = TRUE)))
  expect_true(any(grepl("y = 2.000 x +1.000", annotation_labels, fixed = TRUE)))
})

test_that("bar and violin builders handle summary errors and sparse finite data", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("ggplot2")
  env <- load_critical_plot_env()

  captured_summary <- NULL
  sem_ctx <- make_distribution_edge_ctx(
    values = c(10, 10, 20, 20),
    errbar_stat = "SEM",
    summary_mode = TRUE
  )
  sem_ctx$add_black_t_errorbar <- function(p, data, ...) {
    captured_summary <<- data
    p
  }
  bar_plot <- env$build_barras_plot_impl(sem_ctx)
  expect_s3_class(bar_plot, "ggplot")
  expect_equal(captured_summary$Mean, c(10, 20))
  expect_equal(captured_summary$SD, c(1, 2), tolerance = 1e-12)
  expect_false(any(vapply(bar_plot$layers, function(layer) {
    inherits(layer$geom, "GeomPoint")
  }, logical(1))))

  sparse_ctx <- make_distribution_edge_ctx(values = c(1, 1, 2, 2))
  violin_plot <- env$build_violin_plot_impl(sparse_ctx)
  expect_s3_class(violin_plot, "ggplot")
  expect_false(any(vapply(violin_plot$layers, function(layer) {
    inherits(layer$geom, "GeomViolin")
  }, logical(1))))
  expect_true(any(vapply(violin_plot$layers, function(layer) {
    inherits(layer$geom, "GeomPoint")
  }, logical(1))))
  expect_no_error(ggplot2::ggplot_build(violin_plot))

  empty_ctx <- make_distribution_edge_ctx(values = c(NA_real_, Inf, -Inf, NA_real_))
  empty_bar <- env$build_barras_plot_impl(empty_ctx)
  empty_violin <- env$build_violin_plot_impl(empty_ctx)
  expect_true(any(vapply(empty_bar$layers, function(layer) {
    inherits(layer$geom, "GeomText")
  }, logical(1))))
  expect_true(any(vapply(empty_violin$layers, function(layer) {
    inherits(layer$geom, "GeomText")
  }, logical(1))))
})

test_that("curve builder recovers from non-finite observations and invalid axis settings", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("shiny")
  env <- load_critical_plot_env()

  curve_df <- data.frame(
    Time = rep(0:3, each = 2),
    Value = c(1, 1.1, NA, 1.5, 2, Inf, 2.4, 2.6),
    SD_Input = NA_real_,
    N_Input = NA_real_,
    Strain = "S1",
    Media = "M1",
    Orden = 1L,
    BiologicalReplicate = rep(c("1", "2"), 4),
    Well = rep(c("A1", "A2"), 4),
    stringsAsFactors = FALSE
  )
  settings <- data.frame(
    X_Max = NA_real_,
    Interval_X = NA_real_,
    Y_Max = NA_real_,
    Interval_Y = NA_real_,
    X_Title = "Time",
    Y_Title = "Signal",
    stringsAsFactors = FALSE
  )
  ctx <- list(
    scope = "Por Cepa",
    strain = "S1",
    curve_data = function() curve_df,
    curve_settings = function() settings,
    curve_long_df = function() curve_df,
    curve_summary_mode = function() FALSE,
    order_filter_strain = function(df) df,
    filter_reps_strain = function(df) df,
    input = list(
      cur_xlab = "",
      cur_ylab = "",
      xmax_cur = NA_real_,
      ymax_cur = NA_real_,
      xbreak_cur = NA_real_,
      ybreak_cur = NA_real_,
      cur_show_reps = FALSE,
      cur_show_ci = TRUE,
      cur_ci_style = "errorbar",
      cur_rep_alpha = 0.25,
      curve_geom = "line_only",
      curve_color_mode = "by_group",
      curve_single_color = "#000000",
      curve_lwd = 1,
      curve_pt_size = 2,
      plotTitle = "Sparse curves",
      base_size = 12
    ),
    lang = "en",
    fs_title = 12,
    fs_axis = 10,
    fs_legend = 9,
    axis_size = 0.8,
    sanitize_curve_label = function(x) trimws(as.character(x)),
    get_bottom_margin = function(angle, wrap = FALSE, lines = 2) 30,
    palette_for_levels = function(levels) {
      stats::setNames(rep("#1f77b4", length(levels)), levels)
    },
    margin_adj = function(top, right, bottom, left) {
      ggplot2::margin(top, right, bottom, left, unit = "pt")
    },
    tr_text = function(key, lang) key
  )

  plot <- env$build_curvas_plot_impl(ctx)
  expect_s3_class(plot, "ggplot")
  expect_true(all(is.finite(plot$data$Time)))
  expect_true(all(is.finite(plot$data$Avg)))
  expect_equal(plot$scales$get_scales("x")$limits, c(0, 3))
  expect_true(plot$scales$get_scales("y")$limits[[2]] >= 2.5)
  expect_no_error(ggplot2::ggplot_build(plot))
})
