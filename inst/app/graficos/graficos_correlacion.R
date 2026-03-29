# Helpers para gráficos de correlación ---------------------------------------

# Se asegura de que las funciones de correlación estén disponibles
source("stats/stats_correlation.R")

build_correlation_plot_impl <- function(scope,
                                        scope_df,
                                        input,
                                        lang,
                                        has_ctrl_selected,
                                        corr_adv_last_pair,
                                        tr_text,
                                        margin_adj) {
  corr_placeholder <- function(msg) {
    ggplot2::ggplot() +
      ggplot2::theme_void() +
      ggplot2::annotate("text", 0, 0, label = msg %||% tr_text("no_data_plot", lang))
  }

  raw_x <- trimws(as.character(input$corr_param_x %||% ""))
  raw_y <- trimws(as.character(input$corr_param_y %||% ""))
  if (!nzchar(raw_x) || !nzchar(raw_y) || identical(raw_x, raw_y)) {
    pair <- corr_adv_last_pair()
    if (is.list(pair) && length(pair)) {
      x_pair <- trimws(as.character(pair$x %||% ""))
      y_pair <- trimws(as.character(pair$y %||% ""))
      if (nzchar(x_pair) && nzchar(y_pair) && !identical(x_pair, y_pair)) {
        raw_x <- x_pair
        raw_y <- y_pair
      }
    }
  }
  if (!nzchar(raw_x) || !nzchar(raw_y) || identical(raw_x, raw_y)) {
    return(corr_placeholder(tr_text("corr_select_two_params", lang)))
  }
  norm_mode <- input$corr_norm_target %||% "both"
  use_norm_x <- isTRUE(input$doNorm) && has_ctrl_selected() &&
    norm_mode %in% c("both", "x_only")
  use_norm_y <- isTRUE(input$doNorm) && has_ctrl_selected() &&
    norm_mode %in% c("both", "y_only")
  param_x <- if (use_norm_x) paste0(raw_x, "_Norm") else raw_x
  param_y <- if (use_norm_y) paste0(raw_y, "_Norm") else raw_y

  df_raw <- scope_df
  if (is.null(df_raw) || !is.data.frame(df_raw) || nrow(df_raw) == 0) {
    return(corr_placeholder(tr_text("no_data_selection", lang)))
  }
  req_cols <- c(param_x, param_y)
  if (!all(req_cols %in% names(df_raw))) {
    return(corr_placeholder(tr_text("no_data_selection", lang)))
  }

  df_grouped <- if (scope == "Por Cepa") {
    df_raw %>%
      dplyr::group_by(Media) %>%
      dplyr::summarise(
        X = mean(.data[[param_x]], na.rm = TRUE),
        Y = mean(.data[[param_y]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::rename(Label = Media)
  } else {
    df_raw %>%
      dplyr::group_by(Strain, Media) %>%
      dplyr::summarise(
        X = mean(.data[[param_x]], na.rm = TRUE),
        Y = mean(.data[[param_y]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        Label = if (isTRUE(input$labelMode)) {
          Strain
        } else {
          paste(Strain, Media, sep = "-")
        }
      )
  }
  df_diag <- df_grouped %>%
    dplyr::mutate(
      X = suppressWarnings(as.numeric(X)),
      Y = suppressWarnings(as.numeric(Y)),
      Label = as.character(Label),
      has_x = is.finite(X),
      has_y = is.finite(Y),
      has_both = has_x & has_y & !is.na(Label) & nzchar(Label)
    )

  n_with_x <- sum(df_diag$has_x, na.rm = TRUE)
  n_with_y <- sum(df_diag$has_y, na.rm = TRUE)
  n_with_both <- sum(df_diag$has_both, na.rm = TRUE)

  df <- df_diag %>%
    dplyr::filter(has_both) %>%
    dplyr::select(Label, X, Y)

  if (nrow(df) < 3) {
    if (n_with_both == 0) {
      msg <- sprintf(
        tr_text("corr_no_overlap_points", lang),
        raw_x, raw_y, n_with_x, n_with_y, n_with_both
      )
      valid_labels <- unique(df_diag$Label[df_diag$has_both])
      valid_labels <- valid_labels[!is.na(valid_labels) & nzchar(valid_labels)]
      if (length(valid_labels)) {
        valid_preview <- paste(utils::head(valid_labels, 6), collapse = ", ")
        if (length(valid_labels) > 6) {
          valid_preview <- paste0(valid_preview, ", ...")
        }
        msg <- paste(
          msg,
          sprintf(tr_text("corr_overlap_groups_hint", lang), valid_preview),
          sep = "\n"
        )
      }
      return(corr_placeholder(msg))
    }
    if (n_with_both < 3) {
      return(corr_placeholder(sprintf(
        tr_text("corr_not_enough_overlap_points", lang),
        n_with_both, raw_x, raw_y
      )))
    }
    return(corr_placeholder(tr_text("corr_min_points", input$app_lang %||% "en")))
  }

  cor_method <- input$corr_method %||% "pearson"
  df_stats <- df %>%
    dplyr::transmute(
      X = suppressWarnings(as.numeric(X)),
      Y = suppressWarnings(as.numeric(Y))
    ) %>%
    dplyr::filter(is.finite(X), is.finite(Y))

  cor_res <- NULL
  if (nrow(df_stats) >= 3) {
    stat_pair <- correlation_pair_with_p(
      x = df_stats$X,
      y = df_stats$Y,
      method = cor_method,
      min_points = 3L
    )
    if (is.finite(stat_pair$r) && is.finite(stat_pair$p.value)) {
      cor_res <- list(
        estimate = stat_pair$r,
        p.value = stat_pair$p.value,
        n = stat_pair$n
      )
    }
  }
  fit <- NULL
  r2_val <- NULL
  need_fit <- isTRUE(input$corr_show_line) || isTRUE(input$corr_show_ci) ||
    isTRUE(input$corr_show_eq) || isTRUE(input$corr_show_r2)
  if (need_fit && nrow(df_stats) >= 3 && dplyr::n_distinct(df_stats$X) >= 2) {
    fit <- tryCatch(stats::lm(Y ~ X, data = df_stats), error = function(e) NULL)
    if (!is.null(fit)) {
      r2_val <- tryCatch(summary(fit)$r.squared, error = function(e) NULL)
    }
  }

  eq_lab <- NULL
  if (isTRUE(input$corr_show_eq) && !is.null(fit)) {
    slope <- suppressWarnings(as.numeric(stats::coef(fit)[2]))
    intercept <- suppressWarnings(as.numeric(stats::coef(fit)[1]))
    if (is.finite(slope) && is.finite(intercept)) {
      eq_lab <- sprintf("y = %.3f x %+.3f", slope, intercept)
    }
  }
  stats_lines <- character()
  if (isTRUE(input$corr_show_r) && !is.null(cor_res)) {
    stats_lines <- c(stats_lines, sprintf("r = %.3f", as.numeric(cor_res$estimate)[1]))
  }
  if (isTRUE(input$corr_show_p) && !is.null(cor_res)) {
    stats_lines <- c(stats_lines, sprintf("p = %.3g", cor_res$p.value))
  }
  if (isTRUE(input$corr_show_r2) && !is.null(r2_val) && is.finite(r2_val)) {
    stats_lines <- c(stats_lines, sprintf("R^2 = %.3f", r2_val))
  }
  stats_lab <- if (length(stats_lines)) paste(stats_lines, collapse = "\n") else NULL

  data_xmin <- suppressWarnings(min(df$X, na.rm = TRUE))
  data_xmax <- suppressWarnings(max(df$X, na.rm = TRUE))
  data_ymin <- suppressWarnings(min(df$Y, na.rm = TRUE))
  data_ymax <- suppressWarnings(max(df$Y, na.rm = TRUE))

  xmin_in <- suppressWarnings(as.numeric(input$xmin_corr))
  xmax_in <- suppressWarnings(as.numeric(input$xmax_corr))
  ymin_in <- suppressWarnings(as.numeric(input$ymin_corr))
  ymax_in <- suppressWarnings(as.numeric(input$ymax_corr))

  manual_x_limits <- is.finite(xmin_in) && is.finite(xmax_in) && xmax_in > xmin_in
  manual_y_limits <- is.finite(ymin_in) && is.finite(ymax_in) && ymax_in > ymin_in

  auto_axis <- function(lo, hi) {
    if (!is.finite(lo) || !is.finite(hi)) return(c(0, 1))
    if (hi <= lo) {
      pad <- if (lo == 0) 1 else abs(lo) * 0.1
      return(c(lo - pad, hi + pad))
    }
    pad <- (hi - lo) * 0.02
    c(lo - pad, hi + pad)
  }

  if (manual_x_limits) {
    xmin <- xmin_in
    xmax <- xmax_in
  } else {
    xr <- auto_axis(data_xmin, data_xmax)
    xmin <- xr[[1]]
    xmax <- xr[[2]]
  }

  if (manual_y_limits) {
    ymin <- ymin_in
    ymax <- ymax_in
  } else {
    yr <- auto_axis(data_ymin, data_ymax)
    ymin <- yr[[1]]
    ymax <- yr[[2]]
  }

  xbreak <- suppressWarnings(as.numeric(input$xbreak_corr))
  if (!is.finite(xbreak) || xbreak <= 0) xbreak <- (xmax - xmin) / 5
  if (!is.finite(xbreak) || xbreak <= 0) xbreak <- 0.2

  ybreak <- suppressWarnings(as.numeric(input$ybreak_corr))
  if (!is.finite(ybreak) || ybreak <= 0) ybreak <- (ymax - ymin) / 5
  if (!is.finite(ybreak) || ybreak <= 0) ybreak <- 0.2

  build_breaks <- function(lo, hi, step) {
    if (!is.finite(lo) || !is.finite(hi) || hi <= lo) return(pretty(c(0, 1), n = 6))
    if (!is.finite(step) || step <= 0) return(pretty(c(lo, hi), n = 6))
    n_breaks <- floor((hi - lo) / step) + 1
    if (!is.finite(n_breaks) || n_breaks > 5000) return(pretty(c(lo, hi), n = 6))
    seq(lo, hi, by = step)
  }
  x_breaks <- build_breaks(xmin, xmax, xbreak)
  y_breaks <- build_breaks(ymin, ymax, ybreak)

  dx <- 0.05 * (xmax - xmin)
  dy <- 0.04 * (ymax - ymin)
  x_t <- xmax - dx
  y_t <- ymax - dy
  eq_y <- if (is.null(stats_lab)) y_t else y_t - dy * 2.3

  ci_level <- suppressWarnings(as.numeric(input$corr_ci_level %||% 0.95))
  if (!is.finite(ci_level)) ci_level <- 0.95
  ci_level <- max(min(ci_level, 0.99), 0.80)
  ci_style <- input$corr_ci_style %||% "band"
  if (!ci_style %in% c("band", "dashed")) ci_style <- "band"

  pred_df <- NULL
  if (isTRUE(input$corr_show_ci) && !is.null(fit)) {
    x_seq <- seq(xmin, xmax, length.out = 200)
    pred <- tryCatch(
      stats::predict(
        fit,
        newdata = data.frame(X = x_seq),
        interval = "confidence",
        level = ci_level
      ),
      error = function(e) NULL
    )
    if (!is.null(pred) && nrow(pred) == length(x_seq)) {
      pred_df <- data.frame(
        X = x_seq,
        fit = as.numeric(pred[, "fit"]),
        lwr = as.numeric(pred[, "lwr"]),
        upr = as.numeric(pred[, "upr"])
      ) %>%
        dplyr::mutate(
          fit = pmin(pmax(fit, ymin), ymax),
          lwr = pmin(pmax(lwr, ymin), ymax),
          upr = pmin(pmax(upr, ymin), ymax)
        )
    }
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(X, Y)) +
    ggplot2::geom_point(size = 3, colour = "black")

  if (!is.null(pred_df) && nrow(pred_df) > 0 && identical(ci_style, "band")) {
    p <- p +
      ggplot2::geom_ribbon(
        data = pred_df,
        inherit.aes = FALSE,
        ggplot2::aes(x = X, ymin = lwr, ymax = upr),
        fill = "grey55",
        alpha = 0.20
      )
  }
  if (!is.null(pred_df) && nrow(pred_df) > 0 && identical(ci_style, "dashed")) {
    p <- p +
      ggplot2::geom_line(
        data = pred_df,
        inherit.aes = FALSE,
        ggplot2::aes(x = X, y = lwr),
        colour = "black",
        linetype = "dotted",
        linewidth = 0.6
      ) +
      ggplot2::geom_line(
        data = pred_df,
        inherit.aes = FALSE,
        ggplot2::aes(x = X, y = upr),
        colour = "black",
        linetype = "dotted",
        linewidth = 0.6
      )
  }
  if (isTRUE(input$corr_show_line)) {
    if (!is.null(pred_df) && nrow(pred_df) > 0) {
      p <- p +
        ggplot2::geom_line(
          data = pred_df,
          inherit.aes = FALSE,
          ggplot2::aes(x = X, y = fit),
          colour = "black",
          linetype = "dashed",
          linewidth = 0.8
        )
    } else if (!is.null(fit)) {
      p <- p + ggplot2::geom_smooth(method = "lm", se = FALSE, colour = "black", linetype = "dashed")
    }
  }
  if (isTRUE(input$corr_show_labels)) {
    p <- p +
      ggplot2::geom_text(
        ggplot2::aes(label = Label),
        nudge_y = 0.05 * (ymax - ymin),
        size = input$corr_label_size
      )
  }
  if (!is.null(stats_lab)) {
    p <- p +
      ggplot2::annotate(
        "text",
        x = x_t, y = y_t, hjust = 1, vjust = 1,
        label = stats_lab,
        size = 5
      )
  }
  if (!is.null(eq_lab)) {
    p <- p +
      ggplot2::annotate(
        "text",
        x = x_t, y = eq_y,
        hjust = 1, vjust = 1,
        label = eq_lab,
        size = 5
      )
  }
  p <- p +
    ggplot2::labs(
      title = input$plotTitle,
      x = if (nzchar(input$corr_xlab)) input$corr_xlab else raw_x,
      y = if (nzchar(input$corr_ylab)) input$corr_ylab else raw_y
    ) +
    ggplot2::scale_x_continuous(
      limits = c(xmin, xmax),
      breaks = x_breaks,
      expand = ggplot2::expansion(mult = 0)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(ymin, ymax),
      breaks = y_breaks,
      expand = ggplot2::expansion(mult = 0)
    ) +
    ggplot2::theme_minimal(base_size = input$base_size, base_family = "Helvetica") +
    ggplot2::coord_cartesian(clip = "on") +
    ggplot2::theme(
      plot.margin = margin_adj(20, 50, 10, 10),
      plot.title = ggplot2::element_text(size = input$fs_title, face = "bold"),
      axis.title = ggplot2::element_text(size = input$fs_axis, face = "bold", colour = "black"),
      axis.text = ggplot2::element_text(size = input$fs_axis, colour = "black"),
      axis.line = ggplot2::element_line(linewidth = input$axis_line_size, colour = "black"),
      axis.ticks = ggplot2::element_line(linewidth = input$axis_line_size, colour = "black"),
      panel.grid = ggplot2::element_blank()
    )

  p
}

plot_correlacion <- function(scope, strain = NULL) {
  build_plot(scope, strain, "Correlacion")
}
