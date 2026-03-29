# Helpers for curves plots ----------------------------------------------------

build_curvas_plot_impl <- function(ctx) {
  with(ctx, {
    req(curve_data(), curve_settings())

    df_cur <- curve_long_df()
    use_summary_curve <- isTRUE(curve_summary_mode())
    label_df <- NULL
    rep_df <- NULL

    summarise_curve <- function(data, group_cols) {
      data %>%
        group_by(across(all_of(c("Time", group_cols)))) %>%
        summarise(
          Avg = mean(Value, na.rm = TRUE),
          SD = if (use_summary_curve && any(is.finite(SD_Input))) {
            mean(SD_Input, na.rm = TRUE)
          } else {
            sd(Value, na.rm = TRUE)
          },
          N = if (use_summary_curve && any(is.finite(N_Input))) {
            mean(N_Input, na.rm = TRUE)
          } else {
            dplyr::n_distinct(BiologicalReplicate)
          },
          .groups = "drop"
        )
    }

    if (scope == "Por Cepa") {
      df_cur <- df_cur %>%
        filter(Strain == strain) %>%
        order_filter_strain() %>%
        filter_reps_strain()

      media_order <- df_cur %>%
        distinct(Media, Orden) %>%
        arrange(Orden) %>%
        pull(Media)

      df_sum <- summarise_curve(df_cur, "Media") %>%
        mutate(Label = factor(Media, levels = media_order))

      rep_df <- df_cur %>%
        mutate(Label = factor(Media, levels = levels(df_sum$Label)))
    } else {
      df_cur <- df_cur %>%
        order_filter_group() %>%
        filter_reps_group()

      available_labels <- unique(paste(df_cur$Strain, df_cur$Media, sep = "-"))
      platemap_labels <- datos_agrupados() %>%
        mutate(
          Strain = sanitize_curve_label(Strain),
          Media = sanitize_curve_label(Media)
        ) %>%
        distinct(Strain, Media, Orden) %>%
        mutate(Label = paste(Strain, Media, sep = "-")) %>%
        arrange(Orden) %>%
        pull(Label) %>%
        intersect(available_labels)

      user_order <- NULL
      if (!is.null(input$orderGroups) && nzchar(input$orderGroups)) {
        user_order <- intersect(
          trimws(strsplit(input$orderGroups, ",")[[1]]),
          available_labels
        )
      }
      final_order <- if (!is.null(user_order) && length(user_order) > 0) {
        c(user_order, setdiff(platemap_labels, user_order))
      } else {
        platemap_labels
      }

      if (isTRUE(input$labelMode)) {
        strain_order <- datos_agrupados() %>%
          mutate(Strain = sanitize_curve_label(Strain)) %>%
          group_by(Strain) %>%
          summarise(minO = min(Orden), .groups = "drop") %>%
          arrange(minO) %>%
          pull(Strain)

        df_sum <- summarise_curve(df_cur, "Strain") %>%
          mutate(Label = factor(Strain, levels = strain_order))

        rep_df <- df_cur %>%
          mutate(Label = factor(Strain, levels = levels(df_sum$Label)))
      } else {
        df_sum <- summarise_curve(df_cur, c("Strain", "Media")) %>%
          mutate(Label = factor(paste(Strain, Media, sep = "-"), levels = final_order))

        rep_df <- df_cur %>%
          mutate(Label = factor(paste(Strain, Media, sep = "-"), levels = levels(df_sum$Label)))
      }

      label_df <- df_cur %>%
        mutate(Label = if (isTRUE(input$labelMode)) Strain else paste(Strain, Media, sep = "-")) %>%
        distinct(Label, Strain, Media)
    }

    label_levels <- if (is.factor(df_sum$Label)) {
      levels(df_sum$Label)
    } else {
      unique(as.character(df_sum$Label))
    }
    label_levels <- sanitize_curve_label(label_levels)
    label_levels <- unique(label_levels[!is.na(label_levels) & nzchar(label_levels)])
    if (!length(label_levels)) {
      label_levels <- unique(sanitize_curve_label(as.character(df_sum$Label)))
    }

    df_sum <- df_sum %>%
      mutate(Label = factor(sanitize_curve_label(as.character(Label)), levels = label_levels))
    if (!is.null(rep_df) && nrow(rep_df)) {
      rep_df <- rep_df %>%
        mutate(Label = factor(sanitize_curve_label(as.character(Label)), levels = label_levels))
    }
    if (!is.null(label_df) && nrow(label_df)) {
      label_df <- label_df %>%
        mutate(
          Label = sanitize_curve_label(Label),
          Strain = sanitize_curve_label(Strain),
          Media = sanitize_curve_label(Media)
        )
    }

    df_sum <- df_sum %>%
      filter(is.finite(Time), is.finite(Avg), !is.na(Label)) %>%
      mutate(
        SD = ifelse(is.finite(SD), SD, NA_real_),
        N = ifelse(is.finite(N) & N > 0, N, ifelse(is.finite(SD), 1, NA_real_)),
        SE = ifelse(is.finite(SD) & is.finite(N) & N > 0, SD / sqrt(N), NA_real_),
        CI = ifelse(is.finite(SE), 1.96 * SE, SD)
      )
    validate(need(nrow(df_sum) > 0, tr_text("no_data_selection", lang)))

    cfg_cur <- curve_settings()[1, ]
    x_lab <- if (nzchar(input$cur_xlab)) input$cur_xlab else cfg_cur$X_Title
    y_lab <- if (nzchar(input$cur_ylab)) input$cur_ylab else cfg_cur$Y_Title
    b_mar <- get_bottom_margin(0)

    x_max <- if (!is.null(input$xmax_cur) && is.finite(input$xmax_cur)) input$xmax_cur else cfg_cur$X_Max
    y_max <- if (!is.null(input$ymax_cur) && is.finite(input$ymax_cur)) input$ymax_cur else cfg_cur$Y_Max
    x_break <- if (!is.null(input$xbreak_cur) && is.finite(input$xbreak_cur) && input$xbreak_cur > 0) input$xbreak_cur else cfg_cur$Interval_X
    y_break <- if (!is.null(input$ybreak_cur) && is.finite(input$ybreak_cur) && input$ybreak_cur > 0) input$ybreak_cur else cfg_cur$Interval_Y
    data_x_max <- suppressWarnings(max(df_sum$Time, na.rm = TRUE))
    data_y_max <- suppressWarnings(max(df_sum$Avg + ifelse(is.finite(df_sum$CI), df_sum$CI, 0), na.rm = TRUE))
    if (!is.finite(x_max) || x_max <= 0) x_max <- data_x_max
    if (!is.finite(y_max) || y_max <= 0) y_max <- data_y_max
    if (!is.finite(x_max) || x_max <= 0) x_max <- 1
    if (!is.finite(y_max) || y_max <= 0) y_max <- 1
    if (!is.finite(x_break) || x_break <= 0) x_break <- x_max / 3
    if (!is.finite(y_break) || y_break <= 0) y_break <- y_max / 3
    if (!is.finite(x_break) || x_break <= 0) x_break <- 1
    if (!is.finite(y_break) || y_break <= 0) y_break <- 1

    pal <- if (scope == "Combinado" && !is.null(label_df)) {
      palette_for_labels(
        mutate(label_df, Label = factor(Label, levels = levels(df_sum$Label))),
        levels(df_sum$Label)
      )
    } else {
      palette_for_levels(levels(df_sum$Label))
    }

    curve_geom <- input$curve_geom %||% "line_points"
    color_mode <- input$curve_color_mode %||% "by_group"
    single_col <- input$curve_single_color %||% "#000000"
    show_points <- !identical(curve_geom, "line_only")
    ci_style <- input$cur_ci_style %||% "ribbon"

    plt <- ggplot(df_sum, aes(x = Time, y = Avg, group = Label))

    if (isTRUE(input$cur_show_reps) && !use_summary_curve && !is.null(rep_df) && nrow(rep_df)) {
      rep_df <- rep_df %>% filter(is.finite(Time), is.finite(Value), !is.na(Label))
      if (nrow(rep_df)) {
        if (identical(color_mode, "by_group")) {
          plt <- plt +
            geom_line(
              data = rep_df,
              aes(y = Value, color = Label, group = interaction(Label, BiologicalReplicate, Well)),
              linewidth = max(0.4, input$curve_lwd * 0.6),
              alpha = input$cur_rep_alpha %||% 0.25,
              show.legend = FALSE
            )
        } else {
          plt <- plt +
            geom_line(
              data = rep_df,
              aes(y = Value, group = interaction(Label, BiologicalReplicate, Well)),
              linewidth = max(0.4, input$curve_lwd * 0.6),
              alpha = input$cur_rep_alpha %||% 0.25,
              color = single_col,
              show.legend = FALSE
            )
        }
      }
    }

    if (isTRUE(input$cur_show_ci)) {
      ci_df <- df_sum %>%
        mutate(
          ci_use = ifelse(is.finite(CI), CI, 0),
          ymin = pmax(0, Avg - ci_use),
          ymax = Avg + ci_use
        )
      if (identical(ci_style, "errorbar")) {
        if (identical(color_mode, "by_group")) {
          plt <- plt +
            geom_errorbar(
              data = ci_df,
              aes(ymin = ymin, ymax = ymax, color = Label),
              width = 0.0,
              linewidth = max(0.4, input$curve_lwd * 0.8),
              alpha = 0.8
            )
        } else {
          plt <- plt +
            geom_errorbar(
              data = ci_df,
              aes(ymin = ymin, ymax = ymax),
              color = single_col,
              width = 0.0,
              linewidth = max(0.4, input$curve_lwd * 0.8),
              alpha = 0.8
            )
        }
      } else {
        if (identical(color_mode, "by_group")) {
          plt <- plt +
            geom_ribbon(
              data = ci_df,
              aes(ymin = ymin, ymax = ymax, fill = Label),
              alpha = 0.16,
              colour = NA,
              inherit.aes = TRUE
            )
        } else {
          plt <- plt +
            geom_ribbon(
              data = ci_df,
              aes(ymin = ymin, ymax = ymax),
              fill = single_col,
              alpha = 0.14,
              colour = NA,
              inherit.aes = TRUE
            )
        }
      }
    }

    if (identical(color_mode, "by_group")) {
      plt <- plt +
        geom_line(aes(color = Label), linewidth = input$curve_lwd, lineend = "round")
    } else {
      plt <- plt +
        geom_line(linewidth = input$curve_lwd, lineend = "round", colour = single_col)
    }

    if (show_points) {
      if (identical(color_mode, "by_group")) {
        plt <- plt +
          geom_point(
            aes(fill = Label),
            size = input$curve_lwd * 2.2,
            shape = 21,
            colour = "black",
            stroke = 0.4
          )
      } else {
        plt <- plt +
          geom_point(
            fill = single_col,
            size = input$curve_lwd * 2.2,
            shape = 21,
            colour = "black",
            stroke = 0.4
          )
      }
    } else {
      if (identical(color_mode, "by_group")) {
        plt <- plt +
          geom_point(
            aes(fill = Label),
            size = 0.01,
            alpha = 0,
            shape = 21,
            stroke = 0
          )
      } else {
        plt <- plt +
          geom_point(
            fill = single_col,
            size = 0.01,
            alpha = 0,
            shape = 21,
            stroke = 0
          )
      }
    }

    if (identical(color_mode, "by_group")) {
      plt <- plt +
        scale_fill_manual(values = pal, breaks = levels(df_sum$Label)) +
        scale_color_manual(values = pal, breaks = levels(df_sum$Label))
    }

    plt <- plt +
      labs(
        title = input$plotTitle,
        x = x_lab,
        y = y_lab,
        fill = NULL,
        color = NULL
      ) +
      scale_x_continuous(
        limits = c(0, x_max),
        breaks = seq(0, x_max, by = x_break),
        expand = c(0, 0)
      ) +
      scale_y_continuous(
        limits = c(0, y_max),
        breaks = seq(0, y_max, by = y_break),
        expand = c(0, 0)
      ) +
      theme_classic(base_size = input$base_size, base_family = "Helvetica") +
      coord_cartesian(clip = "off") +
      theme(
        plot.margin = margin_adj(15, 160, b_mar, 28),
        plot.title = element_text(size = input$fs_title, face = "bold", colour = "black"),
        axis.title = element_text(size = input$fs_axis, face = "bold", colour = "black"),
        axis.text = element_text(size = input$fs_axis, colour = "black"),
        axis.line = element_line(linewidth = input$axis_line_size, colour = "black"),
        axis.ticks = element_line(linewidth = input$axis_line_size, colour = "black"),
        axis.ticks.length = unit(4, "pt"),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", colour = NA),
        legend.position = "right",
        legend.box.spacing = unit(18, "pt"),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 20, unit = "pt"),
        legend.text = element_text(size = input$fs_legend, colour = "black"),
        legend.key = element_blank(),
        legend.key.size = unit(1.5, "lines")
      )

    plt
  })
}

plot_curvas <- function(scope, strain = NULL) {
  build_plot(scope, strain, "Curvas")
}
