# Helpers para gráficos Violín -------------------------------------------------

build_violin_plot_impl <- function(ctx) {
  with(ctx, {
    if (scope == "Combinado") {
      df_labels <- scope_df %>% distinct(Label, Strain, Media)
      df_plot <- scope_df %>%
        transmute(Label, Valor = .data[[param_sel]]) %>%
        filter(is.finite(Valor))

      if (nrow(df_plot) == 0) {
        return(
          ggplot() +
            theme_void() +
            annotate("text", 0, 0, label = msg_no_data_sel)
        )
      }

      if (isTRUE(input$x_wrap)) {
        df_plot$Label <- wrap_label(df_plot$Label, lines = input$x_wrap_lines)
        df_labels$Label <- wrap_label(df_labels$Label, lines = input$x_wrap_lines)
      }
      if (is.factor(df_plot$Label)) {
        df_plot$Label <- droplevels(df_plot$Label)
      } else {
        df_plot$Label <- factor(as.character(df_plot$Label), levels = unique(as.character(df_plot$Label)))
      }
      df_labels$Label <- factor(df_labels$Label, levels = levels(df_plot$Label))

      pal <- palette_for_labels(df_labels, levels(df_plot$Label))
      flip_plot <- isTRUE(input$plot_flip)
      x_ang <- get_x_angle(
        n = nlevels(df_plot$Label),
        angle_input = input$x_angle
      )
      x_ang_cat <- if (flip_plot && is.na(input$x_angle)) 0 else x_ang
      b_mar <- get_bottom_margin(x_ang_cat, input$x_wrap, input$x_wrap_lines)

      v_width <- input$violin_width %||% 0.45
      if (!is.finite(v_width) || v_width <= 0) v_width <- 0.45
      v_lwd <- if (!is.null(input$violin_linewidth) &&
        is.finite(input$violin_linewidth) &&
        input$violin_linewidth > 0) {
        input$violin_linewidth
      } else {
        0.6
      }
      violin_args <- list(
        linewidth = v_lwd,
        width = v_width,
        alpha = 0.8,
        trim = TRUE,
        scale = "count",
        adjust = 1.1,
        na.rm = TRUE
      )

      df_split <- df_plot %>%
        group_by(Label) %>%
        mutate(n_unique = n_distinct(Valor)) %>%
        ungroup()
      violin_data <- df_split %>% filter(n_unique >= 2)
      jitter_width <- min(input$pt_jit, v_width * 0.6)

      show_legend <- legend_right_enabled(input$colorMode)

      if (input$colorMode == "Blanco y Negro") {
        p <- ggplot(df_plot, aes(Label, Valor)) +
          {
            if (nrow(violin_data) > 0) {
              do.call(
                geom_violin,
                c(
                  list(
                    data = violin_data, fill = "white",
                    colour = "black",
                    show.legend = show_legend,
                    key_glyph = "rect"
                  ),
                  violin_args
                )
              )
            }
          } +
          geom_point(
            position = position_jitter(width = jitter_width, height = 0),
            shape = 21,
            fill = "black",
            colour = "black",
            stroke = v_lwd / 2,
            size = input$pt_size,
            na.rm = TRUE,
            show.legend = FALSE
          )
      } else {
        p <- ggplot(df_plot, aes(Label, Valor, fill = Label)) +
          {
            if (nrow(violin_data) > 0) {
              do.call(
                geom_violin,
                c(
                  list(
                    data = violin_data,
                    colour = "black",
                    show.legend = show_legend,
                    key_glyph = "rect"
                  ),
                  violin_args
                )
              )
            }
          } +
          geom_point(
            aes(fill = Label),
            position = position_jitter(width = jitter_width, height = 0),
            shape = 21,
            colour = "black",
            stroke = v_lwd / 2,
            size = input$pt_size,
            alpha = 0.95,
            na.rm = TRUE,
            show.legend = FALSE
          ) +
          scale_fill_manual(values = pal)
      }

      base_margin <- if (flip_plot) {
        margin_adj(12, 18, 28, b_mar)
      } else {
        margin_adj(12, 18, b_mar, 28)
      }
      axis_title_x_el <- if (flip_plot) {
        element_text(size = fs_axis, face = "bold", colour = "black")
      } else {
        element_blank()
      }
      axis_title_y_el <- if (flip_plot) {
        element_blank()
      } else {
        element_text(size = fs_axis, face = "bold", colour = "black")
      }
      axis_text_x_el <- if (flip_plot) {
        element_text(size = fs_axis, colour = "black")
      } else {
        element_text(
          size = fs_axis,
          angle = x_ang_cat,
          hjust = ifelse(x_ang_cat == 0, .5, 1),
          colour = "black"
        )
      }
      axis_text_y_el <- if (flip_plot) {
        element_text(
          size = fs_axis,
          angle = x_ang_cat,
          hjust = ifelse(x_ang_cat == 0, .5, 1),
          colour = "black"
        )
      } else {
        element_text(size = fs_axis, colour = "black")
      }
      p <- p +
        labs(
          title = input$plotTitle,
          x = if (flip_plot) ylab else NULL,
          y = if (flip_plot) NULL else ylab
        ) +
        scale_y_continuous(
          limits = c(0, ymax),
          breaks = axis_breaks_limited(ymax, ybreak),
          expand = c(0, 0),
          oob = scales::oob_keep
        ) +
        theme_classic(base_size = input$base_size, base_family = "Helvetica") +
        coord_cartesian(clip = "off") +
        theme(
          plot.margin = base_margin,
          plot.title = element_text(size = fs_title, face = "bold", colour = "black"),
          axis.title.x = axis_title_x_el,
          axis.title.y = axis_title_y_el,
          axis.text.x = axis_text_x_el,
          axis.text.y = axis_text_y_el,
          axis.line = element_line(linewidth = axis_size, colour = "black"),
          axis.ticks = element_line(linewidth = axis_size, colour = "black"),
          axis.ticks.length = unit(4, "pt"),
          panel.background = element_rect(fill = "white", colour = NA),
          panel.grid = element_blank(),
          legend.position = "none"
        )

      if (isTRUE(input$labelMode)) {
        p <- p + scale_x_discrete(labels = function(x) sub("-.*$", "", x))
      }

      sig_tops <- df_plot %>%
        group_by(Label) %>%
        summarise(y_top = max(Valor, na.rm = TRUE), .groups = "drop") %>%
        mutate(y_top = ifelse(is.finite(y_top), y_top, NA_real_)) %>%
        rename(group = Label)
      p <- apply_sig_layers(
        p,
        group_tops = sig_tops,
        margin_base = base_margin,
        plot_height = input$plot_h
      )
      if (flip_plot) {
        p <- suppressMessages(p + coord_flip(clip = "off"))
      }
      if (show_legend) {
        p <- apply_square_legend_right(p)
      }

      return(p)
    }

    df_raw <- scope_df %>%
      filter(is.finite(.data[[param_sel]]))

    if (nrow(df_raw) == 0) {
      return(
        ggplot() + theme_void() +
          annotate("text", x = 0, y = 0, label = msg_no_data_sel)
      )
    }

    if (isTRUE(input$x_wrap)) {
      df_raw$Media <- wrap_label(df_raw$Media, lines = input$x_wrap_lines)
    }

    media_levels <- if (is.factor(df_raw$Media)) levels(df_raw$Media) else unique(as.character(df_raw$Media))
    pal <- palette_for_levels(media_levels)
    flip_plot <- isTRUE(input$plot_flip)
    x_ang <- get_x_angle(
      n = nlevels(factor(df_raw$Media)),
      angle_input = input$x_angle
    )
    x_ang_cat <- if (flip_plot && is.na(input$x_angle)) 0 else x_ang
    b_mar <- get_bottom_margin(x_ang_cat, input$x_wrap, input$x_wrap_lines)

    v_width <- input$violin_width %||% 0.45
    if (!is.finite(v_width) || v_width <= 0) v_width <- 0.45
    v_lwd <- if (!is.null(input$violin_linewidth) &&
      is.finite(input$violin_linewidth) &&
      input$violin_linewidth > 0) {
      input$violin_linewidth
    } else {
      0.6
    }
    violin_args <- list(
      linewidth = v_lwd,
      width = v_width,
      alpha = 0.8,
      trim = TRUE,
      scale = "count",
      adjust = 1.1,
      na.rm = TRUE
    )
    df_split <- df_raw %>%
      group_by(Media) %>%
      mutate(n_unique = n_distinct(.data[[param_sel]])) %>%
      ungroup()
    violin_data <- df_split %>% filter(n_unique >= 2)
    df_points <- if (isTRUE(for_interactive)) {
      downsample_points_by_group(df_raw, "Media", cap_total = 7000L)
    } else {
      df_raw
    }
    jitter_width <- min(input$pt_jit, v_width * 0.6)

    show_legend <- legend_right_enabled(colourMode)

    if (colourMode == "Blanco y Negro") {
      p <- ggplot(df_raw, aes(Media, .data[[param_sel]])) +
        {
          if (nrow(violin_data) > 0) {
            do.call(
              geom_violin,
              c(
                list(
                  data = violin_data, fill = "white", colour = "black",
                  show.legend = show_legend, key_glyph = "rect"
                ),
                violin_args
              )
            )
          }
        } +
        geom_point(
          data = df_points,
          position = position_jitter(width = jitter_width, height = 0),
          shape = 21,
          fill = "black",
          colour = "black",
          stroke = v_lwd / 2,
          size = input$pt_size,
          na.rm = TRUE,
          show.legend = FALSE
        )
    } else {
      p <- ggplot(df_raw, aes(Media, .data[[param_sel]], fill = Media)) +
        {
          if (nrow(violin_data) > 0) {
            do.call(
              geom_violin,
              c(
                list(
                  data = violin_data, colour = "black",
                  show.legend = show_legend, key_glyph = "rect"
                ),
                violin_args
              )
            )
          }
        } +
        geom_point(
          aes(fill = Media),
          data = df_points,
          position = position_jitter(width = jitter_width, height = 0),
          shape = 21,
          colour = "black",
          stroke = v_lwd / 2,
          size = input$pt_size,
          alpha = 0.95,
          na.rm = TRUE,
          show.legend = FALSE
        ) +
        scale_fill_manual(values = pal)
    }

    base_margin <- if (flip_plot) {
      margin_adj(12, 18, 28, b_mar)
    } else {
      margin_adj(12, 18, b_mar, 28)
    }
    axis_title_x_el <- if (flip_plot) {
      element_text(size = fs_axis, face = "bold", colour = "black")
    } else {
      element_blank()
    }
    axis_title_y_el <- if (flip_plot) {
      element_blank()
    } else {
      element_text(size = fs_axis, face = "bold", colour = "black")
    }
    axis_text_x_el <- if (flip_plot) {
      element_text(size = fs_axis, colour = "black")
    } else {
      element_text(
        size = fs_axis,
        angle = x_ang_cat,
        hjust = ifelse(x_ang_cat == 0, .5, 1),
        colour = "black"
      )
    }
    axis_text_y_el <- if (flip_plot) {
      element_text(
        size = fs_axis,
        angle = x_ang_cat,
        hjust = ifelse(x_ang_cat == 0, .5, 1),
        colour = "black"
      )
    } else {
      element_text(size = fs_axis, colour = "black")
    }
    p <- p +
      labs(
        title = input$plotTitle,
        x = if (flip_plot) ylab else NULL,
        y = if (flip_plot) NULL else ylab
      ) +
      scale_y_continuous(
        limits = c(0, ymax),
        breaks = axis_breaks_limited(ymax, ybreak),
        expand = c(0, 0),
        oob = scales::oob_keep
      ) +
      theme_classic(base_size = input$base_size, base_family = "Helvetica") +
      coord_cartesian(clip = "off") +
      theme(
        plot.margin = base_margin,
        plot.title = element_text(size = fs_title, face = "bold", colour = "black"),
        axis.title.x = axis_title_x_el,
        axis.title.y = axis_title_y_el,
        axis.text.x = axis_text_x_el,
        axis.text.y = axis_text_y_el,
        axis.line = element_line(linewidth = axis_size, colour = "black"),
        axis.ticks = element_line(linewidth = axis_size, colour = "black"),
        axis.ticks.length = unit(4, "pt"),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid = element_blank(),
        legend.position = "none"
      )

    sig_tops <- df_raw %>%
      group_by(Media) %>%
      summarise(y_top = max(.data[[param_sel]], na.rm = TRUE), .groups = "drop") %>%
      mutate(y_top = ifelse(is.finite(y_top), y_top, NA_real_)) %>%
      rename(group = Media)
    p <- apply_sig_layers(
      p,
      group_tops = sig_tops,
      margin_base = base_margin,
      plot_height = input$plot_h
    )
    if (flip_plot) {
      p <- suppressMessages(p + coord_flip(clip = "off"))
    }
    if (show_legend) {
      p <- apply_square_legend_right(p)
    }
    if (!is.null(box_stats)) {
      attr(p, "box_stats") <- box_stats
    }

    p
  })
}

plot_violin <- function(scope, strain = NULL) {
  build_plot(scope, strain, "Violin")
}
