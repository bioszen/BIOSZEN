# Helpers para gráficos Boxplot ------------------------------------------------

add_sigline <- function(p, group1, group2, label = "*",
                        height   = .05,
                        vsize    = .02,
                        tpad     = .01,
                        linewidth = .8,
                        textsize  = 5) {
  build  <- ggplot_build(p)
  xbreaks <- build$layout$panel_params[[1]]$x$breaks
  get_x   <- function(g) if (is.numeric(g)) g else match(g, xbreaks)
  x1 <- get_x(group1); x2 <- get_x(group2)
  dat   <- build$data[[1]]
  ytop  <- if ("ymax" %in% names(dat)) max(dat$ymax, na.rm = TRUE)
            else max(dat$y, na.rm = TRUE)
  yrng  <- diff(range(build$layout$panel_params[[1]]$y.range))
  ybar  <- ytop + height * yrng
  ycap  <- ybar - vsize * yrng
  ytxt  <- ybar + tpad * yrng
  p +
    annotate("segment", x = x1, xend = x2, y = ybar, yend = ybar, linewidth = linewidth) +
    annotate("segment", x = x1, xend = x1, y = ybar, yend = ycap, linewidth = linewidth) +
    annotate("segment", x = x2, xend = x2, y = ybar, yend = ycap, linewidth = linewidth) +
    annotate("text",    x = mean(c(x1, x2)), y = ytxt,
             label = label, size = textsize, vjust = 0) +
    coord_cartesian(clip = "off") +
    theme(plot.margin = margin(5.5, 20, 5.5, 5.5))
}

stack_siglines <- function(p, sigs, sep = .05,
                           linewidth = .8, vsize = .02,
                           tpad = .01, tsize = 5) {
  if (length(sigs) == 0 || length(ggplot_build(p)$data) == 0)
    return(p)

  build  <- ggplot_build(p)
  yrng   <- build$layout$panel_params[[1]]$y.range
  xranks <- build$layout$panel_params[[1]]$x$breaks

  get_span <- function(cmp) {
    x1 <- if (is.numeric(cmp$g1)) cmp$g1 else match(cmp$g1, xranks)
    x2 <- if (is.numeric(cmp$g2)) cmp$g2 else match(cmp$g2, xranks)
    c(min(x1, x2), max(x1, x2))
  }

  levels <- list()
  bar_level <- integer(length(sigs))

  for (i in seq_along(sigs)) {
    span <- get_span(sigs[[i]])
    placed <- FALSE
    for (lvl in seq_along(levels)) {
      overlap <- vapply(levels[[lvl]], function(iv)
        !(span[2] < iv[1] || span[1] > iv[2]), logical(1))
      if (!any(overlap)) {
        levels[[lvl]] <- append(levels[[lvl]], list(span))
        bar_level[i]  <- lvl
        placed <- TRUE; break
      }
    }
    if (!placed) {
      levels[[length(levels) + 1]] <- list(span)
      bar_level[i] <- length(levels)
    }
  }

  extra <- (length(levels) + 1) * sep * diff(yrng)
  p <- p + expand_limits(y = max(yrng) + extra)

  for (i in seq_along(sigs)) {
    h   <- bar_level[i] * sep
    cmp <- sigs[[i]]
    p <- add_sigline(p, cmp$g1, cmp$g2, cmp$lab,
                     height = h, vsize = vsize,
                     tpad = tpad, linewidth = linewidth,
                     textsize = tsize)
  }
  p
}

build_boxplot_plot_impl <- function(ctx) {
  with(ctx, {
    if (scope == "Combinado") {
      df_labels <- scope_df %>% distinct(Label, Strain, Media) %>% filter(!is.na(Label))
      names(scope_df) <- enc2utf8(names(scope_df))
      if (!param_sel %in% names(scope_df)) {
        return(ggplot() + theme_void() + annotate("text", 0, 0, label = msg_no_data_sel))
      }
      scope_df[[param_sel]] <- suppressWarnings(as.numeric(scope_df[[param_sel]]))
      df_plot <- scope_df %>%
        transmute(Label, Valor = .data[[param_sel]]) %>%
        filter(is.finite(Valor), !is.na(Label))

      if (nrow(df_plot) == 0) {
        return(
          ggplot() +
            theme_void() +
            annotate("text", 0, 0, label = msg_no_data_sel)
        )
      }

      data_max <- suppressWarnings(max(df_plot$Valor, na.rm = TRUE))
      ymax_plot <- if (is.finite(data_max) && data_max > ymax) data_max else ymax

      if (isTRUE(input$x_wrap)) {
        df_plot$Label <- wrap_label(df_plot$Label, lines = input$x_wrap_lines)
        df_labels$Label <- wrap_label(df_labels$Label, lines = input$x_wrap_lines)
      }
      if (is.factor(df_plot$Label)) {
        df_plot$Label <- droplevels(df_plot$Label)
      } else {
        df_plot$Label <- factor(df_plot$Label, levels = unique(df_plot$Label))
      }
      df_labels <- df_labels %>% filter(Label %in% levels(df_plot$Label))
      df_labels$Label <- factor(df_labels$Label, levels = levels(df_plot$Label))
      box_stats <- df_plot %>%
        group_by(Label) %>%
        summarise(
          q1 = stats::quantile(Valor, 0.25, na.rm = TRUE, names = FALSE),
          median = stats::median(Valor, na.rm = TRUE),
          q3 = stats::quantile(Valor, 0.75, na.rm = TRUE, names = FALSE),
          lower = min(Valor, na.rm = TRUE),
          upper = max(Valor, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(group = as.character(Label)) %>%
        dplyr::select(group, q1, median, q3, lower, upper)
      pal <- palette_for_labels(df_labels, levels(df_plot$Label))
      flip_plot <- isTRUE(input$plot_flip)
      x_ang <- get_x_angle(
        n = nlevels(df_plot$Label),
        angle_input = input$x_angle
      )
      x_ang_cat <- if (flip_plot && is.na(input$x_angle)) 0 else x_ang
      b_mar <- get_bottom_margin(x_ang_cat, input$x_wrap, input$x_wrap_lines)
      p <- ggplot(df_plot, aes(Label, Valor))
      show_legend <- legend_right_enabled(input$colorMode)

      if (input$colorMode == "Blanco y Negro") {
        p <- p +
          geom_boxplot(
            fill = "white", colour = "black", width = input$box_w,
            linewidth = input$errbar_size,
            coef = box_coef,
            outlier.shape = NA,
            na.rm = TRUE,
            show.legend = show_legend,
            key_glyph = "rect"
          ) +
          geom_jitter(
            shape = 21,
            fill = "white",
            colour = "black",
            stroke = 0.4,
            width = input$pt_jit,
            size = input$pt_size,
            na.rm = TRUE,
            show.legend = FALSE
          )
      } else {
        p <- p +
          geom_boxplot(
            aes(fill = Label), width = input$box_w, colour = "black",
            linewidth = input$errbar_size,
            coef = box_coef,
            alpha = 0.5,
            outlier.shape = NA,
            na.rm = TRUE,
            show.legend = show_legend,
            key_glyph = "rect"
          ) +
          geom_jitter(
            aes(fill = Label),
            width = input$pt_jit,
            shape = 21,
            colour = "black",
            stroke = 0.5,
            size = input$pt_size,
            alpha = 1,
            na.rm = TRUE,
            show.legend = FALSE
          ) +
          scale_fill_manual(values = pal)
      }

      base_margin <- if (flip_plot) {
        margin_adj(5.5, 5.5, 25, b_mar)
      } else {
        margin_adj(5.5, 5.5, b_mar, 25)
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
          limits = c(0, ymax_plot),
          breaks = seq(0, ymax_plot, by = ybreak),
          expand = c(0, 0),
          oob = scales::oob_keep
        ) +
        theme_minimal(base_size = input$base_size, base_family = "Helvetica") +
        coord_cartesian(clip = "off") +
        theme(
          plot.margin = base_margin,
          plot.title = element_text(size = fs_title, face = "bold"),
          axis.title.x = axis_title_x_el,
          axis.title.y = axis_title_y_el,
          axis.text.x = axis_text_x_el,
          axis.text.y = axis_text_y_el,
          axis.line = element_line(linewidth = axis_size, colour = "black"),
          axis.ticks = element_line(linewidth = axis_size, colour = "black"),
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
      if (show_legend) {
        p <- apply_square_legend_right(p)
      }
      p <- add_whisker_caps(
        p,
        box_stats,
        levels(df_plot$Label),
        input$box_w,
        input$errbar_size
      )
      if (flip_plot) {
        p <- suppressMessages(p + coord_flip(clip = "off"))
      }
      if (!is.null(box_stats)) {
        attr(p, "box_stats") <- box_stats
      }

      return(p)
    }

    names(scope_df) <- enc2utf8(names(scope_df))
    if (!param_sel %in% names(scope_df)) {
      return(ggplot() + theme_void() + annotate("text", 0, 0, label = msg_no_data_sel))
    }
    scope_df[[param_sel]] <- suppressWarnings(as.numeric(scope_df[[param_sel]]))
    df <- scope_df %>%
      filter(is.finite(.data[[param_sel]]), !is.na(Media))

    if (nrow(df) == 0) {
      return(ggplot() + theme_void() +
        annotate("text", x = 0, y = 0, label = msg_no_data_sel))
    }

    data_max <- suppressWarnings(max(df[[param_sel]], na.rm = TRUE))
    ymax_plot <- if (is.finite(data_max) && data_max > ymax) data_max else ymax
    df_points <- if (isTRUE(for_interactive)) {
      downsample_points_by_group(df, "Media", cap_total = 7000L)
    } else {
      df
    }

    if (isTRUE(input$x_wrap)) {
      df$Media <- wrap_label(df$Media, lines = input$x_wrap_lines)
    }
    if (is.factor(df$Media)) {
      df$Media <- droplevels(df$Media)
    } else {
      df$Media <- factor(df$Media, levels = unique(df$Media))
    }
    box_stats <- df %>%
      group_by(Media) %>%
      summarise(
        q1 = stats::quantile(.data[[param_sel]], 0.25, na.rm = TRUE, names = FALSE),
        median = stats::median(.data[[param_sel]], na.rm = TRUE),
        q3 = stats::quantile(.data[[param_sel]], 0.75, na.rm = TRUE, names = FALSE),
        lower = min(.data[[param_sel]], na.rm = TRUE),
        upper = max(.data[[param_sel]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(group = as.character(Media)) %>%
      dplyr::select(group, q1, median, q3, lower, upper)
    flip_plot <- isTRUE(input$plot_flip)
    x_ang <- get_x_angle(
      n = nlevels(factor(df$Media)),
      angle_input = input$x_angle
    )
    x_ang_cat <- if (flip_plot && is.na(input$x_angle)) 0 else x_ang
    b_mar <- get_bottom_margin(x_ang_cat, input$x_wrap, input$x_wrap_lines)
    show_legend <- legend_right_enabled(colourMode)
    if (colourMode == "Blanco y Negro") {
      p <- ggplot(df, aes(Media, .data[[param_sel]])) +
        geom_boxplot(
          fill = "white", colour = "black", width = input$box_w,
          linewidth = input$errbar_size,
          coef = box_coef,
          outlier.shape = NA,
          na.rm = TRUE,
          show.legend = show_legend,
          key_glyph = "rect"
        ) +
        geom_jitter(
          data = df_points,
          shape = 21,
          fill = "white",
          colour = "black",
          stroke = 0.4,
          width = input$pt_jit,
          size = input$pt_size,
          na.rm = TRUE,
          show.legend = FALSE
        )
    } else {
      media_levels <- if (is.factor(df$Media)) levels(df$Media) else unique(as.character(df$Media))
      pal <- palette_for_levels(media_levels)
      p <- ggplot(df, aes(Media, .data[[param_sel]], fill = Media)) +
        geom_boxplot(
          width = input$box_w, colour = "black",
          linewidth = input$errbar_size,
          coef = box_coef,
          alpha = 0.5,
          outlier.shape = NA,
          na.rm = TRUE,
          show.legend = show_legend,
          key_glyph = "rect"
        ) +
        geom_jitter(
          data = df_points,
          aes(fill = Media),
          width = input$pt_jit,
          shape = 21,
          colour = "black",
          stroke = 0.5,
          size = input$pt_size,
          alpha = 0.95,
          na.rm = TRUE,
          show.legend = FALSE
        ) +
        scale_fill_manual(values = pal)
    }
    base_margin <- if (flip_plot) {
      margin_adj(5.5, 5.5, 25, b_mar)
    } else {
      margin_adj(5.5, 5.5, b_mar, 25)
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
        limits = c(0, ymax_plot),
        breaks = seq(0, ymax_plot, by = ybreak),
        expand = c(0, 0),
        oob = scales::oob_keep
      ) +
      theme_minimal(base_size = input$base_size, base_family = "Helvetica") +
      coord_cartesian(clip = "off") +
      theme(
        plot.margin = base_margin,
        plot.title = element_text(size = fs_title, face = "bold"),
        axis.title.x = axis_title_x_el,
        axis.title.y = axis_title_y_el,
        axis.text.x = axis_text_x_el,
        axis.text.y = axis_text_y_el,
        axis.line = element_line(linewidth = axis_size, colour = "black"),
        axis.ticks = element_line(linewidth = axis_size, colour = "black"),
        panel.grid = element_blank(),
        legend.position = "none"
      )
    sig_tops <- df %>%
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
    if (show_legend) {
      p <- apply_square_legend_right(p)
    }
    p <- add_whisker_caps(
      p,
      box_stats,
      levels(df$Media),
      input$box_w,
      input$errbar_size
    )
    if (flip_plot) {
      p <- suppressMessages(p + coord_flip(clip = "off"))
    }

    p
  })
}

plot_boxplot <- function(scope, strain = NULL) {
  build_plot(scope, strain, "Boxplot")
}
