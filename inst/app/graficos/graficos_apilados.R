# Helpers para gráficos apilados ----------------------------------------------

build_apiladas_plot_impl <- function(ctx) {
  with(ctx, {
    params_apilar <- input$stackParams
    if (length(params_apilar) == 0) {
      return(
        ggplot() + theme_void() +
          annotate("text", 0, 0, label = tr_text("select_params_prompt", lang))
      )
    }

    df_f <- scope_df

    missing <- setdiff(params_apilar, names(df_f))
    if (length(missing)) {
      return(
        ggplot() + theme_void() +
          annotate(
            "text", 0, 0,
            label = sprintf(tr_text("missing_params_list", lang), paste(missing, collapse = ", "))
          )
      )
    }

    order_stack_input <- trimws(strsplit(input$orderStack, ",")[[1]])
    order_levels <- intersect(order_stack_input, params_apilar)
    stack_levels <- if (length(order_levels)) order_levels else params_apilar

    eje_x <- if (scope == "Por Cepa") {
      "Media"
    } else if (isTRUE(input$labelMode)) {
      "Strain"
    } else {
      "Label"
    }

    summary_mode_active <- isTRUE(is_summary_mode())
    if (summary_mode_active) {
      stack_parts <- lapply(params_apilar, function(pm) {
        sd_col <- resolve_prefixed_param_col(df_f, "SD_", pm)
        df_f %>%
          group_by(.data[[eje_x]]) %>%
          summarise(
            Mean = mean(.data[[pm]], na.rm = TRUE),
            SD = if (!is.null(sd_col) && sd_col %in% names(df_f)) {
              mean(.data[[sd_col]], na.rm = TRUE)
            } else {
              sd(.data[[pm]], na.rm = TRUE)
            },
            .groups = "drop"
          ) %>%
          mutate(Parametro = pm)
      })
      df_long <- bind_rows(stack_parts) %>%
        mutate(Parametro = factor(Parametro, levels = stack_levels)) %>%
        arrange(.data[[eje_x]], Parametro)
    } else {
      df_long <- df_f %>%
        pivot_longer(all_of(params_apilar), names_to = "Parametro", values_to = "Valor") %>%
        group_by(.data[[eje_x]], Parametro) %>%
        summarise(
          Mean = mean(Valor, na.rm = TRUE),
          SD = sd(Valor, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(Parametro = factor(Parametro, levels = stack_levels)) %>%
        arrange(.data[[eje_x]], Parametro)
    }

    if (isTRUE(input$x_wrap)) {
      df_long[[eje_x]] <- wrap_label(df_long[[eje_x]], lines = input$x_wrap_lines)
    }

    flip_plot <- isTRUE(input$plot_flip)
    x_ang <- get_x_angle(
      n = length(unique(df_long[[eje_x]])),
      angle_input = input$x_angle
    )
    x_ang_cat <- if (flip_plot && is.na(input$x_angle)) 0 else x_ang
    b_mar <- get_bottom_margin(x_ang_cat, input$x_wrap, input$x_wrap_lines)
    extra_bottom <- ceiling(input$fs_axis * 0.8)
    b_mar <- b_mar + extra_bottom

    pal <- palette_for_levels(stack_levels)
    legend_breaks <- stack_levels
    tone_down_cols <- function(cols, amount = 0.25) {
      amt <- pmin(pmax(amount, 0), 1)
      m <- grDevices::col2rgb(cols)
      m2 <- m + (255 - m) * amt
      grDevices::rgb(m2[1, ] / 255, m2[2, ] / 255, m2[3, ] / 255)
    }
    outline_only <- isTRUE(input$stack_outline_only)
    seg_line_col <- if (outline_only) NA else "black"
    p <- ggplot(df_long, aes(x = .data[[eje_x]], y = Mean, fill = Parametro)) +
      geom_col(
        position = "stack",
        width = 0.7,
        colour = seg_line_col,
        linewidth = 0.6,
        alpha = 0.85
      ) +
      scale_fill_manual(
        values = tone_down_cols(pal[stack_levels], amount = 0.25),
        breaks = legend_breaks,
        guide = guide_legend(title = NULL, reverse = FALSE)
      )

    if (outline_only) {
      border_df <- df_long %>%
        group_by(.data[[eje_x]]) %>%
        summarise(total = sum(Mean, na.rm = TRUE), .groups = "drop")
      p <- p +
        geom_col(
          data = border_df,
          inherit.aes = FALSE,
          aes(x = .data[[eje_x]], y = total),
          width = 0.7,
          fill = NA,
          colour = "black",
          linewidth = 0.6
        )
    }

    if (isTRUE(input$showErrBars)) {
      cap_half_width <- 0.7 / 2

      err_df <- df_long %>%
        mutate(
          xnum = as.numeric(factor(.data[[eje_x]], levels = unique(.data[[eje_x]]))),
          Parametro = factor(Parametro, levels = stack_levels)
        ) %>%
        arrange(xnum, Parametro) %>%
        group_by(xnum) %>%
        mutate(
          ybottom = cumsum(Mean) - Mean,
          ytop = ybottom + Mean,
          ystart = ytop,
          yend = ytop + ifelse(is.finite(SD), SD, 0)
        ) %>%
        ungroup()

      if (isTRUE(input$errbar_param_color)) {
        err_df$err_color <- pal[as.character(err_df$Parametro)]
        err_df$err_color[is.na(err_df$err_color) | !nzchar(err_df$err_color)] <- "black"
        p <- p +
          geom_segment(
            data = err_df,
            inherit.aes = FALSE,
            aes(x = xnum, xend = xnum, y = ystart, yend = yend, color = err_color),
            size = input$errbar_size,
            show.legend = FALSE
          ) +
          geom_segment(
            data = err_df,
            inherit.aes = FALSE,
            aes(
              x = xnum - cap_half_width,
              xend = xnum + cap_half_width,
              y = yend,
              yend = yend,
              color = err_color
            ),
            size = input$errbar_size,
            show.legend = FALSE
          ) +
          scale_color_identity()
      } else {
        p <- p +
          geom_segment(
            data = err_df,
            inherit.aes = FALSE,
            aes(x = xnum, xend = xnum, y = ystart, yend = yend),
            size = input$errbar_size,
            colour = "black",
            show.legend = FALSE
          ) +
          geom_segment(
            data = err_df,
            inherit.aes = FALSE,
            aes(
              x = xnum - cap_half_width,
              xend = xnum + cap_half_width,
              y = yend,
              yend = yend
            ),
            size = input$errbar_size,
            colour = "black",
            show.legend = FALSE
          )
      }
    }

    label_tops <- df_long %>%
      mutate(
        group = .data[[eje_x]],
        Parametro = factor(Parametro, levels = stack_levels)
      ) %>%
      arrange(group, Parametro) %>%
      group_by(group) %>%
      mutate(
        ybottom = cumsum(Mean) - Mean,
        ytop = ybottom + Mean
      ) %>%
      ungroup()
    if (isTRUE(input$showErrBars)) {
      label_tops <- label_tops %>%
        mutate(y_top = ytop + ifelse(is.na(SD) | !is.finite(SD), 0, SD))
    } else {
      label_tops <- label_tops %>%
        mutate(y_top = ytop)
    }
    label_tops <- label_tops %>%
      transmute(group = group, param = Parametro, y_top = y_top)

    base_margin <- if (flip_plot) {
      margin_adj(12, 18, 28, b_mar)
    } else {
      margin_adj(12, 18, b_mar, 28)
    }
    y_axis_label <- if (nzchar(input$yLab)) input$yLab else ps$Y_Title
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
        x = if (flip_plot) y_axis_label else NULL,
        y = if (flip_plot) NULL else y_axis_label
      ) +
      scale_y_continuous(
        limits = c(0, input$ymax),
        breaks = seq(0, input$ymax, by = input$ybreak),
        expand = c(0, 0)
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
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", colour = NA),
        legend.text = element_text(size = fs_legend, colour = "black"),
        legend.key = element_blank(),
        legend.key.size = unit(1.4, "lines")
      )

    p <- apply_sig_layers(
      p,
      group_tops = label_tops,
      margin_base = base_margin,
      plot_height = input$plot_h,
      default_param = input$sig_param
    )
    if (flip_plot) {
      p <- suppressMessages(p + coord_flip(clip = "off"))
    }

    p
  })
}

export_plotly_png <- function(p, file, width, height, delay = 0.5, zoom = 3) {
  p <- p %>% layout(paper_bgcolor = "rgba(0,0,0,0)",
                    plot_bgcolor  = "rgba(0,0,0,0)")
  tmp_html <- tempfile(fileext = ".html")
  on.exit(unlink(tmp_html), add = TRUE)
  htmlwidgets::saveWidget(p, tmp_html, selfcontained = TRUE)
  webshot2::webshot(url = tmp_html, file = file,
                    vwidth = width, vheight = height,
                    delay = delay, zoom = zoom)
}

plot_apiladas <- function(scope, strain = NULL) {
  build_plot(scope, strain, "Apiladas")
}
