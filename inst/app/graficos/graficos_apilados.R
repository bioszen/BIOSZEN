# Helpers para gráficos apilados ----------------------------------------------

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

build_plotly_stack <- function(scope, strain = NULL) {
  num <- function(x) as.numeric(gsub(",", ".", x))
  params_apilar <- input$stackParams
  validate(need(length(params_apilar) > 0,
                "Selecciona ≥1 parámetro en “Parámetros incluidos”"))
  base_df <- if (isTRUE(input$doNorm)) datos_agrupados_norm() else datos_agrupados()
  df_f <- if (scope == "Por Cepa") {
    base_df |>
      filter(Strain == strain) |>
      order_filter_strain()    |>
      filter_reps_strain()
  } else {
    base_df |> order_filter_group()
  }
  eje_x <- if (scope == "Por Cepa") {
    "Media"
  } else if (isTRUE(input$labelMode)) {
    "Strain"
  } else {
    "Label"
  }
  if (is.factor(df_f[[eje_x]])) {
    df_f[[eje_x]] <- droplevels(df_f[[eje_x]])
  }
  df_f[[eje_x]] <- as.character(df_f[[eje_x]])

  order_stack_input <- trimws(strsplit(input$orderStack, ",")[[1]])
  order_levels      <- intersect(order_stack_input, params_apilar)
  stack_levels      <- if (length(order_levels)) order_levels else params_apilar

  df_long <- df_f |>
    pivot_longer(all_of(params_apilar), names_to = "Parametro", values_to = "Valor") |>
    group_by(.data[[eje_x]], Parametro) |>
    summarise(Mean = mean(Valor, na.rm = TRUE), SD = sd(Valor, na.rm = TRUE), .groups = "drop") |>
    mutate(Parametro = factor(Parametro, levels = stack_levels)) |>
    arrange(.data[[eje_x]], Parametro)
  wrap_lines <- if (is.null(input$x_wrap_lines)) 2 else input$x_wrap_lines
  x_ang <- get_x_angle(length(unique(df_long[[eje_x]])), input$x_angle)
  b_mar <- if (isTRUE(input$x_wrap)) {
    50 + 20 * (wrap_lines - 1)
  } else if (is.na(x_ang) || x_ang <= 0) {
    30
  } else {
    60
  }
  b_mar <- b_mar + ceiling(input$fs_axis * 0.8)
  pal <- get_palette(length(params_apilar))
  names(pal) <- params_apilar
  plt <- plot_ly(width = input$plot_w, height = input$plot_h)
  for (p in stack_levels) {
    sub <- df_long[df_long$Parametro == p, ]
    plt <- add_trace(plt,
                     x = sub[[eje_x]], y = sub$Mean,
                     type = "bar", name = p,
                     marker = list(color = pal[[p]], line = list(color = "black", width = 1)))
  }
  if (isTRUE(input$showErrBars)) {
    err_df <- df_long %>% group_by(.data[[eje_x]]) %>%
      arrange(Parametro, .by_group = TRUE) %>%
      mutate(y_top = cumsum(Mean)) %>% ungroup()
    thick <- num(input$errbar_size) * 1.6
    for (p in stack_levels) {
      sub <- err_df[err_df$Parametro == p, ]
      plt <- add_trace(plt, x = sub[[eje_x]], y = sub$y_top,
                       type = "scatter", mode = "markers",
                       marker = list(size = 1, opacity = 0),
                       showlegend = FALSE, hoverinfo = "skip",
                       error_y = list(type = "data", symmetric = FALSE,
                                      array = sub$SD, arrayminus = rep(0, nrow(sub)),
                                      color = "black", thickness = thick, width = 20))
    }
  }
  plt %>% layout(
    barmode = "stack",
    margin = list(t = input$fs_title * 2 + 20, b = b_mar),
    title = list(text = input$plotTitle,
                 font = list(size = input$fs_title, family = "Helvetica"),
                 y = 0.95),
    yaxis = list(titlefont = list(size = input$fs_axis, family = "Helvetica", color = "black"),
                 tickfont  = list(size = input$fs_axis, family = "Helvetica", color = "black"),
                 range = c(0, input$ymax), dtick = input$ybreak,
                 showline = TRUE, linecolor = "black", linewidth = input$axis_line_size,
                 ticks = "outside", ticklen = 5, tickcolor = "black", showgrid = FALSE),
    xaxis = list(title = "", type = "category", categoryorder = "array",
                 categoryarray = unique(df_long[[eje_x]]),
                 titlefont = list(size = input$fs_axis, family = "Helvetica", color = "black"),
                 tickfont = list(size = input$fs_axis, family = "Helvetica", color = "black"),
                 tickangle = -x_ang, showline = TRUE, linecolor = "black",
                 linewidth = input$axis_line_size, ticks = "outside",
                 ticklen = 5, tickcolor = "black", showgrid = FALSE,
                 automargin = TRUE),
    legend = list(title = list(text = ""), traceorder = "normal",
                  font = list(size = input$fs_legend, family = "Helvetica"))
  )
}

plot_apiladas <- function(scope, strain = NULL) {
  build_plot(scope, strain, "Apiladas")
}
