# Panel composition and overrides module
setup_panel_module <- function(input, output, session, plot_bank, panel_inserto,
                              ov_trigger, snapshot_fun, meta_fun, curve_settings) {
  output$plotOverrideUI <- renderUI({
    input$app_lang
    tagList(
      selectInput(
        "ov_tipo",
        tr("override_apply_to"),
        choices = named_choices(
          c("Todos", "Boxplot", "Barras", "Violin", "Apiladas", "Correlacion", "Curvas"),
          c(
            tr("override_all"),
            tr("plot_boxplot"),
            tr("plot_bars"),
            tr("plot_violin"),
            tr("plot_stacked"),
            tr("plot_correlation"),
            tr("plot_curves")
          )
        ),
        selected = "Todos"
      ),
      textInput("ov_title", tr("override_title"), ""),
      numericInput("ov_fs_title", tr("override_title_size"), NA, min = 6),
      numericInput("ov_fs_axis", tr("override_axis_size"), NA, min = 6),
      numericInput("ov_fs_legend", tr("override_legend_size"), NA, min = 6),
      numericInput("ov_axis_size", tr("override_axis_line_size"), NA, min = 0.1, step = 0.1),
      numericInput("ov_cur_lwd", tr("override_curves_linewidth"), NA, min = 0.5, step = 0.1),
      numericInput("ov_cur_pt", tr("override_curves_point_size"), NA, min = 0.5, step = 0.5),
      hr(), h4(tr("override_box_section")),
      numericInput("ov_box_w", tr("override_box_width"), NA, min = 0.1, max = 1.5, step = 0.05),
      numericInput("ov_errbar_size", tr("override_errbar_size"), NA, min = 0.1, step = 0.1),
      numericInput("ov_pt_size", tr("override_point_size"), NA, min = 0.5, max = 20, step = 0.5),
      numericInput("ov_pt_jit", tr("override_point_jitter"), NA, min = 0, max = 0.5, step = 0.01),
      numericInput("ov_sig_lwd", tr("override_sig_linewidth"), NA, min = 0.2, step = 0.2),
      numericInput("ov_sig_txt", tr("override_sig_textsize"), NA, min = 1, step = 0.5),
      actionButton("apply_ov", tr("override_apply_all"), class = "btn btn-success")
    )
  })

  observe({
    ids <- names(plot_bank$all)
    if (is.null(ids)) ids <- character(0)
    choices <- if (length(ids)) named_choices(ids, paste(tr("plot_label"), seq_along(ids))) else character(0)
    updateSelectInput(session, 'plot_edit', choices = choices)
  })


  observeEvent(input$apply_ov, {
    ov_new <- list(
      title     = if (nzchar(input$ov_title))    input$ov_title    else NULL,
      fs_title  = if (!is.na(input$ov_fs_title)) input$ov_fs_title else NULL,
      fs_axis   = if (!is.na(input$ov_fs_axis))  input$ov_fs_axis  else NULL,
      fs_legend = if (!is.na(input$ov_fs_legend))input$ov_fs_legend else NULL,
      axis_size = if (!is.na(input$ov_axis_size)) input$ov_axis_size else NULL,
      cur_lwd   = if (!is.na(input$ov_cur_lwd))  input$ov_cur_lwd  else NULL,
      cur_pt    = if (!is.na(input$ov_cur_pt))   input$ov_cur_pt   else NULL,
      box_w     = if (!is.na(input$ov_box_w))    input$ov_box_w    else NULL,
      errbar_size = if (!is.na(input$ov_errbar_size)) input$ov_errbar_size else NULL,
      pt_size   = if (!is.na(input$ov_pt_size))  input$ov_pt_size  else NULL,
      pt_jit    = if (!is.na(input$ov_pt_jit))   input$ov_pt_jit   else NULL,
      sig_lwd   = if (!is.na(input$ov_sig_lwd))  input$ov_sig_lwd  else NULL,
      sig_txt   = if (!is.na(input$ov_sig_txt))  input$ov_sig_txt  else NULL
    )
    ids <- names(plot_bank$all)
    if (input$ov_tipo != "Todos") {
      ids <- ids[vapply(plot_bank$all[ids], function(x) x$type == input$ov_tipo, logical(1))]
    }
    for (id in ids) plot_bank$all[[id]]$overrides <- ov_new
    if (length(ids)) ov_trigger(ov_trigger() + 1)
  })
  observeEvent(input$add2panel, {
    id   <- paste0('p', as.integer(Sys.time()))
    snap <- snapshot_fun()
    meta <- meta_fun()
    cfg  <- if (identical(input$tipo, 'Curvas')) curve_settings() else NULL
    plot_bank$all[[id]] <- list(
      id        = id,
      plot      = snap,
      overrides = list(),
      type      = input$tipo,
      scope     = input$scope,
      strain    = if (identical(input$scope, 'Por Cepa')) sanitize(input$strain) else NULL,
      meta      = meta,
      curve_cfg = cfg
    )
    showNotification(tr("combo_added"), type = 'message', duration = 2)
    if (!panel_inserto()) {
      insertTab(
        inputId  = 'mainTabs',
        tab      = tab_compos,
        target   = "tab_growth",
        position = 'after',
        select   = FALSE
      )
      panel_inserto(TRUE)
    }
  })

  output$plotPicker <- renderUI({
    input$app_lang
    if (length(plot_bank$all) == 0)
      return(helpText(tr("combo_empty")))
    ids <- names(plot_bank$all)
    choices <- named_choices(ids, paste(tr("plot_label"), seq_along(ids)))
    tagList(
      checkboxGroupInput('plots_chosen', tr("combo_select_plots"),
                         choices = choices, selected = choices),
      selectInput('plot_order', tr("combo_order_label"), choices = ids),
      actionButton('move_up',   tr("combo_move_up"), class = 'btn btn-secondary btn-sm'),
      actionButton('move_down', tr("combo_move_down"), class = 'btn btn-secondary btn-sm')
    )
  })

  combo_plot <- eventReactive(
    list(input$makeCombo, ov_trigger()), {
    input$show_legend_combo; input$combo_pal; input$nrow_combo;
    input$ncol_combo; input$base_size_combo; input$fs_title_all;
    input$fs_axis_title_all; input$fs_axis_text_all; input$fs_legend_all
    req(input$plots_chosen)
    library(patchwork)

    theme_ppt <- function(bs = 18, ax = 1.2) {
      theme_minimal(base_size = bs, base_family = "Helvetica") +
        theme(
          axis.line  = element_line(linewidth = ax),
          axis.ticks = element_line(linewidth = ax),
          panel.grid = element_blank(),
          plot.title = element_text(colour = "black", family = "Helvetica"),
          axis.title = element_text(colour = "black", family = "Helvetica"),
          axis.text  = element_text(colour = "black", family = "Helvetica"),
          legend.text= element_text(colour = "black", family = "Helvetica")
        )
    }

    adjust_sig_layers <- function(p, lwd = NULL, txt = NULL) {
      for (i in seq_along(p$layers)) {
        if (!is.null(lwd) && inherits(p$layers[[i]]$geom, "GeomSegment"))
          p$layers[[i]]$aes_params$linewidth <- lwd
        if (!is.null(txt) && inherits(p$layers[[i]]$geom, "GeomText"))
          p$layers[[i]]$aes_params$size <- txt
      }
      p
    }

    adjust_curve_layers <- function(p, lwd = NULL, size = NULL) {
      for (i in seq_along(p$layers)) {
        g <- p$layers[[i]]$geom
        if (!is.null(lwd) && inherits(g, "GeomLine"))
          p$layers[[i]]$aes_params$linewidth <- lwd
        if (!is.null(size) && inherits(g, "GeomPoint"))
          p$layers[[i]]$aes_params$size <- size
      }
      p
    }

    adjust_box_layers <- function(p, width = NULL, size = NULL, jit = NULL, err_lwd = NULL) {
      for (i in seq_along(p$layers)) {
        g <- p$layers[[i]]$geom
        if (!is.null(width)) {
          if (inherits(g, "GeomBoxplot") || inherits(g, "GeomCol"))
            p$layers[[i]]$geom_params$width <- width
          if (inherits(g, "GeomErrorbar")) {
            w <- width * 0.35
            if (w < 0.1) w <- 0.1
            p$layers[[i]]$geom_params$width <- w
          }
        }

        if (!is.null(err_lwd) && (inherits(g, "GeomErrorbar") || inherits(g, "GeomBoxplot")))
          p$layers[[i]]$aes_params$linewidth <- err_lwd
        if (inherits(g, "GeomPoint")) {
          if (!is.null(size)) p$layers[[i]]$aes_params$size <- size
          if (!is.null(jit) && inherits(p$layers[[i]]$position, "PositionJitter"))
            p$layers[[i]]$position$width <- jit
        }
      }
      p
    }

    plots <- lapply(plot_bank$all[input$plots_chosen], function(info) {
      p  <- info$plot
      ov <- info$overrides
      p  <- p + theme_ppt(input$base_size_combo)
      if (!is.null(ov$title))
        p <- p + ggtitle(ov$title)
      if (!is.null(ov$fs_title))
        p <- p + theme(plot.title = element_text(size = ov$fs_title, face = "bold"))
      if (!is.null(ov$fs_axis))
        p <- p + theme(axis.title = element_text(size = ov$fs_axis, face = "bold"),
                       axis.text  = element_text(size = ov$fs_axis))
      if (!is.null(ov$fs_legend))
        p <- p + theme(legend.text = element_text(size = ov$fs_legend))
      if (!is.null(ov$axis_size))
        p <- p + theme(axis.line = element_line(linewidth = ov$axis_size),
                       axis.ticks = element_line(linewidth = ov$axis_size))
      if (!is.null(ov$sig_lwd) || !is.null(ov$sig_txt))
        p <- adjust_sig_layers(p, ov$sig_lwd, ov$sig_txt)
      p <- adjust_curve_layers(p, ov$cur_lwd, ov$cur_pt)
      p <- adjust_box_layers(p, ov$box_w, ov$pt_size, ov$pt_jit, ov$errbar_size)
      if (isFALSE(input$show_legend_combo))
        p <- p + theme(legend.position = "none")
      p
    })

    max_col <- max(vapply(plots, function(p) {
      b <- ggplot_build(p)
      cols <- unique(unlist(lapply(b$data, function(d) d$group)))
      length(cols)
    }, numeric(1)), 0)
    if (identical(input$combo_pal, 'Original')) {
      pal <- pal_fill <- NULL
    } else {
      pal <- switch(input$combo_pal,
                    'Default'        = safe_hue(max_col),
                    'Blanco y Negro' = rep('black', max_col),
                    'Viridis'        = viridis::viridis(max_col),
                    'Plasma'         = viridis::plasma(max_col),
                    'Magma'          = viridis::magma(max_col),
                    'Cividis'        = viridis::cividis(max_col),
                    'Set1'           = RColorBrewer::brewer.pal(n = max_col, name = 'Set1'),
                    'Set2'           = RColorBrewer::brewer.pal(n = max_col, name = 'Set2'),
                    'Set3'           = RColorBrewer::brewer.pal(n = max_col, name = 'Set3'),
                    'Dark2'          = RColorBrewer::brewer.pal(n = max_col, name = 'Dark2'),
                    'Accent'         = RColorBrewer::brewer.pal(n = max_col, name = 'Accent'),
                    'Paired'         = RColorBrewer::brewer.pal(n = max_col, name = 'Paired'),
                    'Pastel1'        = RColorBrewer::brewer.pal(n = max_col, name = 'Pastel1'),
                    'Pastel2'        = RColorBrewer::brewer.pal(n = max_col, name = 'Pastel2'),
                    safe_hue(max_col))
      pal_fill <- if (identical(input$combo_pal, 'Blanco y Negro'))
        rep('white', length(pal))
      else
        scales::alpha(pal, 0.5)
    }
    res <- wrap_plots(plots, nrow = input$nrow_combo, ncol = input$ncol_combo) &
      theme(
        plot.title  = element_text(size = input$fs_title_all,      face = 'bold'),
        axis.title  = element_text(size = input$fs_axis_title_all, face = 'bold'),
        axis.text   = element_text(size = input$fs_axis_text_all),
        legend.text = element_text(size = input$fs_legend_all)
      )
    if (!is.null(pal)) {
      res <- res & scale_colour_manual(values = pal) &
        scale_fill_manual(values = pal_fill)
    }
    res
  })

  output$comboPreview <- renderPlot({
    req(combo_plot())
    combo_plot()
  }, width = function() input$combo_width,
     height = function() input$combo_height)

  collect_combo_meta <- function() {
    vals <- list(
      show_legend_combo = input$show_legend_combo,
      nrow_combo        = input$nrow_combo,
      ncol_combo        = input$ncol_combo,
      combo_width       = input$combo_width,
      combo_height      = input$combo_height,
      base_size_combo   = input$base_size_combo,
      fs_title_all      = input$fs_title_all,
      fs_axis_title_all = input$fs_axis_title_all,
      fs_axis_text_all  = input$fs_axis_text_all,
      fs_legend_all     = input$fs_legend_all,
      combo_pal         = input$combo_pal,
      ov_fs_title       = input$ov_fs_title,
      ov_fs_axis        = input$ov_fs_axis,
      ov_fs_legend      = input$ov_fs_legend,
      ov_axis_size      = input$ov_axis_size,
      ov_cur_lwd        = input$ov_cur_lwd,
      ov_cur_pt         = input$ov_cur_pt,
      ov_box_w          = input$ov_box_w,
      ov_errbar_size    = input$ov_errbar_size,
      ov_pt_size        = input$ov_pt_size,
      ov_pt_jit         = input$ov_pt_jit,
      ov_sig_lwd        = input$ov_sig_lwd,
      ov_sig_txt        = input$ov_sig_txt
    )
    tibble::tibble(
      Campo = names(vals),
      Valor = vapply(vals, as.character, character(1))
    )
  }

  output$dl_combo_meta <- downloadHandler(
    filename = function() 'metadata_composicion.xlsx',
    content  = function(file){
      wb <- createWorkbook()
      addWorksheet(wb, 'Metadata')
      writeData(wb, 'Metadata', collect_combo_meta(),
                headerStyle = createStyle(textDecoration = 'bold'))
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )

    observeEvent(input$combo_meta, {
      req(input$combo_meta)
      sel_prev   <- isolate(input$plots_chosen)
      order_prev <- isolate(input$plot_order)
      path       <- input$combo_meta$datapath
      meta <- tryCatch(read_excel_tmp(path, sheet = 'Metadata'),
                       error = function(e) NULL)
      if (is.null(meta)) {
        showNotification(tr("composition_meta_invalid"), type = 'error', duration = 6)
        return()
      }
      meta$Valor <- as.character(meta$Valor)
      gv <- function(campo) {
        v <- meta$Valor[meta$Campo == campo]
        if (length(v)) v else NULL
      }
      if (!is.null(v <- gv('show_legend_combo')))
        updateCheckboxInput(session, 'show_legend_combo', value = tolower(v) == 'true')
      if (!is.null(v <- gv('nrow_combo')))
        updateNumericInput(session, 'nrow_combo', value = as.numeric(v))
      if (!is.null(v <- gv('ncol_combo')))
        updateNumericInput(session, 'ncol_combo', value = as.numeric(v))
      if (!is.null(v <- gv('combo_width')))
        updateNumericInput(session, 'combo_width', value = as.numeric(v))
      if (!is.null(v <- gv('combo_height')))
        updateNumericInput(session, 'combo_height', value = as.numeric(v))
      if (!is.null(v <- gv('base_size_combo')))
        updateNumericInput(session, 'base_size_combo', value = as.numeric(v))
      if (!is.null(v <- gv('fs_title_all')))
        updateNumericInput(session, 'fs_title_all', value = as.numeric(v))
      if (!is.null(v <- gv('fs_axis_title_all')))
        updateNumericInput(session, 'fs_axis_title_all', value = as.numeric(v))
      if (!is.null(v <- gv('fs_axis_text_all')))
        updateNumericInput(session, 'fs_axis_text_all', value = as.numeric(v))
      if (!is.null(v <- gv('fs_legend_all')))
        updateNumericInput(session, 'fs_legend_all', value = as.numeric(v))
      if (!is.null(v <- gv('combo_pal')))
        updateSelectInput(session, 'combo_pal', selected = v)

      ov_new <- list()
      if (!is.null(v <- gv('ov_fs_title'))) {
        updateNumericInput(session, 'ov_fs_title', value = as.numeric(v))
        ov_new$fs_title <- as.numeric(v)
      }
      if (!is.null(v <- gv('ov_fs_axis'))) {
        updateNumericInput(session, 'ov_fs_axis', value = as.numeric(v))
        ov_new$fs_axis <- as.numeric(v)
      }
      if (!is.null(v <- gv('ov_fs_legend'))) {
        updateNumericInput(session, 'ov_fs_legend', value = as.numeric(v))
        ov_new$fs_legend <- as.numeric(v)
      }
      if (!is.null(v <- gv('ov_axis_size'))) {
        updateNumericInput(session, 'ov_axis_size', value = as.numeric(v))
        ov_new$axis_size <- as.numeric(v)
      }
      if (!is.null(v <- gv('ov_cur_lwd'))) {
        updateNumericInput(session, 'ov_cur_lwd', value = as.numeric(v))
        ov_new$cur_lwd <- as.numeric(v)
      }
      if (!is.null(v <- gv('ov_cur_pt'))) {
        updateNumericInput(session, 'ov_cur_pt', value = as.numeric(v))
        ov_new$cur_pt <- as.numeric(v)
      }
      if (!is.null(v <- gv('ov_box_w'))) {
        updateNumericInput(session, 'ov_box_w', value = as.numeric(v))
        ov_new$box_w <- as.numeric(v)
      }
      if (!is.null(v <- gv('ov_errbar_size'))) {
        updateNumericInput(session, 'ov_errbar_size', value = as.numeric(v))
        ov_new$errbar_size <- as.numeric(v)
      }
      if (!is.null(v <- gv('ov_pt_size'))) {
        updateNumericInput(session, 'ov_pt_size', value = as.numeric(v))
        ov_new$pt_size <- as.numeric(v)
      }
      if (!is.null(v <- gv('ov_pt_jit'))) {
        updateNumericInput(session, 'ov_pt_jit', value = as.numeric(v))
        ov_new$pt_jit <- as.numeric(v)
      }
      if (!is.null(v <- gv('ov_sig_lwd'))) {
        updateNumericInput(session, 'ov_sig_lwd', value = as.numeric(v))
        ov_new$sig_lwd <- as.numeric(v)
      }
      if (!is.null(v <- gv('ov_sig_txt'))) {
        updateNumericInput(session, 'ov_sig_txt', value = as.numeric(v))
        ov_new$sig_txt <- as.numeric(v)
      }

      if (length(ov_new)) {
        ids <- names(plot_bank$all)
        for (id in ids) plot_bank$all[[id]]$overrides <- ov_new
        if (length(ids)) ov_trigger(ov_trigger() + 1)
      }

      ids <- names(plot_bank$all)
      choices <- named_choices(ids, paste(tr("plot_label"), seq_along(ids)))
      sel_valid <- intersect(sel_prev, ids)
      updateCheckboxGroupInput(session, 'plots_chosen', choices = choices,
                               selected = sel_valid)
      ord_sel <- if (order_prev %in% ids) order_prev else ids[1]
      updateSelectInput(session, 'plot_order', choices = ids, selected = ord_sel)
    })

  observeEvent(input$move_up, {
    req(input$plot_order)
    ids <- names(plot_bank$all)
    idx <- match(input$plot_order, ids)
    if (!is.na(idx) && idx > 1) {
      ids[c(idx-1, idx)] <- ids[c(idx, idx-1)]
      plot_bank$all <- plot_bank$all[ids]
    }
  })

  observeEvent(input$move_down, {
    req(input$plot_order)
    ids <- names(plot_bank$all)
    idx <- match(input$plot_order, ids)
    if (!is.na(idx) && idx < length(ids)) {
      ids[c(idx, idx+1)] <- ids[c(idx+1, idx)]
      plot_bank$all <- plot_bank$all[ids]
    }
  })

  combo_plot
}
