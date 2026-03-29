# Panel composition and overrides module
setup_panel_module <- function(input, output, session, plot_bank, panel_inserto,
                              ov_trigger, snapshot_fun, meta_fun, curve_settings) {
  ov_input_to_key <- c(
    ov_title = "title",
    ov_cur_lwd = "cur_lwd",
    ov_cur_pt = "cur_pt",
    ov_box_w = "box_w",
    ov_errbar_size = "errbar_size",
    ov_pt_size = "pt_size",
    ov_pt_jit = "pt_jit",
    ov_sig_lwd = "sig_lwd",
    ov_sig_txt = "sig_txt"
  )

  override_target_ids <- function(ids = names(plot_bank$all)) {
    ids <- as.character(ids %||% character(0))
    if (!length(ids)) return(character(0))

    mode <- as.character(input$ov_tipo %||% "Todos")
    if (identical(mode, "Seleccionados")) {
      target_ids <- as.character(input$ov_targets %||% character(0))
      if (!length(target_ids)) {
        target_ids <- as.character(input$plots_chosen %||% character(0))
      }
      target_ids <- intersect(ids, target_ids)
      if (!length(target_ids)) target_ids <- ids
      return(target_ids)
    }
    if (!identical(mode, "Todos")) {
      return(ids[vapply(plot_bank$all[ids], function(x) identical(x$type, mode), logical(1))])
    }
    ids
  }

  override_prefill_defaults <- function() {
    list(
      ov_title = "",
      ov_cur_lwd = 1.2,
      ov_cur_pt = 3,
      ov_box_w = as.numeric(input$box_w %||% 0.8),
      ov_errbar_size = as.numeric(input$errbar_size %||% 0.8),
      ov_pt_size = as.numeric(input$pt_size %||% 3),
      ov_pt_jit = as.numeric(input$pt_jit %||% 0.1),
      ov_sig_lwd = 1.2,
      ov_sig_txt = 4
    )
  }

  current_override_form_values <- function(ids = override_target_ids()) {
    vals <- override_prefill_defaults()
    if (!length(ids)) return(vals)

    for (input_id in names(ov_input_to_key)) {
      key <- ov_input_to_key[[input_id]]
      raw_vals <- lapply(ids, function(id) {
        ov <- plot_bank$all[[id]]$overrides %||% list()
        ov[[key]]
      })
      raw_vals <- Filter(function(v) {
        if (is.null(v) || !length(v)) return(FALSE)
        if (is.character(v)) return(nzchar(trimws(v[[1]] %||% "")))
        if (is.numeric(v)) return(is.finite(as.numeric(v[[1]])))
        TRUE
      }, raw_vals)
      if (!length(raw_vals)) next

      first_val <- raw_vals[[1]]
      if (is.numeric(vals[[input_id]])) {
        num <- suppressWarnings(as.numeric(first_val[[1]]))
        if (is.finite(num)) vals[[input_id]] <- num
      } else {
        vals[[input_id]] <- as.character(first_val[[1]] %||% "")
      }
    }
    vals
  }

  output$plotOverrideUI <- renderUI({
    input$app_lang
    input$ov_tipo
    input$ov_targets
    input$plots_chosen
    input$add2panel
    input$remove_current
    input$remove_selected
    input$move_up
    input$move_down
    input$move_top
    input$move_bottom
    ov_trigger()

    lang <- input$app_lang %||% i18n_lang
    ids <- names(plot_bank$all)
    if (is.null(ids)) ids <- character(0)
    plot_label_prefix <- tr_text("plot_label", lang)
    target_choices <- if (length(ids)) named_choices(ids, paste(plot_label_prefix, seq_along(ids))) else character(0)
    ov_vals <- current_override_form_values(override_target_ids(ids))
    selected_mode <- as.character(isolate(input$ov_tipo %||% "Todos"))
    if (!nzchar(selected_mode)) selected_mode <- "Todos"
    selected_targets <- isolate(input$ov_targets %||% character(0))
    selected_targets <- intersect(ids, selected_targets)
    if (!length(selected_targets) && length(ids)) selected_targets <- ids
    tagList(
      selectInput(
        "ov_tipo",
        tr("override_apply_to"),
        choices = named_choices(
          c(
            "Todos",
            "Seleccionados",
            "Boxplot",
            "Barras",
            "Violin",
            "Apiladas",
            "Correlacion",
            "MatrizCorrelacion",
            "Heatmap",
            "Curvas"
          ),
          tr_text(
            c(
              "override_all",
              "combo_selected_plots",
              "plot_boxplot",
              "plot_bars",
              "plot_violin",
              "plot_stacked",
              "plot_correlation",
              "plot_corr_matrix",
              "plot_heatmap",
              "plot_curves"
            ),
            lang
          )
        ),
        selected = selected_mode
      ),
      conditionalPanel(
        condition = "input.ov_tipo == 'Seleccionados'",
        selectizeInput(
          "ov_targets",
          tr("combo_override_targets"),
          choices = target_choices,
          selected = selected_targets,
          multiple = TRUE,
          options = list(placeholder = tr("combo_override_targets_placeholder"))
        )
      ),
      textInput("ov_title", tr("override_title"), ov_vals$ov_title),
      numericInput("ov_cur_lwd", tr("override_curves_linewidth"), ov_vals$ov_cur_lwd, min = 0.5, step = 0.1),
      numericInput("ov_cur_pt", tr("override_curves_point_size"), ov_vals$ov_cur_pt, min = 0.5, step = 0.5),
      hr(), h4(tr("override_box_section")),
      numericInput("ov_box_w", tr("override_box_width"), ov_vals$ov_box_w, min = 0.1, max = 1.5, step = 0.05),
      numericInput("ov_errbar_size", tr("override_errbar_size"), ov_vals$ov_errbar_size, min = 0.1, step = 0.1),
      numericInput("ov_pt_size", tr("override_point_size"), ov_vals$ov_pt_size, min = 0.5, max = 20, step = 0.5),
      numericInput("ov_pt_jit", tr("override_point_jitter"), ov_vals$ov_pt_jit, min = 0, max = 0.5, step = 0.01),
      numericInput("ov_sig_lwd", tr("override_sig_linewidth"), ov_vals$ov_sig_lwd, min = 0.2, step = 0.2),
      numericInput("ov_sig_txt", tr("override_sig_textsize"), ov_vals$ov_sig_txt, min = 1, step = 0.5),
      actionButton("apply_ov", tr("override_apply_all"), class = "btn btn-success")
    )
  })

  ggtext_warned <- reactiveVal(FALSE)

  plot_index_choices <- function(ids, lang = input$app_lang %||% i18n_lang) {
    ids <- as.character(ids %||% character(0))
    if (!length(ids)) return(character(0))
    prefix <- tr_text("plot_label", lang)
    named_choices(ids, paste(prefix, seq_along(ids)))
  }

  refresh_plot_picker_inputs <- function(ids, selected_plots = NULL, order_selected = NULL) {
    ids <- as.character(ids %||% character(0))
    choices <- plot_index_choices(ids)

    if (is.null(selected_plots)) selected_plots <- isolate(input$plots_chosen %||% ids)
    selected_plots <- intersect(ids, selected_plots)
    if (!length(selected_plots) && length(ids)) selected_plots <- ids

    if (is.null(order_selected)) {
      order_selected <- isolate(input$plot_order %||% if (length(ids)) ids[[1]] else character(0))
    }
    if (!length(ids)) {
      order_selected <- character(0)
    } else if (!length(order_selected) || !order_selected[[1]] %in% ids) {
      order_selected <- ids[[1]]
    }

    target_sel <- intersect(ids, isolate(input$ov_targets %||% character(0)))
    if (!length(target_sel) && length(ids)) target_sel <- ids

    updateCheckboxGroupInput(
      session,
      "plots_chosen",
      choices = choices,
      selected = selected_plots
    )
    updateSelectInput(
      session,
      "plot_order",
      choices = choices,
      selected = order_selected
    )
    updateSelectizeInput(
      session,
      "ov_targets",
      choices = choices,
      selected = target_sel,
      server = TRUE
    )
  }

  observe({
    ids <- names(plot_bank$all)
    if (is.null(ids)) ids <- character(0)
    choices <- plot_index_choices(ids)
    updateSelectInput(session, 'plot_edit', choices = choices)
  })


  observeEvent(input$apply_ov, {
    ov_new <- list(
      title     = if (nzchar(input$ov_title))    input$ov_title    else NULL,
      cur_lwd   = if (!is.na(input$ov_cur_lwd))  input$ov_cur_lwd  else NULL,
      cur_pt    = if (!is.na(input$ov_cur_pt))   input$ov_cur_pt   else NULL,
      box_w     = if (!is.na(input$ov_box_w))    input$ov_box_w    else NULL,
      errbar_size = if (!is.na(input$ov_errbar_size)) input$ov_errbar_size else NULL,
      pt_size   = if (!is.na(input$ov_pt_size))  input$ov_pt_size  else NULL,
      pt_jit    = if (!is.na(input$ov_pt_jit))   input$ov_pt_jit   else NULL,
      sig_lwd   = if (!is.na(input$ov_sig_lwd))  input$ov_sig_lwd  else NULL,
      sig_txt   = if (!is.na(input$ov_sig_txt))  input$ov_sig_txt  else NULL
    )
    ov_updates <- ov_new[!vapply(ov_new, is.null, logical(1))]
    if (!length(ov_updates)) return()
    ids <- names(plot_bank$all)
    if (identical(input$ov_tipo, "Seleccionados")) {
      target_ids <- input$ov_targets %||% character(0)
      if (length(target_ids)) {
        ids <- intersect(ids, target_ids)
      } else {
        ids <- intersect(ids, input$plots_chosen %||% character(0))
      }
    } else if (!identical(input$ov_tipo, "Todos")) {
      ids <- ids[vapply(plot_bank$all[ids], function(x) identical(x$type, input$ov_tipo), logical(1))]
    }
    for (id in ids) {
      ov_prev <- plot_bank$all[[id]]$overrides %||% list()
      plot_bank$all[[id]]$overrides <- utils::modifyList(ov_prev, ov_updates)
    }
    if (length(ids)) ov_trigger(ov_trigger() + 1)
  })
  observeEvent(input$add2panel, {
    stamp <- gsub("[^0-9]", "", format(Sys.time(), "%Y%m%d%H%M%OS3"))
    id <- paste0("p", stamp, "_", sprintf("%04d", sample.int(9999, 1)))
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
    choices <- plot_index_choices(ids)
    selected_now <- isolate(input$plots_chosen %||% ids)
    selected_now <- intersect(ids, selected_now)
    if (!length(selected_now)) selected_now <- ids
    order_selected <- isolate(input$plot_order %||% selected_now[[1]])
    if (!order_selected %in% ids) order_selected <- selected_now[[1]]
    tagList(
      checkboxGroupInput('plots_chosen', tr("combo_select_plots"),
                         choices = choices, selected = selected_now),
      selectInput('plot_order', tr("combo_order_label"), choices = choices, selected = order_selected),
      actionButton('move_up',   tr("combo_move_up"), class = 'btn btn-secondary btn-sm'),
      actionButton('move_down', tr("combo_move_down"), class = 'btn btn-secondary btn-sm'),
      actionButton('move_top', tr("combo_move_top"), class = 'btn btn-secondary btn-sm'),
      actionButton('move_bottom', tr("combo_move_bottom"), class = 'btn btn-secondary btn-sm'),
      br(), br(),
      actionButton('remove_current', tr("combo_remove_current"), class = 'btn btn-danger btn-sm'),
      actionButton('remove_selected', tr("combo_remove_selected"), class = 'btn btn-warning btn-sm')
    )
  })

  output$comboLayoutMap <- renderText({
    input$app_lang
    ids <- names(plot_bank$all)
    if (!length(ids)) return(tr("combo_layout_map_empty"))
    selected_ids <- intersect(ids, input$plots_chosen %||% ids)
    if (!length(selected_ids)) selected_ids <- ids
    plot_label_prefix <- tr_text("plot_label", input$app_lang %||% i18n_lang)
    parts <- vapply(seq_along(selected_ids), function(i) {
      sprintf("%d=%s %d", i, plot_label_prefix, match(selected_ids[[i]], ids))
    }, character(1))
    paste(parts, collapse = " | ")
  })

  split_csv_values <- function(text) {
    txt <- as.character(text %||% "")
    if (!nzchar(trimws(txt))) return(character(0))
    vals <- unlist(strsplit(txt, "[,;\\n\\t]+"), use.names = FALSE)
    vals <- trimws(vals)
    vals[nzchar(vals)]
  }

  parse_numeric_vector <- function(text) {
    vals <- split_csv_values(text)
    if (!length(vals)) return(NULL)
    nums <- suppressWarnings(as.numeric(vals))
    nums <- nums[is.finite(nums)]
    if (!length(nums)) return(NULL)
    as.numeric(nums)
  }

  normalize_layout_vector <- function(vec, expected_len) {
    if (is.null(vec)) return(NULL)
    expected_len <- suppressWarnings(as.integer(expected_len))
    if (!is.finite(expected_len) || expected_len <= 0) return(NULL)
    if (length(vec) == expected_len) return(as.numeric(vec))
    if (length(vec) == 1) return(rep(as.numeric(vec[[1]]), expected_len))
    NULL
  }

  parse_layout_grid <- function(text, n_plots) {
    txt <- as.character(text %||% "")
    if (!nzchar(trimws(txt))) return(NULL)
    lines <- unlist(strsplit(txt, "\\r?\\n"), use.names = FALSE)
    lines <- trimws(lines)
    lines <- lines[nzchar(lines)]
    if (!length(lines)) return(NULL)

    rows <- lapply(lines, function(line) {
      toks <- unlist(strsplit(line, "[,;\\s]+"), use.names = FALSE)
      toks <- toks[nzchar(toks)]
      toks
    })
    row_lens <- vapply(rows, length, integer(1))
    if (!length(row_lens) || any(row_lens <= 0) || length(unique(row_lens)) != 1L) return(NULL)

    flat <- unlist(rows, use.names = FALSE)
    vals <- suppressWarnings(as.integer(flat))
    if (any(is.na(vals))) return(NULL)
    if (any(vals < 0L)) return(NULL)
    if (any(vals > as.integer(n_plots))) return(NULL)
    if (!length(vals)) return(NULL)

    nr <- length(rows)
    nc <- row_lens[[1]]
    mat <- matrix(vals, nrow = nr, byrow = TRUE)
    present <- sort(unique(vals[vals > 0L]))
    if (!length(present)) return(NULL)
    n_plots <- suppressWarnings(as.integer(n_plots))
    if (!is.finite(n_plots) || n_plots <= 0) return(NULL)
    if (max(present) != n_plots) return(NULL)
    expected <- seq_len(max(present))
    if (!identical(present, expected)) return(NULL)

    token_pool <- c(LETTERS, letters, as.character(0:9))
    if (length(present) > length(token_pool)) return(NULL)
    tokens <- token_pool[seq_along(present)]
    names(tokens) <- as.character(present)

    char_mat <- matrix("#", nrow = nr, ncol = nc)
    for (idx in present) {
      char_mat[mat == idx] <- tokens[[as.character(idx)]]
    }
    design <- paste(apply(char_mat, 1, paste0, collapse = ""), collapse = "\n")
    list(
      design = design,
      nrow = nr,
      ncol = nc
    )
  }

  clamp_num <- function(x, default, lo = -Inf, hi = Inf) {
    val <- suppressWarnings(as.numeric(x))
    if (!is.finite(val)) val <- default
    val <- max(lo, min(hi, val))
    val
  }

  safe_color <- function(x, fallback = "black") {
    col <- as.character(x %||% fallback)
    if (!nzchar(col)) col <- fallback
    ok <- tryCatch({
      grDevices::col2rgb(col)
      TRUE
    }, error = function(e) FALSE)
    if (isTRUE(ok)) col else fallback
  }

  tone_down <- function(cols, amount = 0.35) {
    if (!length(cols)) return(character(0))
    amt <- pmin(pmax(amount, 0), 1)
    m <- grDevices::col2rgb(cols)
    m2 <- m + (255 - m) * amt
    grDevices::rgb(m2[1, ] / 255, m2[2, ] / 255, m2[3, ] / 255)
  }

  safe_brewer_combo <- function(name, n) {
    if (n <= 0) return(character(0))
    max_col <- RColorBrewer::brewer.pal.info[name, "maxcolors"]
    if (n < 3) {
      base <- RColorBrewer::brewer.pal(3, name)
      return(base[seq_len(n)])
    }
    if (n > max_col) {
      base <- RColorBrewer::brewer.pal(max_col, name)
      return(grDevices::colorRampPalette(base)(n))
    }
    RColorBrewer::brewer.pal(n, name)
  }

  okabe_cols <- c(
    "#E69F00", "#56B4E9", "#009E73", "#F0E442",
    "#0072B2", "#D55E00", "#CC79A7", "#999999"
  )
  tableau_cols <- c(
    "#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F",
    "#EDC949", "#AF7AA1", "#FF9DA7", "#9C755F", "#BAB0AC"
  )

  take_n <- function(cols, n) {
    if (n <= 0) return(character(0))
    if (n > length(cols)) return(grDevices::colorRampPalette(cols)(n))
    cols[seq_len(n)]
  }

  combo_normal_palette <- function(mode, n) {
    if (n <= 0) return(character(0))
    brew <- function(name) safe_brewer_combo(name, n)
    switch(
      mode %||% "Default",
      "Default" = safe_hue(n),
      "Default Suave" = tone_down(safe_hue(n)),
      "Blanco y Negro" = rep("black", n),
      "Blanco y Negro Suave" = rep("#666666", n),
      "Viridis" = viridis::viridis(n),
      "Viridis Suave" = tone_down(viridis::viridis(n)),
      "Plasma" = viridis::plasma(n),
      "Plasma Suave" = tone_down(viridis::plasma(n)),
      "Magma" = viridis::magma(n),
      "Magma Suave" = tone_down(viridis::magma(n)),
      "Cividis" = viridis::cividis(n),
      "Cividis Suave" = tone_down(viridis::cividis(n)),
      "Set1" = brew("Set1"),
      "Set1 Suave" = tone_down(brew("Set1")),
      "Set2" = brew("Set2"),
      "Set2 Suave" = tone_down(brew("Set2")),
      "Set3" = brew("Set3"),
      "Set3 Suave" = tone_down(brew("Set3")),
      "Dark2" = brew("Dark2"),
      "Dark2 Suave" = tone_down(brew("Dark2")),
      "Accent" = brew("Accent"),
      "Accent Suave" = tone_down(brew("Accent")),
      "Paired" = brew("Paired"),
      "Paired Suave" = tone_down(brew("Paired")),
      "Pastel1" = brew("Pastel1"),
      "Pastel1 Suave" = tone_down(brew("Pastel1"), amount = 0.2),
      "Pastel2" = brew("Pastel2"),
      "Pastel2 Suave" = tone_down(brew("Pastel2"), amount = 0.2),
      "OkabeIto" = rep(okabe_cols, length.out = n),
      "OkabeIto Suave" = tone_down(rep(okabe_cols, length.out = n)),
      "Tableau" = rep(tableau_cols, length.out = n),
      "Tableau Suave" = tone_down(rep(tableau_cols, length.out = n)),
      safe_hue(n)
    )
  }

  combo_adv_extra_info <- data.frame(
    name = c(
      "Viridis", "Plasma", "Magma", "Inferno", "Cividis",
      "Aqua", "Rose", "Amber", "Slate", "Forest", "Ocean", "BlueWhiteRed", "SkyWhiteRed",
      "BlueRed", "PurpleOrange", "GreenBrown",
      "BlueOrange", "TealRed", "PurpleGreen", "CyanMagenta", "BrownTeal",
      "Hue", "OkabeIto", "Tableau"
    ),
    category = c(
      "seq", "seq", "seq", "seq", "seq",
      "seq", "seq", "seq", "seq", "seq", "seq", "seq", "seq",
      "div", "div", "div",
      "div", "div", "div", "div", "div",
      "qual", "qual", "qual"
    ),
    stringsAsFactors = FALSE
  )

  combo_brewer_palette_choices <- function(type = "seq", filters = character(0)) {
    info <- RColorBrewer::brewer.pal.info
    cat <- switch(type %||% "seq", seq = "seq", div = "div", qual = "qual", "seq")
    info <- info[info$category == cat, , drop = FALSE]
    if (length(filters)) {
      if ("colorblind" %in% filters) info <- info[info$colorblind, , drop = FALSE]
      if ("print" %in% filters) info <- info[info$print, , drop = FALSE]
      if ("photocopy" %in% filters) info <- info[info$photocopy, , drop = FALSE]
    }
    choices <- rownames(info)
    extra <- combo_adv_extra_info[combo_adv_extra_info$category == cat, , drop = FALSE]
    if (length(filters)) extra <- extra[0, , drop = FALSE]
    unique(c(choices, extra$name))
  }

  combo_adv_palette <- function(name, n) {
    if (n <= 0) return(character(0))
    nm <- as.character(name %||% "")
    if (!nzchar(nm)) return(NULL)
    if (nm %in% rownames(RColorBrewer::brewer.pal.info)) {
      return(safe_brewer_combo(nm, n))
    }
    switch(
      nm,
      "Viridis" = viridis::viridis(n),
      "Plasma" = viridis::plasma(n),
      "Magma" = viridis::magma(n),
      "Inferno" = viridis::inferno(n),
      "Cividis" = viridis::cividis(n),
      "Aqua" = grDevices::colorRampPalette(c("#E8F6F8", "#7FC8D6", "#005B6A"))(n),
      "Rose" = grDevices::colorRampPalette(c("#FFF0F3", "#FF8FA3", "#C9184A"))(n),
      "Amber" = grDevices::colorRampPalette(c("#FFF3E0", "#FDBA74", "#B45309"))(n),
      "Slate" = grDevices::colorRampPalette(c("#F1F5F9", "#94A3B8", "#1E293B"))(n),
      "Forest" = grDevices::colorRampPalette(c("#E8F5E9", "#81C784", "#1B5E20"))(n),
      "Ocean" = grDevices::colorRampPalette(c("#E0F2FE", "#38BDF8", "#0C4A6E"))(n),
      "BlueWhiteRed" = grDevices::colorRampPalette(c("blue", "white", "red"))(n),
      "SkyWhiteRed" = grDevices::colorRampPalette(c("#55BDEB", "#FFFFFF", "#E64B4B"))(n),
      "BlueRed" = grDevices::colorRampPalette(c("#2166AC", "#F7F7F7", "#B2182B"))(n),
      "PurpleOrange" = grDevices::colorRampPalette(c("#5E3C99", "#F7F7F7", "#E66101"))(n),
      "GreenBrown" = grDevices::colorRampPalette(c("#1B7837", "#F7F7F7", "#8C510A"))(n),
      "BlueOrange" = grDevices::colorRampPalette(c("#2166AC", "#F7F7F7", "#F4A582"))(n),
      "TealRed" = grDevices::colorRampPalette(c("#008080", "#F7F7F7", "#D73027"))(n),
      "PurpleGreen" = grDevices::colorRampPalette(c("#762A83", "#F7F7F7", "#1B7837"))(n),
      "CyanMagenta" = grDevices::colorRampPalette(c("#00A6D6", "#F7F7F7", "#D81B60"))(n),
      "BrownTeal" = grDevices::colorRampPalette(c("#8C510A", "#F7F7F7", "#01665E"))(n),
      "Hue" = safe_hue(n),
      "OkabeIto" = take_n(okabe_cols, n),
      "Tableau" = take_n(tableau_cols, n),
      NULL
    )
  }

  apply_combo_manual_scales <- function(plot_obj, pal, pal_fill) {
    withCallingHandlers(
      plot_obj & scale_colour_manual(values = pal) &
        scale_fill_manual(values = pal_fill),
      warning = function(w) {
        msg <- conditionMessage(w)
        if (grepl("Scale for fill is already present", msg, fixed = TRUE) ||
            grepl("Scale for colour is already present", msg, fixed = TRUE)) {
          invokeRestart("muffleWarning")
        }
      }
    )
  }

  observeEvent(
    list(input$combo_adv_pal_type, input$combo_adv_pal_filters),
    {
      choices <- combo_brewer_palette_choices(
        type = input$combo_adv_pal_type %||% "seq",
        filters = input$combo_adv_pal_filters %||% character(0)
      )
      if (!length(choices)) {
        choices <- combo_brewer_palette_choices(type = input$combo_adv_pal_type %||% "seq")
      }
      selected <- isolate(input$combo_adv_pal_name)
      if (!length(selected) || !selected %in% choices) selected <- choices[[1]]
      updateSelectInput(session, "combo_adv_pal_name", choices = choices, selected = selected)
    },
    ignoreInit = FALSE
  )

  add_combo_richtext <- function(p) {
    if (!isTRUE(input$combo_rich_enable)) return(p)
    txt <- as.character(input$combo_rich_text %||% "")
    if (!nzchar(trimws(txt))) return(p)

    x <- clamp_num(input$combo_rich_x, default = 0.5, lo = 0, hi = 1)
    y <- clamp_num(input$combo_rich_y, default = 0.92, lo = 0, hi = 1)
    box_w <- clamp_num(input$combo_rich_box_w, default = 0.4, lo = 0.05, hi = 1)
    box_h <- clamp_num(input$combo_rich_box_h, default = 0.16, lo = 0.05, hi = 1)
    txt_size <- clamp_num(input$combo_rich_size, default = 4.5, lo = 1, hi = 20)
    txt_col <- safe_color(input$combo_rich_color, fallback = "black")
    fill_col <- safe_color(input$combo_rich_fill, fallback = "#FFFFFFCC")

    left <- max(0, x - box_w / 2)
    right <- min(1, x + box_w / 2)
    bottom <- max(0, y - box_h / 2)
    top <- min(1, y + box_h / 2)

    if (requireNamespace("ggtext", quietly = TRUE)) {
      overlay <- ggplot(data.frame(x = x, y = y, label = txt), aes(x = x, y = y, label = label)) +
        ggtext::geom_richtext(
          fill = fill_col,
          colour = txt_col,
          label.colour = scales::alpha("black", 0.25),
          size = txt_size,
          hjust = 0.5,
          vjust = 0.5,
          label.padding = grid::unit(c(4, 6, 4, 6), "pt")
        ) +
        coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE, clip = "off") +
        theme_void()
    } else {
      if (!isTRUE(ggtext_warned())) {
        ggtext_warned(TRUE)
        showNotification(tr("combo_richtext_missing"), type = "warning", duration = 5)
      }
      overlay <- ggplot(data.frame(x = x, y = y, label = txt), aes(x = x, y = y, label = label)) +
        geom_label(
          fill = fill_col,
          colour = txt_col,
          size = txt_size
        ) +
        coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE, clip = "off") +
        theme_void()
    }

    p + patchwork::inset_element(
      overlay,
      left = left,
      bottom = bottom,
      right = right,
      top = top,
      align_to = "full"
    )
  }

  combo_plot <- reactive({
    input$makeCombo
    input$show_legend_combo; input$combo_pal; input$nrow_combo;
    input$ncol_combo; input$fs_title_all;
    input$fs_axis_title_all; input$fs_axis_text_all; input$fs_legend_all;
    input$combo_axis_line_size;
    input$combo_title; input$combo_title_size; input$combo_legend_scope; input$combo_legend_side
    input$combo_apply_paper_theme; input$combo_font_family
    input$combo_adv_pal_enable; input$combo_adv_pal_type; input$combo_adv_pal_reverse
    input$combo_adv_pal_filters; input$combo_adv_pal_name
    input$combo_layout_grid; input$combo_col_widths; input$combo_row_heights
    input$combo_rich_enable; input$combo_rich_text; input$combo_rich_x; input$combo_rich_y
    input$combo_rich_box_w; input$combo_rich_box_h; input$combo_rich_size
    input$combo_rich_color; input$combo_rich_fill
    req(length(plot_bank$all) > 0)
    library(patchwork)

    theme_paper <- function(bs = 18, family = "Helvetica", ax = 1.2) {
      theme_minimal(base_size = bs, base_family = family) +
        theme(
          axis.line = element_line(linewidth = ax, colour = "black"),
          axis.ticks = element_line(linewidth = ax, colour = "black"),
          panel.grid = element_blank(),
          plot.title = element_text(colour = "black", family = family),
          axis.title = element_text(colour = "black", family = family),
          axis.text = element_text(colour = "black", family = family),
          legend.text = element_text(colour = "black", family = family)
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

    force_enable_plot_legend <- function(p, side = "right") {
      if (!inherits(p, "ggplot")) return(p)
      for (i in seq_along(p$layers)) {
        leg <- p$layers[[i]]$show.legend
        if (isFALSE(leg)) {
          p$layers[[i]]$show.legend <- NA
        } else if (is.logical(leg) && length(leg) && any(!leg, na.rm = TRUE)) {
          leg[!leg] <- NA
          p$layers[[i]]$show.legend <- leg
        }
      }
      p +
        guides(
          fill = guide_legend(title = NULL),
          colour = guide_legend(title = NULL),
          linetype = guide_legend(title = NULL),
          shape = guide_legend(title = NULL)
        ) +
        theme(legend.position = side)
    }
    all_ids <- names(plot_bank$all)
    selected_ids <- intersect(all_ids, input$plots_chosen %||% all_ids)
    if (!length(selected_ids)) selected_ids <- all_ids
    req(length(selected_ids) > 0)

    combo_family <- as.character(input$combo_font_family %||% "Helvetica")
    if (!nzchar(combo_family)) combo_family <- "Helvetica"
    use_paper_theme <- isTRUE(input$combo_apply_paper_theme)
    global_title_size <- clamp_num(input$fs_title_all, 20, lo = 6, hi = 120)
    global_axis_title_size <- clamp_num(input$fs_axis_title_all, 16, lo = 6, hi = 120)
    global_axis_text_size <- clamp_num(input$fs_axis_text_all, 14, lo = 6, hi = 120)
    global_legend_size <- clamp_num(input$fs_legend_all, 16, lo = 6, hi = 120)
    global_axis_line_size <- clamp_num(input$combo_axis_line_size, 1, lo = 0.1, hi = 10)
    show_legends <- isTRUE(input$show_legend_combo)
    legend_scope <- as.character(input$combo_legend_scope %||% "by_type")
    legend_side <- as.character(input$combo_legend_side %||% "right")
    if (!legend_side %in% c("right", "left")) legend_side <- "right"

    plots <- lapply(selected_ids, function(id) {
      info <- plot_bank$all[[id]]
      p  <- info$plot
      ov <- info$overrides
      if (use_paper_theme) {
        p <- p + theme_paper(
          bs = 18,
          family = combo_family,
          ax = global_axis_line_size
        )
      }
      p <- p + theme(
        text = element_text(family = combo_family),
        plot.title = element_text(size = global_title_size, face = "bold", family = combo_family),
        axis.title = element_text(size = global_axis_title_size, face = "bold", family = combo_family),
        axis.text = element_text(size = global_axis_text_size, family = combo_family),
        legend.text = element_text(size = global_legend_size, family = combo_family),
        axis.line = element_line(linewidth = global_axis_line_size, colour = "black"),
        axis.ticks = element_line(linewidth = global_axis_line_size, colour = "black")
      )
      if (!is.null(ov$title))
        p <- p + ggtitle(ov$title)
      if (!is.null(ov$sig_lwd) || !is.null(ov$sig_txt))
        p <- adjust_sig_layers(p, ov$sig_lwd, ov$sig_txt)
      p <- adjust_curve_layers(p, ov$cur_lwd, ov$cur_pt)
      p <- adjust_box_layers(p, ov$box_w, ov$pt_size, ov$pt_jit, ov$errbar_size)
      if (show_legends) {
        p <- force_enable_plot_legend(p, legend_side)
      }
      p
    })

    if (!show_legends) {
      plots <- lapply(plots, function(p) p + theme(legend.position = "none"))
    } else if (identical(legend_scope, "by_type")) {
      seen_types <- character(0)
      for (i in seq_along(plots)) {
        type_i <- as.character(plot_bank$all[[selected_ids[[i]]]]$type %||% "")
        if (nzchar(type_i) && type_i %in% seen_types) {
          plots[[i]] <- plots[[i]] + theme(legend.position = "none")
        } else {
          if (nzchar(type_i)) seen_types <- c(seen_types, type_i)
          plots[[i]] <- plots[[i]] + theme(legend.position = legend_side)
        }
      }
    } else {
      plots <- lapply(plots, function(p) p + theme(legend.position = legend_side))
    }

    max_col <- max(vapply(plots, function(p) {
      b <- ggplot_build(p)
      cols <- unique(unlist(lapply(b$data, function(d) d$group)))
      length(cols)
    }, numeric(1)), 0)
    pal <- pal_fill <- NULL
    if (isTRUE(input$combo_adv_pal_enable)) {
      pal <- combo_adv_palette(input$combo_adv_pal_name, max_col)
      if (!is.null(pal) && isTRUE(input$combo_adv_pal_reverse)) pal <- rev(pal)
    } else if (!identical(input$combo_pal, "Original")) {
      pal <- combo_normal_palette(input$combo_pal, max_col)
    }
    if (!is.null(pal) && length(pal)) {
      is_bw_mode <- !isTRUE(input$combo_adv_pal_enable) &&
        (identical(input$combo_pal, "Blanco y Negro") || identical(input$combo_pal, "Blanco y Negro Suave"))
      pal_fill <- if (is_bw_mode) rep("white", length(pal)) else scales::alpha(pal, 0.5)
    } else {
      pal <- pal_fill <- NULL
    }
    layout_grid <- parse_layout_grid(input$combo_layout_grid, length(plots))
    nrow_combo <- suppressWarnings(as.integer(input$nrow_combo))
    ncol_combo <- suppressWarnings(as.integer(input$ncol_combo))
    if (!is.finite(nrow_combo) || nrow_combo < 1) nrow_combo <- 1L
    if (!is.finite(ncol_combo) || ncol_combo < 1) ncol_combo <- 1L
    n_plots <- length(plots)

    col_widths_raw <- parse_numeric_vector(input$combo_col_widths)
    row_heights_raw <- parse_numeric_vector(input$combo_row_heights)

    if (!is.null(layout_grid)) {
      res <- wrap_plots(plots, design = layout_grid$design)
      col_widths <- normalize_layout_vector(col_widths_raw, layout_grid$ncol)
      row_heights <- normalize_layout_vector(row_heights_raw, layout_grid$nrow)
    } else {
      ncol_eff <- ncol_combo
      if (nrow_combo * ncol_eff < n_plots) {
        ncol_eff <- ceiling(n_plots / nrow_combo)
        if (!identical(as.integer(input$ncol_combo %||% 0), as.integer(ncol_eff))) {
          updateNumericInput(session, "ncol_combo", value = as.integer(ncol_eff))
        }
      }
      res <- wrap_plots(plots, nrow = nrow_combo, ncol = ncol_eff)
      col_widths <- normalize_layout_vector(col_widths_raw, ncol_eff)
      row_heights <- normalize_layout_vector(row_heights_raw, nrow_combo)
    }
    if (!is.null(col_widths) || !is.null(row_heights)) {
      res <- res + plot_layout(widths = col_widths, heights = row_heights)
    }
    # Final composition-level style pass so global controls always win
    # even when individual plots define axis.title.x/.y or axis.text.x/.y.
    res <- res & theme(
      text = element_text(family = combo_family),
      plot.title = element_text(size = global_title_size, face = "bold", family = combo_family),
      axis.title = element_text(size = global_axis_title_size, face = "bold", family = combo_family),
      axis.title.x = element_text(size = global_axis_title_size, face = "bold", family = combo_family),
      axis.title.y = element_text(size = global_axis_title_size, face = "bold", family = combo_family),
      axis.text = element_text(size = global_axis_text_size, family = combo_family),
      axis.text.x = element_text(size = global_axis_text_size, family = combo_family),
      axis.text.y = element_text(size = global_axis_text_size, family = combo_family),
      axis.ticks = element_line(linewidth = global_axis_line_size, colour = "black"),
      axis.line = element_line(linewidth = global_axis_line_size, colour = "black"),
      legend.text = element_text(size = global_legend_size, family = combo_family)
    )
    if (!is.null(pal)) {
      res <- apply_combo_manual_scales(res, pal, pal_fill)
    }
    if (show_legends && legend_scope %in% c("by_type", "collect")) {
      res <- res + plot_layout(guides = "collect")
      res <- res & theme(legend.position = legend_side)
    }

    combo_title <- as.character(input$combo_title %||% "")
    if (nzchar(trimws(combo_title))) {
      combo_title_size <- suppressWarnings(as.numeric(input$combo_title_size))
      if (!is.finite(combo_title_size) || combo_title_size <= 0) combo_title_size <- 24
      res <- res + plot_annotation(
        title = combo_title,
        theme = theme(
          plot.title = element_text(
            size = combo_title_size,
            face = "bold",
            family = combo_family,
            hjust = 0.5
          )
        )
      )
    }
    add_combo_richtext(res)
  })

  output$comboPreview <- renderPlot({
    withCallingHandlers({
      req(combo_plot())
      combo_plot()
    }, warning = function(w) {
      msg <- conditionMessage(w)
      if (grepl("Scale for fill is already present", msg, fixed = TRUE) ||
          grepl("Scale for colour is already present", msg, fixed = TRUE) ||
          grepl("Duplicated aesthetics after name standardisation: colour", msg, fixed = TRUE) ||
          grepl("keep_vec_names=TRUE", msg, fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    })
  }, width = function() input$combo_width,
     height = function() input$combo_height,
     res = 96)

  collect_combo_meta <- function() {
    plot_ids <- names(plot_bank$all)
    vals <- list(
      plots_chosen      = paste(input$plots_chosen %||% character(0), collapse = ","),
      plot_order_ids    = paste(plot_ids, collapse = ","),
      show_legend_combo = input$show_legend_combo,
      combo_title       = input$combo_title,
      combo_title_size  = input$combo_title_size,
      combo_legend_scope = input$combo_legend_scope,
      combo_legend_side = input$combo_legend_side,
      nrow_combo        = input$nrow_combo,
      ncol_combo        = input$ncol_combo,
      combo_width       = input$combo_width,
      combo_height      = input$combo_height,
      combo_apply_paper_theme = input$combo_apply_paper_theme,
      combo_font_family = input$combo_font_family,
      combo_adv_pal_enable = input$combo_adv_pal_enable,
      combo_adv_pal_type = input$combo_adv_pal_type,
      combo_adv_pal_reverse = input$combo_adv_pal_reverse,
      combo_adv_pal_filters = paste(input$combo_adv_pal_filters %||% character(0), collapse = ","),
      combo_adv_pal_name = input$combo_adv_pal_name,
      fs_title_all      = input$fs_title_all,
      fs_axis_title_all = input$fs_axis_title_all,
      fs_axis_text_all  = input$fs_axis_text_all,
      combo_axis_line_size = input$combo_axis_line_size,
      fs_legend_all     = input$fs_legend_all,
      combo_pal         = input$combo_pal,
      combo_layout_grid = input$combo_layout_grid,
      combo_col_widths  = input$combo_col_widths,
      combo_row_heights = input$combo_row_heights,
      combo_rich_enable = input$combo_rich_enable,
      combo_rich_text   = input$combo_rich_text,
      combo_rich_x      = input$combo_rich_x,
      combo_rich_y      = input$combo_rich_y,
      combo_rich_box_w  = input$combo_rich_box_w,
      combo_rich_box_h  = input$combo_rich_box_h,
      combo_rich_size   = input$combo_rich_size,
      combo_rich_color  = input$combo_rich_color,
      combo_rich_fill   = input$combo_rich_fill,
      ov_targets        = paste(input$ov_targets %||% character(0), collapse = ","),
      ov_title          = input$ov_title,
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
      if (!is.null(v <- gv('combo_title')))
        updateTextInput(session, 'combo_title', value = v)
      if (!is.null(v <- gv('combo_title_size')))
        updateNumericInput(session, 'combo_title_size', value = as.numeric(v))
      if (!is.null(v <- gv('combo_legend_scope')))
        updateSelectInput(session, 'combo_legend_scope', selected = v)
      if (!is.null(v <- gv('combo_legend_side')))
        updateRadioButtons(session, 'combo_legend_side', selected = v)
      if (!is.null(v <- gv('nrow_combo')))
        updateNumericInput(session, 'nrow_combo', value = as.numeric(v))
      if (!is.null(v <- gv('ncol_combo')))
        updateNumericInput(session, 'ncol_combo', value = as.numeric(v))
      if (!is.null(v <- gv('combo_width')))
        updateNumericInput(session, 'combo_width', value = as.numeric(v))
      if (!is.null(v <- gv('combo_height')))
        updateNumericInput(session, 'combo_height', value = as.numeric(v))
      if (!is.null(v <- gv('fs_title_all')))
        updateNumericInput(session, 'fs_title_all', value = as.numeric(v))
      if (!is.null(v <- gv('fs_axis_title_all')))
        updateNumericInput(session, 'fs_axis_title_all', value = as.numeric(v))
      if (!is.null(v <- gv('fs_axis_text_all')))
        updateNumericInput(session, 'fs_axis_text_all', value = as.numeric(v))
      if (!is.null(v <- gv('combo_axis_line_size')))
        updateNumericInput(session, 'combo_axis_line_size', value = as.numeric(v))
      if (!is.null(v <- gv('fs_legend_all')))
        updateNumericInput(session, 'fs_legend_all', value = as.numeric(v))
      if (!is.null(v <- gv('combo_pal')))
        updateSelectInput(session, 'combo_pal', selected = v)
      if (!is.null(v <- gv('combo_apply_paper_theme')))
        updateCheckboxInput(session, 'combo_apply_paper_theme', value = tolower(v) == 'true')
      if (!is.null(v <- gv('combo_font_family')))
        updateSelectInput(session, 'combo_font_family', selected = v)
      if (!is.null(v <- gv('combo_adv_pal_enable')))
        updateCheckboxInput(session, 'combo_adv_pal_enable', value = tolower(v) == 'true')
      if (!is.null(v <- gv('combo_adv_pal_type')))
        updateRadioButtons(session, 'combo_adv_pal_type', selected = v)
      if (!is.null(v <- gv('combo_adv_pal_reverse')))
        updateCheckboxInput(session, 'combo_adv_pal_reverse', value = tolower(v) == 'true')
      if (!is.null(v <- gv('combo_adv_pal_filters')))
        updateCheckboxGroupInput(session, 'combo_adv_pal_filters', selected = split_csv_values(v))
      if (!is.null(v <- gv('combo_adv_pal_name')))
        updateSelectInput(session, 'combo_adv_pal_name', selected = v)
      if (!is.null(v <- gv('combo_layout_grid')))
        updateTextAreaInput(session, 'combo_layout_grid', value = v)
      if (!is.null(v <- gv('combo_col_widths')))
        updateTextInput(session, 'combo_col_widths', value = v)
      if (!is.null(v <- gv('combo_row_heights')))
        updateTextInput(session, 'combo_row_heights', value = v)
      if (!is.null(v <- gv('combo_rich_enable')))
        updateCheckboxInput(session, 'combo_rich_enable', value = tolower(v) == 'true')
      if (!is.null(v <- gv('combo_rich_text')))
        updateTextAreaInput(session, 'combo_rich_text', value = v)
      if (!is.null(v <- gv('combo_rich_x')))
        updateNumericInput(session, 'combo_rich_x', value = as.numeric(v))
      if (!is.null(v <- gv('combo_rich_y')))
        updateNumericInput(session, 'combo_rich_y', value = as.numeric(v))
      if (!is.null(v <- gv('combo_rich_box_w')))
        updateNumericInput(session, 'combo_rich_box_w', value = as.numeric(v))
      if (!is.null(v <- gv('combo_rich_box_h')))
        updateNumericInput(session, 'combo_rich_box_h', value = as.numeric(v))
      if (!is.null(v <- gv('combo_rich_size')))
        updateNumericInput(session, 'combo_rich_size', value = as.numeric(v))
      if (!is.null(v <- gv('combo_rich_color')))
        updateTextInput(session, 'combo_rich_color', value = v)
      if (!is.null(v <- gv('combo_rich_fill')))
        updateTextInput(session, 'combo_rich_fill', value = v)
      if (!is.null(v <- gv('ov_targets')))
        updateSelectizeInput(session, 'ov_targets', selected = split_csv_values(v), server = TRUE)

      ov_new <- list()
      if (!is.null(v <- gv('ov_title'))) {
        updateTextInput(session, 'ov_title', value = v)
        ov_new$title <- v
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
      ord_meta <- split_csv_values(gv("plot_order_ids"))
      if (length(ord_meta)) {
        ids_new <- unique(c(intersect(ord_meta, ids), setdiff(ids, ord_meta)))
        if (length(ids_new) == length(ids)) {
          plot_bank$all <- plot_bank$all[ids_new]
          ids <- ids_new
        }
      }
      sel_meta <- split_csv_values(gv("plots_chosen"))
      sel_base <- if (length(sel_meta)) sel_meta else sel_prev
      sel_valid <- intersect(sel_base, ids)
      if (!length(sel_valid)) sel_valid <- ids
      ord_meta_sel <- if (length(ord_meta)) ord_meta[[1]] else NULL
      ord_sel <- if (!is.null(ord_meta_sel) && ord_meta_sel %in% ids) {
        ord_meta_sel
      } else if (order_prev %in% ids) {
        order_prev
      } else {
        ids[1]
      }
      refresh_plot_picker_inputs(ids, selected_plots = sel_valid, order_selected = ord_sel)
    })

  observeEvent(input$move_up, {
    req(input$plot_order)
    ids <- names(plot_bank$all)
    idx <- match(input$plot_order, ids)
    if (!is.na(idx) && idx > 1) {
      ids[c(idx-1, idx)] <- ids[c(idx, idx-1)]
      plot_bank$all <- plot_bank$all[ids]
      sel <- intersect(ids, input$plots_chosen %||% ids)
      if (!length(sel)) sel <- ids
      refresh_plot_picker_inputs(ids, selected_plots = sel, order_selected = ids[[idx - 1]])
    }
  })

  observeEvent(input$move_down, {
    req(input$plot_order)
    ids <- names(plot_bank$all)
    idx <- match(input$plot_order, ids)
    if (!is.na(idx) && idx < length(ids)) {
      ids[c(idx, idx+1)] <- ids[c(idx+1, idx)]
      plot_bank$all <- plot_bank$all[ids]
      sel <- intersect(ids, input$plots_chosen %||% ids)
      if (!length(sel)) sel <- ids
      refresh_plot_picker_inputs(ids, selected_plots = sel, order_selected = ids[[idx + 1]])
    }
  })

  observeEvent(input$move_top, {
    req(input$plot_order)
    ids <- names(plot_bank$all)
    idx <- match(input$plot_order, ids)
    if (!is.na(idx) && idx > 1) {
      moved <- ids[[idx]]
      ids <- c(moved, ids[-idx])
      plot_bank$all <- plot_bank$all[ids]
      sel <- intersect(ids, input$plots_chosen %||% ids)
      if (!length(sel)) sel <- ids
      refresh_plot_picker_inputs(ids, selected_plots = sel, order_selected = moved)
    }
  })

  observeEvent(input$move_bottom, {
    req(input$plot_order)
    ids <- names(plot_bank$all)
    idx <- match(input$plot_order, ids)
    if (!is.na(idx) && idx < length(ids)) {
      moved <- ids[[idx]]
      ids <- c(ids[-idx], moved)
      plot_bank$all <- plot_bank$all[ids]
      sel <- intersect(ids, input$plots_chosen %||% ids)
      if (!length(sel)) sel <- ids
      refresh_plot_picker_inputs(ids, selected_plots = sel, order_selected = moved)
    }
  })

  observeEvent(input$remove_current, {
    req(input$plot_order)
    ids <- names(plot_bank$all)
    rem_id <- input$plot_order
    if (!length(rem_id) || !rem_id %in% ids) return()

    plot_bank$all[[rem_id]] <- NULL
    ids_new <- names(plot_bank$all)
    sel_new <- setdiff(input$plots_chosen %||% ids, rem_id)
    ord_new <- if (length(ids_new)) ids_new[[1]] else character(0)
    refresh_plot_picker_inputs(ids_new, selected_plots = sel_new, order_selected = ord_new)

    msg <- sprintf(tr_text("combo_removed_count", input$app_lang %||% i18n_lang), 1)
    showNotification(msg, type = "message", duration = 3)
  })

  observeEvent(input$remove_selected, {
    ids <- names(plot_bank$all)
    rem_ids <- intersect(ids, input$plots_chosen %||% character(0))
    if (!length(rem_ids)) {
      showNotification(tr("combo_remove_none"), type = "warning", duration = 3)
      return()
    }
    for (id in rem_ids) plot_bank$all[[id]] <- NULL

    ids_new <- names(plot_bank$all)
    ord_new <- if (length(ids_new)) ids_new[[1]] else character(0)
    refresh_plot_picker_inputs(ids_new, selected_plots = ids_new, order_selected = ord_new)

    msg <- sprintf(tr_text("combo_removed_count", input$app_lang %||% i18n_lang), length(rem_ids))
    showNotification(msg, type = "message", duration = 4)
  })

  combo_plot
}
