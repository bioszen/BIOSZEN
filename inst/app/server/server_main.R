# --- Server logic ---
server <- function(input, output, session) {
  
  # almacenamiento reactivo de los plots que el usuario añade
  plot_bank  <- reactiveValues(all = list())
  
  # bandera para saber si ya inserté la pestaña “Combinado”
  panel_inserto <- reactiveVal(FALSE)

  # trigger para forzar repintado tras overrides globales
  ov_trigger <- reactiveVal(0)

  # ───── contenedores reactivos (vacíos al arrancar) ─────
  datos_box     <- reactiveVal(NULL)   # hoja «Datos»
  plot_cfg_box  <- reactiveVal(NULL)   # hoja «PlotSettings»
  cur_data_box  <- reactiveVal(NULL)   # curvas Sheet1
  cur_cfg_box   <- reactiveVal(NULL)   # curvas Sheet2
  sig_list      <- reactiveVal(list()) # guardará comparaciones
  sig_preselect <- reactiveVal(NULL)   # selección pendiente en el manejador de barras
  meta_store    <- reactiveValues()    # metadata por tipo de gráfico
  is_group_data <- reactiveVal(FALSE)
  bundle_store  <- reactiveValues(datasets = list(), versions = list())
  current_dataset_key <- reactiveVal(NULL)

  curve_data     <- reactive(cur_data_box())
  curve_settings <- reactive(cur_cfg_box())

  # --- Helper: recopilar metadata actual para reproducibilidad ---
  collect_metadata_tbl <- function() {
    base_vals <- list(
      scope          = input$scope,
      tipo           = input$tipo,
      colorMode      = input$colorMode,
      plot_w         = input$plot_w,
      plot_h         = input$plot_h,
      base_size      = input$base_size,
      fs_title       = input$fs_title,
      fs_axis        = input$fs_axis,
      fs_legend      = input$fs_legend,
      axis_line_size = input$axis_line_size
    )
    meta <- tibble::tibble(
      Campo = names(base_vals),
      Valor = vapply(base_vals, as.character, character(1))
    )

    if (input$tipo %in% c("Boxplot", "Barras", "Apiladas", "Violin")) {
      meta <- add_row(
        meta,
        Campo = c("pt_size", "x_angle", "x_wrap", "x_wrap_lines"),
        Valor = c(
          as.character(input$pt_size),
          as.character(input$x_angle),
          as.character(input$x_wrap),
          as.character(input$x_wrap_lines)
        )
      )
    }
    if (input$tipo == "Boxplot") {
      meta <- add_row(meta,
                      Campo = c("pt_jit", "box_w"),
                      Valor = c(as.character(input$pt_jit),
                                as.character(input$box_w)))
    }
    if (input$tipo == "Curvas") {
      meta <- add_row(meta,
                      Campo = "curve_lwd", Valor = as.character(input$curve_lwd))
    }
    if (input$tipo %in% c("Boxplot", "Barras", "Apiladas", "Violin")) {
      meta <- add_row(meta,
                      Campo = c("sig_linewidth", "sig_textsize",
                                "sig_sep", "sig_textpad", "sig_offset",
                                "sig_hide_caps"),
                      Valor = c(as.character(input$sig_linewidth),
                                as.character(input$sig_textsize),
                                as.character(input$sig_sep),
                                as.character(input$sig_textpad),
                                as.character(input$sig_offset),
                                as.character(input$sig_hide_caps)))
    }

    if (input$tipo %in% c("Boxplot", "Barras", "Violin")) {
      meta <- add_row(
        meta,
        Campo = c("param", "doNorm", "ctrlMedium",
                  "errbar_size", "ymax", "ybreak"),
        Valor = c(
          safe_param(),
          as.character(input$doNorm),
          if (is.null(input$ctrlMedium)) "NULL" else input$ctrlMedium,
          as.character(input$errbar_size),
          as.character(get_ylim(safe_param())$ymax),
          as.character(get_ylim(safe_param())$ybreak)
        )
      )
    }
    if (input$tipo == "Violin") {
      meta <- add_row(
        meta,
        Campo = c("violin_width", "violin_linewidth"),
        Valor = c(as.character(input$violin_width),
                  as.character(input$violin_linewidth))
      )
    } else if (input$tipo == "Apiladas") {
      meta <- add_row(
        meta,
        Campo = c("stackParams",
                  "orderStack",
                  "showErrBars",
                  "errbar_size",
                  "ymax", "ybreak"),
        Valor = c(
          paste(input$stackParams, collapse = ","),
          input$orderStack %||% "",
          as.character(input$showErrBars),
          as.character(input$errbar_size),
          as.character(input$ymax),
          as.character(input$ybreak)
        )
      )
    } else if (input$tipo == "Curvas") {
      meta <- add_row(
        meta,
        Campo = c("xmax_cur","xbreak_cur",
                  "ymax_cur","ybreak_cur"),
        Valor = c(as.character(input$xmax_cur), as.character(input$xbreak_cur),
                  as.character(input$ymax_cur), as.character(input$ybreak_cur))
      )
    }
    meta
  }

  observeEvent(input$btn_light, {
    # 1· cambia el tema visual
    session$setCurrentTheme(theme_light)
    # 2· guarda la preferencia en localStorage
    session$sendCustomMessage("saveMode", "light")
    # 3· actualiza input$mode (por si lo usas en otros lugares)
    shinyjs::runjs(
      "Shiny.setInputValue('mode', 'light', {priority: 'event'});"
    )
  })

  write_metadata_xlsx <- function(file){
    wb <- createWorkbook()
    addWorksheet(wb, "Metadata")
    meta <- collect_metadata_tbl()
    if (input$tipo == "Curvas" && !is.null(curve_settings())){
      addWorksheet(wb, "CurvasSettings")
      writeData(wb, "CurvasSettings", curve_settings())
    }
    writeData(wb, "Metadata", meta,
              headerStyle = createStyle(textDecoration = "bold"))
    saveWorkbook(wb, file, overwrite = TRUE)
  }

  write_current_plot_png <- function(file, width = NULL, height = NULL){
    width  <- width  %||% input$plot_w
    height <- height %||% input$plot_h
    scope_sel  <- if (input$scope == "Combinado") "Combinado" else "Por Cepa"
    strain_sel <- if (scope_sel == "Por Cepa") input$strain else NULL

    if (input$tipo == "Apiladas") {
      plt <- build_plotly_stack(scope_sel, strain_sel,
                                width = width, height = height)
      export_plotly_image(
        p      = plt,
        file   = file,
        width  = width,
        height = height
      )
    } else {
      p <- plot_base()
      if (inherits(p, "ggplot")) {
        ggplot2::ggsave(
          filename = file,
          plot     = p,
          width    = width  / 100,
          height   = height / 100,
          dpi      = 300,
          bg       = "transparent"
        )
      } else {
        p <- p %>%
          layout(
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor  = "rgba(0,0,0,0)"
          )
        export_plotly_image(
          p      = p,
          file   = file,
          width  = width,
          height = height
        )
      }
    }
  }
  
  write_current_plot_pdf <- function(file, width = NULL, height = NULL){
    width  <- width  %||% input$plot_w
    height <- height %||% input$plot_h
    scope_sel  <- if (input$scope == "Combinado") "Combinado" else "Por Cepa"
    strain_sel <- if (scope_sel == "Por Cepa") input$strain else NULL
    
    if (input$tipo == "Apiladas") {
      plt <- build_plotly_stack(scope_sel, strain_sel,
                                width = width, height = height)
      export_plotly_image(
        p      = plt,
        file   = file,
        width  = width,
        height = height
      )
    } else {
      p <- plot_base()
      if (inherits(p, "ggplot")) {
        ggplot2::ggsave(
          filename = file,
          plot     = p,
          width    = width  / 100,
          height   = height / 100,
          device   = cairo_pdf,
          bg       = "transparent"
        )
      } else {
        p <- p %>%
          layout(
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor  = "rgba(0,0,0,0)"
          )
        export_plotly_image(
          p      = p,
          file   = file,
          width  = width,
          height = height
        )
      }
    }
  }

  observeEvent(input$btn_dark, {
    session$setCurrentTheme(theme_dark)
    session$sendCustomMessage("saveMode", "dark")
    shinyjs::runjs(
      "Shiny.setInputValue('mode', 'dark', {priority: 'event'});"
    )
  })
  # ---- Cambio de idioma -------------------------------------------
  # ---- Cambio de idioma desde el nuevo menú desplegable -------------------
  observeEvent(input$lang_es, {
    shinyjs::runjs("setLang('es');")          # Google Translate → Español
  })
  
  observeEvent(input$lang_en, {
    shinyjs::runjs("setLang('en');")          # Google Translate → English
  })
  
  
  # ---------- helper: genera SIEMPRE un ggplot (o grob) -----------------
  make_snapshot <- function(){
    scope_sel  <- if (input$scope == "Combinado") "Combinado" else "Por Cepa"
    strain_sel <- if (scope_sel == "Por Cepa")    input$strain else NULL
    
    if (input$tipo == "Apiladas"){
      # build_plot() devuelve la versión ggplot de Apiladas
      build_plot(scope_sel, strain_sel, "Apiladas")
    } else {
      # para los demás tipos, plot_base() ya es ggplot
      plot_base()
    }
  }
  
  # Devuelve el ángulo que realmente se aplicará a las etiquetas X
  get_x_angle <- function(n, angle_input){
    if (!is.na(angle_input))             # el usuario escribió algo → úsalo
      return(angle_input)
    if (n > 6) return(45)                # “adaptativo”: >6 etiquetas ⇒ 45°
    0                                     # caso contrario ⇒ horizontal
  }

  # Margen inferior según el ángulo de etiquetas X
  get_bottom_margin <- function(angle, wrap = FALSE, lines = 2){
    if (isTRUE(wrap)) return(50 + 20 * (lines - 1))
    if (is.na(angle) || angle <= 0) 30 else 60
  }
  
  margin_adj <- function(top, right, bottom, left){
    margin(
      top    + (input$margin_top_adj    %||% 0),
      right  + (input$margin_right_adj  %||% 0),
      bottom + (input$margin_bottom_adj %||% 0),
      left   + (input$margin_left_adj   %||% 0)
    )
  }

  # Convierte "A-B" en "A B" y luego introduce saltos de línea
  wrap_label <- function(x, lines = 2) {
    make_lbl <- function(s) {
      s <- gsub("-", " ", s, fixed = TRUE)
      words <- strsplit(s, " +")[[1]]
      if (length(words) <= lines) {
        paste(words, collapse = "\n")
      } else {
        paste(c(words[seq_len(lines - 1)],
                paste(words[lines:length(words)], collapse = " ")),
              collapse = "\n")
      }
    }
    if (is.factor(x)) {
      levels(x) <- vapply(levels(x), make_lbl, character(1))
      x
    } else {
      vapply(x, make_lbl, character(1))
    }
  }

  wrap_label_html <- function(x, lines = 2) {
    make_lbl <- function(s) {
      s <- gsub("-", " ", s, fixed = TRUE)
      words <- strsplit(s, " +")[[1]]
      if (length(words) <= lines) {
        paste(words, collapse = "<br>")
      } else {
        paste(c(words[seq_len(lines - 1)],
                paste(words[lines:length(words)], collapse = " ")),
              collapse = "<br>")
      }
    }
    if (is.factor(x)) {
      levels(x) <- vapply(levels(x), make_lbl, character(1))
      x
    } else {
      vapply(x, make_lbl, character(1))
    }
  }

  # Inicializa módulos con los helpers generados arriba
  combo_plot <- setup_panel_module(input, output, session,
                                   plot_bank, panel_inserto, ov_trigger,
                                   make_snapshot, collect_metadata_tbl,
                                   curve_settings)
  growth_mod    <- setup_growth_module(input, output, session)
  growth_out_dir <- growth_mod$growth_dir

  # ─── Logo siempre visible ─────────────────────────────────────────
  output$logo_img <- renderUI({
    # si todavía no hay input$mode asumimos “light”
    modo <- input$mode %||% "light"      # operador %||%  → usa la derecha si es NULL
    
    archivo <- if (modo == "dark") "logo_dark.png" else "logo_light.png"
    tags$img(src = archivo, style = "height:220px;")
  })
  
  observeEvent(input$doNorm, {
    if (isTRUE(input$doNorm) && is.null(input$ctrlMedium)) {  
      showNotification("Selecciona primero el medio control",  
                       type = "warning", duration = 4)  
    }  
  })  
  
  sig_choice_vec <- function(sl){
    if (!length(sl)) return(setNames(character(), character()))
    labels <- vapply(seq_along(sl), function(i){
      cmp <- sl[[i]]
      lab <- cmp$lab %||% ""
      if (is.na(lab)) lab <- ""
      base <- sprintf("%02d) %s vs %s", i, cmp$g1, cmp$g2)
      if (nzchar(lab)) paste0(base, " [", lab, "]") else base
    }, character(1))
    stats::setNames(as.character(seq_along(sl)), labels)
  }

  observeEvent(sig_list(), {
    choices  <- sig_choice_vec(sig_list())
    current  <- isolate(input$sig_current)
    pre_sel  <- sig_preselect()
    sig_preselect(NULL)
    selected <- intersect(if (length(pre_sel)) pre_sel else current,
                          unname(choices))
    updateSelectizeInput(
      session, "sig_current",
      choices  = choices,
      selected = selected,
      server   = TRUE
    )
  })
  
  observeEvent(input$add_sig, {  
    req(input$sig_group1, input$sig_group2, nzchar(input$sig_label))  
    # no duplicar  
    new_cmp <- list(g1 = input$sig_group1,  
                    g2 = input$sig_group2,  
                    lab = input$sig_label)  
    sl <- sig_list()  
    if (!any(vapply(sl, function(x) identical(x, new_cmp), logical(1)))) {  
      sig_preselect(as.character(length(sl) + 1))
      sig_list( append(sl, list(new_cmp)) )  
    }  
  })  
  
  observeEvent(input$remove_sig, {
    ids <- as.integer(input$sig_current)
    if (!length(ids)) {
      showNotification("Selecciona al menos una barra para eliminar.",
                       type = "message", duration = 3)
      return()
    }
    sl <- sig_list()
    keep <- setdiff(seq_along(sl), ids[ids >= 1 & ids <= length(sl)])
    sig_preselect(if (length(keep)) as.character(min(keep)) else NULL)
    sig_list(sl[keep])
  })

  move_sig <- function(direction){
    sl <- sig_list()
    if (!length(sl)) return()
    ids <- as.integer(input$sig_current)
    if (length(ids) != 1) {
      showNotification("Selecciona solo una barra para reordenar.",
                       type = "message", duration = 3)
      return()
    }
    idx <- ids[1]
    if (idx < 1 || idx > length(sl)) return()
    target <- if (identical(direction, "up")) idx - 1 else idx + 1
    if (target < 1 || target > length(sl)) return()
    sl[c(idx, target)] <- sl[c(target, idx)]
    sig_preselect(as.character(target))
    sig_list(sl)
  }

  observeEvent(input$sig_move_up,  { move_sig("up")  }, ignoreInit = TRUE)
  observeEvent(input$sig_move_down,{ move_sig("down")}, ignoreInit = TRUE)
  
  observeEvent(input$clear_sig, {  
    sig_preselect(NULL)
    sig_list(list())  
  })  
  
  
  observeEvent(input$curveFile, {
    ok <- tryCatch({
      # 1) Leer curvas (Sheet1)
      d <- read_excel_tmp(input$curveFile$datapath, sheet = "Sheet1")
      
      # 2) Leer configuración (Sheet2)
      s <- tryCatch(
        read_excel_tmp(input$curveFile$datapath, sheet = "Sheet2"),
        error = function(e) NULL
      )
      
      # 3) Si falta config o le faltan columnas, armamos defaults basados en d
      required_cols <- c("X_Max","Interval_X","Y_Max","Interval_Y","X_Title","Y_Title")
      if (is.null(s) || !all(required_cols %in% names(s))) {
        s <- tibble::tibble(
          X_Max      = max(d$Time, na.rm = TRUE),
          Interval_X = max(d$Time, na.rm = TRUE) / 3,
          Y_Max      = max(d[ , setdiff(names(d), "Time")], na.rm = TRUE),
          Interval_Y = max(d[ , setdiff(names(d), "Time")], na.rm = TRUE) / 3,
          X_Title    = "Tiempo (min)",
          Y_Title    = "OD (620 nm)",
          Well       = NA,                    # si tu lógica luego asume estas columnas
          BiologicalReplicate = NA
        )
      }
      
      # 4) Guardar en los reactivos
      cur_data_box(d)
      cur_cfg_box(s)
      
      # 5) Inicializar límites de curvas
      ylims$Curvas <- list(
        xmax   = s$X_Max[1],
        xbreak = s$Interval_X[1],
        ymax   = s$Y_Max[1],
        ybreak = s$Interval_Y[1]
      )
      
      TRUE
    }, error = function(e) {
      showNotification(
        paste("❌ Archivo de curvas inválido:", e$message),
        type = "error", duration = 6
      )
      FALSE
    })
    if (!ok) return()
  }, ignoreInit = TRUE)
  
  observeEvent(curve_settings(), {
    req(curve_settings())
    # tomo la primera fila de tu Sheet2
    cfg <- curve_settings()[1, ]
    # actualizo los controles de Curvas
    updateNumericInput(session, "xmax_cur",   value = cfg$X_Max)
    updateNumericInput(session, "xbreak_cur", value = cfg$Interval_X)
    updateNumericInput(session, "ymax_cur",   value = cfg$Y_Max)
    updateNumericInput(session, "ybreak_cur", value = cfg$Interval_Y)
    
    fluidRow(
      column(6, numericInput("ymin_corr",      "Y min:",            value = 0, min = 0)),
      column(6, numericInput("ymax_corr",      "Y max:",            value = 1, min = 0))
    )
    fluidRow(
      column(6, numericInput("corr_label_size","Tamaño etiquetas:", value = 5, min = 1 ))
    )
    
    
    updateTextInput(session, "cur_xlab", value = cfg$X_Title)
    updateTextInput(session, "cur_ylab", value = cfg$Y_Title)
  })
  
  
  
  curve_data     <- reactive( cur_data_box() )  
  curve_settings <- reactive( cur_cfg_box() )  
  
  
  # ── Inputs dinámicos para Curvas: selección de réplicas por pozo ──  
  output$repSelCurvas <- renderUI({  
    # metadatos de wells  
    cfg <- curve_settings()  
    req(cfg$Well)  # asumimos que sheet2 tiene una columna Well  
    
    meta <- datos_combinados()
    if (input$scope == "Por Cepa") {
      meta <- meta %>% filter(Strain == input$strain)
      if (!is.null(input$showMedios))
        meta <- meta %>% filter(Media %in% input$showMedios)
    } else {
      if (!is.null(input$showGroups) && length(input$showGroups))
        meta <- meta %>% filter(paste(Strain, Media, sep = "-") %in% input$showGroups)
    }
    
    # cada well es un valor único de cfg$Well presente en la selección  
    wells <- intersect(unique(cfg$Well), meta$Well)  
    if (!length(wells)) return(NULL)
    lapply(wells, function(w) {  
      reps <- sort(unique(cfg$BiologicalReplicate[cfg$Well == w]))  
      checkboxGroupInput(  
        paste0("reps_cur_", make.names(w)),  
        paste("Réplicas –", w),  
        choices  = reps,  
        selected = reps  
      )  
    })  
  })  
  
  # --- Reactivos de lectura seguros -------------------------------------------  
  ## ────────── NUEVO manejo robusto del archivo principal ──────────  
  datos_raw      <- reactiveVal(NULL)   #   guarda «Datos»  
  plot_settings  <- reactiveVal(NULL)   #   guarda «PlotSettings»  
  
  # ── Lectura robusta del Excel de metadata+parámetros ─────────────────────────
  observeEvent(input$dataFile, {

    ok <- tryCatch({

      df_raw <- tryCatch(
        read_excel_tmp(input$dataFile$datapath, sheet = "Datos"),
        error = function(e) NULL
      )
      cfg_raw <- NULL
      is_group <- FALSE

      if (is.null(df_raw) || !all(c("Strain", "Media") %in% names(df_raw))) {
        conv <- build_platemap_from_summary(input$dataFile$datapath)
        if (is.null(conv)) stop("Formato de archivo no reconocido.")
        df_raw  <- conv$Datos
        cfg_raw <- conv$PlotSettings
        is_group <- TRUE
      } else {
        cfg_raw <- tryCatch(
          read_excel_tmp(input$dataFile$datapath, sheet = "PlotSettings"),
          error = function(e) NULL
        )
      }

      prep <- prepare_platemap(df_raw, cfg_raw)
      df   <- prep$datos
      cfg  <- prep$cfg

      ylims <<- reactiveValues()
      for (p in cfg$Parameter) {
        row <- cfg[cfg$Parameter == p, ]
        ylims[[p]] <- list(
          ymax   = row$Y_Max,
          ybreak = row$Interval
        )
        ylims[[paste0(p, "_Norm")]] <- list(
          ymax   = 1,
          ybreak = 0.2
        )
      }
      datos_box(NULL);          plot_cfg_box(NULL)
      datos_box(df);            plot_cfg_box(cfg)

      is_group_data(is_group)
      TRUE

    }, error = function(e){
      showNotification(paste("❌ Archivo inválido:", e$message),
                       type = "error", duration = 6)
      FALSE
    })
    
    if (!ok) return()
    
    ## ──  REFRESCAR UI   (según haya o no parámetros «reales») ────────────────
    params <- plot_cfg_box()$Parameter
    
    # 2.a) actualiza selector de tipo de gráfico ------------------------------
    if (length(params) == 0 || identical(params, "Parametro_dummy")) {
      updateRadioButtons(session, "tipo",
                         choices  = c("Curvas"), selected = "Curvas")
    } else {
      updateRadioButtons(session, "tipo",
                         choices  = c("Boxplot","Barras","Violin","Curvas","Apiladas",
                                      "Correlación"),
                         selected = "Boxplot")
    }
    
    # 2.b) selector de parámetro (lo deje vacío si no hay)
      updateSelectInput(session, "param",
                        choices  = params,
                        selected = if (length(params)) params[1] else character(0))

      if (length(params)) {
        first_cfg <- cfg[cfg$Parameter == params[1], ]
        updateNumericInput(session, "ymax",
                           label  = paste0("Y max (", params[1], "):"),
                           value  = first_cfg$Y_Max)
        updateNumericInput(session, "ybreak",
                           label  = paste0("Int Y (", params[1], "):"),
                           value  = first_cfg$Interval)
      }

    }, ignoreInit = TRUE)


  observeEvent(is_group_data(), {
    if (isTRUE(is_group_data())) {
      shinyjs::hide("downloadExcel_section")
    } else {
      shinyjs::show("downloadExcel_section")
    }
  })

  # --- Descargar carpeta con archivos de referencia (platemap, datos agrupados, curvas)
  output$download_refs <- downloadHandler(
    filename = function() {
      paste0("Archivos_de_referencia.zip")
    },
    content = function(file) {
      # Localizar la carpeta 'www/Archivos de referencia' tanto en desarrollo como instalado
      find_www <- function() {
        # preferible: dentro del paquete instalado
        pkg_www <- system.file("app/www", package = "BIOSZEN")
        if (!is.null(pkg_www) && nzchar(pkg_www) && dir.exists(pkg_www)) return(pkg_www)
        # desarrollo: cuando se ejecuta desde 'inst/app'
        if (dir.exists("www")) return(normalizePath("www", winslash = "/", mustWork = TRUE))
        # desarrollo: cuando se ejecuta desde raíz del proyecto
        p <- file.path("inst", "app", "www")
        if (dir.exists(p)) return(normalizePath(p, winslash = "/", mustWork = TRUE))
        stop("No se encontró la carpeta 'www'.")
      }

      www_dir <- find_www()
      ref_dir <- file.path(www_dir, "Archivos de referencia")
      if (!dir.exists(ref_dir)) {
        stop("No se encontró la carpeta 'Archivos de referencia' dentro de 'www'.")
      }

      # Crear zip preservando el nombre de la carpeta
      parent <- dirname(ref_dir)
      base   <- basename(ref_dir)
      zip::zipr(zipfile = file, files = base, root = parent)
    }
  )

  # --- Descargar Manual (PDF). Si falta PDF, intenta convertir desde DOCX ---
  output$downloadManual <- downloadHandler(
    filename = function() {
      lang <- input$manual_lang %||% 'es'
      if (lang == 'en') 'MANUAL_EN.pdf' else 'MANUAL_ES.pdf'
    },
    content = function(file) {
      lang <- input$manual_lang %||% 'es'
      fname_pdf  <- if (lang == 'en') 'MANUAL_EN.pdf' else 'MANUAL_ES.pdf'
      fname_docx <- if (lang == 'en') 'MANUAL_EN.docx' else 'MANUAL_ES.docx'

      # localizar carpeta www
      find_www <- function() {
        pkg_www <- system.file('app/www', package = 'BIOSZEN')
        if (!is.null(pkg_www) && nzchar(pkg_www) && dir.exists(pkg_www)) return(pkg_www)
        if (dir.exists('www')) return(normalizePath('www', winslash = '/', mustWork = TRUE))
        p <- file.path('inst','app','www')
        if (dir.exists(p)) return(normalizePath(p, winslash = '/', mustWork = TRUE))
        stop("No se encontró la carpeta 'www'.")
      }

      www_dir <- find_www()
      src_pdf  <- file.path(www_dir, fname_pdf)
      src_docx <- file.path(www_dir, fname_docx)

      # si ya existe el PDF, copiarlo directamente
      if (file.exists(src_pdf)) {
        file.copy(src_pdf, file, overwrite = TRUE)
        return()
      }

      # intentar convertir DOCX -> PDF con varias estrategias
      convert_ok <- FALSE
      if (file.exists(src_docx)) {
        # 1) doconv (LibreOffice / Word) si está disponible
        if (!convert_ok && requireNamespace('doconv', quietly = TRUE)) {
          convert_ok <- tryCatch({
            doconv::to_pdf(src_docx, output = file)
            file.exists(file)
          }, error = function(e) FALSE)
        }

        # 2) pandoc + motor PDF si está disponible
        if (!convert_ok && requireNamespace('rmarkdown', quietly = TRUE)) {
          engines <- c('xelatex','lualatex','pdflatex','wkhtmltopdf')
          have <- engines[nzchar(Sys.which(engines))]
          if (length(have) > 0) {
            eng <- have[[1]]
            convert_ok <- tryCatch({
              rmarkdown::pandoc_convert(
                src_docx,
                to      = 'pdf',
                output  = file,
                options = sprintf('--pdf-engine=%s', eng)
              )
              file.exists(file)
            }, error = function(e) FALSE)
          }
        }
      }

      if (!convert_ok) {
        # Último recurso: entregar DOCX si no se pudo generar PDF
        shiny::req(file.exists(src_docx))
        file.copy(src_docx, file, overwrite = TRUE)
      }
    }
  )


  observeEvent(plot_settings(), {
    req(plot_settings())
    params <- plot_settings()$Parameter
    updateCheckboxGroupInput(
      session, "stackParams",
      choices  = params,
      selected = params           # default = todos
    )  
    # ---------- Correlación: poblar selectInputs --------------------------
    updateSelectInput(
      session, "corr_param_x",
      choices  = params,
      selected = params[1]
    )
    updateSelectInput(
      session, "corr_param_y",
      choices  = params,
      selected = params[min(2, length(params))]
    )
    
    # inicializar también el orden de parámetros apilados  
    updateTextInput(  
      session, "orderStack",  
      value = paste(params, collapse = ",")  
    )  
  }, ignoreInit = FALSE)  
  
  
  ## accesos rápidos (usan las reactiveVal que acabamos de crear)  
  datos_combinados <- reactive( datos_box() )  
  plot_settings    <- reactive( plot_cfg_box() )  
  
  # --- Parám. seguro: siempre existe en el Excel cargado ----  
  safe_param <- reactive({  
    req(input$param)  
    if (isTRUE(input$doNorm) && !is.null(input$ctrlMedium))  
      paste0(input$param, "_Norm")  
    else  
      input$param  
  })  
  
  
  
  
  # justo después, dentro de server(), inicializa también la entrada “Curvas”  
  observeEvent(curve_settings(), {  
    req(curve_settings())          # ← evita que se ejecute con NULL  
    cfg <- curve_settings()[1, ]  
    ylims$Curvas <- list(  
      ymax   = cfg$Y_Max,  
      ybreak = cfg$Interval_Y,  
      xmax   = cfg$X_Max,  
      xbreak = cfg$Interval_X  
    )  
  }, ignoreNULL = TRUE)             # ← cualquiera de las dos opciones  
  
  
  # La función safe_hue ahora se define de forma global en helpers.R
  # y se carga al inicio de la aplicación, por lo que ya no se
  # declara localmente dentro del servidor principal.
  
  
  # 1) Inicialización: crear una entrada en ylims para cada parámetro  
  observeEvent(plot_settings(), {  
    params <- plot_settings()$Parameter  
    for (p in params) {  
      # ya estáis haciendo esto  
      if (is.null(ylims[[p]])) {  
        cfg <- plot_settings() %>% filter(Parameter == p)  
        ylims[[p]] <- list(  
          ymax   = cfg$Y_Max,  
          ybreak = cfg$Interval  
        )  
      }  
      # ─── PEGA ABAJO este bloque ─────────────────────────────────────────  
      # límites por defecto para valores normalizados  
      ylims[[paste0(p, "_Norm")]] <- list(  
        ymax   = 1,     # rango típico normalizado: 0–1  
        ybreak = 0.2    # intervalo de 0.2  
      )  
      # ────────────────────────────────────────────────────────────────────  
    }  
  }, ignoreNULL = FALSE)  
  
  
  # ── Reset general al cargar un nuevo archivo ───────────────────────  
  observeEvent(input$dataFile, {  
    req(plot_settings())                     # esperamos a tener la hoja  
    
    ## 1· reiniciar parámetro seleccionado  
    updateSelectInput(session, "param",  
                      choices  = plot_settings()$Parameter,  
                      selected = plot_settings()$Parameter[1])  
    
    ## 2· reiniciar strain / scope  
    updateRadioButtons(session, "scope", selected = "Por Cepa")  
    updateSelectInput(session, "strain", choices = NULL)  
    
    ## 3· reiniciar selección de gráficos  
    isolate({  
      strains <- sort(unique(datos_combinados()$Strain))  
      tipos <- c("Boxplot","Barras","Violin","Curvas","Apiladas")  
      cepa    <- as.vector(t(outer(strains, tipos,  
                                   FUN = function(s, t) paste0(s, "_", t))))  
      combo   <- paste0("Combinado_", tipos)  
      
    })  
  })  
  
  # Para Boxplot/Barras  
  ## ── guardar cambios de Y‑axis hechos por el usuario ──────────────────  
  observeEvent(
    list(input$ymax, input$ybreak, input$param),
    {  
      # clave correcta:  “PAR”    o  “PAR_Norm”  
      tgt <- if (isTRUE(input$doNorm) && !is.null(input$ctrlMedium))  
        paste0(input$param, "_Norm") else input$param  
      
      ylims[[tgt]] <- list(  
        ymax   = input$ymax,  
        ybreak = input$ybreak  
      )  
    },  
    ignoreInit = TRUE  
  )  
  
  # Para Curvas  
  observeEvent(list(input$xmax_cur, input$xbreak_cur, input$ymax_cur, input$ybreak_cur), {  
    req(curve_settings())  
    ylims$Curvas <- list(  
      xmax   = input$xmax_cur,  
      xbreak = input$xbreak_cur,  
      ymax   = input$ymax_cur,  
      ybreak = input$ybreak_cur  
    )  
  }, ignoreInit = TRUE)  
  
  
  
  # 3) Y en build_plot(), para obtener los límites, ya no usas Curvas/AUC fijos,  
  #    sino:  
  get_ylim <- function(param) {  
    req(ylims[[param]])  
    ylims[[param]]  
  }  
  
  
  # dentro de server(), tras definir plot_settings()  
  output$paramSel <- renderUI({
    req(plot_cfg_box())                         # cfg ya cargada
    params <- plot_cfg_box()$Parameter
    if (length(params) == 0 || identical(params, "Parametro_dummy")) {
      helpText("⚠️ Sin parámetros: sólo se pueden graficar Curvas")
    } else {
      selectInput("param", "Parámetro:",
                  choices = params, selected = params[1])
    }
  })
  
  
  # --- Procesamiento de datos dinámico ---  
  datos_agrupados <- reactive({
    req(datos_combinados(), plot_settings())
    
    # 1) Parametros de configuración vs columnas reales
    params <- plot_settings()$Parameter
    df     <- datos_combinados()
    present <- intersect(params, names(df))
    missing <- setdiff(params, present)
    
    # 2) Notificar si faltan
    if (length(missing) > 0) {
      showNotification(
        paste0("Estos parámetros no existen en los datos y fueron omitidos: ",
               paste(missing, collapse = ", ")),
        type = "warning", duration = 5
      )
    }
    
    # 3) Agrupar y resumir sólo con los parámetros presentes
    df %>%
      filter(!is.na(Strain), !is.na(Media), Strain != "C-") %>%
      group_by(Strain, Media, BiologicalReplicate) %>%
      summarise(
        across(all_of(present), ~ mean(.x, na.rm = TRUE)),
        Orden = first(Orden),
        .groups = "drop"
      )
  })
  
  
  
  # --- Inputs dinámicos: Por Cepa ---  
  
  observeEvent(datos_agrupados(), {  
    
    
    # 1) poblar selector de cepas  
    updateSelectInput(session, "strain",  
                      choices = sort(unique(datos_agrupados()$Strain)))  
    # 2) poblar filtro de medios  
    medias <- sort(unique(datos_agrupados()$Media))  
    output$showMediosUI <- renderUI({  
      checkboxGroupInput("showMedios", "Medios:",  
                         choices = medias, selected = medias)  
    })  
    # 3) inicializar orden de medios  
    # Inicializa orderMedios usando el orden de la columna ‘Orden’
    medias_order <- datos_agrupados() %>%
      filter(Strain == input$strain) %>%
      distinct(Media, Orden) %>%
      arrange(Orden) %>%
      pull(Media)
    
    updateTextInput(session, "orderMedios",
                    value = paste(medias_order, collapse = ","))
    
    updateCheckboxInput(session, "toggleMedios",  value = TRUE)  
    updateCheckboxInput(session, "toggleGroups", value = TRUE)  
    
    # ------------------------------------------------------------------  
    
    
    
  })  
  
  output$rmRepsGlobalUI <- renderUI({
    scope_sel <- input$scope %||% "Por Cepa"
    df <- datos_agrupados()
    if (scope_sel == "Por Cepa" && !is.null(input$showMedios)) {
      df <- df %>% filter(Media %in% input$showMedios, Strain == input$strain)
    } else if (scope_sel == "Combinado" && !is.null(input$showGroups)) {
      df <- df %>% filter(paste(Strain, Media, sep = "-") %in% input$showGroups)
    }
    reps <- sort(unique(df$BiologicalReplicate))
    if (!length(reps)) return(NULL)
    checkboxGroupInput(
      "rm_reps_all",
      "Excluir réplica(s) en todos los grupos",
      choices  = reps,
      selected = NULL
    )
  })
  
  
  ## ------------------------------------------------------------------  
  ## Reactive: datos_agrupados_norm() – copia de datos_agrupados() con  
  ##           columnas normalizadas (sufijo "_Norm")  
  ## ------------------------------------------------------------------  
  datos_agrupados_norm <- reactive({  
    df <- datos_agrupados()  
    if (!isTRUE(input$doNorm)) return(df)          # sin normalizar  
    
    params <- plot_settings()$Parameter  
    
    # 2a) si el control NO está definido, solo ‘clonamos’ las columnas  
    if (is.null(input$ctrlMedium)) {  
      return(  
        df %>% mutate(across(all_of(params),  
                             ~ .x,                # copia tal cual  
                             .names = "{.col}_Norm"))  
      )  
    }  
    
    # 2b) control definido → normalización real  
    ctrl <- input$ctrlMedium  
    df %>%  
      group_by(Strain, BiologicalReplicate) %>%  
      mutate(across(  
        all_of(params),  
        ~ {  
          base <- .x[Media == ctrl][1]          # valor del control  
          if (is.na(base) || base == 0) NA_real_ else .x / base  
        },  
        .names = "{.col}_Norm"  
      )) %>%  
      ungroup()  
  })  
  
  
  # ── Toggle “Por Cepa” ───────────────────────────────────  
  observeEvent(input$toggleMedios, {  
    medias <- sort(unique(datos_agrupados()$Media))  
    sel    <- if (isTRUE(input$toggleMedios)) medias else character(0)  
    updateCheckboxGroupInput(session,  
                             inputId  = "showMedios",  
                             choices  = medias,  
                             selected = sel  
    )  
  })  
  
  # ── Toggle “Combinado” ─────────────────────────────────  
  observeEvent(input$toggleGroups, {  
    grps <- unique(paste(datos_agrupados()$Strain, datos_agrupados()$Media, sep = "-"))  
    sel  <- if (isTRUE(input$toggleGroups)) grps else character(0)  
    updateCheckboxGroupInput(session,  
                             inputId  = "showGroups",  
                             choices  = grps,  
                             selected = sel  
    )  
    
    
  })  
  
  
  
  # ---------- NUEVO: checkboxes de réplicas por medio (modo Por Cepa) ----------  
  output$repsStrainUI <- renderUI({  
    df <- datos_agrupados() %>%              # datos promediados  
      filter(Strain == input$strain)  
    if (!is.null(input$showMedios)) {
      df <- df %>% filter(Media %in% input$showMedios)
    }
    
    # construye un “sub-checkbox” por cada medio de esa cepa  
    tagList(lapply(unique(df$Media), function(m){  
      reps <- sort(unique(df$BiologicalReplicate[df$Media == m]))  
     drop_all <- as.character(input$rm_reps_all %||% character(0))
      checkboxGroupInput(  
        paste0("reps_", make.names(m)),       # id = reps_<medio>  
        paste("Réplicas -", m),  
        choices  = reps,  
        selected = setdiff(as.character(reps), drop_all)  
      )  
    }))  
  })  
  
  output$ctrlSelUI <- renderUI({  
    req(input$doNorm)                          # sólo cuando se active el check  
    
    # ‑‑ si la cepa aún no está elegida, muestra TODOS los medios  
    if (input$scope == "Por Cepa" && !is.null(input$strain)) {  
      opts <- sort(unique(  
        datos_agrupados()$Media[  
          datos_agrupados()$Strain == input$strain]))  
    } else {  
      opts <- sort(unique(datos_agrupados()$Media))  
    }  
    
    selectInput("ctrlMedium", "Medio normalizador:",    # ← etiqueta genérica  
                choices = opts,  
                selected = if (length(opts)) opts[1] else character(0))  
  })  
  
  # --- Inputs dinámicos: Combinado ---  
  observeEvent(datos_agrupados(), {
    grps <- unique(paste(datos_agrupados()$Strain, datos_agrupados()$Media, sep = "-"))
    # ── SERVER  ─────────────────────────────────────────────
    output$groupSel <- renderUI({
      grps <- unique(paste(datos_agrupados()$Strain,
                           datos_agrupados()$Media, sep = "-"))
      
      labels_order <- datos_agrupados() %>%
        distinct(Strain, Media, Orden) %>%
        mutate(Label = paste(Strain, Media, sep = "-")) %>%
        arrange(Orden) %>% pull(Label)
      
      tagList(
        checkboxGroupInput(
          "showGroups", "Grupos:",
          choices  = grps,
          selected = grps
        ),
        textInput(
          "orderGroups", "Orden (csv):",
          value = paste(labels_order, collapse = ",")
        )
      )
    })
    updateCheckboxInput(session, "toggleGroups", value = TRUE)
  })
  
  output$repsGrpUI <- renderUI({
    grps <- input$showGroups
    if (is.null(grps) || !length(grps)) return(helpText("Selecciona primero los grupos a mostrar."))
    df <- datos_agrupados() %>%
      dplyr::filter(paste(Strain, Media, sep = "-") %in% grps)
    
    accordion(
      id       = "repsGrpPanel",
      open     = FALSE,   # empieza cerrada
      multiple = TRUE,
      accordion_panel(
        "Réplicas por grupo",
        tagList(                       # envolver lapply
          lapply(grps, function(g){
            reps <- unique(
              df$BiologicalReplicate[
                paste(df$Strain, df$Media, sep = "-") == g
              ]
            )
            drop_all <- as.character(input$rm_reps_all %||% character(0))
            checkboxGroupInput(
              paste0("reps_grp_", make.names(g)),
              paste("Réplicas -", g),
              choices  = reps,
              selected = setdiff(as.character(reps), drop_all)
            )
          })
        ),
        style = "default"
      )
    )
  })
  
  # -------------------------------------------------------------------------
  # ► ACTUALIZA las listas de grupos disponibles para las barras de
  #   significancia.  Toma SIEMPRE los grupos que realmente se muestran en
  #   el gráfico (tras pasar por los filtros order_filter_*).
  # -------------------------------------------------------------------------
  observe({
    req(datos_agrupados())                       # hay datos cargados
    
    if (input$scope == "Por Cepa") {             # ── modo ‘Por Cepa’ ──
      req(input$strain)                          # ya hay una cepa elegida
      
      # Sólo los medios visibles con los filtros actuales
      medios_visibles <- datos_agrupados()       |>
        filter(Strain == input$strain)           |>
        order_filter_strain()                    |>      # respeta showMedios / orden
        pull(Media)                              |> 
        unique()                                 |> 
        sort()
      
      updateSelectInput(session, "sig_group1",
                        choices  = medios_visibles,
                        selected = medios_visibles[1])
      updateSelectInput(session, "sig_group2",
                        choices  = medios_visibles,
                        selected = if (length(medios_visibles) > 1)
                          medios_visibles[2] else medios_visibles[1])
      
    } else {                                     # ── modo ‘Combinado’ ──
      # Los grupos que siguen visibles después de los filtros “showGroups”
      grupos_visibles <- datos_agrupados()       |>
        order_filter_group()                     |>
        pull(Label)                              |>
        unique()                                 |>
        sort()
      
      updateSelectInput(session, "sig_group1",
                        choices  = grupos_visibles,
                        selected = grupos_visibles[1])
      updateSelectInput(session, "sig_group2",
                        choices  = grupos_visibles,
                        selected = if (length(grupos_visibles) > 1)
                          grupos_visibles[2] else grupos_visibles[1])
    }
  })
  # -------------------------------------------------------------------------
  
  
  # -- actualizar listas de Control / Pareo cuando cambian los grupos visibles --  
  observeEvent(input$showGroups, {  
    grps <- input$showGroups  
    updateSelectInput(session, "controlGroup", choices = grps,  
                      selected = if (length(grps)) grps[1] else NULL)  
    updateSelectInput(session, "group1", choices = grps,  
                      selected = if (length(grps)) grps[1] else NULL)  
    updateSelectInput(session, "group2", choices = grps,  
                      selected = if (length(grps) > 1) grps[2] else NULL)  
  }, ignoreNULL = FALSE)  
  
  # --- Defaults de escala y labels -----------------------------------
  observeEvent(input$param, {
    req(plot_settings(), input$param)
    
    cfg <- plot_settings() %>%
      dplyr::filter(Parameter == input$param)
    
    # 1· refrescar las cajas
    updateNumericInput(session, "ymax",
                       label  = paste0("Y max (", input$param, "):"),
                       value  = cfg$Y_Max)
    updateNumericInput(session, "ybreak",
                       label  = paste0("Int Y (", input$param, "):"),
                       value  = cfg$Interval)
    
    # 2· sincronizar ylims con el Excel
    ylims[[ input$param ]] <- list(
      ymax   = cfg$Y_Max,
      ybreak = cfg$Interval
    )
  }, ignoreNULL = TRUE)
  
  # ---------- Correlación: actualizar límites por defecto -------------
  observeEvent(
    list(input$corr_param_x, input$corr_param_y,
         input$doNorm, input$ctrlMedium),
    {
      req(plot_settings(), datos_box())
      # nombres reales (sin "_Norm")
      raw_x <- input$corr_param_x
      raw_y <- input$corr_param_y
      updateTextInput(session, "corr_xlab", value = raw_x)
      updateTextInput(session, "corr_ylab", value = raw_y)

      cfg_x <- plot_settings() %>% filter(Parameter == raw_x)
      cfg_y <- plot_settings() %>% filter(Parameter == raw_y)
      df    <- datos_box()

      xmax <- if (nrow(cfg_x)) cfg_x$Y_Max[1] else NA_real_

      if (!is.finite(xmax)) {
        xmax <- suppressWarnings(max(df[[raw_x]], na.rm = TRUE))
      }
      if (!is.finite(xmax) || xmax <= 0) xmax <- 1

      xbreak <- if (nrow(cfg_x)) cfg_x$Interval[1] else NA_real_
      if (!is.finite(xbreak) || xbreak <= 0) xbreak <- xmax/5

      ymax <- if (nrow(cfg_y)) cfg_y$Y_Max[1] else NA_real_

      if (!is.finite(ymax)) {
        ymax <- suppressWarnings(max(df[[raw_y]], na.rm = TRUE))
      }
      if (!is.finite(ymax) || ymax <= 0) ymax <- 1

      ybreak <- if (nrow(cfg_y)) cfg_y$Interval[1] else NA_real_
      if (!is.finite(ybreak) || ybreak <= 0) ybreak <- ymax/5

      updateNumericInput(session, "xmax_corr", value = xmax, max = Inf)
      updateNumericInput(session, "xbreak_corr", value = xbreak, min = 0.0001)
      updateNumericInput(session, "xmin_corr", value = 0)
      updateNumericInput(session, "ymin_corr", value = 0)
      updateNumericInput(session, "ymax_corr", value = ymax, max = Inf)
      updateNumericInput(session, "ybreak_corr", value = ybreak, min = 0.0001)
    },
    ignoreInit = FALSE
  )
  
  
  
  ## ── reajustar Y‑axis cuando se (des)activa la normalización ─────────  
  observeEvent(
    list(input$doNorm, input$ctrlMedium, input$param),
    {
      req(plot_settings(), input$param)

      tgt <- if (isTRUE(input$doNorm) && !is.null(input$ctrlMedium))
        paste0(input$param, "_Norm") else input$param

      lims <- get_ylim(tgt)

      updateNumericInput(session, "ymax",
                         label  = paste0("Y max (", tgt, "):"),
                         value  = lims$ymax)
      updateNumericInput(session, "ybreak",
                         label  = paste0("Int Y (", tgt, "):"),
                         value  = lims$ybreak)
    },
    ignoreInit = TRUE
  )
  
  
  # ── Sincronizar título editable con el título por defecto ─────────────────  
  observeEvent(
    list(input$scope, input$tipo, input$param, input$strain,
         input$corr_param_x, input$corr_param_y),
    {
      req(input$tipo)
      defaultTitle <- switch(
        input$tipo,
        "Correlación" = paste("Correlación",
                              input$corr_param_y, "vs",
                              input$corr_param_x),
        if (input$scope == "Combinado")
          paste(input$tipo, "combinado de", input$param)
        else
          paste(input$tipo, "de", input$param, "para", input$strain)
      )
      updateTextInput(session, "plotTitle", value = defaultTitle)
    }, ignoreInit = FALSE)
  
  
  
  # ---- Helpers para filtrar réplicas -------------------------------  
  filter_reps_strain <- function(df){  
    drop_all <- input$rm_reps_all
    if (!is.null(drop_all)) {
      drop_all <- as.character(drop_all)
      df <- df[ !(as.character(df$BiologicalReplicate) %in% drop_all), ]  
    }
    
    for (m in unique(df$Media)){  
      sel <- input[[paste0("reps_", make.names(m))]]  
      if (!is.null(sel))  
        df <- df[ !(df$Media == m & !as.character(df$BiologicalReplicate) %in% as.character(sel)), ]  
    }  
    df  
  }  
  
  filter_reps_group <- function(df){  
    grps <- input$showGroups  
    if (is.null(grps)) return(df[0, ])  
    df <- df[ paste(df$Strain, df$Media, sep = "-") %in% grps, ]  
    drop_all <- input$rm_reps_all
    if (!is.null(drop_all)) {
      drop_all <- as.character(drop_all)
      df <- df[ !(as.character(df$BiologicalReplicate) %in% drop_all), ]  
    }
    for (g in grps){  
      sel <- input[[paste0("reps_grp_", make.names(g))]]  
      if (!is.null(sel))  
        df <- df[ !(paste(df$Strain, df$Media, sep = "-") == g &  
                      !as.character(df$BiologicalReplicate) %in% as.character(sel)), ]  
    }  
    df  
  }  
  
  # ---- Reactivos base (sin cache forzado) para filtrar una sola vez por alcance ----
  base_plot_df <- reactive({
    if (isTRUE(input$doNorm)) datos_agrupados_norm() else datos_agrupados()
  })

  scoped_plot_df <- reactive({
    df <- base_plot_df()
    if (input$scope == "Por Cepa") {
      req(input$strain)
      df <- df %>%
        filter(Strain == input$strain) %>%
        order_filter_strain() %>%
        filter_reps_strain()
    } else {
      df <- df %>%
        order_filter_group()
    }
    df
  })

  get_scope_df <- function(scope, strain = NULL) {
    same_scope  <- scope == input$scope
    same_strain <- scope != "Por Cepa" || isTRUE(identical(strain, input$strain))

    if (same_scope && same_strain) {
      return(scoped_plot_df())
    }

    df <- base_plot_df()
    if (scope == "Por Cepa") {
      req(strain)
      df <- df %>%
        filter(Strain == strain) %>%
        order_filter_strain() %>%
        filter_reps_strain()
    } else {
      df <- df %>%
        order_filter_group()
    }
    df
  }
  
  curve_long_df <- reactive({
    req(curve_data(), datos_combinados())
    curve_data() %>%
      mutate(across(-Time, ~ suppressWarnings(as.numeric(.x)))) %>%
      pivot_longer(cols = -Time, names_to = "Well", values_to = "Value") %>%
      left_join(datos_combinados(), by = "Well")
  })
  
  # --- helper: orden seguro de grupos (vacio si el input esta vacio) ----
  safe_orderGroups <- function() {  
    if (!is.null(input$orderGroups) && nzchar(input$orderGroups)) {  
      trimws(unlist(strsplit(input$orderGroups, ",")))  
    } else {  
      NULL  
    }  
  }  
  
  
  
  # ──────────────────────────────────────────────────────────────────────────────
  # Helpers de filtrado + orden basados en la columna ‘Orden’ del platemap
  # ──────────────────────────────────────────────────────────────────────────────
  
  order_filter_strain <- function(df) {
    # 1) Aplica el filtro showMedios
    if (!is.null(input$showMedios)) {
      df <- df %>% filter(Media %in% input$showMedios)
    }
    # 2) Niveles originales según Orden
    final_levels <- df %>%
      distinct(Media, Orden) %>%
      arrange(Orden) %>%
      pull(Media)
    # 3) Si el usuario escribió un CSV en orderMedios, lo prioriza
    if (!is.null(input$orderMedios) && nzchar(input$orderMedios)) {
      user_order   <- trimws(strsplit(input$orderMedios, ",")[[1]])
      final_levels <- intersect(user_order, final_levels)
    }
    # 4) Devuelve Media como factor con niveles en el orden correcto
    df %>% mutate(Media = factor(Media, levels = final_levels))
  }
  
  order_filter_group <- function(df) {
    # 1) Aplica el filtro de réplicas por grupo
    df2 <- filter_reps_group(df)
    # 2) Grupos realmente visibles (después de filtros)
    available <- unique(paste(df2$Strain, df2$Media, sep = "-"))
    # 3) Etiquetas originales según Orden solo para los visibles
    platemap_levels <- datos_agrupados() %>%
      distinct(Strain, Media, Orden) %>%
      mutate(Label = paste(Strain, Media, sep = "-")) %>%
      arrange(Orden) %>%
      pull(Label) %>%
      intersect(available)
    # 4) Si el usuario escribió un CSV en orderGroups, lo prioriza
    user_order <- NULL
    if (!is.null(input$orderGroups) && nzchar(input$orderGroups)) {
      user_order <- intersect(trimws(strsplit(input$orderGroups, ",")[[1]]), available)
    }
    # 5) Orden final: primero el pedido explícito, luego el resto en orden de platemap
    final_levels <- if (!is.null(user_order) && length(user_order) > 0) {
      c(user_order, setdiff(platemap_levels, user_order))
    } else {
      platemap_levels
    }
    # 6) Devuelve Label como factor con niveles en el orden correcto (solo los visibles)
    df2 %>% mutate(Label = factor(paste(Strain, Media, sep = "-"), levels = final_levels))
  }
  
  
  
  
  # ── Post-hoc dinámico ────────────────────────────────────────────────────  
  output$postHocUI <- renderUI({  
    req(input$sigTest)  
    if (input$sigTest == "ANOVA") {  
      selectInput("postHoc", "Post-hoc:",  
                  choices = c(  
                    "Tukey"         = "Tukey",  
                    "Bonferroni"    = "Bonferroni",  
                    "Sidak"         = "Sidak",  
                    "Dunnett"       = "Dunnett",  
                    "Scheffé"       = "Scheffe",  
                    "Games–Howell"  = "GamesHowell"  
                  ),  
                  selected = "Tukey"  
      )  
    } else if (input$sigTest == "Kruskal–Wallis") {  
      selectInput("postHoc", "Post-hoc:",  
                  choices = c(  
                    "Dunn (Bonf.)" = "Dunn",  
                    "Conover"      = "Conover",  
                    "Nemenyi"      = "Nemenyi",  
                    "DSCF"         = "DSCF"  
                  ),  
                  selected = "Dunn"  
      )  
    } else {  
      NULL  
    }  
  })  
  
  
  
  # ── helpers de filtrado + orden ─────────────────────────────────────────  
  # (deja los tuyos: order_filter_strain() / order_filter_group())  
  
  # ── Data frame unificado para Significancia ───────────────  
  make_test_df <- function() {  
    p   <- safe_param()                                  # puede traer *_Norm  
    src <- if (isTRUE(input$doNorm))                     # ← NUEVO  
      datos_agrupados_norm() else datos_agrupados()  
    
    if (input$scope == "Por Cepa") {  
      src %>%  
        filter(Strain == input$strain) %>%  
        order_filter_strain() %>%  
        filter_reps_strain() %>%  
        transmute(Label = Media,  
                  Valor = .data[[p]])  
    } else {  
      src %>%  
        order_filter_group() %>%  
        transmute(Label,  
                  Valor = .data[[p]])  
    }  
  }  
  
  
  
  # ── Reusar el mismo para Normalidad ────────────────────────  
  make_norm_df <- make_test_df  
  
  observe({  
    # aseguramos que ya hay configuración y datos  
    req(plot_settings(), nrow(datos_agrupados()) > 0)  
    
    df_test <- make_test_df()  
    req(nrow(df_test) > 0)  
    
    grupos <- unique(df_test$Label)  
    updateSelectInput(session, "controlGroup",  
                      choices  = grupos,  
                      selected = if ("Control" %in% grupos) "Control" else grupos[1])  
    updateSelectInput(session, "group1",  
                      choices  = grupos,  
                      selected = grupos[1])  
    updateSelectInput(session, "group2",  
                      choices  = grupos,  
                      selected = if (length(grupos) >= 2) grupos[2] else grupos[1])  
  })  
  
  
  # ── Normalidad ───────────────────────────────────────────────────────────  
  norm_res <- eventReactive(input$runNorm, {  
    df <- make_norm_df()  
    # Deben existir al menos 2 grupos distintos  
    if (nrow(df)==0 || dplyr::n_distinct(df$Label)<2) {  
      showNotification("Se necesitan ≥2 grupos con datos para normalidad", type="error", duration=4)  
      return(tibble::tibble(Label=character(), shapiro.stat=numeric(), shapiro.p=numeric(),  
                            ks.stat=numeric(), ks.p=numeric(),  
                            ad.stat=numeric(), ad.p=numeric()))  
    }  
    # Shapiro–Wilk  
    sw <- df %>% group_by(Label) %>%  
      summarise(  
        shapiro.stat = stats::shapiro.test(Valor)$statistic,  
        shapiro.p    = stats::shapiro.test(Valor)$p.value,  
        .groups="drop"  
      )  
    res <- sw  
    # Kolmogorov–Smirnov (opcional)  
    if ("ks" %in% input$normTests) {  
      ksdf <- df %>% group_by(Label) %>%  
        summarise(  
          ks.stat = stats::ks.test(Valor, "pnorm",  
                                   mean(Valor), sd(Valor))$statistic,  
          ks.p    = stats::ks.test(Valor, "pnorm",  
                                   mean(Valor), sd(Valor))$p.value,  
          .groups="drop"  
        )  
      res <- left_join(res, ksdf, by="Label")  
    } else {  
      res <- mutate(res, ks.stat=NA_real_, ks.p=NA_real_)  
    }  
    # Anderson–Darling (opcional, con nortest) — sólo si n ≥ 8  
    if ("ad" %in% input$normTests) {  
      addf <- df %>%  
        dplyr::group_by(Label) %>%  
        dplyr::summarise(  
          ad.stat = if (dplyr::n() >= 8) nortest::ad.test(Valor)$statistic else NA_real_,  
          ad.p    = if (dplyr::n() >= 8) nortest::ad.test(Valor)$p.value     else NA_real_,  
          .groups = "drop"  
        )  
      res <- dplyr::left_join(res, addf, by = "Label")  
    } else {  
      res <- dplyr::mutate(res, ad.stat = NA_real_, ad.p = NA_real_)  
    }  
    res  
  })  
  
  
  # ── Significancia ─────────────────────────────────────────────────────────  
  sig_res <- eventReactive(input$runSig, {
    df_raw <- make_test_df()
    df <- filter_min_obs(df_raw)
    dropped <- setdiff(unique(df_raw$Label), unique(df$Label))
    if (length(dropped) > 0)
      showNotification(paste("Grupos con menos de dos observaciones:",
                              paste(dropped, collapse = ", ")), type = "warning", duration = 5)

    if (n_distinct(df$Label) < 2) {
      showNotification("No hay grupos suficientes para realizar el test", type = "error", duration = 5)
      return(tibble())
    }

    tryCatch({
      
      do_anova <- function() {  
        aovm <- aov(Valor ~ Label, data = df)  
        switch(input$postHoc,  
               "Tukey"      = broom::tidy(TukeyHSD(aovm)) |> rename(p.adj = adj.p.value),  
               "Bonferroni" = rstatix::pairwise_t_test(df, Valor ~ Label,  
                                                       p.adjust.method = "bonferroni"),  
               "Sidak"      = safe_pairwise_t(df, "sidak"),  
               "Dunnett"    = dunnett_to_tibble(  
                 DescTools::DunnettTest(Valor ~ Label,  
                                        data = set_control(df, input$controlGroup))),  
               "Scheffe"    = pmcmr_to_tibble(PMCMRplus::scheffeTest(aovm, "Label")),  
               "GamesHowell"= rstatix::games_howell_test(df, Valor ~ Label)  
        )  
      }  
      
      do_kw <- function() {  
        switch(input$postHoc,  
               "Dunn"    = rstatix::dunn_test(df, Valor ~ Label, p.adjust.method = "bonferroni"),  
               "Conover" = pmcmr_to_tibble(PMCMRplus::kwAllPairsConoverTest(df$Valor, df$Label)),  
               "Nemenyi" = pmcmr_to_tibble(PMCMRplus::kwAllPairsNemenyiTest(df$Valor, df$Label)),  
               "DSCF"    = {  
                 f <- if (exists("kwAllPairsDSCFTest", asNamespace("PMCMRplus"), FALSE))  
                   PMCMRplus::kwAllPairsDSCFTest  
                 else  
                   PMCMRplus::kwAllPairsDscfTest  
                 pmcmr_to_tibble(f(df$Valor, df$Label))  
               }  
        )  
      }  
      
      switch(input$sigTest,  
             "ANOVA"          = do_anova(),  
             "Kruskal–Wallis" = do_kw(),  
             "ttest" = {
               if (input$compMode == "all") {
                 safe_pairwise_t(df, "holm")
               } else if (input$compMode == "control") {
                 if (!input$controlGroup %in% df$Label) {
                   showNotification(paste("El grupo control", input$controlGroup,
                                          "no tiene suficientes observaciones"),
                                    type = "error", duration = 5)
                   tibble()
                 } else {
                   rstatix::t_test(df, Valor ~ Label, ref.group = input$controlGroup)
                 }
               } else {
                 grupos <- c(input$group1, input$group2)
                 sub <- df %>% filter(Label %in% grupos) %>% droplevels()
                 counts <- table(sub$Label)
                 faltantes <- c(setdiff(grupos, names(counts)),
                                names(counts[counts < 2]))
                 if (length(faltantes) > 0) {
                   showNotification(paste("Grupos con observaciones insuficientes:",
                                          paste(faltantes, collapse = ", ")),
                                    type = "error", duration = 5)
                   tibble()
                 } else {
                   rstatix::t_test(sub, Valor ~ Label, paired = can_paired(sub))
                 }
               }
            },
            "wilcox" = {
              if (input$compMode == "all") {
                safe_pairwise_wilcox(df, "holm")
              } else if (input$compMode == "control") {
                if (!input$controlGroup %in% df$Label) {
                  showNotification(paste("El grupo control", input$controlGroup,
                                         "no tiene suficientes observaciones"),
                                   type = "error", duration = 5)
                  tibble()
                } else {
                  rstatix::wilcox_test(df, Valor ~ Label, ref.group = input$controlGroup)
                }
              } else {
                grupos <- c(input$group1, input$group2)
                sub <- df %>% filter(Label %in% grupos) %>% droplevels()
                faltantes <- setdiff(grupos, names(table(sub$Label)))
                if (length(faltantes) > 0) {
                  showNotification(paste("Grupos con observaciones insuficientes:",
                                         paste(faltantes, collapse = ", ")),
                                   type = "error", duration = 5)
                  tibble()
                } else {
                  rstatix::wilcox_test(sub, Valor ~ Label, paired = can_paired(sub))
                }
              }
            }
      )
      
    }, error = function(e) {  
      showNotification(paste("Error en test:", e$message),  
                       type = "error", duration = 5)  
      tibble()  
    })  
  })  
  
  
  # ─── Hacer que al pulsar abra el panel ────────────────────────────────  
  observeEvent(input$runNorm, {  
    updateCollapse(session, "statsPanel", open = "Analisis Estadísticos")  
  })  
  observeEvent(input$runSig, {  
    updateCollapse(session, "statsPanel", open = "Analisis Estadísticos")  
  })  
  
  # ─── Renderizar tabla de normalidad ─────────────────────────────────  
  output$normTable <- renderDT({  
    req(input$runNorm)            # solo después de pulsar  
    df <- norm_res()              # eventReactive definido arriba  
    
    # ── Indicadores “Normal / No” para cada test ──────────────────────  
    df2 <- df %>%  
      mutate(  
        Shapiro = if_else(!is.na(shapiro.p) & shapiro.p  > 0.05, "Sí", "No"),  
        KS      = if_else(!is.na(ks.p)      & ks.p       > 0.05, "Sí", "No"),  
        AD      = if_else(!is.na(ad.p)      & ad.p       > 0.05, "Sí", "No")  
      )  
    
    # Opcional: ordena columnas a gusto  
    df2 <- df2 %>%   
      dplyr::select(Label,  
                    shapiro.stat, shapiro.p, Shapiro,  
                    ks.stat,      ks.p,      KS,  
                    ad.stat,      ad.p,      AD)  
    validate(need(nrow(df2) > 0, "No hay datos para normalidad."))  
    datatable(df2, options = list(pageLength = 10, scrollX = TRUE))  
  }, server = FALSE)  
  
  
  # ─── Renderizar tabla de significancia ──────────────────────────────  
  output$sigTable <- renderDT({
    req(input$runSig)
    df <- sig_res()
    validate(need(ncol(df) > 0, "No hay resultados de significancia."))
    # ── Unificar nombres de columna de comparación ────────────  
    # 1) broom::tidy(TukeyHSD) sale con 'comparison'  
    if ("comparison" %in% names(df)) {  
      cmp <- split_comparison(df$comparison)  
      df$group1 <- cmp[, 1]  
      df$group2 <- cmp[, 2]  
    }  
    # 2) PMCMRplus (pmcmr_to_tibble) devuelve 'grupo1','grupo2'  
    if (all(c("grupo1","grupo2") %in% names(df))) {  
      df$group1 <- df$grupo1  
      df$group2 <- df$grupo2  
    }  
    
    
    # --- detectar la columna de p-value de forma robusta -----------------
    p_candidates <- intersect(
      c("p", "p.value", "p.adj", "adj.p.value", "p_val", "p.value.adj"),
      names(df)
    )
    validate(need(length(p_candidates) > 0,
                  paste("No se encontró ninguna columna de p-value válida. Columnas:",
                        paste(names(df), collapse = ", "))))
    pcol <- p_candidates[1]   # primera coincidencia
    
    
    df2 <- df %>%  
      mutate(  
        P_valor       = .data[[pcol]],  
        Significativo = if_else(P_valor < 0.05, "Sí", "No"),  
        Estrellas     = case_when(  
          P_valor < 0.001 ~ "***",  
          P_valor < 0.01  ~ "**",  
          P_valor < 0.05  ~ "*",  
          TRUE            ~ ""  
        )  
      )  
    
    validate(need(nrow(df2)>0, "No hay comparaciones válidas."))  
    datatable(df2, options = list(pageLength=10, scrollX=TRUE))  
  }, server = FALSE)  
  
  # Tabla de valores (debajo del grafico) segun tipo  
  output$statsTable <- renderDT({  
    tipo <- input$tipo %||% ""  
    base_df <- if (isTRUE(input$doNorm)) datos_agrupados_norm() else datos_agrupados()  
    
    # Helper: aplica filtros de scope y réplicas  
    filter_scope <- function(df){  
      if (input$scope == "Por Cepa") {  
        df %>% filter(Strain == input$strain) %>% order_filter_strain() %>% filter_reps_strain()  
      } else {  
        df %>% order_filter_group() %>% filter_reps_group()  
      }  
    }  
    
    if (tipo %in% c("Barras", "Boxplot", "Violin")) {  
      param <- safe_param()  
      validate(need(param %in% names(base_df), "Parametro no disponible en los datos."))  
      df <- filter_scope(base_df) %>%  
        filter(!is.na(Strain), !is.na(Media), !is.na(BiologicalReplicate)) %>%  
        mutate(Valor = .data[[param]])  
      drop_all <- as.character(input$rm_reps_all %||% character(0))  
      if (length(drop_all)) df <- df %>% filter(!as.character(BiologicalReplicate) %in% drop_all)  
      validate(need(nrow(df) > 0, "No hay datos para mostrar con la seleccion actual."))  
      
      strain_order <- df %>%  
        distinct(Strain, Orden) %>%  
        group_by(Strain) %>%  
        summarise(minO = min(Orden, na.rm = TRUE), .groups = "drop") %>%  
        arrange(minO) %>%  
        pull(Strain)  
      
      build_block <- function(sub_df) {  
        media_order <- sub_df %>%  
          distinct(Media, Orden) %>%  
          arrange(Orden) %>%  
          pull(Media)  
        
        base_tbl <- sub_df %>%  
          group_by(BiologicalReplicate, Media, Orden) %>%  
          summarise(Valor = mean(Valor, na.rm = TRUE), .groups = "drop") %>%  
          tidyr::pivot_wider(  
            id_cols    = BiologicalReplicate,  
            names_from = Media,  
            values_from = Valor,  
            values_fill = NA,  
            values_fn  = list(Valor = ~ mean(.x, na.rm = TRUE))  
          ) %>%  
          arrange(BiologicalReplicate) %>%  
          dplyr::rename(RepBiol = BiologicalReplicate) %>%  
          dplyr::mutate(RepBiol = as.character(RepBiol)) %>%  
          dplyr::select(RepBiol, dplyr::any_of(media_order))  
        
        avg_tbl <- base_tbl %>%  
          dplyr::summarise(dplyr::across(-RepBiol, ~ mean(.x, na.rm = TRUE))) %>%  
          dplyr::mutate(RepBiol = "Promedio") %>%  
          dplyr::select(RepBiol, dplyr::any_of(media_order))  
        
        bind_rows(base_tbl, avg_tbl) %>%  
          mutate(Strain = unique(sub_df$Strain)[1], .before = 1)  
      }  
      
      grupos <- df %>% group_split(Strain, .keep = TRUE)  
      tabla <- grupos %>%  
        lapply(build_block) %>%  
        dplyr::bind_rows() %>%  
        mutate(  
          Strain  = factor(Strain, levels = strain_order),  
          RepBiol = factor(RepBiol, levels = c(sort(as.character(unique(df$BiologicalReplicate))), "Promedio"))  
        ) %>%  
        arrange(Strain, RepBiol)  
      
      return(datatable(tabla, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE))  
    }  
    
    if (tipo == "Apiladas") {  
      params <- input$stackParams %||% plot_settings()$Parameter  
      validate(need(length(params) > 0, "No hay parámetros para mostrar."))  
      df <- filter_scope(base_df)  
      if (!"Label" %in% names(df)) {  
        if (input$scope == "Por Cepa") {  
          df <- df %>% mutate(Label = Media)  
        } else {  
          df <- df %>% mutate(Label = paste(Strain, Media, sep = "-"))  
        }  
      }  
      df <- df %>% mutate(BiologicalReplicate = as.character(BiologicalReplicate))  
      validate(need(nrow(df) > 0, "No hay datos para mostrar con la seleccion actual."))  
      df_long <- df %>%  
        tidyr::pivot_longer(cols = dplyr::all_of(params), names_to = "Parametro", values_to = "Valor") %>%  
        filter(!is.na(Valor))  
      resumen <- df_long %>%  
        group_by(Parametro, Label) %>%  
        summarise(Promedio = mean(Valor, na.rm = TRUE), .groups = "drop") %>%  
        mutate(BiologicalReplicate = "Promedio")  
      tabla <- df_long %>%  
        group_by(Parametro, Label, BiologicalReplicate) %>%  
        summarise(Valor = mean(Valor, na.rm = TRUE), .groups = "drop") %>%  
        bind_rows(resumen) %>%  
        arrange(Parametro, Label, BiologicalReplicate)  
      return(datatable(tabla, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE))  
    }  
    
    if (tipo == "Correlación") {  
      raw_x <- input$corr_param_x %||% NULL  
      raw_y <- input$corr_param_y %||% NULL  
      params <- c(raw_x, raw_y) %>% stats::na.omit()  
      validate(need(length(params) == 2, "Selecciona dos parámetros."))  
      df <- filter_scope(base_df)  
      if (!"Label" %in% names(df)) {  
        if (input$scope == "Por Cepa") {  
          df <- df %>% mutate(Label = Media)  
        } else {  
          df <- df %>% mutate(Label = paste(Strain, Media, sep = "-"))  
        }  
      }  
      df <- df %>% mutate(BiologicalReplicate = as.character(BiologicalReplicate))  
      validate(need(nrow(df) > 0, "No hay datos para mostrar con la seleccion actual."))  
      df_long <- df %>%  
        tidyr::pivot_longer(cols = dplyr::all_of(params), names_to = "Parametro", values_to = "Valor") %>%  
        filter(!is.na(Valor))  
      resumen <- df_long %>%  
        group_by(Parametro, Label) %>%  
        summarise(Promedio = mean(Valor, na.rm = TRUE), .groups = "drop") %>%  
        mutate(BiologicalReplicate = "Promedio")  
      tabla <- df_long %>%  
        group_by(Parametro, Label, BiologicalReplicate) %>%  
        summarise(Valor = mean(Valor, na.rm = TRUE), .groups = "drop") %>%  
        bind_rows(resumen) %>%  
        arrange(Parametro, Label, BiologicalReplicate)  
      return(datatable(tabla, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE))  
    }  
    
    datatable(tibble::tibble(Mensaje = "Tabla no disponible para este tipo de gráfico"), options = list(dom = 't'))  
  })  
  # ──────────────────────────────────────────────────────────────────────────────  
  
  # ── Helper para elegir paleta según input$colorMode ────────────────  
  get_palette <- function(n) {  
    tone_down <- function(cols, amount = 0.35) {  
      amt <- pmin(pmax(amount, 0), 1)  
      m   <- grDevices::col2rgb(cols)  
      m2  <- m + (255 - m) * amt  
      grDevices::rgb(m2[1, ]/255, m2[2, ]/255, m2[3, ]/255)  
    }  
    okabe <- function(n) rep(c(  
      "#E69F00", "#56B4E9", "#009E73", "#F0E442",  
      "#0072B2", "#D55E00", "#CC79A7", "#999999"  
    ), length.out = n)  
    tableau <- function(n) rep(c(  
      "#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F",  
      "#EDC949", "#AF7AA1", "#FF9DA7", "#9C755F", "#BAB0AC"  
    ), length.out = n)  
    brew <- function(n, name){  
      info <- RColorBrewer::brewer.pal.info[name, "maxcolors"]  
      pal  <- RColorBrewer::brewer.pal(info, name)  
      rep(pal, length.out = n)  
    }  
    brew <- function(n, name){  
      info <- RColorBrewer::brewer.pal.info[name, "maxcolors"]  
      pal  <- RColorBrewer::brewer.pal(info, name)  
      rep(pal, length.out = n)  
    }  
  
    switch(input$colorMode,  
           "Default"              = safe_hue(n),  
           "Default Suave"        = tone_down(safe_hue(n)),  
           "Blanco y Negro"       = rep("black", n),  
           "Blanco y Negro Suave" = rep("#666666", n),  
           "Viridis"              = viridis::viridis(n),  
           "Viridis Suave"        = tone_down(viridis::viridis(n)),  
           "Plasma"               = viridis::plasma(n),  
           "Plasma Suave"         = tone_down(viridis::plasma(n)),  
           "Magma"                = viridis::magma(n),  
           "Magma Suave"          = tone_down(viridis::magma(n)),  
           "Cividis"              = viridis::cividis(n),  
           "Cividis Suave"        = tone_down(viridis::cividis(n)),  
           "Set1"                 = brew(n, "Set1"),  
           "Set1 Suave"           = tone_down(brew(n, "Set1")),  
           "Set2"                 = brew(n, "Set2"),  
           "Set2 Suave"           = tone_down(brew(n, "Set2")),  
           "Set3"                 = brew(n, "Set3"),  
           "Set3 Suave"           = tone_down(brew(n, "Set3")),  
           "Dark2"                = brew(n, "Dark2"),  
           "Dark2 Suave"          = tone_down(brew(n, "Dark2")),  
           "Accent"               = brew(n, "Accent"),  
           "Accent Suave"         = tone_down(brew(n, "Accent")),  
           "Paired"               = brew(n, "Paired"),  
           "Paired Suave"         = tone_down(brew(n, "Paired")),  
           "Pastel1"              = brew(n, "Pastel1"),  
           "Pastel1 Suave"        = tone_down(brew(n, "Pastel1"), amount = 0.2),  
           "Pastel2"              = brew(n, "Pastel2"),  
           "Pastel2 Suave"        = tone_down(brew(n, "Pastel2"), amount = 0.2),  
           "OkabeIto"             = okabe(n),  
           "OkabeIto Suave"       = tone_down(okabe(n)),  
           "Tableau"              = tableau(n),  
           "Tableau Suave"        = tone_down(tableau(n)),  
           safe_hue(n)        # fallback  
    )  
  }  
  
  # Paleta por cepa para Combinado (repite los mismos colores que en Por Cepa)
  palette_by_strain <- function(df_lab){  
    if (!all(c("Label","Strain","Media") %in% names(df_lab))) return(NULL)  
    df_lab <- df_lab %>% distinct(Label, Strain, Media)  
    if (!nrow(df_lab)) return(NULL)  
    col_map <- character()  
    for (st in unique(df_lab$Strain)) {  
      sub <- df_lab[df_lab$Strain == st, , drop = FALSE]  
      medias <- unique(as.character(sub$Media))  
      pal <- get_palette(length(medias))  
      names(pal) <- medias  
      cols <- pal[as.character(sub$Media)]  
      names(cols) <- as.character(sub$Label)  
      col_map <- c(col_map, cols)  
    }  
    col_map  
  }  
  
  palette_for_labels <- function(df_lab, levels_vec){  
    levels_vec <- as.character(levels_vec)  
    if (isTRUE(input$repeat_colors_combined)) {  
      col_map <- palette_by_strain(df_lab)  
      if (!is.null(col_map) && length(col_map)) {  
        pal <- col_map[match(levels_vec, names(col_map))]  
        names(pal) <- levels_vec  
        return(pal)  
      }  
    }  
    pal <- get_palette(length(levels_vec))  
    names(pal) <- levels_vec  
    pal  
  }  
  # ───────────────────────────────────────────────────────────────────  
  
  ###############################################################################  
  # PEGAR AQUÍ  ➜  antes de build_plot() y después de las demás helpers  
  ###############################################################################  
  # Barra de significancia estilo “T” (base‑ggplot2)  
  ###############################################################################
  is_star_label <- function(label) {
    if (is.null(label)) return(FALSE)
    lab <- as.character(label)
    if (!length(lab) || !nzchar(lab)) return(FALSE)
    grepl("^\\*+$", lab)
  }

  sig_tpad <- function(label, tpad) {
    if (!is.numeric(tpad) || !is.finite(tpad)) return(tpad)
    if (is_star_label(label)) tpad * 0.25 else tpad
  }

  # 1-A  Dibuja UNA barra de significancia tipo “T”
  add_sigline <- function(p, group1, group2, label = "*",
                          height   = .05,  # separación barra‑datos  (proporción del rango Y)
                          vsize    = .02,  # largo de los “postes”
                          tpad     = .01,  # distancia texto-barra
                          linewidth = .8,
                          textsize  = 5,
                          show_caps = TRUE,
                          panel_info = NULL){
    info <- panel_info
    if (is.null(info)) {
      build  <- ggplot_build(p)
      if (!length(build$data)) return(p)
      panel  <- build$layout$panel_params[[1]]
      xbreaks <- panel$x$breaks
      if (is.null(xbreaks) || anyNA(xbreaks)) {
        xbreaks <- unique(unlist(lapply(build$data, function(d) d$x)))
      }
      if (is.null(xbreaks) || !length(xbreaks)) return(p)
      dat   <- build$data[[1]]
      ytop  <- if ("ymax" %in% names(dat)) max(dat$ymax, dat$y, na.rm = TRUE)
      else                               max(dat$y,    na.rm = TRUE)
      yrng  <- diff(range(panel$y.range))
      if (!is.finite(yrng) || yrng <= 0) yrng <- 1
      info <- list(
        xbreaks = xbreaks,
        ytop    = if (is.finite(ytop)) ytop else max(panel$y.range, na.rm = TRUE),
        yrng    = yrng
      )
    }
    
    xbreaks <- info$xbreaks
    if (is.null(xbreaks) || !length(xbreaks)) return(p)
    get_x   <- function(g) if (is.numeric(g)) g else match(g, xbreaks)
    x1 <- get_x(group1);  x2 <- get_x(group2)
    if (any(is.na(c(x1, x2)))) return(p)
    
    yrng_use <- if (is.finite(info$yrng) && info$yrng > 0) info$yrng else 1
    tpad_use <- tpad
    ybar  <- info$ytop + height * yrng_use
    ycap  <- ybar - vsize * yrng_use
    ytxt  <- ybar + tpad_use * yrng_use          # texto un poco más arriba

    bar_df  <- data.frame(x = x1, xend = x2, y = ybar, yend = ybar, .sig_layer = TRUE)
    cap1_df <- data.frame(x = x1, xend = x1, y = ybar, yend = ycap, .sig_layer = TRUE)
    cap2_df <- data.frame(x = x2, xend = x2, y = ybar, yend = ycap, .sig_layer = TRUE)
    txt_df  <- data.frame(x = mean(c(x1, x2)), y = ytxt, label = label, .sig_layer = TRUE)

    p_out <- p +
      geom_segment(
        data = bar_df,
        inherit.aes = FALSE,
        aes(x = x, xend = xend, y = y, yend = yend),
        linewidth = linewidth
      )
    if (isTRUE(show_caps)) {
      p_out <- p_out +
        geom_segment(
          data = cap1_df,
          inherit.aes = FALSE,
          aes(x = x, xend = xend, y = y, yend = yend),
          linewidth = linewidth
        ) +
        geom_segment(
          data = cap2_df,
          inherit.aes = FALSE,
          aes(x = x, xend = xend, y = y, yend = yend),
          linewidth = linewidth
        )
    }
    p_out +
      geom_text(
        data = txt_df,
        inherit.aes = FALSE,
        aes(x = x, y = y, label = label),
        size = textsize,
        vjust = 0
      )
  }

  bump_title_margin <- function(el, extra_bottom = 0){
    cm <- el$margin %||% margin()
    vals <- grid::convertUnit(cm, "pt", valueOnly = TRUE)
    new_margin <- margin(
      t = vals[1],
      r = vals[2],
      b = vals[3] + extra_bottom,
      l = vals[4],
      unit = "pt"
    )
    el$margin <- new_margin
    el
  }
  
  ###############################################################################
  # 1-B  Coloca MUCHAS barras sin que se choquen entre sí
  ###############################################################################
  stack_siglines <- function(p, sigs,
                             sep         = .05,  # distancia entre niveles
                             base_height = NULL, # distancia desde los datos a la primera barra
                             linewidth   = .8,
                             vsize       = .02,
                             tpad        = .01,
                             tsize       = 5,
                             margin_base = NULL,
                             plot_height = NULL,
                             show_caps   = TRUE){
    
    if (length(sigs) == 0) return(p)
    
    build  <- ggplot_build(p)
    if (!length(build$data)) return(p)
    panel  <- build$layout$panel_params[[1]]
    y_range <- panel$y.range %||% panel$y$range
    if (is.null(y_range)) y_range <- c(0, 1)
    y_span <- diff(range(y_range))
    if (!is.finite(y_span) || y_span <= 0) y_span <- 1
    
    xranks <- panel$x$breaks   # posiciones 1,2,3,?
    if (is.null(xranks) || anyNA(xranks)) {
      xranks <- unique(unlist(lapply(build$data, function(d) d$x)))
    }
    if (is.null(xranks) || !length(xranks)) return(p)
    
    dat   <- build$data[[1]]
    ytop_raw <- if ('ymax' %in% names(dat)) max(dat$ymax, dat$y, na.rm = TRUE)
                else                        max(dat$y,    na.rm = TRUE)
    ytop <- if (is.finite(ytop_raw)) ytop_raw else max(y_range, na.rm = TRUE)
    
    info <- list(
      xbreaks = xranks,
      ytop    = ytop,
      yrng    = y_span,
      y_range = y_range
    )
    base_h <- base_height
    if (!is.numeric(base_h) || !is.finite(base_h)) base_h <- sep
    
    get_span <- function(cmp){
      x1 <- if (is.numeric(cmp$g1)) cmp$g1 else match(cmp$g1, xranks)
      x2 <- if (is.numeric(cmp$g2)) cmp$g2 else match(cmp$g2, xranks)
      if (any(is.na(c(x1, x2)))) return(NULL)
      c(min(x1,x2), max(x1,x2))
    }
    
    spans <- lapply(sigs, get_span)
    keep  <- vapply(spans, function(s) !is.null(s) && length(s) == 2, logical(1))
    spans <- spans[keep]
    sigs  <- sigs[keep]
    if (!length(spans)) return(p)
    
    levels <- list()                # ocupaci?n por nivel
    bar_level <- integer(length(sigs))
    
    for (i in seq_along(sigs)){
      span <- spans[[i]]
      placed <- FALSE
      for (lvl in seq_along(levels)){
        overlap <- vapply(levels[[lvl]], function(iv)
          !(span[2] < iv[1] || span[1] > iv[2]), logical(1))
        if (!any(overlap, na.rm = TRUE)){       # cabe en este nivel
          levels[[lvl]] <- append(levels[[lvl]], list(span))
          bar_level[i]  <- lvl
          placed <- TRUE; break
        }
      }
      if (!placed){                             # crea nivel nuevo
        levels[[length(levels)+1]] <- list(span)
        bar_level[i] <- length(levels)
      }
    }
    
    p_out <- p
    y_txt_vals <- numeric(length(sigs))
    sig_bars <- vector("list", length(sigs))
    
    for (i in seq_along(sigs)){
      h <- base_h + (bar_level[i] - 1) * sep
      cmp <- sigs[[i]]
      x1 <- if (is.numeric(cmp$g1)) cmp$g1 else match(cmp$g1, info$xbreaks)
      x2 <- if (is.numeric(cmp$g2)) cmp$g2 else match(cmp$g2, info$xbreaks)
      if (any(is.na(c(x1, x2)))) {
        y_txt_vals[i] <- NA_real_
        next
      }
      tpad_use <- sig_tpad(cmp$lab, tpad)
      ybar <- info$ytop + h * info$yrng
      ycap <- ybar - vsize * info$yrng
      ytxt <- ybar + tpad_use * info$yrng
      sig_bars[[i]] <- list(
        x1 = x1,
        x2 = x2,
        ybar = ybar,
        ycap = ycap,
        ytxt = ytxt,
        label = cmp$lab,
        linewidth = linewidth,
        textsize = tsize
      )
      p_out <- add_sigline(p_out,
                           group1   = cmp$g1,
                           group2   = cmp$g2,
                           label    = cmp$lab,
                           height   = h,
                           vsize     = vsize,
                           tpad      = tpad_use,
                           linewidth= linewidth,
                           textsize = tsize,
                           show_caps = show_caps,
                           panel_info = info)
      y_txt_vals[i] <- ytxt
    }
    
    overflow   <- max(c(0, y_txt_vals - max(info$y_range)), na.rm = TRUE)
    ph         <- plot_height %||% input$plot_h %||% 700
    if (!is.numeric(ph) || !is.finite(ph) || ph <= 0) ph <- 700
    overflow_pt <- (overflow / info$yrng) * ph
    # margen superior del gráfico (no del eje) para que se vean barras fuera del panel
    extra_top   <- if (overflow > 0) max(60, overflow_pt * 1.2 + tsize * 2) else 0
    title_extra <- if (overflow > 0 && nzchar(input$plotTitle %||% '')) {
      max(10, extra_top * 0.4 + tsize)
    } else 0
    
    if (!is.null(margin_base) || extra_top > 0) {
      margin_ref <- margin_base %||% p_out$theme$plot.margin %||% theme_get()$plot.margin
      if (!is.null(margin_ref)) {
        m_vals <- grid::convertUnit(margin_ref, 'pt', valueOnly = TRUE)
        margin_vals <- c(
          t = m_vals[1] + extra_top,
          r = m_vals[2],
          b = m_vals[3],
          l = m_vals[4]
        )
        new_margin <- margin(
          t = margin_vals[["t"]],
          r = margin_vals[["r"]],
          b = margin_vals[["b"]],
          l = margin_vals[["l"]],
          unit = 'pt'
        )
        p_out <- p_out + theme(plot.margin = new_margin)
        if (is.finite(extra_top) && extra_top > 0) {
          attr(p_out, "sig_plot_margin_pt") <- margin_vals
        }
      }
    }
    if (is.finite(extra_top) && extra_top > 0) {
      attr(p_out, "sig_extra_top_pt") <- extra_top
    }
    
    if (title_extra > 0) {
      title_el <- p_out$theme$plot.title %||% theme_get('plot.title')
      if (!is.null(title_el)) {
        p_out <- p_out + theme(plot.title = bump_title_margin(title_el, title_extra))
      }
    }
    
    sig_bars <- sig_bars[!vapply(sig_bars, is.null, logical(1))]
    if (length(sig_bars)) {
      attr(p_out, "sig_plotly") <- list(
        bars = sig_bars,
        y_range = info$y_range,
        xbreaks = info$xbreaks,
        show_caps = isTRUE(show_caps)
      )
    }

    # Clip desactivado para dibujar fuera del panel; no toca el eje
    p_out <- p_out + coord_cartesian(clip = 'off')
    p_out
  }
  
  
  # helpers_plotly.R  (o donde la tengas)
  # Guarda el plot en PNG o PDF
  export_plotly_image <- function(p, file,
                                  width, height,
                                  delay = 0.5,   # deja que Plotly acabe de renderizar
                                  zoom  = 3) {    # 3 × ⇒ 300 dpi aprox. si usas 100 px = 1 in
    # Fondo transparente
    p <- p %>% layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)"
    )
    
    tmp_html <- tempfile(fileext = ".html")
    on.exit(unlink(tmp_html), add = TRUE)
    
    htmlwidgets::saveWidget(p, tmp_html, selfcontained = TRUE)
    
    webshot2::webshot(
      url      = tmp_html,
      file     = file,
      vwidth   = width,
      vheight  = height,
      delay    = delay,
      zoom     = zoom          # ↑ resolución final  = zoom × vwidth
    )
  }
  
  
  
  ###############################################################################  
  # build_plotly_stack() – 100 % reactiva a todos los controles del panel  
  ###############################################################################  
  build_plotly_stack <- function(scope, strain = NULL, width = NULL, height = NULL) {
    
    num <- function(x) as.numeric(gsub(",", ".", x))  
    
    params_apilar <- input$stackParams  
    validate(need(length(params_apilar) > 0,  "Selecciona al menos 1 parametro en \"Parametros incluidos\""))  
    
    df_f <- get_scope_df(scope, strain)
    
    eje_x <- if (scope == "Por Cepa") {
      "Media"
    } else if (isTRUE(input$labelMode)) {
      "Strain"
    } else {
      "Label"
    }
    
    if (is.factor(df_f[[eje_x]])) {
      df_f[[eje_x]] <- droplevels(df_f[[eje_x]])
      eje_levels <- levels(df_f[[eje_x]])
    } else {
      eje_levels <- unique(df_f[[eje_x]])
    }
    
    order_stack_input <- trimws(strsplit(input$orderStack, ",")[[1]])
    order_levels      <- intersect(order_stack_input, params_apilar)
    stack_levels      <- if (length(order_levels)) order_levels else params_apilar
    
    df_long <- df_f |>
      pivot_longer(all_of(params_apilar),
                   names_to  = "Parametro",
                   values_to = "Valor") |>
      group_by(.data[[eje_x]], Parametro) |>
      summarise(
        Mean = mean(Valor, na.rm = TRUE),
        SD   = sd  (Valor, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(
        Parametro = factor(Parametro, levels = stack_levels),
        !!eje_x := factor(.data[[eje_x]], levels = eje_levels)
      ) |>
      arrange(.data[[eje_x]], Parametro)
    
    if (nrow(df_long) == 0) {
      return(plot_ly(width  = width %||% input$plot_w, height = height %||% input$plot_h))
    }
    
    if (isTRUE(input$x_wrap)) {
      eje_levels_wrapped <- wrap_label_html(eje_levels,
                                            lines = input$x_wrap_lines)
      df_long[[eje_x]]   <- wrap_label_html(df_long[[eje_x]],
                                            lines = input$x_wrap_lines)
      df_long[[eje_x]]   <- factor(df_long[[eje_x]], levels = eje_levels_wrapped)
      eje_levels         <- eje_levels_wrapped
    }
    # --- ángulo de las etiquetas del eje X -------------------------------
    x_ang <- get_x_angle(
      n           = length(eje_levels),
      angle_input = input$x_angle
    )
    b_mar <- get_bottom_margin(x_ang, input$x_wrap, input$x_wrap_lines)
    extra_bottom <- ceiling(input$fs_axis * 0.8)
    b_mar <- b_mar + extra_bottom
    
    
    ## 3 · Paleta --------------------------------------------------------------  
    pal <- get_palette(length(params_apilar))  
    names(pal) <- params_apilar  
    
    ## 4 · Trazas de barras -----------------------------------------------------  
    canvas_w <- width %||% input$plot_w
    canvas_h <- height %||% input$plot_h
    plt <- plot_ly(
      width  = canvas_w,
      height = canvas_h
    )

    for (p in stack_levels) {  
      sub <- df_long[df_long$Parametro == p, ]  
      if (!nrow(sub)) next
      plt <- add_trace(  
        plt,  
        x        = sub[[eje_x]],  
        y        = sub$Mean,  
        type     = "bar",  
        name     = p,  
        marker   = list(color = pal[[p]],  
                        line  = list(color = "black", width = 1))  
      )  
    }  
    
    ## 5 · Trazas “fantasma” con barras de error --------------------------------  
    if (isTRUE(input$showErrBars)) {
      
      err_df <- df_long %>%                       # tope de cada tramo
        group_by(.data[[eje_x]]) %>%
        arrange(factor(Parametro, levels = stack_levels), .by_group = TRUE) %>%
        mutate(y_top = cumsum(Mean)) %>%
        ungroup()
      
      thick <- num(input$errbar_size) * 1.6       # grosor cabeza
      
      for (p in stack_levels) {                   # una traza fantasma por tramo
        sub <- err_df[err_df$Parametro == p, ]
        plt <- add_trace(
          plt,
          x          = sub[[eje_x]],
          y          = sub$y_top,
          type       = "scatter",
          mode       = "markers",
          marker     = list(size = 1, opacity = 0),
          showlegend = FALSE,
          hoverinfo  = "skip",
          error_y = list(
            type       = "data",
            symmetric  = FALSE,
            array      = sub$SD,                  # +SD hacia arriba
            arrayminus = rep(0, nrow(sub)),       # nada hacia abajo
            color      = "black",
            thickness  = thick,
            width      = 20                       # longitud de la “cabeza”
          )
        )
      }
    }
    
    ## 6 · Layout (ejes y cuadrícula) ------------------------------------------  
    plt <- plt %>%
      layout(
        barmode = "stack",
        margin = list(
          t = input$fs_title * 2 + 20,
          b = b_mar
        ),
        title = list(
          text = input$plotTitle,
          font = list(size = input$fs_title, family = "Helvetica"),
          y    = 0.95                     # opcional: también puedes mover el título un poco hacia abajo
        ),
        yaxis = list(
          titlefont = list(size = input$fs_axis,
                           family = "Helvetica",
                           color  = "black"),
          tickfont  = list(size = input$fs_axis,
                           family = "Helvetica",
                           color  = "black"),
          range     = c(0, input$ymax),
          dtick     = input$ybreak,
          showline  = TRUE,
          linecolor = "black",
          linewidth = input$axis_line_size,
          ticks     = "outside",
          ticklen   = 5,
          tickcolor = "black",
          showgrid  = FALSE
        ),
        xaxis = list(
          title         = "",
          type          = "category",
          categoryorder = "array",
          categoryarray = eje_levels,
          titlefont     = list(size = input$fs_axis,
                               family = "Helvetica",
                               color  = "black"),
          tickfont      = list(size = input$fs_axis,
                               family = "Helvetica",
                               color  = "black"),
          tickangle     = -x_ang,
          showline      = TRUE,
          linecolor     = "black",
          linewidth     = input$axis_line_size,
          ticks         = "outside",
          ticklen       = 5,
          tickcolor     = "black",
          showgrid      = FALSE,
          automargin    = TRUE
        ),
        legend = list(
          title      = list(text = ""),
          traceorder = "normal",
          font       = list(size = input$fs_legend, family = "Helvetica")
        )
      )
    plt  
  }  
  ###############################################################################  
  
  
  
  
  # --------------------------------------------------------------------  
  # Función auxiliar para exportar PNG sin tocar <input> (para ZIP)  
  #         ► MISMO LOOK que plot_base()  
  # --------------------------------------------------------------------  
  # ── build_plot() ──  
  build_plot <- function(scope, strain = NULL, tipo) {  
    req(plot_settings(), input$param)  
    
    # ───── Nuevo bloque para normalización ─────  
    rawParam <- input$param  
    is_norm  <- isTRUE(input$doNorm)  
    # -------------------------------------------------------------------
    # -------------------------------------------------------------------
    
    param_sel <- if (is_norm) paste0(rawParam, "_Norm") else rawParam  
    
    # toma siempre la config del parámetro sin “_Norm”  
    # → calcula el título por defecto  
    ps       <- plot_settings() %>% filter(Parameter == rawParam)  
    defaultY <- if (is_norm)  
      paste0(ps$Y_Title, " (normalizado)")  
    else  
      ps$Y_Title  
    
    # → si el usuario puso algo en input$yLab, úsalo; sino el default  
    ylab     <- if (nzchar(input$yLab)) input$yLab else defaultY  
    
    
    # límites según param_sel  
    lims    <- get_ylim(param_sel)  
    ymax    <- lims$ymax  
    ybreak  <- lims$ybreak  
    # ────────────────────────────────────────────  
    
    ## ---- parche: si ymax o ybreak no son finitos, asigna valores seguros  
    if (!is.finite(ymax)  || ymax  <= 0) ymax  <- 1  
    if (!is.finite(ybreak) || ybreak <= 0) ybreak <- ymax / 5  
    
    # ——— 2) Estilos comunes ———  
    colourMode <- input$colorMode  
    fs_title   <- input$fs_title  
    fs_axis    <- input$fs_axis  
    fs_legend  <- input$fs_legend      # ← nuevo  
    axis_size  <- input$axis_line_size  
    scope_df <- get_scope_df(scope, strain)
    
    
    # 0) Si es Curvas, lo procesamos aquí y devolvemos  
    
    # ─── 3.x) NUEVO: Barras apiladas ───────────────────────────────  
    if (tipo == "Apiladas") {  
      params_apilar <- input$stackParams  
      if (length(params_apilar) == 0) {  
        return(  
          ggplot() + theme_void() +  
            annotate("text", 0, 0, label = "Selecciona ≥1 parámetro")  
        )  
      }  
      
      # 1) Filtrado y transformacion
      df_f   <- scope_df
      
      # 2) Revisa que existan las columnas  
      missing <- setdiff(params_apilar, names(df_f))  
      if (length(missing)) {  
        return(  
          ggplot() + theme_void() +  
            annotate("text", 0, 0, label =  
                       paste0("No se encontró el/los parámetro(s):\n",  
                              paste(missing, collapse = ", "))  
            )  
        )  
      }  
      
      # 3) Prepara df_long con medias y SD  
      # -----------------------------
      # Tomo el orden pedido por el usuario,
      # pero me quedo sólo con los parámetros que están seleccionados
      order_stack_input <- trimws(strsplit(input$orderStack, ",")[[1]])
      order_levels      <- intersect(order_stack_input, params_apilar)
      stack_levels      <- if (length(order_levels)) order_levels else params_apilar
      # -----------------------------
      
      # 1) Elige la variable de eje X según el scope  
      eje_x <- if (scope == "Por Cepa") {
        "Media"
      } else if (isTRUE(input$labelMode)) {
        # si piden sólo cepa, usamos directamente la columna Strain
        "Strain"
      } else {
        "Label"
      }
      
      
      # 2) Ahora construyes df_long  
      df_long <- df_f %>%
        pivot_longer(all_of(params_apilar),
                     names_to  = "Parametro",
                     values_to = "Valor") %>%
        group_by(.data[[eje_x]], Parametro) %>%
        summarise(
          Mean = mean(Valor, na.rm = TRUE),
          SD   = sd(Valor, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(Parametro = factor(Parametro, levels = stack_levels)) %>%
        arrange(.data[[eje_x]], Parametro)

      if (isTRUE(input$x_wrap)) {
        df_long[[eje_x]] <- wrap_label(df_long[[eje_x]],
                                      lines = input$x_wrap_lines)
      }
      
      # -----------------------------  
      # --- ángulo de las etiquetas del eje X -------------------------------
      x_ang <- get_x_angle(
        n           = length(unique(df_long[[eje_x]])),
        angle_input = input$x_angle
      )
      b_mar <- get_bottom_margin(x_ang, input$x_wrap, input$x_wrap_lines)
      extra_bottom <- ceiling(input$fs_axis * 0.8)
      b_mar <- b_mar + extra_bottom
      
      
      # 4) Gráfico base  
      pal <- get_palette(length(params_apilar))  
      names(pal) <- params_apilar  
      legend_breaks <- stack_levels
      tone_down_cols <- function(cols, amount = 0.25) {
        amt <- pmin(pmax(amount, 0), 1)
        m   <- grDevices::col2rgb(cols)
        m2  <- m + (255 - m) * amt
        grDevices::rgb(m2[1, ]/255, m2[2, ]/255, m2[3, ]/255)
      }
      p <- ggplot(df_long,
                  aes(x = .data[[eje_x]], y = Mean, fill = Parametro)) +
        geom_col(
          position = "stack",
          width    = 0.7,
          colour   = "black",
          linewidth = 0.6,
          alpha    = 0.85
        ) +
        scale_fill_manual(
          values  = tone_down_cols(pal[stack_levels], amount = 0.25),   # respeta el orden del usuario
          breaks  = legend_breaks,
          guide   = guide_legend(title = NULL, reverse = FALSE) # leyenda sigue el orden definido
        )
      
      
      # 5 ·  BARRAS DE DESVIACIÓN – versión auto-contenida -------------------
      if (isTRUE(input$showErrBars)) {
        
        ## ancho de la barra → 0.7  → mitad = 0.35
        cap_half_width <- 0.7 / 2   # ← NUEVO
        
        err_df <- df_long %>%                        # … (código idéntico)
          mutate(
            xnum   = as.numeric(factor(.data[[eje_x]], levels = unique(.data[[eje_x]]))),
            Parametro = factor(Parametro, levels = stack_levels)
          ) %>%
          arrange(xnum, Parametro) %>%
          group_by(xnum) %>%
          mutate(
            ybottom = cumsum(Mean) - Mean,
            ytop    = ybottom + Mean,
            ystart  = ytop,
            yend    = ytop + SD
          ) %>%
          ungroup()
        
        p <- p +
          geom_segment(
            data        = err_df,
            inherit.aes = FALSE,
            aes(x = xnum, xend = xnum, y = ystart, yend = yend),
            size        = input$errbar_size,
            colour      = "black",
            show.legend = FALSE
          ) +
          geom_segment(
            data        = err_df,
            inherit.aes = FALSE,
            aes(
              x    = xnum - cap_half_width,
              xend = xnum + cap_half_width,
              y    = yend,
              yend = yend
            ),
            size        = input$errbar_size,
            colour      = "black",
            show.legend = FALSE
          )
      }
      
      
      # 6) Etiquetas, límites y tema final  
      p <- p +  
        labs(title = input$plotTitle, x = NULL, y = if (nzchar(input$yLab)) input$yLab else ps$Y_Title) +  
        scale_y_continuous(limits = c(0, input$ymax), breaks = seq(0, input$ymax, by = input$ybreak), expand = c(0,0)) +  
        theme_classic(base_size = input$base_size, base_family = "Helvetica") +
        coord_cartesian(clip = "off") +
        theme(
          plot.margin = margin_adj(12, 18, b_mar, 28),
          plot.title      = element_text(size = fs_title, face = "bold", colour = "black"),
          axis.title.y    = element_text(size = fs_axis, face = "bold", colour = "black"),
          axis.text.x = element_text(
            size  = fs_axis,
            angle = x_ang,
            hjust = ifelse(x_ang == 0, .5, 1),
            colour = "black"
          ),
          axis.text.y     = element_text(size = fs_axis, colour = "black"),
          axis.line       = element_line(linewidth = axis_size, colour = "black"),
          axis.ticks      = element_line(linewidth = axis_size, colour = "black"),
          axis.ticks.length = unit(4, "pt"),
          panel.grid      = element_blank(),
          panel.background = element_rect(fill = "white", colour = NA),
          legend.text     = element_text(size = fs_legend, colour = "black"),
          legend.key      = element_blank(),
          legend.key.size = unit(1.4, "lines")
        )  
      
      return(p)  
    }  
    
    # --- 3.x) Correlación -------------------------------------------------
    if (tipo == "Correlación") {
      
      ## 1 · nombres reales de las columnas (con o sin _Norm)
      raw_x   <- input$corr_param_x
      raw_y   <- input$corr_param_y
      param_x <- if (isTRUE(input$doNorm)) paste0(raw_x, "_Norm") else raw_x
      param_y <- if (isTRUE(input$doNorm)) paste0(raw_y, "_Norm") else raw_y
      
      # 2) tabla fuente ya filtrada por scope/reps
      df_raw <- scope_df
      
      df <- if (scope == "Por Cepa") {          # etiquetas = medio
        df_raw %>%
          group_by(Media) %>%
          summarise(
            X = mean(.data[[param_x]], na.rm = TRUE),
            Y = mean(.data[[param_y]], na.rm = TRUE),
            .groups = "drop"
          ) %>%
          rename(Label = Media)
        
      } else {                                  # etiquetas combinadas
        df_raw %>%
          group_by(Strain, Media) %>%
          summarise(
            X = mean(.data[[param_x]], na.rm = TRUE),
            Y = mean(.data[[param_y]], na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(
            Label = if (isTRUE(input$labelMode))
              Strain
            else
              paste(Strain, Media, sep = "-")
          )
      }
      
      ## 4 · chequeo de puntos
      validate(need(nrow(df) >= 3,
                    "Se necesitan ≥3 puntos para calcular la correlación"))
      
      cor_res <- cor.test(df$X, df$Y, method = input$corr_method)
      
      ## 5 · Ecuación de la recta (opcional)
      eq_lab <- NULL
      if (isTRUE(input$corr_show_eq)) {
        fit      <- lm(Y ~ X, data = df)
        slope    <- coef(fit)[2]
        intercept<- coef(fit)[1]
        r2       <- summary(fit)$r.squared
        eq_lab   <- sprintf("y = %.3f·x %+.3f\nR² = %.3f", slope, intercept, r2)
      }
      
      ## 6 · saneo de ejes y posiciones de los textos
      xmin   <- ifelse(is.finite(input$xmin_corr),  input$xmin_corr, 0)
      xmax   <- ifelse(is.finite(input$xmax_corr),  input$xmax_corr, xmin + 1)
      if (!is.finite(xmax) || xmax <= xmin) xmax <- xmin + 1
      xbreak <- ifelse(is.finite(input$xbreak_corr) && input$xbreak_corr > 0,
                       input$xbreak_corr, (xmax - xmin)/5)
      ymin   <- ifelse(is.finite(input$ymin_corr),  input$ymin_corr, 0)
      ymax   <- ifelse(is.finite(input$ymax_corr),  input$ymax_corr, ymin + 1)
      if (!is.finite(ymax) || ymax <= ymin) ymax <- ymin + 1
      ybreak <- ifelse(is.finite(input$ybreak_corr) && input$ybreak_corr > 0,
                       input$ybreak_corr, (ymax - ymin)/5)
      dx  <- 0.05 * (xmax - xmin)
      dy  <- 0.04 * (ymax - ymin)
      x_t <- xmax - dx
      y_t <- ymax - dy

      ## 7 · gráfico --------------------------------------------------------
      p <- ggplot(df, aes(X, Y)) +
        geom_point(size = 3, colour = "black") +
        { if (isTRUE(input$corr_show_line))
          geom_smooth(method = "lm", se = FALSE,
                      colour = "black", linetype = "dashed") } +
        { if (isTRUE(input$corr_show_labels))
          geom_text(aes(label = Label),
                    nudge_y = 0.05 * (ymax - ymin),
                    size    = input$corr_label_size) } +
        annotate("text",
                 x = x_t, y = y_t, hjust = 1, vjust = 1,
                 label = sprintf("r = %.3f\np = %.3g",
                                 cor_res$estimate, cor_res$p.value),
                 size = 5) +
        { if (!is.null(eq_lab))
          annotate("text",
                   x = x_t, y = y_t - dy*2.3,
                   hjust = 1, vjust = 1,
                   label = eq_lab, size = 5) } +
        labs(
          title = input$plotTitle,
          x     = if (nzchar(input$corr_xlab)) input$corr_xlab else raw_x,
          y     = if (nzchar(input$corr_ylab)) input$corr_ylab else raw_y
        ) +
        scale_x_continuous(
          limits = c(xmin, xmax),
          breaks = seq(xmin, xmax, by = xbreak),
          expand = c(0, 0)
        ) +
        scale_y_continuous(
          limits = c(ymin, ymax),
          breaks = seq(ymin, ymax, by = ybreak),
          expand = c(0, 0)
        ) +
        theme_minimal(base_size = input$base_size, base_family = "Helvetica") +
        coord_cartesian(clip = "off") +     # ← SOLO UNA vez
        theme(
          plot.margin = margin_adj(20, 50, 10, 10),      # ← SOLO UNA vez
          plot.title  = element_text(size = input$fs_title, face = "bold"),
          axis.title  = element_text(size = input$fs_axis,  face = "bold"),
          axis.text   = element_text(size = input$fs_axis),
          axis.line   = element_line(linewidth = input$axis_line_size),
          axis.ticks  = element_line(linewidth = input$axis_line_size),
          panel.grid  = element_blank()
        )
      
      return(p)
    }
    
    
    # --- 3.x) Curvas (Por Cepa y Combinado) ---  
    if (tipo == "Curvas") {
      req(curve_data(), curve_settings())
      
      # 1) Leer y unir curvas + metadatos
      df_cur <- curve_long_df()
      label_df <- NULL
      # 2) Filtrar y ordenar según ámbito
      if (scope == "Por Cepa") {
        df_cur <- df_cur %>%
          filter(Strain == strain) %>%
          order_filter_strain() %>%
          filter_reps_strain()
        
        media_order <- df_cur %>%
          distinct(Media, Orden) %>%
          arrange(Orden) %>%
          pull(Media)
        
        df_sum <- df_cur %>%
          group_by(Time, Media) %>%
          summarise(Avg = mean(Value, na.rm = TRUE), .groups = "drop") %>%
          mutate(Label = factor(Media, levels = media_order))
        
      } else {
        df_cur <- df_cur %>%
          order_filter_group() %>%
          filter_reps_group()
        
        available_labels <- unique(paste(df_cur$Strain, df_cur$Media, sep = "-"))
        platemap_labels <- datos_agrupados() %>%
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
            group_by(Strain) %>%
            summarise(minO = min(Orden), .groups = "drop") %>%
            arrange(minO) %>%
            pull(Strain)
          
          df_sum <- df_cur %>%
            group_by(Time, Strain) %>%
            summarise(Avg = mean(Value, na.rm = TRUE), .groups = "drop") %>%
            mutate(Label = factor(Strain, levels = strain_order))
          
        } else {
          df_sum <- df_cur %>%
            group_by(Time, Strain, Media) %>%
            summarise(Avg = mean(Value, na.rm = TRUE), .groups = "drop") %>%
            mutate(Label = factor(paste(Strain, Media, sep = "-"), levels = final_order))
        }
        label_df <- df_cur %>%
          mutate(Label = if (isTRUE(input$labelMode)) Strain else paste(Strain, Media, sep = "-")) %>%
          distinct(Label, Strain, Media)
      }
      
      # 3) Extraer etiquetas de ejes desde la configuración (ahora editables)
        cfg_cur <- curve_settings()[1, ]
        x_lab <- if (nzchar(input$cur_xlab)) input$cur_xlab else cfg_cur$X_Title
        y_lab <- if (nzchar(input$cur_ylab)) input$cur_ylab else cfg_cur$Y_Title
        b_mar <- get_bottom_margin(0)
      
      # 4) Crear gráfico con ggplot2
      plt <- ggplot(df_sum, aes(x = Time, y = Avg, group = Label)) +
        geom_line(linewidth = input$curve_lwd, lineend = "round", colour = "black") +
        geom_point(
          aes(fill = Label),
          size   = input$curve_lwd * 2.2,
          shape  = 21,
          colour = "black",
          stroke = 0.4
        ) +
        scale_fill_manual(
          values = if (scope == "Combinado" && exists("label_df")) {
            palette_for_labels(
              mutate(label_df, Label = factor(Label, levels = levels(df_sum$Label))),
              levels(df_sum$Label)
            )
          } else {
            get_palette(nlevels(df_sum$Label))
          },
          breaks = levels(df_sum$Label)
        ) +
        labs(
          title  = input$plotTitle,
          x      = x_lab,
          y      = y_lab,
          fill   = NULL
        ) +
        scale_x_continuous(
          limits = c(0, input$xmax_cur),
          breaks = seq(0, input$xmax_cur, by = input$xbreak_cur),
          expand = c(0, 0)
        ) +
        scale_y_continuous(
          limits = c(0, input$ymax_cur),
          breaks = seq(0, input$ymax_cur, by = input$ybreak_cur),
          expand = c(0, 0)
        ) +
        theme_classic(base_size = input$base_size, base_family = "Helvetica") +
        coord_cartesian(clip = "off") +
        theme(
          plot.margin     = margin_adj(15, 20, b_mar, 28),
          plot.title      = element_text(size = input$fs_title, face = "bold", colour = "black"),
          axis.title      = element_text(size = input$fs_axis, face = "bold", colour = "black"),
          axis.text       = element_text(size = input$fs_axis, colour = "black"),
          axis.line       = element_line(linewidth = input$axis_line_size, colour = "black"),
          axis.ticks      = element_line(linewidth = input$axis_line_size, colour = "black"),
          axis.ticks.length = unit(4, "pt"),
          panel.grid      = element_blank(),
          panel.background = element_rect(fill = "white", colour = NA),
          legend.text     = element_text(size = input$fs_legend, colour = "black"),
          legend.key      = element_blank(),
          legend.key.size = unit(1.5, "lines")
        )
      
      return(plt)
    }
    
    
    
    
    
    
    
    # ——— 3) Lógica principal ———  
    if (scope == "Combinado") {  
      
      # --- 3.1) Boxplot combinado ---  
      if (scope == "Combinado" && tipo == "Boxplot") {
          df_labels <- scope_df %>% distinct(Label, Strain, Media)
          df_plot <- scope_df %>%
            transmute(Label, Valor = .data[[param_sel]]) %>%
            filter(is.finite(Valor))

        if (nrow(df_plot) == 0) {
          return(
            ggplot() +
              theme_void() +
              annotate("text", 0, 0, label = "Sin datos con la selección actual")
          )
        }

        if (isTRUE(input$x_wrap)) {
          df_plot$Label   <- wrap_label(df_plot$Label,   lines = input$x_wrap_lines)
          df_labels$Label <- wrap_label(df_labels$Label, lines = input$x_wrap_lines)
        }
        df_plot$Label   <- droplevels(df_plot$Label)
        df_labels$Label <- factor(df_labels$Label, levels = levels(df_plot$Label))
        pal  <- palette_for_labels(df_labels, levels(df_plot$Label))
        x_ang <- get_x_angle(
          n            = nlevels(df_plot$Label),   # ← categorías reales
          angle_input  = input$x_angle
        )
        b_mar <- get_bottom_margin(x_ang, input$x_wrap, input$x_wrap_lines)
        p <- ggplot(df_plot, aes(Label, Valor))
        
        if (input$colorMode == "Blanco y Negro") {  
          p <- p +
            geom_boxplot(fill = "white", colour = "black", width = input$box_w,
                         linewidth = input$errbar_size,
                         outlier.shape = NA,
                         na.rm         = TRUE) +
            geom_jitter(
              shape  = 21,
              fill   = "black",
              colour = "black",
              stroke = 0.4,
              width  = input$pt_jit,
              size   = input$pt_size,
              na.rm  = TRUE
            )
        } else {
          p <- p +
            geom_boxplot(aes(fill = Label), width = input$box_w, colour = "black",
                         linewidth = input$errbar_size,
                         alpha    = 0.5,
                         outlier.shape = NA,
                         na.rm         = TRUE) +
            geom_jitter(
              aes(fill = Label),
              width  = input$pt_jit,
              shape  = 21,
              colour = "black",
              stroke = 0.5,
              size   = input$pt_size,
              alpha  = 1,
              na.rm  = TRUE
            ) +
            scale_fill_manual(values = pal)
        }  
        
        base_margin <- margin_adj(5.5, 5.5, b_mar, 25)
        p <- p +  
          labs(title = input$plotTitle, y = ylab, x = NULL) +  
          scale_y_continuous(
            limits = c(0, ymax),
            breaks = seq(0, ymax, by = ybreak),
            expand = c(0,0),
            oob    = scales::oob_keep
          ) +  
          theme_minimal(base_size = input$base_size, base_family = "Helvetica") +
          coord_cartesian(clip = "off") +
          theme(
            plot.margin = base_margin,   # margen izq. +20 px aprox.
            plot.title      = element_text(size = fs_title, face = "bold"),  
            axis.title.y    = element_text(size = fs_axis, face = "bold"),  
            axis.text.x = element_text(
              size  = fs_axis,
              angle = x_ang,
              hjust = ifelse(x_ang == 0, .5, 1)
            ),
            axis.text.y     = element_text(size = fs_axis),  
            axis.line       = element_line(linewidth = axis_size),  
            axis.ticks      = element_line(linewidth = axis_size),  
            panel.grid      = element_blank(),  
            legend.position = "none"  
          )  
        
        if (isTRUE(input$labelMode)) {  
          p <- p + scale_x_discrete(labels = function(x) sub("-.*$", "", x))  
        }  
        p <- stack_siglines(p, sig_list(),
                            sep         = input$sig_sep,
                            base_height = input$sig_offset,
                            linewidth   = input$sig_linewidth,
                            vsize       = .02,
                            tpad        = input$sig_textpad,
                            tsize       = input$sig_textsize,
                            margin_base = base_margin,
                            plot_height = input$plot_h,
                            show_caps   = !isTRUE(input$sig_hide_caps))
        
        return(p)  
      }  
      
      # --- 3.2) Violín combinado ---
      if (scope == "Combinado" && tipo == "Violin") {
        df_labels <- scope_df %>% distinct(Label, Strain, Media)
        df_plot <- scope_df %>%
          transmute(Label, Valor = .data[[param_sel]]) %>%
          filter(is.finite(Valor))

        if (nrow(df_plot) == 0) {
          return(
            ggplot() +
              theme_void() +
              annotate("text", 0, 0, label = "Sin datos con la selección actual")
          )
        }

        if (isTRUE(input$x_wrap)) {
          df_plot$Label   <- wrap_label(df_plot$Label,   lines = input$x_wrap_lines)
          df_labels$Label <- wrap_label(df_labels$Label, lines = input$x_wrap_lines)
        }
        df_plot$Label   <- droplevels(df_plot$Label)
        df_labels$Label <- factor(df_labels$Label, levels = levels(df_plot$Label))

        pal   <- palette_for_labels(df_labels, levels(df_plot$Label))
        x_ang <- get_x_angle(
          n           = nlevels(df_plot$Label),
          angle_input = input$x_angle
        )
        b_mar <- get_bottom_margin(x_ang, input$x_wrap, input$x_wrap_lines)

        v_width <- input$violin_width %||% 0.45
        if (!is.finite(v_width) || v_width <= 0) v_width <- 0.45
        v_lwd   <- if (!is.null(input$violin_linewidth) && is.finite(input$violin_linewidth) &&
                         input$violin_linewidth > 0) input$violin_linewidth else 0.6
        violin_args <- list(
          linewidth = v_lwd,
          width     = v_width,
          alpha     = 0.8,
          trim      = TRUE,
          scale     = "count",
          adjust    = 1.1,
          na.rm     = TRUE
        )

        df_split <- df_plot %>%
          group_by(Label) %>%
          mutate(n_unique = n_distinct(Valor)) %>%
          ungroup()
        violin_data <- df_split %>% filter(n_unique >= 2)
        jitter_width <- min(input$pt_jit, v_width * 0.6)

        if (input$colorMode == "Blanco y Negro") {
          p <- ggplot(df_plot, aes(Label, Valor)) +
            {
              if (nrow(violin_data) > 0)
                do.call(geom_violin, c(list(data = violin_data, fill = "white",
                                            colour = "black"), violin_args))
            } +
            geom_point(
              position = position_jitter(width = jitter_width, height = 0),
              shape    = 21,
              fill     = "black",
              colour   = "black",
              stroke   = v_lwd / 2,
              size     = input$pt_size,
              na.rm    = TRUE
            )
        } else {
          p <- ggplot(df_plot, aes(Label, Valor, fill = Label)) +
            {
              if (nrow(violin_data) > 0)
                do.call(geom_violin, c(list(data = violin_data, colour = "black"), violin_args))
            } +
            geom_point(
              aes(fill = Label),
              position = position_jitter(width = jitter_width, height = 0),
              shape    = 21,
              colour   = "black",
              stroke   = v_lwd / 2,
              size     = input$pt_size,
              alpha    = 0.95,
              na.rm    = TRUE
            ) +
            scale_fill_manual(values = pal)
        }

        base_margin <- margin_adj(12, 18, b_mar, 28)
        p <- p +
          labs(title = input$plotTitle, y = ylab, x = NULL) +
          scale_y_continuous(
            limits = c(0, ymax),
            breaks = seq(0, ymax, by = ybreak),
            expand = c(0, 0),
            oob    = scales::oob_keep
          ) +
          theme_classic(base_size = input$base_size, base_family = "Helvetica") +
          coord_cartesian(clip = "off") +
          theme(
            plot.margin      = base_margin,
            plot.title       = element_text(size = fs_title, face = "bold", colour = "black"),
            axis.title.y     = element_text(size = fs_axis, face = "bold", colour = "black"),
            axis.text.x      = element_text(
              size  = fs_axis,
              angle = x_ang,
              hjust = ifelse(x_ang == 0, .5, 1),
              colour = "black"
            ),
            axis.text.y      = element_text(size = fs_axis, colour = "black"),
            axis.line        = element_line(linewidth = axis_size, colour = "black"),
            axis.ticks       = element_line(linewidth = axis_size, colour = "black"),
            axis.ticks.length = unit(4, "pt"),
            panel.background = element_rect(fill = "white", colour = NA),
            panel.grid       = element_blank(),
            legend.position  = "none"
          )

        if (isTRUE(input$labelMode)) {
          p <- p + scale_x_discrete(labels = function(x) sub("-.*$", "", x))
        }

        p <- stack_siglines(p, sig_list(),
                            sep         = input$sig_sep,
                            base_height = input$sig_offset,
                            linewidth   = input$sig_linewidth,
                            vsize       = .02,
                            tpad        = input$sig_textpad,
                            tsize       = input$sig_textsize,
                            margin_base = base_margin,
                            plot_height = input$plot_h,
                            show_caps   = !isTRUE(input$sig_hide_caps))

        return(p)
      }
      
      # --- 3.2) Barras combinado ---  
      if (scope == "Combinado" && tipo == "Barras") {  
        df_labels <- scope_df %>% distinct(Label, Strain, Media)
        df_raw <- scope_df

        if (isTRUE(input$x_wrap)) {
          df_raw$Label    <- wrap_label(df_raw$Label,    lines = input$x_wrap_lines)
          df_labels$Label <- wrap_label(df_labels$Label, lines = input$x_wrap_lines)
        }
        if (nrow(df_raw) == 0) {  
          return(  
            ggplot() +  
              theme_void() +  
              annotate("text", 0, 0, label = "Sin datos con la selección actual")  
          )  
        }  
        ## 1) RESUMEN (¡creado ANTES de usarlo!)
        resumen <- df_raw %>%
          group_by(Label) %>%
          summarise(
            Mean = mean(.data[[param_sel]], na.rm = TRUE),
            SD   = sd  (.data[[param_sel]], na.rm = TRUE),
            .groups = "drop"
          )
        
        ## 2) Ángulo adaptativo de las etiquetas X
        x_ang <- get_x_angle(
          n           = nlevels(resumen$Label),      # ahora sí existe
          angle_input = input$x_angle
        )
        b_mar <- get_bottom_margin(x_ang, input$x_wrap, input$x_wrap_lines)
        resumen$Label <- droplevels(resumen$Label)
        df_labels$Label <- factor(df_labels$Label, levels = levels(resumen$Label))
        pal  <- palette_for_labels(df_labels, levels(resumen$Label))
        
        if (input$colorMode == "Blanco y Negro") {
          p <- ggplot(resumen, aes(Label, Mean)) +
            geom_col(
              fill    = "white",
              colour  = "black",
              width   = 0.65,
              linewidth = 0.6,
              alpha   = 0.95
            ) +
            geom_errorbar(
              aes(ymin = Mean - SD, ymax = Mean + SD),
              width    = 0.2,
              linewidth = input$errbar_size,
              colour   = "black"
            ) +
            geom_point(
              data        = df_raw,
              inherit.aes = FALSE,
              aes(x = Label, y = .data[[param_sel]]),
              position    = position_jitter(width = input$pt_jit, height = 0),
              shape       = 21,
              fill        = "black",
              colour      = "black",
              stroke      = 0.4,
              size        = input$pt_size
            )
          
        } else {  
          p <- ggplot(resumen, aes(Label, Mean, fill = Label)) +
            geom_col(
              width    = 0.65,
              colour   = "black",
              linewidth = 0.6,
              alpha    = 0.7
            ) +
            geom_errorbar(
              aes(ymin = Mean - SD, ymax = Mean + SD),
              width    = 0.2,
              linewidth = input$errbar_size,
              colour   = "black"
            ) +
            geom_point(
              data        = df_raw,
              inherit.aes = FALSE,
              aes(x = Label, y = .data[[param_sel]], fill = Label),
              position    = position_jitter(width = input$pt_jit, height = 0),
              shape       = 21,
              colour      = "black",
              stroke      = 0.5,
              size        = input$pt_size
            ) +
            scale_fill_manual(values = pal)
        }  
        
        base_margin <- margin_adj(12, 18, b_mar, 28)
        p <- p +  
          labs(title = input$plotTitle, y = ylab, x = NULL) +  
          scale_y_continuous(
            limits = c(0, ymax),
            breaks = seq(0, ymax, by = ybreak),
            expand = c(0,0),
            oob    = scales::oob_keep
          ) +  
          theme_classic(base_size = input$base_size, base_family = "Helvetica") +
          coord_cartesian(clip = "off") +
          theme(
            plot.margin = base_margin,
            plot.title      = element_text(size = fs_title, face = "bold", colour = "black"),
            axis.title.y    = element_text(size = fs_axis, face = "bold", colour = "black"),
            axis.text.x = element_text(
              size  = fs_axis,
              angle = x_ang,
              hjust = ifelse(x_ang == 0, .5, 1),
              colour = "black"
            ),
            axis.text.y     = element_text(size = fs_axis, colour = "black"),
            axis.line       = element_line(linewidth = axis_size, colour = "black"),
            axis.ticks      = element_line(linewidth = axis_size, colour = "black"),
            axis.ticks.length = unit(4, "pt"),
            panel.grid      = element_blank(),
            panel.background = element_rect(fill = "white", colour = NA),
            legend.position = "none"  
          )  
        
        if (isTRUE(input$labelMode)) {  
          p <- p + scale_x_discrete(labels = function(x) sub("-.*$", "", x))  
        }  
        # ── Añadir barras de significancia seleccionadas ──────────────────────────  
        p <- stack_siglines(p, sig_list(),  
                            sep         = input$sig_sep,  
                            base_height = input$sig_offset,
                            linewidth   = input$sig_linewidth,  
                            vsize       = .02,  
                            tpad        = input$sig_textpad,  
                            tsize       = input$sig_textsize,
                            margin_base = base_margin,
                            plot_height = input$plot_h,
                            show_caps   = !isTRUE(input$sig_hide_caps))  
        
        
        return(p)  
      }  
      
      if (tipo == "Violin") {
        df_raw <- scope_df %>%
          filter(is.finite(.data[[param_sel]]))

        if (nrow(df_raw) == 0) {
          return(
            ggplot() + theme_void() +
              annotate("text", x = 0, y = 0, label = "Sin datos con la selección actual")
          )
        }

        if (isTRUE(input$x_wrap)) {
          df_raw$Media <- wrap_label(df_raw$Media, lines = input$x_wrap_lines)
        }

        pal   <- get_palette(nlevels(factor(df_raw$Media)))
        x_ang <- get_x_angle(
          n           = nlevels(factor(df_raw$Media)),
          angle_input = input$x_angle
        )
        b_mar <- get_bottom_margin(x_ang, input$x_wrap, input$x_wrap_lines)

        v_width <- input$violin_width %||% 0.45
        if (!is.finite(v_width) || v_width <= 0) v_width <- 0.45
        v_lwd   <- if (!is.null(input$violin_linewidth) && is.finite(input$violin_linewidth) &&
                         input$violin_linewidth > 0) input$violin_linewidth else 0.6
        violin_args <- list(
          linewidth = v_lwd,
          width     = v_width,
          alpha     = 0.8,
          trim      = TRUE,
          scale     = "count",
          adjust    = 1.1,
          na.rm     = TRUE
        )
        df_split <- df_raw %>%
          group_by(Media) %>%
          mutate(n_unique = n_distinct(.data[[param_sel]])) %>%
          ungroup()
        violin_data <- df_split %>% filter(n_unique >= 2)
        jitter_width <- min(input$pt_jit, v_width * 0.6)

        if (colourMode == "Blanco y Negro") {
          p <- ggplot(df_raw, aes(Media, .data[[param_sel]])) +
            {
              if (nrow(violin_data) > 0)
                do.call(geom_violin,
                        c(list(data = violin_data, fill = "white", colour = "black"), violin_args))
            } +
            geom_point(
              data     = df_raw,
              position = position_jitter(width = jitter_width, height = 0),
              shape    = 21,
              fill     = "black",
              colour   = "black",
              stroke   = v_lwd / 2,
              size     = input$pt_size,
              na.rm    = TRUE
            )
        } else {
          p <- ggplot(df_raw, aes(Media, .data[[param_sel]], fill = Media)) +
            {
              if (nrow(violin_data) > 0)
                do.call(geom_violin,
                        c(list(data = violin_data, colour = "black"), violin_args))
            } +
            geom_point(
              aes(fill = Media),
              data     = df_raw,
              position = position_jitter(width = jitter_width, height = 0),
              shape    = 21,
              colour   = "black",
              stroke   = v_lwd / 2,
              size     = input$pt_size,
              alpha    = 0.95,
              na.rm    = TRUE
            ) +
            scale_fill_manual(values = pal)
        }

        base_margin <- margin_adj(12, 18, b_mar, 28)
        p <- p +
          labs(title = input$plotTitle, y = ylab, x = NULL) +
          scale_y_continuous(
            limits = c(0, ymax),
            breaks = seq(0, ymax, by = ybreak),
            expand = c(0, 0),
            oob    = scales::oob_keep
          ) +
          theme_classic(base_size = input$base_size, base_family = "Helvetica") +
          coord_cartesian(clip = "off") +
          theme(
            plot.margin      = base_margin,
            plot.title       = element_text(size = fs_title, face = "bold", colour = "black"),
            axis.title.y     = element_text(size = fs_axis, face = "bold", colour = "black"),
            axis.text.x      = element_text(
              size  = fs_axis,
              angle = x_ang,
              hjust = ifelse(x_ang == 0, .5, 1),
              colour = "black"
            ),
            axis.text.y      = element_text(size = fs_axis, colour = "black"),
            axis.line        = element_line(linewidth = axis_size, colour = "black"),
            axis.ticks       = element_line(linewidth = axis_size, colour = "black"),
            axis.ticks.length = unit(4, "pt"),
            panel.background = element_rect(fill = "white", colour = NA),
            panel.grid       = element_blank(),
            legend.position  = "none"
          )

        p <- stack_siglines(p, sig_list(),
                            sep         = input$sig_sep,
                            base_height = input$sig_offset,
                            linewidth   = input$sig_linewidth,
                            vsize       = .02,
                            tpad        = input$sig_textpad,
                            tsize       = input$sig_textsize,
                            margin_base = base_margin,
                            plot_height = input$plot_h,
                            show_caps   = !isTRUE(input$sig_hide_caps))

        return(p)
      }
      
      
      
    } else {  
      
      # --- 3.3) Boxplot por cepa ---  
      if (tipo == "Boxplot") {
        df <- scope_df %>%
          filter(is.finite(.data[[param_sel]]))

        if (nrow(df) == 0) {
          return(ggplot() + theme_void() +
                   annotate("text", x = 0, y = 0, label = "Sin datos con la selección actual"))
        }

        if (isTRUE(input$x_wrap)) {
          df$Media <- wrap_label(df$Media, lines = input$x_wrap_lines)
        }
        x_ang <- get_x_angle(
          n            = nlevels(factor(df$Media)),
          angle_input  = input$x_angle
        )
        b_mar <- get_bottom_margin(x_ang, input$x_wrap, input$x_wrap_lines)
        p <- if (colourMode == "Blanco y Negro") {
          ggplot(df, aes(Media, .data[[param_sel]])) +
            geom_boxplot(fill = "white", colour = "black", width = input$box_w,
                         linewidth = input$errbar_size,
                         outlier.shape = NA,
                         na.rm         = TRUE) +
            geom_jitter(
              shape = 21,
              fill  = "black",
              colour = "black",
              stroke = 0.4,
              width  = input$pt_jit,
              size   = input$pt_size,
              na.rm  = TRUE
            )
        } else {  
          pal <- get_palette(nlevels(factor(df$Media)))
          ggplot(df, aes(Media, .data[[param_sel]], fill = Media)) +
            geom_boxplot(width = input$box_w, colour = "black",
                         linewidth = input$errbar_size,
                         alpha    = 0.5,
                         outlier.shape = NA,
                         na.rm         = TRUE) +
            geom_jitter(
              aes(fill = Media),
              width  = input$pt_jit,
              shape  = 21,
              colour = "black",
              stroke = 0.5,
              size   = input$pt_size,
              alpha  = 0.95,
              na.rm  = TRUE
            ) +
            scale_fill_manual(values = pal)
        }  
        base_margin <- margin_adj(5.5, 5.5, b_mar, 25)
        p <- p +  
          labs(title = input$plotTitle, y = ylab, x = NULL) +  
          scale_y_continuous(
            limits = c(0, ymax),  
            breaks = seq(0, ymax, by = ybreak),  
            expand = c(0, 0),
            oob    = scales::oob_keep
          ) +  
          theme_minimal(base_size = input$base_size, base_family = "Helvetica") +
          coord_cartesian(clip = "off") +
          theme(
            plot.margin = base_margin,   # margen izq. +20 px aprox.
            plot.title      = element_text(size = fs_title, face = "bold"),  
            axis.title.y    = element_text(size = fs_axis,  face = "bold"),  
            axis.text.x = element_text(
              size  = fs_axis,
              angle = x_ang,
              hjust = ifelse(x_ang == 0, .5, 1)
            ),
            axis.text.y     = element_text(size = fs_axis),  
            axis.line       = element_line(linewidth = axis_size),  
            axis.ticks      = element_line(linewidth = axis_size),  
            panel.grid      = element_blank(),  
            legend.position = "none"  
          )  
        # ── Añadir barras de significancia seleccionadas ──────────────────────────  
        p <- stack_siglines(p, sig_list(),  
                            sep         = input$sig_sep,  
                            base_height = input$sig_offset,
                            linewidth   = input$sig_linewidth,  
                            vsize       = .02,  
                            tpad        = input$sig_textpad,  
                            tsize       = input$sig_textsize,
                            margin_base = base_margin,
                            plot_height = input$plot_h,
                            show_caps   = !isTRUE(input$sig_hide_caps))  
        
        
        return(p)  
      }  
      
      
      # --- 3.4) Barras por cepa ---  
      if (tipo == "Barras") {  
        df_raw <- scope_df

        if (isTRUE(input$x_wrap)) {
          df_raw$Media <- wrap_label(df_raw$Media, lines = input$x_wrap_lines)
        }
        if (nrow(df_raw) == 0) {  
          return(ggplot() + theme_void() +  
                   annotate("text", x = 0, y = 0, label = "Sin datos con la selección actual"))  
        }  
        x_ang <- get_x_angle(
          n           = nlevels(factor(df_raw$Media)),   # antes: df$Media
          angle_input = input$x_angle
        )
        b_mar <- get_bottom_margin(x_ang, input$x_wrap, input$x_wrap_lines)
        resumen <- df_raw %>%
          group_by(Media) %>%  
          summarise(  
            Mean = mean(.data[[param_sel]], na.rm = TRUE),  
            SD   = sd  (.data[[param_sel]], na.rm = TRUE),  
            .groups = "drop"  
          )  
        pal  <- get_palette(nlevels(resumen$Media))
        if (colourMode == "Blanco y Negro") {
          p <- ggplot(resumen, aes(Media, Mean)) +
            geom_col(
              fill     = "white",
              colour   = "black",
              width    = 0.65,
              linewidth = 0.6,
              alpha    = 0.95
            ) +
            geom_errorbar(
              aes(ymin = Mean - SD, ymax = Mean + SD),
              width    = 0.2,
              linewidth = input$errbar_size,
              colour   = "black"
            ) +
            geom_point(
              data        = df_raw,
              inherit.aes = FALSE,
              aes(x = Media, y = .data[[param_sel]]),
              position    = position_jitter(width = input$pt_jit, height = 0),
              shape       = 21,
              fill        = "black",
              colour      = "black",
              stroke      = 0.4,
              size        = input$pt_size
            )
        } else {  
          p <- ggplot(resumen, aes(Media, Mean, fill = Media)) +  
            geom_col(
              width    = 0.65,
              colour   = "black",
              linewidth = 0.6,
              alpha    = 0.7
            ) +  
            geom_errorbar(
              aes(ymin = Mean - SD, ymax = Mean + SD),
              width    = 0.2,
              linewidth = input$errbar_size,
              colour   = "black"
            ) +
            geom_point(
              data        = df_raw,
              inherit.aes = FALSE,
              aes(x = Media, y = .data[[param_sel]], fill = Media),
              position    = position_jitter(width = input$pt_jit, height = 0),
              shape       = 21,
              colour      = "black",
              stroke      = 0.5,
              size        = input$pt_size
            ) +
            scale_fill_manual(values = pal)  
        }  
        base_margin <- margin_adj(12, 18, b_mar, 28)
        p <- p +  
          labs(title = input$plotTitle, y = ylab, x = NULL) +  
          scale_y_continuous(
            limits = c(0, ymax),  
            breaks = seq(0, ymax, by = ybreak),  
            expand = c(0, 0),
            oob    = scales::oob_keep
          ) +  
          theme_classic(base_size = input$base_size, base_family = "Helvetica") +
          coord_cartesian(clip = "off") +
          theme(
            plot.margin = base_margin,
            plot.title      = element_text(size = fs_title, face = "bold", colour = "black"),
            axis.title.y    = element_text(size = fs_axis,  face = "bold", colour = "black"),
            axis.text.x = element_text(
              size  = fs_axis,
              angle = x_ang,
              hjust = ifelse(x_ang == 0, .5, 1),
              colour = "black"
            ),
            axis.text.y     = element_text(size = fs_axis, colour = "black"),
            axis.line       = element_line(linewidth = axis_size, colour = "black"),
            axis.ticks      = element_line(linewidth = axis_size, colour = "black"),
            axis.ticks.length = unit(4, "pt"),
            panel.grid      = element_blank(),
            panel.background = element_rect(fill = "white", colour = NA),
            legend.position = "none"  
          )  
        # ── Añadir barras de significancia seleccionadas ──────────────────────────  
        p <- stack_siglines(p, sig_list(),  
                            sep         = input$sig_sep,  
                            base_height = input$sig_offset,
                            linewidth   = input$sig_linewidth,  
                            vsize       = .02,  
                            tpad        = input$sig_textpad,  
                            tsize       = input$sig_textsize,
                            margin_base = base_margin,
                            plot_height = input$plot_h,
                            show_caps   = !isTRUE(input$sig_hide_caps))  
        
        return(p)  
      }  
      
      # ─────────────────────────────────────────────────────────────────────────────  
    }  
    
    # ——— fallback para nunca retornar NULL ———  
    return( ggplot() + theme_void() )  
  }  
  
  # ──────────────────────────────────────────────────────────────
  # Helper: versión protegida de ggplotly que descarta capas vacías
  # (corregido: asigna y devuelve plt)
  # ──────────────────────────────────────────────────────────────
  strip_sig_layers <- function(p) {
    if (!inherits(p, "ggplot")) return(p)
    keep <- vapply(p$layers, function(layer) {
      data <- layer$data
      if (is.null(data) || !is.data.frame(data)) return(TRUE)
      if (!".sig_layer" %in% names(data)) return(TRUE)
      !any(isTRUE(data$.sig_layer) | (data$.sig_layer %in% TRUE))
    }, logical(1))
    p$layers <- p$layers[keep]
    p
  }

  add_sig_shapes_plotly <- function(plt, sig_info) {
    if (is.null(sig_info) || !length(sig_info$bars)) return(plt)
    y_range <- sig_info$y_range
    if (is.null(y_range) || !is.numeric(y_range) || length(y_range) < 2) return(plt)
    y_min <- min(y_range, na.rm = TRUE)
    y_max <- max(y_range, na.rm = TRUE)
    y_span <- y_max - y_min
    if (!is.finite(y_span) || y_span <= 0) y_span <- 1

    bars <- sig_info$bars
    show_caps <- sig_info$show_caps
    if (is.null(show_caps)) show_caps <- TRUE
    line_annots <- list()
    annots <- list()
    mm_to_px <- 96 / 25.4
    height_px <- plt$x$layout$height %||% 700
    if (!is.numeric(height_px) || !is.finite(height_px) || height_px <= 0) {
      height_px <- 700
    }
    tickvals <- plt$x$layout$xaxis$tickvals
    if (!is.null(tickvals)) tickvals <- unlist(tickvals)
    map_x <- function(x_val) {
      if (!is.numeric(x_val) || !is.finite(x_val)) return(NA_real_)
      if (is.numeric(tickvals) && length(tickvals)) {
        if (isTRUE(all.equal(x_val, round(x_val))) &&
            x_val >= 1 && x_val <= length(tickvals)) {
          return(tickvals[round(x_val)])
        }
      }
      x_val
    }
    x_spacing <- 1
    if (is.numeric(tickvals) && length(tickvals) > 1) {
      diffs <- diff(sort(unique(tickvals)))
      diffs <- diffs[is.finite(diffs) & diffs > 0]
      if (length(diffs)) x_spacing <- min(diffs)
    }
    pad_left  <- x_spacing * 0.0005
    pad_right <- x_spacing * 0.006

    for (bar in bars) {
      x0 <- map_x(bar$x1)
      x1 <- map_x(bar$x2)
      if (!is.numeric(x0) || !is.numeric(x1) || any(!is.finite(c(x0, x1)))) next
      ybar <- (bar$ybar - y_min) / y_span
      ycap <- (bar$ycap - y_min) / y_span
      ytxt <- (bar$ytxt - y_min) / y_span
      if (any(!is.finite(c(ybar, ycap, ytxt)))) next

      lw <- (bar$linewidth %||% 0.8) * mm_to_px
      lw <- if (is.finite(lw) && lw > 0) lw else 1
      text_px <- (bar$textsize %||% 5) * mm_to_px
      text_px <- if (is.finite(text_px) && text_px > 0) text_px else 12

      x0_line <- x0 - pad_left
      x1_line <- x1 + pad_right
      y_overlap <- (lw / height_px) * 1.2
      if (!is.finite(y_overlap) || y_overlap <= 0) y_overlap <- 0.001
      y_overlap <- min(max(y_overlap, 0.001), 0.004)
      ybar_line <- ybar + y_overlap
      if (isTRUE(show_caps)) {
        line_annots <- append(line_annots, list(list(
          x = x0_line, y = ycap, xref = "x", yref = "paper",
          ax = x0_line, ay = ybar_line, axref = "x", ayref = "paper",
          showarrow = TRUE,
          arrowhead = 0,
          arrowsize = 1,
          arrowwidth = lw,
          arrowcolor = "black",
          text = ""
        )))
        line_annots <- append(line_annots, list(list(
          x = x1_line, y = ycap, xref = "x", yref = "paper",
          ax = x1_line, ay = ybar_line, axref = "x", ayref = "paper",
          showarrow = TRUE,
          arrowhead = 0,
          arrowsize = 1,
          arrowwidth = lw,
          arrowcolor = "black",
          text = ""
        )))
      }
      line_annots <- append(line_annots, list(list(
        x = x1_line, y = ybar, xref = "x", yref = "paper",
        ax = x0_line, ay = ybar, axref = "x", ayref = "paper",
        showarrow = TRUE,
        arrowhead = 0,
        arrowsize = 1,
        arrowwidth = lw,
        arrowcolor = "black",
        text = ""
      )))

      if (!is.null(bar$label) && nzchar(as.character(bar$label))) {
        xmid <- (x0 + x1) / 2
        label_txt <- as.character(bar$label)
        yshift_val <- if (is_star_label(label_txt)) -text_px * 0.35 else 0
        annots <- append(annots, list(list(
          x = xmid, y = ytxt,
          xref = "x", yref = "paper",
          text = label_txt,
          showarrow = FALSE,
          yanchor = "bottom",
          yshift = yshift_val,
          font = list(size = text_px, color = "black")
        )))
      }
    }

    if (length(line_annots) || length(annots)) {
      cur_annots <- plt$x$layout$annotations %||% list()
      plt <- plt %>% layout(annotations = c(cur_annots, line_annots, annots))
    }
    plt
  }

  safe_ggplotly <- function(p, ...) {
    # Si el objeto ya es plotly, simplemente devuélvelo
    if (!inherits(p, "ggplot")) return(p)

    sig_info <- attr(p, "sig_plotly", exact = TRUE)
    sig_margin_pt <- attr(p, "sig_plot_margin_pt", exact = TRUE)
    sig_extra_top_pt <- attr(p, "sig_extra_top_pt", exact = TRUE)
    p_clean <- if (!is.null(sig_info)) strip_sig_layers(p) else p

    # Convierte a plotly directamente
    plt <- ggplotly(p_clean, ...)

    # Ajustes globales de fuente/colores
    plt <- plt %>% layout(
      font      = list(family = "Helvetica", color = "black"),
      hovermode = "closest"
    )
    if (!is.null(sig_margin_pt) && is.numeric(sig_margin_pt)) {
      pt_to_px <- 96 / 72
      sig_margin_px <- sig_margin_pt * pt_to_px
      cur_margin <- plt$x$layout$margin %||% list(t = 0, r = 0, b = 0, l = 0)
      margin_vals <- list(
        t = max(c(cur_margin$t %||% 0, sig_margin_px[["t"]]), na.rm = TRUE),
        r = max(c(cur_margin$r %||% 0, sig_margin_px[["r"]]), na.rm = TRUE),
        b = max(c(cur_margin$b %||% 0, sig_margin_px[["b"]]), na.rm = TRUE),
        l = max(c(cur_margin$l %||% 0, sig_margin_px[["l"]]), na.rm = TRUE)
      )
      plt <- plt %>% layout(margin = margin_vals)
    }
    if (!is.null(sig_extra_top_pt) && is.numeric(sig_extra_top_pt) &&
        is.finite(sig_extra_top_pt) && sig_extra_top_pt > 0) {
      pt_to_px <- 96 / 72
      extra_px <- sig_extra_top_pt * pt_to_px
      cur_h <- plt$x$layout$height %||% input$plot_h %||% 700
      if (!is.numeric(cur_h) || !is.finite(cur_h) || cur_h <= 0) cur_h <- 700
      plt <- plt %>% layout(height = cur_h + extra_px)
    }
    if (!is.null(sig_info)) {
      plt <- add_sig_shapes_plotly(plt, sig_info)
    }
    # Evita que plotly recorte segmentos/texto fuera del panel
    if (!is.null(plt$x$data)) {
      plt$x$data <- lapply(plt$x$data, function(tr) {
        tr$cliponaxis <- FALSE
        tr
      })
    }
    
    # 4. Devuelve SIEMPRE el objeto plotly resultante
    return(plt)
  }
  
  
  # ---- plot_base: la versión “reactive” que usa la interfaz actual ----  
  plot_base <- reactive({  
    scope_sel  <- if (input$scope == "Combinado") "Combinado" else "Por Cepa"  
    strain_sel <- if (scope_sel == "Por Cepa")    input$strain else NULL  
    
    if (input$tipo == "Apiladas") {  
      # Devuelve un objeto *plotly* listo  
      build_plotly_stack(scope_sel, strain_sel)  
    } else {  
      # Todo lo demás sigue con tu función ggplot2  
      build_plot(scope_sel, strain_sel, input$tipo)  
    }  
  })  
  
  # --- Salidas ---  
  output$plotInteractivo <- renderPlotly({
    req(input$dataFile)
    if (input$tipo == "Curvas") {
      req(cur_data_box(), cur_cfg_box())
    }
    
    p <- plot_base()
    if (inherits(p, "ggplot")) {
      validate(need(length(ggplot_build(p)$data)>0, "Sin datos para graficar"))
      safe_ggplotly(
        p,
        tooltip      = "all",
        width        = input$plot_w,
        height       = input$plot_h,
        originalData = FALSE
      ) %>% config(responsive = TRUE)
    } else {
      # Ya p es un plotly puro generado por build_plotly_stack()
      p %>% config(responsive = TRUE)
    }
  })
  
  
  # --- Descarga individual PNG -----------------------------------------------
  output$downloadPlot_png <- downloadHandler(
    filename = function(){
      paste0(
        if (input$scope == "Combinado") "Combinado" else sanitize(input$strain),
        "_", input$tipo, ".png"
      )
    },
    content = function(file){
      write_current_plot_png(file)
    }
  )

  # --- Descarga individual PDF ----------------------------------------------
  output$downloadPlot_pdf <- downloadHandler(
    filename = function(){
      paste0(
        if (input$scope == "Combinado") "Combinado" else sanitize(input$strain),
        "_", input$tipo, ".pdf"
      )
    },
    content = function(file){
      scope_sel  <- if (input$scope=="Combinado") "Combinado" else "Por Cepa"
      strain_sel <- if (scope_sel=="Por Cepa") input$strain else NULL

      if (input$tipo == "Apiladas") {
        plt <- build_plotly_stack(scope_sel, strain_sel,
                                  width  = width %||% input$plot_w,
                                  height = height %||% input$plot_h)
        export_plotly_image(
          p      = plt,
          file   = file,
          width  = input$plot_w,
          height = height %||% input$plot_h
        )
      }
      else {
        p <- plot_base()
        if (inherits(p, "ggplot")) {
          ggplot2::ggsave(
            filename = file,
            plot     = p,
            width    = input$plot_w  / 100,
            height   = input$plot_h  / 100,
            device   = cairo_pdf,
            bg       = "transparent"
          )
        } else {
          p <- p %>%
            layout(
              paper_bgcolor = "rgba(0,0,0,0)",
              plot_bgcolor  = "rgba(0,0,0,0)"
            )
          export_plotly_image(
            p      = p,
            file   = file,
            width  = input$plot_w,
            height = height %||% input$plot_h
          )
        }
      }
    }
  )

  observeEvent(input$copy_plot_clipboard, {
    session$sendCustomMessage(
      "copyPlotToClipboard",
      list(
        width   = input$plot_w,
        height  = input$plot_h,
        scale   = 3,
        success = "plot_copy_success",
        fail    = "plot_copy_error"
      )
    )
  })

  observeEvent(input$plot_copy_success, {
    showNotification("Gráfico copiado al portapapeles.", type = "message", duration = 3)
  }, ignoreNULL = TRUE)

  observeEvent(input$plot_copy_error, {
    msg <- input$plot_copy_error$message %||% "Error desconocido."
    showNotification(
      paste("No se pudo copiar el gráfico:", msg),
      type = "error",
      duration = 6
    )
  }, ignoreNULL = TRUE)

  observeEvent(input$downloadPlotly_png, {
    req(input$tipo == "Apiladas")
    fname <- paste0(
      if (input$scope == "Combinado") "Combinado" else sanitize(input$strain),
      "_", input$tipo
    )
    session$sendCustomMessage("downloadPlotlyImage", list(
      filename = fname,
      width    = input$plot_w,
      height   = input$plot_h,
      format   = "png"
    ))
  })

  observeEvent(input$downloadPlotly_pdf, {
    req(input$tipo == "Apiladas")
    fname <- paste0(
      if (input$scope == "Combinado") "Combinado" else sanitize(input$strain),
      "_", input$tipo
    )
    session$sendCustomMessage("downloadPlotlyImage", list(
      filename = fname,
      width    = input$plot_w,
      height   = input$plot_h,
      format   = "pdf"
    ))
  })
  
  
  download_param_content <- function(file) {
    datos  <- datos_combinados()
    params <- plot_settings()$Parameter
    wb_sum <- generate_summary_wb(datos, params)
    
    cur_wide <- curve_data()
    if (!is.null(cur_wide)) {
      wb_sum <- add_curves_by_group_sheet(
        wb         = wb_sum,
        curve_wide = cur_wide,
        meta_df    = datos
      )
    }
    openxlsx::saveWorkbook(wb_sum, file, overwrite = TRUE)
  }

  output$downloadExcel <- downloadHandler(  
    filename    = function() "Parametros_por_grupo.xlsx",  
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",  
    content     = download_param_content
  )  
  
  capture_to_raw <- function(ext, writer){
    tmp <- tempfile(fileext = ext)
    on.exit(unlink(tmp), add = TRUE)
    writer(tmp)
    size <- file.info(tmp)$size
    if (is.na(size) || size <= 0) return(raw(0))
    readBin(tmp, "raw", n = size)
  }

  df_to_csv_raw <- function(df){
    tmp <- tempfile(fileext = ".csv")
    on.exit(unlink(tmp), add = TRUE)
    utils::write.csv(as.data.frame(df), file = tmp, row.names = FALSE, na = "")
    size <- file.info(tmp)$size
    if (is.na(size) || size <= 0) return(raw(0))
    readBin(tmp, "raw", n = size)
  }

  md5_raw <- function(bin){
    if (is.null(bin) || length(bin) == 0) return(NULL)
    digest::digest(bin, algo = "md5")
  }

  ensure_dataset_record <- function(dataset_key, dataset_name,
                                    datos_comb, datos_agru,
                                    comb_raw = NULL, agru_raw = NULL,
                                    stats_hash = NULL, stats_raw = NULL){
    make_unique <- function(label, existing){
      if (!label %in% existing) return(label)
      idx <- 2
      repeat {
        candidate <- paste(label, idx, sep = "_")
        if (!candidate %in% existing) return(candidate)
        idx <- idx + 1
      }
    }

    record <- bundle_store$datasets[[dataset_key]]
    if (is.null(record)) {
      dir_label <- sanitize(tools::file_path_sans_ext(dataset_name))
      if (!nzchar(dir_label)) dir_label <- "dataset"
      existing_labels <- vapply(bundle_store$datasets, function(x) x$dir_label,
                                character(1), USE.NAMES = FALSE)
      dir_label <- make_unique(dir_label, existing_labels)
      record <- list(
        key       = dataset_key,
        name      = dataset_name,
        dir_label = dir_label,
        created   = Sys.time(),
        files     = list(),
        stats     = list()
      )
    }

    if (is.null(record$files$datos_combinados)) {
      raw <- comb_raw %||% df_to_csv_raw(datos_comb)
      record$files$datos_combinados <- list(
        name = "datos_combinados.csv",
        raw  = raw,
        description = "Hoja 'Datos' (combinado)"
      )
    }
    if (is.null(record$files$datos_agrupados)) {
      raw <- agru_raw %||% df_to_csv_raw(datos_agru)
      record$files$datos_agrupados <- list(
        name = "datos_agrupados.csv",
        raw  = raw,
        description = "Datos agregados por cepa/medio"
      )
    }
    if (is.null(record$files$parametros)) {
      record$files$parametros <- list(
        name = "Parametros_por_grupo.xlsx",
        raw  = capture_to_raw(".xlsx", download_param_content),
        description = "Archivo de parámetros exportado"
      )
    }

    if (!is.null(stats_hash) && !is.null(stats_raw) && length(stats_raw) > 0) {
      if (is.null(record$stats)) record$stats <- list()
      if (is.null(record$stats[[stats_hash]])) {
        stat_name <- paste0("estadisticas_", length(record$stats) + 1, ".xlsx")
        record$stats[[stats_hash]] <- list(
          name    = stat_name,
          raw     = stats_raw,
          created = Sys.time()
        )
      }
    }

    bundle_store$datasets[[dataset_key]] <- record
    record
  }
  
  ##############################################################################
  ## DESCARGAR RESULTADOS ESTADÍSTICOS (normalidad + significancia)
  ##############################################################################
  download_stats_content <- function(file){
      
      req(input$dataFile)                # <-- asegúrate de que hay datos cargados
      
      params <- plot_settings()$Parameter
      datos  <- datos_combinados()
      
      scope_sel    <- isolate(input$scope)          %||% "Por Cepa"
      strain_sel   <- isolate(input$strain)
      sigTest_sel  <- isolate(input$sigTest)        %||% "ANOVA"
      postHoc_sel  <- isolate(input$postHoc)        %||% "Tukey"
      compMode_sel <- isolate(input$compMode)       %||% "all"
      controlGroup_sel <- isolate(input$controlGroup) %||% ""
      group1_sel   <- isolate(input$group1)         %||% ""
      group2_sel   <- isolate(input$group2)         %||% ""
      
      wb_tests <- createWorkbook()      # <- libro que vamos a rellenar
      
      ## helper interno (idéntico al usado arriba)
      split_comparison <- function(x) stringr::str_split_fixed(x, "-", 2)
      
      for (param in params){
        
        sheet <- safe_sheet(param)      # nombre seguro de hoja
        addWorksheet(wb_tests, sheet)
        
        # ---------- reconstruimos el df con los mismos filtros -----------------
        if (scope_sel == "Por Cepa"){
          df_param <- datos_agrupados() |>
            dplyr::filter(Strain == strain_sel) |>
            order_filter_strain()              |>
            filter_reps_strain()               |>
            dplyr::transmute(Label = Media,
                             Valor = .data[[param]])
        } else { # Combinado
          df_param <- datos_agrupados() |>
            order_filter_group() |>
            dplyr::transmute(Label,
                             Valor = .data[[param]])
        }
        
        # ── si no hay datos suficientes se elimina la hoja y se pasa al sig.
        if (nrow(df_param) < 3 || dplyr::n_distinct(df_param$Label) < 2){
          removeWorksheet(wb_tests, sheet)
          next
        }
        
        norm_tbl <- df_param |>
          dplyr::group_by(Label) |>
          dplyr::summarise(
            Shapiro.stat = stats::shapiro.test(Valor)$statistic,
            Shapiro.p    = stats::shapiro.test(Valor)$p.value,
            Normal       = dplyr::if_else(Shapiro.p > 0.05, "Sí", "No"),
            .groups      = "drop"
          )
        
        writeData(wb_tests, sheet, "Normalidad",
                  startRow = 1, startCol = 1,
                  headerStyle = createStyle(textDecoration = "bold"))
        writeData(wb_tests, sheet, norm_tbl,
                  startRow = 2, startCol = 1,
                  headerStyle = createStyle(textDecoration = "bold"))
        
        ## ► helpers ───────────────────────────────────────────────────────────
        do_anova <- function(df){
          aovm <- aov(Valor ~ Label, data = df)
          switch(postHoc_sel,
                 "Tukey"      = broom::tidy(TukeyHSD(aovm)),
                 "Bonferroni" = rstatix::pairwise_t_test(df, Valor ~ Label,
                                                         p.adjust.method = "bonferroni"),
                 "Sidak"      = rstatix::pairwise_t_test(df, Valor ~ Label,
                                                         p.adjust.method = "sidak"),
                 "Dunnett"    = dunnett_to_tibble(
                   DescTools::DunnettTest(Valor ~ Label,
                                          data = set_control(df, controlGroup_sel))),
                 "Scheffe"    = pmcmr_to_tibble(PMCMRplus::scheffeTest(aovm, "Label")),
                 "GamesHowell"= rstatix::games_howell_test(df, Valor ~ Label)
          )
        }
        do_kw <- function(df){
          switch(postHoc_sel,
                 "Dunn"    = rstatix::dunn_test(df, Valor ~ Label, p.adjust.method = "bonferroni"),
                 "Conover" = pmcmr_to_tibble(PMCMRplus::kwAllPairsConoverTest(df$Valor, df$Label)),
                 "Nemenyi" = pmcmr_to_tibble(PMCMRplus::kwAllPairsNemenyiTest(df$Valor, df$Label)),
                 "DSCF"    = {
                   f <- if (exists("kwAllPairsDSCFTest", asNamespace("PMCMRplus"), FALSE))
                     PMCMRplus::kwAllPairsDSCFTest
                   else
                     PMCMRplus::kwAllPairsDscfTest
                   pmcmr_to_tibble(f(df$Valor, df$Label))
                 }
          )
        }
        ## ► cálculo ────────────────────────────────────────────────────────────
        sig_raw <- switch(sigTest_sel,
                          "ANOVA"          = do_anova(df_param),
                          "Kruskal–Wallis" = do_kw(df_param),
                          "ttest" = {
                            if (compMode_sel == "all"){
                              safe_pairwise_t(df_param, "holm")
                            } else if (compMode_sel == "control"){
                              rstatix::t_test(df_param, Valor ~ Label, ref.group = controlGroup_sel)
                            } else {
                              grupos <- c(group1_sel, group2_sel)
                              sub <- df_param |> dplyr::filter(Label %in% grupos) |> droplevels()
                              counts <- table(sub$Label)
                              faltantes <- c(setdiff(grupos, names(counts)),
                                             names(counts[counts < 2]))
                              if (length(faltantes) > 0) {
                                showNotification(paste("Grupos con observaciones insuficientes:",
                                                       paste(faltantes, collapse = ", ")),
                                                 type = "error", duration = 5)
                                tibble()
                              } else {
                                rstatix::t_test(sub, Valor ~ Label, paired = can_paired(sub))
                              }
                            }
                          },
                          "wilcox" = {
                            if (compMode_sel == "all"){
                              safe_pairwise_wilcox(df_param, "holm")
                            } else if (compMode_sel == "control"){
                              rstatix::wilcox_test(df_param, Valor ~ Label, ref.group = controlGroup_sel)
                            } else {
                              grupos <- c(group1_sel, group2_sel)
                              sub <- df_param |> dplyr::filter(Label %in% grupos) |> droplevels()
                              faltantes <- setdiff(grupos, names(table(sub$Label)))
                              if (length(faltantes) > 0) {
                                showNotification(paste("Grupos con observaciones insuficientes:",
                                                       paste(faltantes, collapse = ", ")),
                                                 type = "error", duration = 5)
                                tibble()
                              } else {
                                rstatix::wilcox_test(sub, Valor ~ Label, paired = can_paired(sub))
                              }
                            }
                          }
        )
        
        # armoniza columnas de comparación
        if ("comparison" %in% names(sig_raw)){
          cmp <- split_comparison(sig_raw$comparison)
          sig_raw$group1 <- cmp[,1]; sig_raw$group2 <- cmp[,2]
        }
        if (all(c("grupo1","grupo2") %in% names(sig_raw))){
          sig_raw$group1 <- sig_raw$grupo1
          sig_raw$group2 <- sig_raw$grupo2
        }
        
        # identifica la columna de p-value
        p_candidates <- intersect(
          c("p","p.value","p.adj","adj.p.value","p_val","p.value.adj"),
          names(sig_raw)
        )
        pcol <- p_candidates[1]
        
        sig_tbl <- sig_raw |>
          dplyr::mutate(
            P_valor       = .data[[pcol]],
            Significativo = dplyr::if_else(P_valor < 0.05, "Sí", "No"),
            Estrellas     = dplyr::case_when(
              P_valor < 0.001 ~ "***",
              P_valor < 0.01  ~ "**",
              P_valor < 0.05  ~ "*",
              TRUE            ~ ""
            )
          )
        
        # ------------------- ESCRITURA EN LA HOJA ------------------------------
        start <- nrow(norm_tbl) + 4
        
        writeData(wb_tests, sheet,
                  paste("Test usado:", sigTest_sel),
                  startRow = start - 1, startCol = 1,
                  headerStyle = createStyle(textDecoration = "bold"))
        
        if (sigTest_sel %in% c("ANOVA","Kruskal–Wallis")){
          writeData(wb_tests, sheet,
                    paste("Post-hoc:", postHoc_sel),
                    startRow = start, startCol = 1,
                    headerStyle = createStyle(textDecoration = "bold"))
          sig_header_row <- start + 1
        } else {
          sig_header_row <- start
        }
        
        writeData(wb_tests, sheet, "Significancia",
                  startRow = sig_header_row, startCol = 1,
                  headerStyle = createStyle(textDecoration = "bold"))
        writeData(wb_tests, sheet, sig_tbl,
                  startRow = sig_header_row + 1, startCol = 1,
                  headerStyle = createStyle(textDecoration = "bold"))
      } # fin for(param)
      
      saveWorkbook(wb_tests, file, overwrite = TRUE)
  }

  output$downloadStats <- downloadHandler(
    filename = function() "Tests_estadisticos.xlsx",
    content  = download_stats_content
  )

  observeEvent(input$save_bundle_version, {
    req(input$dataFile)
    req(datos_combinados(), datos_agrupados())

    plot_raw <- capture_to_raw(".png", write_current_plot_png)
    if (length(plot_raw) == 0) {
      showNotification("No se pudo capturar el gráfico actual.", type = "error", duration = 5)
      return()
    }
    plot_pdf_raw <- capture_to_raw(".pdf", write_current_plot_pdf)
    if (length(plot_pdf_raw) == 0) {
      showNotification("No se pudo generar el PDF del gráfico.", type = "error", duration = 5)
      return()
    }
    metadata_raw <- capture_to_raw(".xlsx", write_metadata_xlsx)
    if (length(metadata_raw) == 0) {
      showNotification("No se pudo capturar la metadata.", type = "error", duration = 5)
      return()
    }

    comb_df <- datos_combinados()
    agru_df <- datos_agrupados()

    dataset_name <- input$dataFile$name %||% "dataset"
    comb_raw <- df_to_csv_raw(comb_df)
    dataset_key <- md5_raw(comb_raw)
    if (is.null(dataset_key)) {
      dataset_key <- paste0("dataset_", format(Sys.time(), "%Y%m%d%H%M%S"))
    }
    current_dataset_key(dataset_key)

    stats_try  <- try(capture_to_raw(".xlsx", download_stats_content), silent = TRUE)
    stats_raw  <- if (!inherits(stats_try, "try-error")) stats_try else NULL
    stats_hash <- md5_raw(stats_raw)

    ensure_dataset_record(
      dataset_key   = dataset_key,
      dataset_name  = dataset_name,
      datos_comb    = comb_df,
      datos_agru    = agru_df,
      comb_raw      = comb_raw,
      stats_hash    = stats_hash,
      stats_raw     = stats_raw
    )

    label_input <- input$bundle_label %||% ""
    dir_label <- sanitize(label_input)
    if (!nzchar(dir_label)) dir_label <- sanitize(input$tipo)
    existing_versions <- bundle_store$versions
    type_label <- sanitize(input$tipo %||% "plot")
    version_idx <- if (length(existing_versions)) {
      sum(vapply(existing_versions, function(v) identical(v$tipo, input$tipo), logical(1))) + 1
    } else {
      1
    }
    if (!nzchar(dir_label)) dir_label <- paste0("version_", type_label, "_", version_idx)

    timestamp <- Sys.time()
    scope_sel <- if (input$scope == "Combinado") "Combinado" else "Por Cepa"
    strain_val <- if (scope_sel == "Por Cepa") (input$strain %||% "") else ""
    version_suffix <- paste0("_", version_idx)
    plot_png_name <- paste0("grafico_", type_label, version_suffix, ".png")
    plot_pdf_name <- paste0("grafico_", type_label, version_suffix, ".pdf")
    metadata_name <- paste0("metadata_", type_label, version_suffix, ".xlsx")

    version_record <- list(
      id            = paste0("v_", format(timestamp, "%Y%m%d%H%M%S")),
      dataset       = dataset_key,
      dataset_name  = dataset_name,
      label_input   = label_input,
      dir_label     = dir_label,
      created       = timestamp,
      scope         = scope_sel,
      strain        = strain_val,
      tipo          = input$tipo,
      width         = input$plot_w,
      height        = input$plot_h,
      plot_raw      = plot_raw,
      plot_png_name = plot_png_name,
      plot_pdf_raw  = plot_pdf_raw,
      plot_pdf_name = plot_pdf_name,
      metadata_raw  = metadata_raw,
      metadata_name = metadata_name,
      stats_hash    = stats_hash
    )

    bundle_store$versions <- c(bundle_store$versions, list(version_record))
    updateTextInput(session, "bundle_label", value = "")
    showNotification("Versión guardada en el paquete.", type = "message", duration = 4)
  })

  observe({
    if (length(bundle_store$versions) > 0) {
      shinyjs::enable("downloadBundleZip")
    } else {
      shinyjs::disable("downloadBundleZip")
    }
  })

  output$downloadBundleZip <- downloadHandler(
    filename = function(){
      paste0("BIOSZEN_paquete_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".zip")
    },
    content = function(file){
      versions <- bundle_store$versions
      req(length(versions) > 0)

      tmpdir <- tempfile("bioszen_bundle_")
      dir.create(tmpdir, recursive = TRUE)
      datasets_dir  <- file.path(tmpdir, "datasets");  dir.create(datasets_dir, recursive = TRUE)
      versions_dir  <- file.path(tmpdir, "versiones"); dir.create(versions_dir, recursive = TRUE)

      used_dataset_keys <- unique(vapply(versions, function(v) v$dataset, character(1)))
      dataset_dirs <- list()

      for (ds_key in used_dataset_keys) {
        ds <- bundle_store$datasets[[ds_key]]
        if (is.null(ds)) next
        ds_dir_name <- paste0(format(ds$created, "%Y%m%d-%H%M%S"), "_", ds$dir_label)
        dataset_dirs[[ds_key]] <- file.path("datasets", ds_dir_name)
        ds_dir <- file.path(datasets_dir, ds_dir_name)
        dir.create(ds_dir, recursive = TRUE, showWarnings = FALSE)

        info_lines <- c(
          paste("Nombre original:", ds$name),
          paste("Identificador:", ds$key),
          paste("Creado:", format(ds$created, "%Y-%m-%d %H:%M:%S"))
        )
        writeLines(info_lines, file.path(ds_dir, "INFO.txt"))

        for (entry in ds$files) {
          writeBin(entry$raw, file.path(ds_dir, entry$name))
        }

        if (!is.null(ds$stats) && length(ds$stats)) {
          stats_dir <- file.path(ds_dir, "estadisticas")
          dir.create(stats_dir, showWarnings = FALSE)
          for (hash in names(ds$stats)) {
            entry <- ds$stats[[hash]]
            writeBin(entry$raw, file.path(stats_dir, entry$name))
            info_path <- file.path(
              stats_dir,
              paste0(tools::file_path_sans_ext(entry$name), "_INFO.txt")
            )
            writeLines(
              c(
                paste("Hash:", hash),
                paste("Creado:", format(entry$created, "%Y-%m-%d %H:%M:%S"))
              ),
              info_path
            )
          }
        }
      }

      for (v in versions) {
        ds_ref <- bundle_store$datasets[[v$dataset]]
        dir_label <- if (nzchar(v$dir_label)) v$dir_label else "version"
        ver_dir_name <- paste0(format(v$created, "%Y%m%d-%H%M%S"), "_", dir_label)
        ver_dir <- file.path(versions_dir, ver_dir_name)
        dir.create(ver_dir, recursive = TRUE, showWarnings = FALSE)

        png_name <- v$plot_png_name %||% "grafico.png"
        writeBin(v$plot_raw, file.path(ver_dir, png_name))

        if (!is.null(v$plot_pdf_raw) && length(v$plot_pdf_raw) > 0) {
          pdf_name <- v$plot_pdf_name %||% sub("\\.png$", ".pdf", png_name)
          writeBin(v$plot_pdf_raw, file.path(ver_dir, pdf_name))
        }

        meta_name <- v$metadata_name %||% "metadata.xlsx"
        writeBin(v$metadata_raw, file.path(ver_dir, meta_name))

        dataset_ref <- dataset_dirs[[v$dataset]] %||% ""
        stats_ref <- NULL
        if (!is.null(v$stats_hash) && !is.null(ds_ref) && !is.null(ds_ref$stats[[v$stats_hash]])) {
          stats_entry <- ds_ref$stats[[v$stats_hash]]
          stats_ref <- file.path(dataset_ref, "estadisticas", stats_entry$name)
        }

        info_lines <- c(
          paste("Etiqueta:", if (nzchar(v$label_input)) v$label_input else "(sin etiqueta)"),
          paste("Tipo:", v$tipo),
          paste("Ámbito:", v$scope)
        )
        if (nzchar(v$strain)) {
          info_lines <- c(info_lines, paste("Cepa:", v$strain))
        }
        info_lines <- c(
          info_lines,
          paste("Dimensiones:", sprintf("%d x %d px", v$width, v$height))
        )
        if (nzchar(dataset_ref)) {
          info_lines <- c(info_lines, paste("Dataset asociado:", dataset_ref))
        }
        if (!is.null(stats_ref)) {
          info_lines <- c(info_lines, paste("Estadísticas:", stats_ref))
        }
        writeLines(info_lines, file.path(ver_dir, "INFO.txt"))
      }

      old_wd <- getwd()
      on.exit({
        setwd(old_wd)
        unlink(tmpdir, recursive = TRUE, force = TRUE)
      }, add = TRUE)

      setwd(tmpdir)
      zip::zipr(zipfile = file, files = list.files(".", recursive = TRUE))
    }
  )
  
  output$showImportBtn <- renderUI({
    req(input$growthFiles)
    if (length(input$growthFiles$name) == 1)
      actionButton("importToPlots", "Importar a Gráficos & Stats", class = "btn btn-primary")
    else
      NULL
  })


  # --- Helper: aplicar metadata cargada en los inputs ---
  apply_metadata <- function(meta){
    get_val <- function(campo){
      v <- meta$Valor[meta$Campo == campo]
      if (length(v)) v else NULL
    }

    if (!is.null(v <- get_val("scope")))      updateRadioButtons(session, "scope",      selected = v)
    if (!is.null(v <- get_val("colorMode")))  updateSelectInput(session,  "colorMode",  selected = v)
    if (!is.null(v <- get_val("plot_w")))     updateNumericInput(session, "plot_w",     value = as.numeric(v))
    if (!is.null(v <- get_val("plot_h")))     updateNumericInput(session, "plot_h",     value = as.numeric(v))
    if (!is.null(v <- get_val("base_size")))   updateNumericInput(session, "base_size",   value = as.numeric(v))
    if (!is.null(v <- get_val("fs_title")))   updateNumericInput(session, "fs_title",   value = as.numeric(v))
    if (!is.null(v <- get_val("fs_axis")))    updateNumericInput(session, "fs_axis",    value = as.numeric(v))
    if (!is.null(v <- get_val("fs_legend")))  updateNumericInput(session, "fs_legend",  value = as.numeric(v))
    if (!is.null(v <- get_val("axis_line_size"))) updateNumericInput(session, "axis_line_size", value = as.numeric(v))
    if (!is.null(v <- get_val("pt_size")))     updateNumericInput(session, "pt_size",     value = as.numeric(v))
    if (!is.null(v <- get_val("x_angle")))     updateNumericInput(session, "x_angle",     value = as.numeric(v))
    if (!is.null(v <- get_val("x_wrap")))      updateCheckboxInput(session, "x_wrap", value = tolower(v) == "true")
    if (!is.null(v <- get_val("x_wrap_lines")))updateNumericInput(session, "x_wrap_lines", value = as.numeric(v))
    if (!is.null(v <- get_val("pt_jit")))      updateNumericInput(session, "pt_jit",      value = as.numeric(v))
    if (!is.null(v <- get_val("box_w")))      updateNumericInput(session, "box_w",      value = as.numeric(v))
    if (!is.null(v <- get_val("violin_width")))     updateNumericInput(session, "violin_width",     value = as.numeric(v))
    if (!is.null(v <- get_val("violin_linewidth"))) updateNumericInput(session, "violin_linewidth", value = as.numeric(v))
    if (!is.null(v <- get_val("curve_lwd")))   updateNumericInput(session, "curve_lwd",   value = as.numeric(v))
    if (!is.null(v <- get_val("sig_linewidth")))updateNumericInput(session, "sig_linewidth", value = as.numeric(v))
    if (!is.null(v <- get_val("sig_textsize"))) updateNumericInput(session, "sig_textsize", value = as.numeric(v))
    if (!is.null(v <- get_val("sig_sep")))      updateNumericInput(session, "sig_sep",      value = as.numeric(v))
    if (!is.null(v <- get_val("sig_textpad")))  updateNumericInput(session, "sig_textpad",  value = as.numeric(v))
    if (!is.null(v <- get_val("param")))      updateSelectInput(session, "param",      selected = v)
    if (!is.null(v <- get_val("doNorm")))     updateCheckboxInput(session, "doNorm",    value = tolower(v) == "true")
    if (!is.null(v <- get_val("ctrlMedium"))) updateSelectInput(session, "ctrlMedium", selected = if (v == "NULL") character(0) else v)
    if (!is.null(v <- get_val("errbar_size")))updateNumericInput(session, "errbar_size", value = as.numeric(v))
    if (!is.null(v <- get_val("ymax")))       updateNumericInput(session, "ymax",       value = as.numeric(v))
    if (!is.null(v <- get_val("ybreak")))     updateNumericInput(session, "ybreak",     value = as.numeric(v))
    if (!is.null(v <- get_val("stackParams"))){
      updateCheckboxGroupInput(session, "stackParams", selected = strsplit(v, ",")[[1]])
    }
    if (!is.null(v <- get_val("orderStack"))) updateTextInput(session, "orderStack", value = v)
    if (!is.null(v <- get_val("showErrBars")))updateCheckboxInput(session, "showErrBars", value = tolower(v) == "true")
    if (!is.null(v <- get_val("xmax_cur")))   updateNumericInput(session, "xmax_cur",   value = as.numeric(v))
    if (!is.null(v <- get_val("xbreak_cur"))) updateNumericInput(session, "xbreak_cur", value = as.numeric(v))
    if (!is.null(v <- get_val("ymax_cur")))   updateNumericInput(session, "ymax_cur",   value = as.numeric(v))
    if (!is.null(v <- get_val("ybreak_cur"))) updateNumericInput(session, "ybreak_cur", value = as.numeric(v))
  }

  # --- Corregir descarga de Metadata ---
  output$downloadMetadata <- downloadHandler(
    filename = function(){
      paste0("metadata_", input$tipo, ".xlsx")
    },
    content  = function(file){
      write_metadata_xlsx(file)
    }
  )

  # --- Carga de metadata de diseño -----------------------------------
  observeEvent(input$metaFiles, {
    req(input$metaFiles)
    valid_types <- c("Boxplot", "Barras", "Violin", "Curvas", "Apiladas", "Correlación")
    for (i in seq_len(nrow(input$metaFiles))) {
      path  <- input$metaFiles$datapath[i]
      fname <- input$metaFiles$name[i]
      tipo  <- sub("^metadata_", "", sub("\\.xlsx$", "", fname))

      # Validación de nombre
      if (!startsWith(fname, "metadata_") || !tipo %in% valid_types) {
        showNotification(
          paste("❌ Nombre inválido:", fname,
                "Debe llamarse metadata_<Tipo>.xlsx (ej. metadata_Boxplot.xlsx)"),
          type = "error", duration = 6
        )
        next
      }

      # Intentar lectura de la hoja 'Metadata' de forma robusta
      meta <- tryCatch(read_excel_tmp(path, sheet = "Metadata"),
                       error = function(e) NULL)
      if (is.null(meta)) {
        showNotification(paste("❌ Estructura inválida en", fname, ": no se pudo leer la hoja 'Metadata'."),
                         type = "error", duration = 7)
        next
      }

      # Validación de columnas mínimas requeridas
      required_cols <- c("Campo", "Valor")
      if (!all(required_cols %in% names(meta))) {
        showNotification(
          paste0("❌ Estructura inválida en ", fname,
                 ": la hoja 'Metadata' debe contener las columnas 'Campo' y 'Valor'."),
          type = "error", duration = 7
        )
        next
      }

      # Si pasa validaciones, almacenar y aplicar si corresponde
      meta_store[[tipo]] <- meta
      if (identical(tipo, input$tipo)) apply_metadata(meta)
    }
  }, ignoreNULL = TRUE)

  observeEvent(input$tipo, {
    meta <- meta_store[[input$tipo]]
    if (!is.null(meta)) apply_metadata(meta)
  })

  # 5.1 PNG
  output$dl_combo_png <- downloadHandler(
    filename = function() "combo.png",
    content  = function(file){
      ggsave(
        file, combo_plot(),
        width  = input$combo_width  / 100,
        height = input$combo_height / 100,
        dpi    = 300, bg = "white"
      )
    }
  )
  
  # 5.2 PPTX vectorial editable
  output$dl_combo_pptx <- downloadHandler(
    filename = function() "combo.pptx",
    content  = function(file){
      library(officer); library(rvg)
      doc <- read_pptx()
      doc <- add_slide(doc, layout = "Title and Content",
                       master = "Office Theme") |>
        ph_with(dml(ggobj = combo_plot()),
                location = ph_location_fullsize())
      print(doc, target = file)
    }
  )

  # 5.3 PDF
  output$dl_combo_pdf <- downloadHandler(
    filename = function() "combo.pdf",
    content  = function(file){
      ggsave(
        file, combo_plot(),
        width  = input$combo_width  / 100,
        height = input$combo_height / 100,
        device = cairo_pdf,
        bg     = "white"
      )
    }
  )
  
  observeEvent(input$importToPlots, {
    # 0) Asegurarnos de que ya hay datos cargados en Gráficos & Stats
    if (is.null(datos_box())) {
      showNotification(
        "Primero carga el archivo de metadata-parametros en la pestaña Gráficos & Stats.",
        type = "error", duration = 5
      )
      return()
    }
    # 1) Localiza los dos archivos que generó runGrowth
    curvas_f <- list.files(growth_out_dir, pattern = "^Curvas_.*\\.xlsx$", full.names = TRUE)
    params_f <- list.files(growth_out_dir, pattern = "^Parametros_.*\\.xlsx$", full.names = TRUE)
    
    # Validación
    if (length(curvas_f) != 1 || length(params_f) != 1) {
      showNotification("Necesitas generar primero un archivo de curvas y uno de parámetros.",
                       type = "error")
      return()
    }
    
    # 2) Leer Sheet1 y Sheet2 del Excel de curvas
    curvas     <- read_excel_tmp(curvas_f, sheet = "Sheet1")
    curvas_cfg <- read_excel_tmp(curvas_f, sheet = "Sheet2")
    params      <- readxl::read_excel(params_f, sheet = "Resultados Combinados")
    
    params <- params %>% dplyr::select(where(~ !all(is.na(.x))))
    
    # 3) Guardar ambos reactivos
    cur_data_box(curvas)
    cur_cfg_box(curvas_cfg)
    
    df0 <- datos_box() %>%
      dplyr::select(where(~ !all(is.na(.x))))
    
    # 4) Hacer el join de parámetros sobre los datos originales
    df1 <- df0 %>% left_join(params, by = "Well")
    datos_box(df1)
    # 5) Sincroniza PlotSettings con los parámetros que SÍ existen
    plot_cfg_box(
      plot_cfg_box() %>%
        filter(Parameter %in% names(df1))          # descarta los que ya no existen
    )
    
    new_params <- plot_cfg_box()$Parameter
    if (!is.null(input$param) && !input$param %in% new_params) {
      updateSelectInput(session, "param",
                        choices  = new_params,
                        selected = new_params[1])
    }
    
    # 5) Refrescar todos los inputs que dependen de plot_cfg_box()
    updateCheckboxGroupInput(session, "stackParams",
                             choices  = plot_cfg_box()$Parameter,
                             selected = plot_cfg_box()$Parameter)
    updateSelectInput(session, "param",
                      choices  = plot_cfg_box()$Parameter,
                      selected = plot_cfg_box()$Parameter[1])
    updateSelectInput(session, "corr_param_x",
                      choices  = plot_cfg_box()$Parameter,
                      selected = plot_cfg_box()$Parameter[1])
    updateSelectInput(session, "corr_param_y",
                      choices  = plot_cfg_box()$Parameter,
                      selected = plot_cfg_box()$Parameter[min(2, length(plot_cfg_box()$Parameter))])
    
    # 6) Volvemos a la pestaña de resultados
    updateTabsetPanel(session, "mainTabs", selected = "Gráficos & Stats")
  })
  
}  
