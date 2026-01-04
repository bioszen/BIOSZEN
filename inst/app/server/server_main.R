# --- Server logic ---
active_sessions <- 0

# Announcement banner helpers (reads www/announcement.dcf)
announcement_file_path <- function() {
  env_path <- Sys.getenv("BIOSZEN_ANNOUNCEMENT_FILE", "")
  if (nzchar(env_path)) return(env_path)
  file.path("www", "announcement.dcf")
}

escape_js_string <- function(x) {
  x <- gsub("\\\\", "\\\\\\\\", x)
  gsub("'", "\\\\'", x, fixed = TRUE)
}

format_announcement_body <- function(text) {
  if (is.null(text) || is.na(text) || !nzchar(text)) return(NULL)
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  parts <- vector("list", length(lines) * 2 - 1)
  idx <- 1
  for (i in seq_along(lines)) {
    parts[[idx]] <- lines[[i]]
    idx <- idx + 1
    if (i < length(lines)) {
      parts[[idx]] <- tags$br()
      idx <- idx + 1
    }
  }
  do.call(tagList, parts)
}

read_announcement_dcf <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) return(NULL)

  raw <- tryCatch(read.dcf(path), error = function(e) NULL)
  if (is.null(raw) || !nrow(raw)) return(NULL)

  vals <- as.list(raw[1, , drop = FALSE])
  names(vals) <- tolower(names(vals))

  get_field <- function(name, default = "") {
    value <- vals[[name]]
    if (is.null(value)) return(default)
    value <- trimws(as.character(value))
    if (!nzchar(value)) return(default)
    value
  }

  enabled_raw <- tolower(get_field("enabled", "true"))
  if (!enabled_raw %in% c("true", "1", "yes", "y")) return(NULL)

  title <- get_field("title")
  body <- get_field("body")
  link_url <- get_field("link_url")
  if (!nzchar(title) && !nzchar(body) && !nzchar(link_url)) return(NULL)

  id <- get_field("id")
  if (!nzchar(id)) {
    id <- unname(as.character(tools::md5sum(path)))
  }
  if (!nzchar(id)) id <- format(Sys.Date(), "%Y%m%d")

  type <- tolower(get_field("type", "info"))
  valid_types <- c("primary", "secondary", "success", "danger",
                   "warning", "info", "light", "dark")
  if (!type %in% valid_types) type <- "info"

  parse_time <- function(x) {
    if (!nzchar(x)) return(NA)
    out <- suppressWarnings(as.POSIXct(x, tz = "UTC"))
    if (!is.na(out)) return(out)
    out_date <- suppressWarnings(as.Date(x))
    if (!is.na(out_date)) return(as.POSIXct(out_date, tz = "UTC"))
    NA
  }

  start_at <- parse_time(get_field("start"))
  end_at <- parse_time(get_field("end"))
  now <- Sys.time()
  if (!is.na(start_at) && now < start_at) return(NULL)
  if (!is.na(end_at) && now > end_at) return(NULL)

  list(
    id = id,
    type = type,
    title = title,
    body = body,
    link_text = get_field("link_text"),
    link_url = link_url
  )
}

remote_announcement_base <- function() {
  env_path <- Sys.getenv("BIOSZEN_ANNOUNCEMENT_BASE", "")
  if (nzchar(env_path)) return(env_path)
  "https://raw.githubusercontent.com/bioszen/BIOSZEN-Announcements/refs/heads/main/announcements/index.dcf"
}

get_dcf_remote <- function(url, timeout = 10) {
  tmp <- tempfile(fileext = ".dcf")
  old_timeout <- getOption("timeout")
  on.exit({
    options(timeout = old_timeout)
    if (file.exists(tmp)) unlink(tmp)
  }, add = TRUE)
  options(timeout = timeout)

  read_from_dcf <- function(dcf_obj) {
    if (is.null(dcf_obj) || !nrow(dcf_obj)) return(NULL)
    out <- as.list(dcf_obj[1, , drop = TRUE])
    nms <- tolower(names(out))
    nms <- sub("^\\?+", "", nms)
    names(out) <- nms
    out
  }

  try_download <- function(method = NULL) {
    if (file.exists(tmp)) unlink(tmp)
    status <- tryCatch(
      suppressWarnings(utils::download.file(
        url,
        destfile = tmp,
        quiet = TRUE,
        mode = "wb",
        method = method
      )),
      error = function(e) e
    )
    if (inherits(status, "error")) return(NULL)
    if (!identical(status, 0L)) return(NULL)
    if (!file.exists(tmp) || is.na(file.info(tmp)$size) || file.info(tmp)$size <= 0) return(NULL)
    d <- tryCatch(read.dcf(tmp), error = function(e) NULL)
    read_from_dcf(d)
  }

  out <- NULL
  if (capabilities("libcurl")) out <- try_download("libcurl")
  if (is.null(out)) out <- try_download("auto")
  if (is.null(out)) out <- try_download("wininet")
  if (is.null(out)) {
    lines <- tryCatch(readLines(url, warn = FALSE), error = function(e) NULL)
    if (!is.null(lines) && length(lines)) {
      con <- textConnection(lines)
      on.exit(close(con), add = TRUE)
      d <- tryCatch(read.dcf(con), error = function(e) NULL)
      out <- read_from_dcf(d)
    }
  }
  out
}

announcements_state_path <- function(pkg = "BIOSZEN") {
  dir <- tryCatch(tools::R_user_dir(pkg, "config"), error = function(e) NULL)
  if (is.null(dir)) dir <- file.path(path.expand("~"), ".bioszen")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  file.path(dir, "seen_announcements.rds")
}

has_seen_announcement <- function(id, pkg = "BIOSZEN") {
  p <- announcements_state_path(pkg)
  if (!file.exists(p)) return(FALSE)
  seen <- tryCatch(readRDS(p), error = function(e) character())
  id %in% seen
}

mark_seen_announcement <- function(id, pkg = "BIOSZEN") {
  p <- announcements_state_path(pkg)
  seen <- if (file.exists(p)) tryCatch(readRDS(p), error = function(e) character()) else character()
  saveRDS(unique(c(seen, id)), p)
}

should_show_announcement <- function(a, current_version = NULL) {
  if (is.null(a$id) || is.na(a$id)) return(FALSE)
  id_val <- trimws(as.character(a$id))
  if (!nzchar(id_val)) return(FALSE)

  if (!is.null(a$enabled) && !is.na(a$enabled)) {
    enabled_val <- tolower(trimws(as.character(a$enabled)))
    if (nzchar(enabled_val) && enabled_val %in% c("false", "0", "no", "n")) return(FALSE)
  }

  if (!is.null(a$start) && !is.na(a$start)) {
    start_val <- trimws(as.character(a$start))
    if (nzchar(start_val) && Sys.Date() < as.Date(start_val)) return(FALSE)
  }
  if (!is.null(a$end) && !is.na(a$end)) {
    end_val <- trimws(as.character(a$end))
    if (nzchar(end_val) && Sys.Date() > as.Date(end_val)) return(FALSE)
  }

  if (!is.null(current_version) && nzchar(current_version)) {
    if (!is.null(a$min_version) && !is.na(a$min_version)) {
      min_ver <- trimws(as.character(a$min_version))
      if (nzchar(min_ver) && utils::compareVersion(current_version, min_ver) < 0) return(FALSE)
    }
    if (!is.null(a$max_version) && !is.na(a$max_version)) {
      max_ver <- trimws(as.character(a$max_version))
      if (nzchar(max_ver) && utils::compareVersion(current_version, max_ver) > 0) return(FALSE)
    }
  }

  TRUE
}

fetch_latest_announcement <- function(base_raw) {
  base_raw <- trimws(as.character(base_raw %||% ""))
  if (!nzchar(base_raw)) return(NULL)

  idx_url <- if (grepl("/announcements/index\\.dcf$", base_raw, ignore.case = TRUE)) {
    base_raw
  } else {
    paste0(base_raw, "/announcements/index.dcf")
  }

  base_root <- if (grepl("/announcements/index\\.dcf$", base_raw, ignore.case = TRUE)) {
    sub("/announcements/index\\.dcf$", "", base_raw, ignore.case = TRUE)
  } else {
    base_raw
  }

  idx <- get_dcf_remote(idx_url)
  if (is.null(idx)) return(NULL)
  latest <- idx$latest
  if (is.null(latest) || is.na(latest)) return(NULL)
  latest <- trimws(as.character(latest))
  if (!nzchar(latest)) return(NULL)

  item_url <- paste0(base_root, "/announcements/items/", latest, ".dcf")
  ann <- get_dcf_remote(item_url)
  if (is.null(ann)) return(NULL)

  if (is.null(ann$id) || is.na(ann$id) || !nzchar(ann$id)) ann$id <- latest
  ann$id <- trimws(as.character(ann$id))
  ann
}

parse_seen_ids <- function(raw) {
  if (is.null(raw)) return(character())
  raw <- as.character(raw)
  if (!length(raw) || is.na(raw) || !nzchar(raw)) return(character())
  out <- unlist(strsplit(raw, "\\|"))
  out <- trimws(out)
  out[!is.na(out) & nzchar(out)]
}

build_seen_ids <- function(ids) {
  ids <- unique(trimws(as.character(ids)))
  ids <- ids[!is.na(ids) & nzchar(ids)]
  paste(ids, collapse = "|")
}

is_local_session <- function(session) {
  host <- tolower(session$clientData$url_hostname %||% "")
  host %in% c("127.0.0.1", "localhost", "::1")
}

get_current_version <- function(pkg = "BIOSZEN") {
  out <- tryCatch(as.character(utils::packageVersion(pkg)), error = function(e) "")
  if (!nzchar(out)) {
    desc_path <- "DESCRIPTION"
    if (file.exists(desc_path)) {
      d <- tryCatch(read.dcf(desc_path), error = function(e) NULL)
      if (!is.null(d) && nrow(d) && "Version" %in% colnames(d)) {
        out <- as.character(d[1, "Version"])
      }
    }
  }
  if (!nzchar(out)) NULL else out
}

server <- function(input, output, session) {

  active_sessions <<- active_sessions + 1
  session$onSessionEnded(function() {
    active_sessions <<- max(0, active_sessions - 1)
    if (active_sessions == 0) shiny::stopApp()
  })

  announcement_data <- shiny::reactiveFileReader(
    intervalMillis = 10000,
    session = session,
    filePath = announcement_file_path,
    readFunc = read_announcement_dcf
  )

  output$app_announcement <- renderUI({
    ann <- announcement_data()
    req(!is.null(ann))

    seen <- input$announcement_seen
    if (!is.null(seen) && nzchar(seen) && identical(seen, ann$id)) return(NULL)

    id_js <- escape_js_string(ann$id)
    close_js <- sprintf(
      "localStorage.setItem('announcementSeen', '%s'); Shiny.setInputValue('announcement_seen', '%s', {priority: 'event'});",
      id_js, id_js
    )

    title_node <- if (nzchar(ann$title)) {
      tags$div(tags$strong(ann$title))
    } else {
      NULL
    }
    body_node <- if (nzchar(ann$body)) {
      tags$div(format_announcement_body(ann$body))
    } else {
      NULL
    }
    link_label <- ann$link_text
    if (!nzchar(link_label)) link_label <- ann$link_url
    link_node <- if (nzchar(ann$link_url)) {
      tags$div(tags$a(link_label, href = ann$link_url,
                      target = "_blank", rel = "noopener"))
    } else {
      NULL
    }

    tags$div(
      style = "margin: 0 12px 10px 12px;",
      tags$div(
        class = paste0("alert alert-", ann$type, " alert-dismissible fade show"),
        role = "alert",
        title_node,
        body_node,
        link_node,
        tags$button(
          type = "button",
          class = "btn-close",
          `data-bs-dismiss` = "alert",
          `aria-label` = "Close",
          onclick = close_js
        )
      )
    )
  })
  
  remote_announcement_storage_key <- "bioszenRemoteAnnouncementsSeen"
  observeEvent(input$remote_announcement_payload, {
    payload <- input$remote_announcement_payload
    if (is.null(payload) || !is.list(payload)) return()

    ann <- payload
    ann$id <- trimws(as.character(ann$id %||% ""))
    if (!nzchar(ann$id)) return()

    current_version <- get_current_version("BIOSZEN")
    if (!should_show_announcement(ann, current_version)) return()

    seen_ids <- parse_seen_ids(input$remote_announcement_seen)
    if (ann$id %in% seen_ids) return()
    if (is_local_session(session) && has_seen_announcement(ann$id, pkg = "BIOSZEN")) return()

    title_text <- trimws(as.character(ann$title %||% ""))
    message_text <- trimws(as.character(ann$message %||% ann$body %||% ""))
    url_val <- trimws(as.character(ann$url %||% ann$link_url %||% ""))

    if (!nzchar(title_text) && !nzchar(message_text) && !nzchar(url_val)) return()

    showModal(modalDialog(
      title = if (nzchar(title_text)) title_text else "Aviso",
      if (nzchar(message_text)) tags$div(format_announcement_body(message_text)) else NULL,
      if (nzchar(url_val)) {
        tags$p(tags$a("Mas info", href = url_val, target = "_blank", rel = "noopener"))
      } else {
        NULL
      },
      easyClose = TRUE,
      footer = tagList(modalButton("Cerrar"))
    ))

    new_seen <- build_seen_ids(c(seen_ids, ann$id))
    seen_js <- escape_js_string(new_seen)
    shinyjs::runjs(sprintf(
      "localStorage.setItem('%s', '%s'); Shiny.setInputValue('remote_announcement_seen', '%s', {priority: 'event'});",
      remote_announcement_storage_key, seen_js, seen_js
    ))

    if (is_local_session(session)) {
      mark_seen_announcement(ann$id, pkg = "BIOSZEN")
    }
  }, ignoreInit = TRUE)

  # almacenamiento reactivo de los plots que el usuario aÃƒÂ±ade
  plot_bank  <- reactiveValues(all = list())
  
  # bandera para saber si ya insertÃƒÂ© la pestaÃƒÂ±a Ã¢â‚¬Å“CombinadoÃ¢â‚¬Â
  panel_inserto <- reactiveVal(FALSE)

  # trigger para forzar repintado tras overrides globales
  ov_trigger <- reactiveVal(0)

  # Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬ contenedores reactivos (vacÃƒÂ­os al arrancar) Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬
  datos_box     <- reactiveVal(NULL)   # hoja Ã‚Â«DatosÃ‚Â»
  plot_cfg_box  <- reactiveVal(NULL)   # hoja Ã‚Â«PlotSettingsÃ‚Â»
  cur_data_box  <- reactiveVal(NULL)   # curvas Sheet1
  cur_cfg_box   <- reactiveVal(NULL)   # curvas Sheet2
  sig_list      <- reactiveVal(list()) # guardarÃƒÂ¡ comparaciones
  sig_preselect <- reactiveVal(NULL)   # selecciÃƒÂ³n pendiente en el manejador de barras
  meta_store    <- reactiveValues()    # metadata por tipo de grÃƒÂ¡fico
  is_group_data <- reactiveVal(FALSE)
  bundle_store  <- reactiveValues(datasets = list(), versions = list())
  current_dataset_key <- reactiveVal(NULL)

  ylims <- reactiveValues()

  clear_reactive_values <- function(rv) {
    current <- shiny::reactiveValuesToList(rv)
    if (!length(current)) return(invisible(NULL))
    for (nm in names(current)) rv[[nm]] <- NULL
    invisible(NULL)
  }

  reset_curve_state <- function() {
    cur_data_box(NULL)
    cur_cfg_box(NULL)
    ylims$Curvas <- NULL
  }

  reset_dataset_state <- function() {
    datos_box(NULL)
    plot_cfg_box(NULL)
    sig_list(list())
    sig_preselect(NULL)
    reset_curve_state()
    clear_reactive_values(ylims)
    clear_reactive_values(meta_store)
  }

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
                                "sig_hide_caps", "sig_mode",
                                "sig_label_param_color"),
                      Valor = c(as.character(input$sig_linewidth),
                                as.character(input$sig_textsize),
                                as.character(input$sig_sep),
                                as.character(input$sig_textpad),
                                as.character(input$sig_offset),
                                as.character(input$sig_hide_caps),
                                as.character(input$sig_mode),
                                as.character(input$sig_label_param_color)))
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
                  "stack_outline_only",
                  "errbar_param_color",
                  "errbar_size",
                  "ymax", "ybreak"),
        Valor = c(
          paste(input$stackParams, collapse = ","),
          input$orderStack %||% "",
          as.character(input$showErrBars),
          as.character(input$stack_outline_only %||% FALSE),
          as.character(input$errbar_param_color %||% FALSE),
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
    } else if (input$tipo == "Correlacion") {
      meta <- add_row(
        meta,
        Campo = c(
          "corr_param_x", "corr_param_y",
          "doNorm", "ctrlMedium", "corr_method",
          "corr_norm_target",
          "corr_show_line", "corr_show_labels",
          "corr_show_r", "corr_show_p", "corr_show_r2", "corr_show_eq",
          "corr_xlab", "corr_ylab",
          "xmin_corr", "xmax_corr", "xbreak_corr",
          "ymin_corr", "ymax_corr", "ybreak_corr",
          "corr_label_size"
        ),
        Valor = c(
          input$corr_param_x %||% "",
          input$corr_param_y %||% "",
          as.character(input$doNorm),
          if (is.null(input$ctrlMedium)) "NULL" else input$ctrlMedium,
          input$corr_method %||% "",
          input$corr_norm_target %||% "both",
          as.character(input$corr_show_line),
          as.character(input$corr_show_labels),
          as.character(input$corr_show_r),
          as.character(input$corr_show_p),
          as.character(input$corr_show_r2),
          as.character(input$corr_show_eq),
          input$corr_xlab %||% "",
          input$corr_ylab %||% "",
          as.character(input$xmin_corr),
          as.character(input$xmax_corr),
          as.character(input$xbreak_corr),
          as.character(input$ymin_corr),
          as.character(input$ymax_corr),
          as.character(input$ybreak_corr),
          as.character(input$corr_label_size)
        )
      )
    }
    meta
  }

  observeEvent(input$btn_light, {
    # 1Ã‚Â· cambia el tema visual
    session$setCurrentTheme(theme_light)
    # 2Ã‚Â· guarda la preferencia en localStorage
    session$sendCustomMessage("saveMode", "light")
    # 3Ã‚Â· actualiza input$mode (por si lo usas en otros lugares)
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
  # ---- Cambio de idioma desde el nuevo menÃƒÂº desplegable -------------------
  observeEvent(input$lang_es, {
    shinyjs::runjs("if (window.BIOSZEN_applyLang) { window.BIOSZEN_applyLang('es'); }")
  })
  
  observeEvent(input$lang_en, {
    shinyjs::runjs("if (window.BIOSZEN_applyLang) { window.BIOSZEN_applyLang('en'); }")
  })
  
  observeEvent(input$app_lang, {
    lang <- input$app_lang %||% i18n_lang
    updateRadioButtons(
      session, "corr_norm_target",
      choices = setNames(
        c("both", "x_only", "y_only"),
        tr_text(c("corr_norm_both", "corr_norm_x", "corr_norm_y"), lang)
      ),
      selected = input$corr_norm_target %||% "both"
    )
  }, ignoreInit = TRUE)

    refresh_static_choices <- function() {
    updateRadioButtons(
      session,
      "scope",
      choices  = named_choices(
        c("Por Cepa", "Combinado"),
        c(tr("scope_by_strain"), tr("scope_combined"))
      ),
      selected = input$scope %||% "Por Cepa"
    )

    updateRadioButtons(
      session,
      "tipo",
      choices  = named_choices(
        c("Boxplot", "Barras", "Violin", "Curvas", "Apiladas", "Correlacion"),
        c(
          tr("plot_boxplot"),
          tr("plot_bars"),
          tr("plot_violin"),
          tr("plot_curves"),
          tr("plot_stacked"),
          tr("plot_correlation")
        )
      ),
      selected = input$tipo %||% "Boxplot"
    )

    updateRadioButtons(
      session,
      "corr_method",
      choices  = named_choices(
        c("pearson", "spearman"),
        c(tr("corr_method_pearson"), tr("corr_method_spearman"))
      ),
      selected = input$corr_method %||% "pearson"
    )

    updateSelectInput(
      session,
      "colorMode",
      choices = named_choices(
        c(
          "Default", "Default Suave",
          "Blanco y Negro", "Blanco y Negro Suave",
          "Viridis", "Viridis Suave",
          "Plasma", "Plasma Suave",
          "Magma", "Magma Suave",
          "Cividis", "Cividis Suave",
          "Set1", "Set1 Suave",
          "Set2", "Set2 Suave",
          "Set3", "Set3 Suave",
          "Dark2", "Dark2 Suave",
          "Accent", "Accent Suave",
          "Paired", "Paired Suave",
          "Pastel1", "Pastel1 Suave",
          "Pastel2", "Pastel2 Suave",
          "OkabeIto", "OkabeIto Suave",
          "Tableau", "Tableau Suave"
        ),
        c(
          tr("palette_default"),
          tr("palette_default_soft"),
          tr("palette_bw"),
          tr("palette_bw_soft"),
          tr("palette_viridis"),
          tr("palette_viridis_soft"),
          tr("palette_plasma"),
          tr("palette_plasma_soft"),
          tr("palette_magma"),
          tr("palette_magma_soft"),
          tr("palette_cividis"),
          tr("palette_cividis_soft"),
          tr("palette_set1"),
          tr("palette_set1_soft"),
          tr("palette_set2"),
          tr("palette_set2_soft"),
          tr("palette_set3"),
          tr("palette_set3_soft"),
          tr("palette_dark2"),
          tr("palette_dark2_soft"),
          tr("palette_accent"),
          tr("palette_accent_soft"),
          tr("palette_paired"),
          tr("palette_paired_soft"),
          tr("palette_pastel1"),
          tr("palette_pastel1_soft"),
          tr("palette_pastel2"),
          tr("palette_pastel2_soft"),
          tr("palette_okabeito"),
          tr("palette_okabeito_soft"),
          tr("palette_tableau"),
          tr("palette_tableau_soft")
        )
      ),
      selected = input$colorMode %||% "Default"
    )

    updateRadioButtons(
      session,
      "adv_pal_type",
      choices = named_choices(
        c("seq", "div", "qual"),
        c(tr("palette_type_seq"), tr("palette_type_div"), tr("palette_type_qual"))
      ),
      selected = input$adv_pal_type %||% "seq"
    )

    updateCheckboxGroupInput(
      session,
      "adv_pal_filters",
      choices = named_choices(
        c("colorblind", "print", "photocopy"),
        c(tr("palette_filter_colorblind"), tr("palette_filter_print"), tr("palette_filter_photocopy"))
      ),
      selected = input$adv_pal_filters %||% character(0)
    )

    updateCheckboxGroupInput(
      session,
      "normTests",
      choices = named_choices(
        c("shapiro", "ks", "ad"),
        c(tr("norm_shapiro"), tr("norm_ks"), tr("norm_ad"))
      ),
      selected = input$normTests %||% c("shapiro", "ks", "ad")
    )

    updateRadioButtons(
      session,
      "sigTest",
      choices = named_choices(
        c("ANOVA", "Kruskal-Wallis", "ttest", "wilcox"),
        c(
          tr("sigtest_anova"),
          tr("sigtest_kruskal"),
          tr("sigtest_ttest"),
          tr("sigtest_wilcox")
        )
      ),
      selected = input$sigTest %||% "ANOVA"
    )

    updateRadioButtons(
      session,
      "compMode",
      choices = named_choices(
        c("all", "control", "pair"),
        c(tr("comp_all"), tr("comp_control"), tr("comp_pair"))
      ),
      selected = input$compMode %||% "all"
    )

    updateRadioButtons(
      session,
      "sig_mode",
      choices = named_choices(
        c("bars", "labels"),
        c(tr("sig_mode_bars"), tr("sig_mode_labels"))
      ),
      selected = input$sig_mode %||% "bars"
    )

    if (!is.null(input$combo_pal)) {
      updateSelectInput(
        session,
        "combo_pal",
        choices = named_choices(
          c(
            "Original", "Default", "Blanco y Negro", "Viridis",
            "Plasma", "Magma", "Cividis", "Set1", "Set2",
            "Set3", "Dark2", "Accent", "Paired", "Pastel1",
            "Pastel2"
          ),
          c(
            tr("palette_original"),
            tr("palette_default"),
            tr("palette_bw"),
            tr("palette_viridis"),
            tr("palette_plasma"),
            tr("palette_magma"),
            tr("palette_cividis"),
            tr("palette_set1"),
            tr("palette_set2"),
            tr("palette_set3"),
            tr("palette_dark2"),
            tr("palette_accent"),
            tr("palette_paired"),
            tr("palette_pastel1"),
            tr("palette_pastel2")
          )
        ),
        selected = input$combo_pal %||% "Original"
      )
    }

    if (!is.null(input$bundle_label)) {
      updateTextInput(session, "bundle_label", placeholder = tr_text("bundle_label_placeholder"))
    }

    if (!is.null(input$ov_tipo)) {
      updateSelectInput(
        session,
        "ov_tipo",
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
        selected = input$ov_tipo %||% "Todos"
      )
    }
  }

  observeEvent(input$app_lang, {
    lang <- input$app_lang %||% i18n_lang
    if (!i18n_valid_lang(lang)) return()
    if (!identical(lang, i18n_lang)) {
      i18n_lang <<- lang
      i18n$set_translation_language(lang)
    }
    shiny.i18n::update_lang(lang, session)
    refresh_static_choices()
  }, ignoreInit = TRUE)
  
  
  # ---------- helper: genera SIEMPRE un ggplot (o grob) -----------------
  make_snapshot <- function(){
    scope_sel  <- if (input$scope == "Combinado") "Combinado" else "Por Cepa"
    strain_sel <- if (scope_sel == "Por Cepa")    input$strain else NULL
    
    if (input$tipo == "Apiladas"){
      # build_plot() devuelve la versiÃƒÂ³n ggplot de Apiladas
      build_plot(scope_sel, strain_sel, "Apiladas")
    } else {
      # para los demÃƒÂ¡s tipos, plot_base() ya es ggplot
      plot_base()
    }
  }
  
  # Devuelve el ÃƒÂ¡ngulo que realmente se aplicarÃƒÂ¡ a las etiquetas X
  get_x_angle <- function(n, angle_input){
    if (!is.na(angle_input))             # el usuario escribiÃƒÂ³ algo Ã¢â€ â€™ ÃƒÂºsalo
      return(angle_input)
    if (n > 6) return(45)                # Ã¢â‚¬Å“adaptativoÃ¢â‚¬Â: >6 etiquetas Ã¢â€¡â€™ 45Ã‚Â°
    0                                     # caso contrario Ã¢â€¡â€™ horizontal
  }

  # Margen inferior segÃƒÂºn el ÃƒÂ¡ngulo de etiquetas X
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

  # Convierte "A-B" en "A B" y luego introduce saltos de lÃƒÂ­nea
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

  wrap_sig_group <- function(value, levels_ref = NULL) {
    if (is.null(value)) return(value)
    if (is.numeric(value)) return(value)
    val_chr <- as.character(value)
    if (!isTRUE(input$x_wrap) || !nzchar(val_chr) || grepl("\n", val_chr, fixed = TRUE)) {
      return(val_chr)
    }
    wrapped <- wrap_label(val_chr, lines = input$x_wrap_lines)
    if (is.null(levels_ref)) return(wrapped)
    levels_ref <- as.character(levels_ref)
    if (wrapped %in% levels_ref) wrapped else val_chr
  }

  wrap_sig_group_html <- function(value, levels_ref = NULL) {
    if (is.null(value)) return(value)
    if (is.numeric(value)) return(value)
    val_chr <- as.character(value)
    if (!isTRUE(input$x_wrap) || !nzchar(val_chr) || grepl("<br>", val_chr, fixed = TRUE)) {
      return(val_chr)
    }
    wrapped <- wrap_label_html(val_chr, lines = input$x_wrap_lines)
    if (is.null(levels_ref)) return(wrapped)
    levels_ref <- as.character(levels_ref)
    if (wrapped %in% levels_ref) wrapped else val_chr
  }

  # Inicializa mÃƒÂ³dulos con los helpers generados arriba
  combo_plot <- setup_panel_module(input, output, session,
                                   plot_bank, panel_inserto, ov_trigger,
                                   make_snapshot, collect_metadata_tbl,
                                   curve_settings)
  growth_mod    <- setup_growth_module(input, output, session)
  growth_out_dir <- growth_mod$growth_dir

  # Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬ Logo siempre visible Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬
  output$logo_img <- renderUI({
    # si todavÃƒÂ­a no hay input$mode asumimos Ã¢â‚¬Å“lightÃ¢â‚¬Â
    modo <- input$mode %||% "light"      # operador %||%  Ã¢â€ â€™ usa la derecha si es NULL
    
    archivo <- if (modo == "dark") "logo_dark.png" else "logo_light.png"
    tags$img(src = archivo, style = "height:220px;")
  })
  
  observeEvent(input$doNorm, {
    if (isTRUE(input$doNorm) && is.null(input$ctrlMedium)) {  
      lang <- input$app_lang %||% i18n_lang
      showNotification(tr_text("norm_ctrl_required", lang),  
                       type = "warning", duration = 4)  
    }  
  })  
  
  sig_choice_vec <- function(sl){
    if (!length(sl)) return(named_choices(character(), character()))
    labels <- vapply(seq_along(sl), function(i){
      cmp <- sl[[i]]
      lab <- cmp$lab %||% ""
      if (is.na(lab)) lab <- ""
      base <- sprintf("%02d) %s vs %s", i, cmp$g1, cmp$g2)
      param_val <- cmp$param %||% ""
      if (is.na(param_val)) param_val <- ""
      param_lbl <- if (nzchar(param_val)) paste0("@", param_val) else ""
      if (nzchar(param_lbl)) base <- paste(base, param_lbl)
      if (nzchar(lab)) paste0(base, " [", lab, "]") else base
    }, character(1))
    stats::setNames(as.character(seq_along(sl)), labels)
  }

  observeEvent(list(sig_list(), input$app_lang), {
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
      options  = list(
        plugins = list("remove_button"),
        placeholder = tr("sig_current_placeholder")
      ),
      server   = TRUE
    )
  })
  
  observeEvent(input$add_sig, {  
    req(input$sig_group1, input$sig_group2, nzchar(input$sig_label))  
    if (identical(input$sig_mode, "labels") && identical(input$tipo, "Apiladas")) {
      req(input$sig_param)
    }
    # no duplicar  
    param_val <- if (identical(input$sig_mode, "labels") && identical(input$tipo, "Apiladas"))
      input$sig_param else NULL
    new_cmp <- list(g1 = input$sig_group1,  
                    g2 = input$sig_group2,  
                    lab = input$sig_label,
                    param = param_val)
    sl <- sig_list()  
    if (!any(vapply(sl, function(x) identical(x, new_cmp), logical(1)))) {  
      sig_preselect(as.character(length(sl) + 1))
      sig_list( append(sl, list(new_cmp)) )  
    }  
  })  
  
  observeEvent(input$remove_sig, {
    ids <- as.integer(input$sig_current)
    if (!length(ids)) {
      lang <- input$app_lang %||% i18n_lang
      showNotification(tr_text("sig_remove_select", lang),
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
      lang <- input$app_lang %||% i18n_lang
      showNotification(tr_text("sig_reorder_single", lang),
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
    reset_curve_state()
    ok <- tryCatch({
      # 1) Leer curvas (Sheet1)
      d <- read_excel_tmp(input$curveFile$datapath, sheet = "Sheet1")
      
      d_time_max <- suppressWarnings(max(as.numeric(d$Time), na.rm = TRUE))
      if (!is.finite(d_time_max)) d_time_max <- 1
      d_vals <- d[, setdiff(names(d), "Time"), drop = FALSE]
      if (ncol(d_vals) == 0) {
        d_val_max <- NA_real_
      } else {
        d_val_max <- suppressWarnings(max(unlist(lapply(d_vals, as.numeric)), na.rm = TRUE))
      }
      if (!is.finite(d_val_max)) d_val_max <- 1
      default_x_break <- if (is.finite(d_time_max) && d_time_max > 0) d_time_max / 3 else 1
      default_y_break <- if (is.finite(d_val_max) && d_val_max > 0) d_val_max / 3 else 1
      
      # 2) Leer configuraciÃƒÂ³n (Sheet2)
      s <- tryCatch(
        read_excel_tmp(input$curveFile$datapath, sheet = "Sheet2"),
        error = function(e) NULL
      )
      
      # 3) Si falta config o le faltan columnas, armamos defaults basados en d
      required_cols <- c("X_Max","Interval_X","Y_Max","Interval_Y","X_Title","Y_Title")
      if (is.null(s) || !all(required_cols %in% names(s))) {
        s <- tibble::tibble(
          X_Max      = d_time_max,
          Interval_X = default_x_break,
          Y_Max      = d_val_max,
          Interval_Y = default_y_break,
          X_Title    = "Tiempo (min)",
          Y_Title    = "OD (620 nm)",
          Well       = NA,                    # si tu lÃƒÂ³gica luego asume estas columnas
          BiologicalReplicate = NA
        )
      } else {
        if (!"Well" %in% names(s)) s$Well <- NA
        if (!"BiologicalReplicate" %in% names(s)) s$BiologicalReplicate <- NA
      }
      
      s <- s %>%
        mutate(
          X_Max      = suppressWarnings(as.numeric(X_Max)),
          Interval_X = suppressWarnings(as.numeric(Interval_X)),
          Y_Max      = suppressWarnings(as.numeric(Y_Max)),
          Interval_Y = suppressWarnings(as.numeric(Interval_Y))
        )
      s$X_Max[!is.finite(s$X_Max)] <- d_time_max
      s$Y_Max[!is.finite(s$Y_Max)] <- d_val_max
      s$Interval_X[!is.finite(s$Interval_X) | s$Interval_X <= 0] <- default_x_break
      s$Interval_Y[!is.finite(s$Interval_Y) | s$Interval_Y <= 0] <- default_y_break
      
      # 4) Guardar en los reactivos
      cur_data_box(d)
      cur_cfg_box(s)
      
      # 5) Inicializar lÃƒÂ­mites de curvas
      ylims$Curvas <- list(
        xmax   = s$X_Max[1],
        xbreak = s$Interval_X[1],
        ymax   = s$Y_Max[1],
        ybreak = s$Interval_Y[1]
      )
      
      TRUE
    }, error = function(e) {
      showNotification(
        sprintf(tr_text("curve_file_invalid", input$app_lang %||% i18n_lang), e$message),
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
    if (is.finite(cfg$X_Max))      updateNumericInput(session, "xmax_cur",   value = cfg$X_Max)
    if (is.finite(cfg$Interval_X)) updateNumericInput(session, "xbreak_cur", value = cfg$Interval_X)
    if (is.finite(cfg$Y_Max))      updateNumericInput(session, "ymax_cur",   value = cfg$Y_Max)
    if (is.finite(cfg$Interval_Y)) updateNumericInput(session, "ybreak_cur", value = cfg$Interval_Y)
    
    fluidRow(
      column(6, numericInput("ymin_corr",      "Y min:",            value = 0, min = 0)),
      column(6, numericInput("ymax_corr",      "Y max:",            value = 1, min = 0))
    )
    fluidRow(
      column(6, numericInput("corr_label_size","TamaÃƒÂ±o etiquetas:", value = 5, min = 1 ))
    )
    
    
    updateTextInput(session, "cur_xlab", value = cfg$X_Title)
    updateTextInput(session, "cur_ylab", value = cfg$Y_Title)
  })
  
  
  
  curve_data     <- reactive( cur_data_box() )  
  curve_settings <- reactive( cur_cfg_box() )  
  
  
  # Ã¢â€â‚¬Ã¢â€â‚¬ Inputs dinÃƒÂ¡micos para Curvas: selecciÃƒÂ³n de rÃƒÂ©plicas por pozo Ã¢â€â‚¬Ã¢â€â‚¬  
  output$repSelCurvas <- renderUI({  
    input$app_lang
    # metadatos de wells  
    cfg <- curve_settings()  
    req(cfg)
    if (!"Well" %in% names(cfg) || !"BiologicalReplicate" %in% names(cfg)) return(NULL)
    if (all(is.na(cfg$Well))) return(NULL)
    
    meta <- datos_combinados()
    if (input$scope == "Por Cepa") {
      meta <- meta %>% filter(Strain == input$strain)
      if (!is.null(input$showMedios))
        meta <- meta %>% filter(Media %in% input$showMedios)
    } else {
      if (!is.null(input$showGroups) && length(input$showGroups))
        meta <- meta %>% filter(paste(Strain, Media, sep = "-") %in% input$showGroups)
    }
    
    # cada well es un valor ÃƒÂºnico de cfg$Well presente en la selecciÃƒÂ³n  
    wells <- intersect(unique(cfg$Well), meta$Well)  
    if (!length(wells)) return(NULL)
    panels <- lapply(wells, function(w) {
      reps <- sort(unique(na.omit(cfg$BiologicalReplicate[cfg$Well == w])))
      if (!length(reps)) return(NULL)
      checkboxGroupInput(  
        paste0("reps_cur_", make.names(w)),  
        paste(tr("reps_prefix"), w),  
        choices  = reps,  
        selected = reps  
      )  
    })  
    panels <- Filter(Negate(is.null), panels)
    if (!length(panels)) return(NULL)
    panels
  })  
  
  # --- Reactivos de lectura seguros -------------------------------------------  
  ## Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬ NUEVO manejo robusto del archivo principal Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬  
  datos_raw      <- reactiveVal(NULL)   #   guarda Ã‚Â«DatosÃ‚Â»  
  plot_settings  <- reactiveVal(NULL)   #   guarda Ã‚Â«PlotSettingsÃ‚Â»  
  
  # Ã¢â€â‚¬Ã¢â€â‚¬ Lectura robusta del Excel de metadata+parÃƒÂ¡metros Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬
  observeEvent(input$dataFile, {
    reset_dataset_state()

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
      datos_box(df);            plot_cfg_box(cfg)

      is_group_data(is_group)
      TRUE

    }, error = function(e){
      showNotification(paste(tr_text("data_file_invalid", input$app_lang %||% i18n_lang), e$message),
                       type = "error", duration = 6)
      FALSE
    })
    
    if (!ok) return()
    
    ## Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â‚¬â€  REFRESCAR UI Ã¢â‚¬â€ Ã¢â‚¬Å (segÃƒÂºn haya o no parÃƒÂ¡metros Ã‚Â«realesÃ‚Â») Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬
    params <- plot_cfg_box()$Parameter
    
    # 2.a) actualiza selector de tipo de grÃƒÂ¡fico ------------------------------
    if (length(params) == 0 || identical(params, "Parametro_dummy")) {
      updateRadioButtons(
        session,
        "tipo",
        choices  = named_choices(c("Curvas"), c(tr("plot_curves"))),
        selected = "Curvas"
      )
    } else {
      updateRadioButtons(
        session,
        "tipo",
        choices  = named_choices(
          c("Boxplot", "Barras", "Violin", "Curvas", "Apiladas", "Correlacion"),
          c(
            tr("plot_boxplot"),
            tr("plot_bars"),
            tr("plot_violin"),
            tr("plot_curves"),
            tr("plot_stacked"),
            tr("plot_correlation")
          )
        ),
        selected = "Boxplot"
      )
    }
    
    # 2.b) selector de parÃƒÂ¡metro (lo deje vacÃƒÂ­o si no hay)
      updateSelectInput(session, "param",
                        choices  = params,
                        selected = if (length(params)) params[1] else character(0))

      if (length(params)) {
        first_cfg <- cfg[cfg$Parameter == params[1], ]
        updateNumericInput(session, "ymax",
                           label  = paste0(tr("y_max"), " (", params[1], "):"),
                           value  = first_cfg$Y_Max)
        updateNumericInput(session, "ybreak",
                           label  = paste0(tr("y_interval"), " (", params[1], "):"),
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
        # desarrollo: cuando se ejecuta desde raÃƒÂ­z del proyecto
        p <- file.path("inst", "app", "www")
        if (dir.exists(p)) return(normalizePath(p, winslash = "/", mustWork = TRUE))
        stop("No se encontrÃƒÂ³ la carpeta 'www'.")
      }

      www_dir <- find_www()
      ref_dir <- file.path(www_dir, "Archivos de referencia")
      if (!dir.exists(ref_dir)) {
        stop("No se encontrÃƒÂ³ la carpeta 'Archivos de referencia' dentro de 'www'.")
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
      lang <- input$manual_lang %||% 'en'
      if (lang == 'en') 'MANUAL_EN.pdf' else 'MANUAL_ES.pdf'
    },
    content = function(file) {
      lang <- input$manual_lang %||% 'en'
      fname_pdf  <- if (lang == 'en') 'MANUAL_EN.pdf' else 'MANUAL_ES.pdf'
      fname_docx <- if (lang == 'en') 'MANUAL_EN.docx' else 'MANUAL_ES.docx'

      # localizar carpeta www
      find_www <- function() {
        pkg_www <- system.file('app/www', package = 'BIOSZEN')
        if (!is.null(pkg_www) && nzchar(pkg_www) && dir.exists(pkg_www)) return(pkg_www)
        if (dir.exists('www')) return(normalizePath('www', winslash = '/', mustWork = TRUE))
        p <- file.path('inst','app','www')
        if (dir.exists(p)) return(normalizePath(p, winslash = '/', mustWork = TRUE))
        stop("No se encontrÃƒÂ³ la carpeta 'www'.")
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
        # 1) doconv (LibreOffice / Word) si estÃƒÂ¡ disponible
        if (!convert_ok && requireNamespace('doconv', quietly = TRUE)) {
          convert_ok <- tryCatch({
            doconv::to_pdf(src_docx, output = file)
            file.exists(file)
          }, error = function(e) FALSE)
        }

        # 2) pandoc + motor PDF si estÃƒÂ¡ disponible
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
        # ÃƒÅ¡ltimo recurso: entregar DOCX si no se pudo generar PDF
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
    # ---------- Correlacion poblar selectInputs --------------------------
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
    
    # inicializar tambiÃƒÂ©n el orden de parÃƒÂ¡metros apilados  
    updateTextInput(  
      session, "orderStack",  
      value = paste(params, collapse = ",")  
    )  
  }, ignoreInit = FALSE)  
  
  
  ## accesos rÃƒÂ¡pidos (usan las reactiveVal que acabamos de crear)  
  datos_combinados <- reactive( datos_box() )  
  plot_settings    <- reactive( plot_cfg_box() )  
  
  # --- ParÃƒÂ¡m. seguro: siempre existe en el Excel cargado ----  
  safe_param <- reactive({  
    req(input$param)  
    if (isTRUE(input$doNorm) && !is.null(input$ctrlMedium))  
      paste0(input$param, "_Norm")  
    else  
      input$param  
  })  
  
  
  
  
  # justo despuÃƒÂ©s, dentro de server(), inicializa tambiÃƒÂ©n la entrada Ã¢â‚¬Å“CurvasÃ¢â‚¬Â  
  observeEvent(curve_settings(), {  
    req(curve_settings())          # Ã¢â€ Â evita que se ejecute con NULL  
    cfg <- curve_settings()[1, ]  
    ylims$Curvas <- list(  
      ymax   = cfg$Y_Max,  
      ybreak = cfg$Interval_Y,  
      xmax   = cfg$X_Max,  
      xbreak = cfg$Interval_X  
    )  
  }, ignoreNULL = TRUE)             # Ã¢â€ Â cualquiera de las dos opciones  
  
  
  # La funciÃƒÂ³n safe_hue ahora se define de forma global en helpers.R
  # y se carga al inicio de la aplicaciÃƒÂ³n, por lo que ya no se
  # declara localmente dentro del servidor principal.
  
  
  # 1) InicializaciÃƒÂ³n: crear una entrada en ylims para cada parÃƒÂ¡metro  
  observeEvent(plot_settings(), {  
    params <- plot_settings()$Parameter  
    for (p in params) {  
      # ya estÃƒÂ¡is haciendo esto  
      if (is.null(ylims[[p]])) {  
        cfg <- plot_settings() %>% filter(Parameter == p)  
        ylims[[p]] <- list(  
          ymax   = cfg$Y_Max,  
          ybreak = cfg$Interval  
        )  
      }  
      # Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬ PEGA ABAJO este bloque Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬  
      # lÃƒÂ­mites por defecto para valores normalizados  
      ylims[[paste0(p, "_Norm")]] <- list(  
        ymax   = 1,     # rango tÃƒÂ­pico normalizado: 0Ã¢â‚¬â€œ1  
        ybreak = 0.2    # intervalo de 0.2  
      )  
      # Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬  
    }  
  }, ignoreNULL = FALSE)  
  
  
  # Ã¢â€â‚¬Ã¢â€â‚¬ Reset general al cargar un nuevo archivo Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬  
  observeEvent(input$dataFile, {  
    req(plot_settings())                     # esperamos a tener la hoja  
    
    ## 1Ã‚Â· reiniciar parÃƒÂ¡metro seleccionado  
    updateSelectInput(session, "param",  
                      choices  = plot_settings()$Parameter,  
                      selected = plot_settings()$Parameter[1])  
    
    ## 2Ã‚Â· reiniciar strain / scope  
    updateRadioButtons(session, "scope", selected = "Por Cepa")  
    updateSelectInput(session, "strain", choices = NULL)  
    
    ## 3Ã‚Â· reiniciar selecciÃƒÂ³n de grÃƒÂ¡ficos  
    isolate({  
      strains <- sort(unique(datos_combinados()$Strain))  
      tipos <- c("Boxplot","Barras","Violin","Curvas","Apiladas")  
      cepa    <- as.vector(t(outer(strains, tipos,  
                                   FUN = function(s, t) paste0(s, "_", t))))  
      combo   <- paste0("Combinado_", tipos)  
      
    })  
  })  
  
  # Para Boxplot/Barras  
  ## Ã¢â€â‚¬Ã¢â€â‚¬ guardar cambios de YÃ¢â‚¬â€˜axis hechos por el usuario Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬  
  observeEvent(
    list(input$ymax, input$ybreak, input$param),
    {  
      # clave correcta:  Ã¢â‚¬Å“PARÃ¢â‚¬Â    o  Ã¢â‚¬Å“PAR_NormÃ¢â‚¬Â  
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
  
  
  
  # 3) Y en build_plot(), para obtener los lÃƒÂ­mites, ya no usas Curvas/AUC fijos,  
  #    sino:  
  get_ylim <- function(param) {  
    req(ylims[[param]])  
    ylims[[param]]  
  }  
  
  
  # dentro de server(), tras definir plot_settings()  
  output$paramSel <- renderUI({
    input$app_lang
    req(plot_cfg_box())                         # cfg ya cargada
    params <- plot_cfg_box()$Parameter
    if (length(params) == 0 || identical(params, "Parametro_dummy")) {
      helpText(tr("no_params_curves"))
    } else {
      selectInput("param", tr("param_label"),
                  choices = params, selected = params[1])
    }
  })
  
  
  # --- Procesamiento de datos dinÃƒÂ¡mico ---  
  datos_agrupados <- reactive({
    req(datos_combinados(), plot_settings())
    
    # 1) Parametros de configuraciÃƒÂ³n vs columnas reales
    params <- plot_settings()$Parameter
    df     <- datos_combinados()
    present <- intersect(params, names(df))
    missing <- setdiff(params, present)
    
    # 2) Notificar si faltan
    if (length(missing) > 0) {
      showNotification(
        sprintf(
          tr_text("missing_params_warning", input$app_lang %||% i18n_lang),
          paste(missing, collapse = ", ")
        ),
        type = "warning", duration = 5
      )
    }
    
    # 3) Agrupar y resumir sÃƒÂ³lo con los parÃƒÂ¡metros presentes
    df %>%
      filter(!is.na(Strain), !is.na(Media), Strain != "C-") %>%
      group_by(Strain, Media, BiologicalReplicate) %>%
      summarise(
        across(all_of(present), ~ mean(.x, na.rm = TRUE)),
        Orden = dplyr::first(Orden),
        .groups = "drop"
      )
  })
  
  
  
  # --- Inputs dinÃƒÂ¡micos: Por Cepa ---  
  
  observeEvent(datos_agrupados(), {  
    
    
    # 1) poblar selector de cepas  
    updateSelectInput(session, "strain",  
                      choices = sort(unique(datos_agrupados()$Strain)))  
    # 2) poblar filtro de medios  
    medias <- sort(unique(datos_agrupados()$Media))  
    output$showMediosUI <- renderUI({  
      input$app_lang
      checkboxGroupInput("showMedios", tr("media_label"),  
                         choices = medias, selected = medias)  
    })  
    # 3) inicializar orden de medios  
    # Inicializa orderMedios usando el orden de la columna Ã¢â‚¬ËœOrdenÃ¢â‚¬â„¢
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
    input$app_lang
    scope_sel <- input$scope %||% "Por Cepa"
    use_param <- !is.null(input$tipo) && input$tipo %in% c("Boxplot", "Barras", "Violin")
    df <- if (use_param) {
      param_rep_df()
    } else {
      if (isTRUE(input$doNorm)) datos_agrupados_norm() else datos_agrupados()
    }
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)
    if (scope_sel == "Por Cepa" && !is.null(input$showMedios)) {
      df <- df %>% filter(Media %in% input$showMedios, Strain == input$strain)
    } else if (scope_sel == "Combinado" && !is.null(input$showGroups)) {
      df <- df %>% filter(paste(Strain, Media, sep = "-") %in% input$showGroups)
    }
    reps <- sort(unique(df$BiologicalReplicate))
    if (!length(reps)) return(NULL)
    checkboxGroupInput(
      "rm_reps_all",
      tr("exclude_reps_global"),
      choices  = reps,
      selected = NULL
    )
  })
  
  
  ## ------------------------------------------------------------------  
  ## Reactive: datos_agrupados_norm() Ã¢â‚¬â€œÃ‚Â copia de datos_agrupados() con  
  ##           columnas normalizadas (sufijo "_Norm")  
  ## ------------------------------------------------------------------  
  datos_agrupados_norm <- reactive({  
    df_raw <- datos_agrupados()  
    if (!isTRUE(input$doNorm)) return(df_raw)  
  
    params <- plot_settings()$Parameter  
    lang   <- input$app_lang %||% i18n_lang  
    ctrl   <- input$ctrlMedium  
  
    res <- tryCatch(  
      normalize_params(  
        df          = df_raw,  
        params      = params,  
        do_norm     = TRUE,  
        ctrl_medium = ctrl  
      ),  
      error = function(e) {  
        showNotification(tr_text("norm_failed_generic", lang), type = "warning", duration = 6)  
        df_raw %>% dplyr::mutate(dplyr::across(  
          dplyr::all_of(params),  
          ~ .x,  
          .names = "{.col}_Norm"  
        ))  
      }  
    )  
  
    if (isTRUE(attr(res, "norm_fallback"))) {  
      msg <- if (!is.null(ctrl) && nzchar(ctrl)) {  
        sprintf(tr_text("norm_fallback_ctrl", lang), ctrl)  
      } else {  
        tr_text("norm_failed_generic", lang)  
      }  
      showNotification(msg, type = "warning", duration = 6)  
    }  
  
    res  
  })  
  
  
  # Ã¢â€â‚¬Ã¢â€â‚¬ Toggle Ã¢â‚¬Å“Por CepaÃ¢â‚¬Â Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬  
  observeEvent(input$toggleMedios, {  
    df <- datos_agrupados()
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0 || !"Media" %in% names(df)) return()
    medias <- sort(unique(df$Media))  
    sel    <- if (isTRUE(input$toggleMedios)) medias else character(0)  
    updateCheckboxGroupInput(session,  
                             inputId  = "showMedios",  
                             choices  = medias,  
                             selected = sel  
    )  
  })  
  
  # Ã¢â€â‚¬Ã¢â€â‚¬ Toggle Ã¢â‚¬Å“CombinadoÃ¢â‚¬Â Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬  
  observeEvent(input$toggleGroups, {  
    df <- datos_agrupados()
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0 ||
        !"Strain" %in% names(df) || !"Media" %in% names(df)) return()
    grps <- unique(paste(df$Strain, df$Media, sep = "-"))  
    sel  <- if (isTRUE(input$toggleGroups)) grps else character(0)  
    updateCheckboxGroupInput(session,  
                             inputId  = "showGroups",  
                             choices  = grps,  
                             selected = sel  
    )  
  })  
  
  
  
  # ---------- NUEVO: checkboxes de rÃƒÂ©plicas por medio (modo Por Cepa) ----------  
  output$repsStrainUI <- renderUI({  
    input$app_lang
    use_param <- !is.null(input$tipo) && input$tipo %in% c("Boxplot", "Barras", "Violin")
    df <- if (use_param) {
      param_rep_df()
    } else {
      if (isTRUE(input$doNorm)) datos_agrupados_norm() else datos_agrupados()
    }
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)
    df <- df %>%
      filter(Strain == input$strain)  
    if (!is.null(input$showMedios)) {
      df <- df %>% filter(Media %in% input$showMedios)
    }
    
    # construye un "sub-checkbox" por cada medio de esa cepa  
    tagList(lapply(unique(df$Media), function(m){  
      reps <- sort(unique(df$BiologicalReplicate[df$Media == m]))  
     drop_all <- as.character(input$rm_reps_all %||% character(0))
      checkboxGroupInput(  
        paste0("reps_", make.names(m)),       # id = reps_<medio>  
        paste(tr("reps_prefix"), m),  
        choices  = reps,  
        selected = setdiff(as.character(reps), drop_all)  
      )  
    }))  
  })
  
  output$ctrlSelUI <- renderUI({  
    input$app_lang
    req(input$doNorm)                          # sÃƒÂ³lo cuando se active el check  
    
    # Ã¢â‚¬â€˜Ã¢â‚¬â€˜Ã‚Â si la cepa aÃƒÂºn no estÃƒÂ¡ elegida, muestra TODOS los medios  
    if (input$scope == "Por Cepa" && !is.null(input$strain)) {  
      opts <- sort(unique(  
        datos_agrupados()$Media[  
          datos_agrupados()$Strain == input$strain]))  
    } else {  
      opts <- sort(unique(datos_agrupados()$Media))  
    }  
    
    selectInput("ctrlMedium", tr("norm_medium_label"),    # Ã¢â€ Â etiqueta genÃƒÂ©rica  
                choices = opts,  
                selected = if (length(opts)) opts[1] else character(0))  
  })  
  
  # --- Inputs dinÃƒÂ¡micos: Combinado ---  
  observeEvent(datos_agrupados(), {
    grps <- unique(paste(datos_agrupados()$Strain, datos_agrupados()$Media, sep = "-"))
    # Ã¢â€â‚¬Ã¢â€â‚¬ SERVER  Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬
    output$groupSel <- renderUI({
      input$app_lang
      grps <- unique(paste(datos_agrupados()$Strain,
                           datos_agrupados()$Media, sep = "-"))
      
      labels_order <- datos_agrupados() %>%
        distinct(Strain, Media, Orden) %>%
        mutate(Label = paste(Strain, Media, sep = "-")) %>%
        arrange(Orden) %>% pull(Label)
      
      tagList(
        checkboxGroupInput(
          "showGroups", tr("groups_label"),
          choices  = grps,
          selected = grps
        ),
        textInput(
          "orderGroups", tr("order_csv"),
          value = paste(labels_order, collapse = ",")
        )
      )
    })
    updateCheckboxInput(session, "toggleGroups", value = TRUE)
  })
  
  output$repsGrpUI <- renderUI({
    input$app_lang
    grps <- input$showGroups
    if (is.null(grps) || !length(grps)) return(helpText(tr("select_groups_prompt")))
    use_param <- !is.null(input$tipo) && input$tipo %in% c("Boxplot", "Barras", "Violin")
    df <- if (use_param) {
      param_rep_df()
    } else {
      if (isTRUE(input$doNorm)) datos_agrupados_norm() else datos_agrupados()
    }
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)
    df <- df %>%
      dplyr::filter(paste(Strain, Media, sep = "-") %in% grps)
    
    accordion(
      id       = "repsGrpPanel",
      open     = FALSE,   # empieza cerrada
      multiple = TRUE,
      accordion_panel_safe(
        tr("reps_by_group"),
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
              paste(tr("reps_prefix"), g),
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
  # Ã¢â€“Âº ACTUALIZA las listas de grupos disponibles para las barras de
  #   significancia.  Toma SIEMPRE los grupos que realmente se muestran en
  #   el grÃƒÂ¡fico (tras pasar por los filtros order_filter_*).
  # -------------------------------------------------------------------------
  observe({
    req(datos_agrupados())                       # hay datos cargados
    
    if (input$scope == "Por Cepa") {             # Ã¢â€â‚¬Ã¢â€â‚¬ modo Ã¢â‚¬ËœPor CepaÃ¢â‚¬â„¢ Ã¢â€â‚¬Ã¢â€â‚¬
      req(input$strain)                          # ya hay una cepa elegida
      
      # SÃƒÂ³lo los medios visibles con los filtros actuales
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
      
    } else {                                     # Ã¢â€â‚¬Ã¢â€â‚¬ modo Ã¢â‚¬ËœCombinadoÃ¢â‚¬â„¢ Ã¢â€â‚¬Ã¢â€â‚¬
      # Los grupos que siguen visibles despuÃƒÂ©s de los filtros Ã¢â‚¬Å“showGroupsÃ¢â‚¬Â
      grupos_visibles <- datos_agrupados()       |>
        order_filter_group()                     |>
        pull(Label)                              |>
        unique()                                 |>
        sort()

      if (identical(input$tipo, "Apiladas") && isTRUE(input$labelMode)) {
        grupos_visibles <- datos_agrupados() |>
          order_filter_group() |>
          pull(Strain) |>
          unique() |>
          sort()
      }
      
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

  observe({
    ps <- plot_settings()
    params <- input$stackParams
    if (is.null(params) || !length(params)) {
      params <- if (!is.null(ps)) ps$Parameter else character(0)
    }
    current <- isolate(input$sig_param)
    if (!length(params)) {
      updateSelectInput(session, "sig_param", choices = character(0), selected = character(0))
      return()
    }
    if (is.null(current) || !current %in% params) current <- params[1]
    updateSelectInput(session, "sig_param", choices = params, selected = current)
  })
  
  
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
    
    # 1Ã‚Â· refrescar las cajas
    updateNumericInput(session, "ymax",
                       label  = paste0("Y max (", input$param, "):"),
                       value  = cfg$Y_Max)
    updateNumericInput(session, "ybreak",
                       label  = paste0("Int Y (", input$param, "):"),
                       value  = cfg$Interval)
    
    # 2Ã‚Â· sincronizar ylims con el Excel
    ylims[[ input$param ]] <- list(
      ymax   = cfg$Y_Max,
      ybreak = cfg$Interval
    )
  }, ignoreNULL = TRUE)
  

  # ---------- Correlacion actualizar limites por defecto -------------
  observeEvent(
    list(input$corr_param_x, input$corr_param_y,
         input$doNorm, input$ctrlMedium, input$corr_norm_target),
    {
      req(plot_settings(), datos_box())
      raw_x <- input$corr_param_x
      raw_y <- input$corr_param_y
      if (is.null(raw_x) || is.null(raw_y) ||
          !nzchar(as.character(raw_x)) || !nzchar(as.character(raw_y))) {
        return()
      }
      updateTextInput(session, "corr_xlab", value = raw_x)
      updateTextInput(session, "corr_ylab", value = raw_y)

      norm_mode <- input$corr_norm_target %||% "both"
      use_norm_x <- isTRUE(input$doNorm) && !is.null(input$ctrlMedium) &&
        norm_mode %in% c("both", "x_only")
      use_norm_y <- isTRUE(input$doNorm) && !is.null(input$ctrlMedium) &&
        norm_mode %in% c("both", "y_only")

      col_x <- if (use_norm_x) paste0(raw_x, "_Norm") else raw_x
      col_y <- if (use_norm_y) paste0(raw_y, "_Norm") else raw_y

      cfg_x <- plot_settings() %>% filter(Parameter == raw_x)
      cfg_y <- plot_settings() %>% filter(Parameter == raw_y)
      df    <- datos_box()

      xmax <- if (!use_norm_x && nrow(cfg_x)) cfg_x$Y_Max[1] else NA_real_
      if (!is.finite(xmax)) {
        xmax <- suppressWarnings(max(df[[col_x]], na.rm = TRUE))
      }
      if (!is.finite(xmax) || xmax <= 0) xmax <- 1

      xbreak <- if (!use_norm_x && nrow(cfg_x)) cfg_x$Interval[1] else NA_real_
      if (!is.finite(xbreak) || xbreak <= 0) xbreak <- xmax/5

      ymax <- if (!use_norm_y && nrow(cfg_y)) cfg_y$Y_Max[1] else NA_real_
      if (!is.finite(ymax)) {
        ymax <- suppressWarnings(max(df[[col_y]], na.rm = TRUE))
      }
      if (!is.finite(ymax) || ymax <= 0) ymax <- 1

      ybreak <- if (!use_norm_y && nrow(cfg_y)) cfg_y$Interval[1] else NA_real_
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
  
  
  
  ## Ã¢â€â‚¬Ã¢â€â‚¬ reajustar YÃ¢â‚¬â€˜axis cuando se (des)activa la normalizaciÃƒÂ³n Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬  
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
  
  
  # Ã¢â€â‚¬Ã¢â€â‚¬ Sincronizar tÃƒÂ­tulo editable con el tÃƒÂ­tulo por defecto Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬  
  observeEvent(
    list(input$scope, input$tipo, input$param, input$strain,
         input$corr_param_x, input$corr_param_y, input$app_lang),
    {
      req(input$tipo)
      lang <- input$app_lang %||% i18n_lang
      type_label <- switch(
        input$tipo,
        "Boxplot"     = tr_text("plot_boxplot", lang),
        "Barras"      = tr_text("plot_bars", lang),
        "Violin"      = tr_text("plot_violin", lang),
        "Curvas"      = tr_text("plot_curves", lang),
        "Apiladas"    = tr_text("plot_stacked", lang),
        "Correlacion" = tr_text("plot_correlation", lang),
        input$tipo
      )
      defaultTitle <- switch(
        input$tipo,
        "Correlacion" = sprintf(
          tr_text("default_title_corr", lang),
          type_label,
          input$corr_param_y %||% "",
          input$corr_param_x %||% ""
        ),
        if (input$scope == "Combinado")
          sprintf(tr_text("default_title_combined", lang), type_label, input$param %||% "")
        else
          sprintf(tr_text("default_title_strain", lang), type_label, input$param %||% "", input$strain %||% "")
      )
      updateTextInput(session, "plotTitle", value = defaultTitle)
    }, ignoreInit = FALSE)
  
  
  
  # ---- Helpers para filtrar rÃƒÂ©plicas -------------------------------  
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
  
  param_rep_df <- function() {
    df <- datos_combinados()
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(df)
    param <- input$param
    if (is.null(param) || !nzchar(param) || !param %in% names(df)) return(df[0, ])
    df %>%
      filter(!is.na(Strain), !is.na(Media), Strain != "C-") %>%
      filter(is.finite(.data[[param]]))
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
  
  
  
  # Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬
  # Helpers de filtrado + orden basados en la columna Ã¢â‚¬ËœOrdenÃ¢â‚¬â„¢ del platemap
  # Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬
  
  order_filter_strain <- function(df) {
    # 1) Aplica el filtro showMedios
    if (!is.null(input$showMedios)) {
      df <- df %>% filter(Media %in% input$showMedios)
    }
    # 2) Niveles originales segÃƒÂºn Orden
    final_levels <- df %>%
      distinct(Media, Orden) %>%
      arrange(Orden) %>%
      pull(Media)
    # 3) Si el usuario escribiÃƒÂ³ un CSV en orderMedios, lo prioriza
    if (!is.null(input$orderMedios) && nzchar(input$orderMedios)) {
      user_order   <- trimws(strsplit(input$orderMedios, ",")[[1]])
      final_levels <- intersect(user_order, final_levels)
    }
    final_levels <- unique(final_levels)
    # 4) Devuelve Media como factor con niveles en el orden correcto
    df %>% mutate(Media = factor(Media, levels = final_levels))
  }
  
  order_filter_group <- function(df) {
    # 1) Aplica el filtro de rÃƒÂ©plicas por grupo
    df2 <- filter_reps_group(df)
    # 2) Grupos realmente visibles (despuÃƒÂ©s de filtros)
    available <- unique(paste(df2$Strain, df2$Media, sep = "-"))
    # 3) Etiquetas originales segÃƒÂºn Orden solo para los visibles
    platemap_levels <- datos_agrupados() %>%
      distinct(Strain, Media, Orden) %>%
      mutate(Label = paste(Strain, Media, sep = "-")) %>%
      arrange(Orden) %>%
      pull(Label) %>%
      intersect(available)
    # 4) Si el usuario escribiÃƒÂ³ un CSV en orderGroups, lo prioriza
    user_order <- NULL
    if (!is.null(input$orderGroups) && nzchar(input$orderGroups)) {
      user_order <- intersect(trimws(strsplit(input$orderGroups, ",")[[1]]), available)
    }
    # 5) Orden final: primero el pedido explÃƒÂ­cito, luego el resto en orden de platemap
    final_levels <- if (!is.null(user_order) && length(user_order) > 0) {
      c(user_order, setdiff(platemap_levels, user_order))
    } else {
      platemap_levels
    }
    final_levels <- unique(final_levels)
    # 6) Devuelve Label como factor con niveles en el orden correcto (solo los visibles)
    df2 %>% mutate(Label = factor(paste(Strain, Media, sep = "-"), levels = final_levels))
  }
  
  
  
  
  # Ã¢â€â‚¬Ã¢â€â‚¬ Post-hoc dinÃƒÂ¡mico Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬  
  output$postHocUI <- renderUI({  
    input$app_lang
    req(input$sigTest)  
    if (input$sigTest == "ANOVA") {  
      selectInput("postHoc", tr("posthoc_label"),  
                  choices = c(  
                    "Tukey"         = "Tukey",  
                    "Bonferroni"    = "Bonferroni",  
                    "Sidak"         = "Sidak",  
                    "Dunnett"       = "Dunnett",  
                    "Scheffe"       = "Scheffe",  
                    "Games-Howell"  = "GamesHowell"  
                  ),  
                  selected = "Tukey"  
      )  
    } else if (input$sigTest == "Kruskal-Wallis") {  
      selectInput("postHoc", tr("posthoc_label"),  
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
  
  
  
  # Ã¢â€â‚¬Ã¢â€â‚¬ helpers de filtrado + orden Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬  
  # (deja los tuyos: order_filter_strain() / order_filter_group())  
  
  # Ã¢â€â‚¬Ã¢â€â‚¬ Data frame unificado para Significancia Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬  
  make_test_df <- function() {
    empty_df <- tibble::tibble(Label = character(), Valor = numeric())
    p <- input$param
    if (is.null(p) || !length(p) || is.na(p[1]) || !nzchar(p[1])) return(empty_df)
    if (isTRUE(input$doNorm) && !is.null(input$ctrlMedium)) p <- paste0(p, "_Norm")

    src <- if (isTRUE(input$doNorm)) datos_agrupados_norm() else datos_agrupados()
    if (!is.data.frame(src) || !nrow(src) || !p %in% names(src)) return(empty_df)
    if (input$scope == "Por Cepa" &&
        (is.null(input$strain) || is.na(input$strain) || !nzchar(input$strain))) {
      return(empty_df)
    }

    if (input$scope == "Por Cepa") {
      src %>%
        filter(Strain == input$strain) %>%
        order_filter_strain() %>%
        filter_reps_strain() %>%
        transmute(Label = Media,
                  Valor = .data[[p]]) %>%
        filter(is.finite(Valor))
    } else {
      src %>%
        order_filter_group() %>%
        transmute(Label,
                  Valor = .data[[p]]) %>%
        filter(is.finite(Valor))
    }
  }
  
  
  
  # Ã¢â€â‚¬Ã¢â€â‚¬ Reusar el mismo para Normalidad Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬  
  make_norm_df <- make_test_df  
  
  observe({  
    # aseguramos que ya hay configuraciÃƒÂ³n y datos  
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
  
  
  # Ã¢â€â‚¬Ã¢â€â‚¬ Normalidad Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬  
  norm_res <- eventReactive(input$runNorm, {  
    lang <- input$app_lang %||% i18n_lang
    df <- make_norm_df()  
    # Deben existir al menos 2 grupos distintos  
    if (nrow(df)==0 || dplyr::n_distinct(df$Label)<2) {  
      showNotification(tr_text("norm_min_groups", lang), type="error", duration=4)  
      return(tibble::tibble(Label=character(), shapiro.stat=numeric(), shapiro.p=numeric(),  
                            ks.stat=numeric(), ks.p=numeric(),  
                            ad.stat=numeric(), ad.p=numeric()))  
    }  
    # ShapiroÃ¢â‚¬â€œWilk  
    sw <- df %>% group_by(Label) %>%  
      summarise(  
        shapiro.stat = stats::shapiro.test(Valor)$statistic,  
        shapiro.p    = stats::shapiro.test(Valor)$p.value,  
        .groups="drop"  
      )  
    res <- sw  
    # KolmogorovÃ¢â‚¬â€œSmirnov (opcional)  
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
    # AndersonÃ¢â‚¬â€œDarling (opcional, con nortest) Ã¢â‚¬â€ sÃƒÂ³lo si n Ã¢â€°Â¥ 8  
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
  
  
  # Ã¢â€â‚¬Ã¢â€â‚¬ Significancia Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬  
  sig_res <- eventReactive(input$runSig, {
    lang <- input$app_lang %||% i18n_lang
    df_raw <- make_test_df()
    df <- filter_min_obs(df_raw)
    dropped <- setdiff(unique(df_raw$Label), unique(df$Label))
    if (length(dropped) > 0)
      showNotification(sprintf(tr_text("norm_low_obs", lang),
                              paste(dropped, collapse = ", ")), type = "warning", duration = 5)

    if (n_distinct(df$Label) < 2) {
      showNotification(tr_text("norm_groups_insufficient", lang), type = "error", duration = 5)
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
            "Kruskal-Wallis" = do_kw(),  
             "ttest" = {
               if (input$compMode == "all") {
                 safe_pairwise_t(df, "holm")
               } else if (input$compMode == "control") {
                 if (!input$controlGroup %in% df$Label) {
                   showNotification(
                     sprintf(tr_text("control_group_insufficient", lang), input$controlGroup),
                     type = "error", duration = 5
                   )
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
                   showNotification(
                     sprintf(tr_text("sig_insufficient_obs", lang), paste(faltantes, collapse = ", ")),
                     type = "error", duration = 5
                   )
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
                   showNotification(
                     sprintf(tr_text("control_group_insufficient", lang), input$controlGroup),
                     type = "error", duration = 5
                   )
                   tibble()
                 } else {
                   rstatix::wilcox_test(df, Valor ~ Label, ref.group = input$controlGroup)
                 }
               } else {
                 grupos <- c(input$group1, input$group2)
                 sub <- df %>% filter(Label %in% grupos) %>% droplevels()
                 faltantes <- setdiff(grupos, names(table(sub$Label)))
                 if (length(faltantes) > 0) {
                   showNotification(
                     sprintf(tr_text("sig_insufficient_obs", lang), paste(faltantes, collapse = ", ")),
                     type = "error", duration = 5
                   )
                   tibble()
                 } else {
                   rstatix::wilcox_test(sub, Valor ~ Label, paired = can_paired(sub))
                 }
               }
            }
      )
      
    }, error = function(e) {  
      showNotification(paste(tr_text("sig_test_error_prefix", lang), e$message),  
                       type = "error", duration = 5)  
      tibble()  
    })  
  })  
  
  
  # Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬ Hacer que al pulsar abra el panel Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬  
  observeEvent(input$runNorm, {  
    updateCollapse(session, "statsPanel", open = "Analisis EstadÃƒÂ­sticos")  
  })  
  observeEvent(input$runSig, {  
    updateCollapse(session, "statsPanel", open = "Analisis EstadÃƒÂ­sticos")  
  })  
  
  # Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬ Renderizar tabla de normalidad Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬  
  output$normTable <- renderDT({  
    req(input$runNorm)            # solo despuÃƒÂ©s de pulsar  
    lang <- input$app_lang %||% i18n_lang
    df <- norm_res()              # eventReactive definido arriba  
    
    # Ã¢â€â‚¬Ã¢â€â‚¬ Indicadores Ã¢â‚¬Å“Normal / NoÃ¢â‚¬Â para cada test Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬  
    df2 <- df %>%  
      mutate(  
        Shapiro = if_else(!is.na(shapiro.p) & shapiro.p  > 0.05, tr_text("yes_label", lang), tr_text("no_label", lang)),  
        KS      = if_else(!is.na(ks.p)      & ks.p       > 0.05, tr_text("yes_label", lang), tr_text("no_label", lang)),  
        AD      = if_else(!is.na(ad.p)      & ad.p       > 0.05, tr_text("yes_label", lang), tr_text("no_label", lang))  
      )  
    
    # Opcional: ordena columnas a gusto  
    df2 <- df2 %>%   
      dplyr::select(Label,  
                    shapiro.stat, shapiro.p, Shapiro,  
                    ks.stat,      ks.p,      KS,  
                    ad.stat,      ad.p,      AD)  
    validate(need(nrow(df2) > 0, tr_text("no_data_normality", lang)))  
    datatable(df2, options = list(pageLength = 10, scrollX = TRUE))  
  }, server = FALSE)  
  
  
  # Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬ Renderizar tabla de significancia Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬  
  output$sigTable <- renderDT({
    req(input$runSig)
    lang <- input$app_lang %||% i18n_lang
    df <- sig_res()
    validate(need(ncol(df) > 0, tr_text("no_sig_results", lang)))
    # Ã¢â€â‚¬Ã¢â€â‚¬ Unificar nombres de columna de comparaciÃƒÂ³n Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬  
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
                  paste("No se encontrÃƒÂ³ ninguna columna de p-value vÃƒÂ¡lida. Columnas:",
                        paste(names(df), collapse = ", "))))
    pcol <- p_candidates[1]   # primera coincidencia
    
    
    df2 <- df %>%  
      mutate(  
        P_valor       = .data[[pcol]],  
        Significativo = if_else(P_valor < 0.05, "SÃƒÂ­", "No"),  
        Estrellas     = case_when(  
          P_valor < 0.001 ~ "***",  
          P_valor < 0.01  ~ "**",  
          P_valor < 0.05  ~ "*",  
          TRUE            ~ ""  
        )  
      )  
    
    validate(need(nrow(df2)>0, tr_text("no_valid_comparisons", lang)))  
    datatable(df2, options = list(pageLength=10, scrollX=TRUE))  
  }, server = FALSE)  
  
  # Tabla de valores (debajo del grafico) segun tipo  
  output$statsTable <- renderDT({  
    tipo <- input$tipo %||% ""  
    base_df <- if (isTRUE(input$doNorm)) datos_agrupados_norm() else datos_agrupados()  
    lang <- input$app_lang %||% i18n_lang
    
    # Helper: aplica filtros de scope y rÃƒÂ©plicas  
    filter_scope <- function(df){  
      if (input$scope == "Por Cepa") {  
        df %>% filter(Strain == input$strain) %>% order_filter_strain() %>% filter_reps_strain()  
      } else {  
        df %>% order_filter_group() %>% filter_reps_group()  
      }  
    }  
    
    if (tipo %in% c("Barras", "Boxplot", "Violin")) {  
      param <- safe_param()  
      validate(need(param %in% names(base_df), tr_text("param_not_available", lang)))  
      df <- filter_scope(base_df) %>%  
        filter(!is.na(Strain), !is.na(Media), !is.na(BiologicalReplicate)) %>%  
        mutate(Valor = .data[[param]]) %>%
        filter(is.finite(Valor))
      drop_all <- as.character(input$rm_reps_all %||% character(0))  
      if (length(drop_all)) df <- df %>% filter(!as.character(BiologicalReplicate) %in% drop_all)  
      validate(need(nrow(df) > 0, tr_text("no_data_selection", lang)))  
      
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
      validate(need(length(params) > 0, tr_text("no_params_to_show", lang)))  
      df <- filter_scope(base_df)  
      if (!"Label" %in% names(df)) {  
        if (input$scope == "Por Cepa") {  
          df <- df %>% mutate(Label = Media)  
        } else {  
          df <- df %>% mutate(Label = paste(Strain, Media, sep = "-"))  
        }  
      }  
      df <- df %>% mutate(BiologicalReplicate = as.character(BiologicalReplicate))  
      validate(need(nrow(df) > 0, tr_text("no_data_selection", lang)))  
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
    
    if (tipo == "Correlacion") {
      raw_x <- input$corr_param_x %||% NULL
      raw_y <- input$corr_param_y %||% NULL
      params <- c(raw_x, raw_y) %>% stats::na.omit()
      validate(need(
        length(params) == 2,
        tr_text("corr_select_two_params", input$app_lang %||% i18n_lang)
      ))
      norm_mode <- input$corr_norm_target %||% "both"
      use_norm_x <- isTRUE(input$doNorm) && !is.null(input$ctrlMedium) && norm_mode %in% c("both", "x_only")
      use_norm_y <- isTRUE(input$doNorm) && !is.null(input$ctrlMedium) && norm_mode %in% c("both", "y_only")
      col_x <- if (use_norm_x) paste0(raw_x, "_Norm") else raw_x
      col_y <- if (use_norm_y) paste0(raw_y, "_Norm") else raw_y
      df <- filter_scope(base_df)
      if (!"Label" %in% names(df)) {
        if (input$scope == "Por Cepa") {
          df <- df %>% mutate(Label = Media)
        } else {
          df <- df %>% mutate(Label = paste(Strain, Media, sep = "-"))
        }
      }
      df <- df %>% mutate(BiologicalReplicate = as.character(BiologicalReplicate))
      validate(need(nrow(df) > 0, tr_text("no_data_selection", lang)))  
      df_long <- df %>%
        tidyr::pivot_longer(cols = dplyr::all_of(c(col_x, col_y)), names_to = "Parametro", values_to = "Valor") %>%
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
    
    datatable(tibble::tibble(Mensaje = "Tabla no disponible para este tipo de grÃƒÂ¡fico"), options = list(dom = 't'))  
  })  
  # Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬  
  
  # Ã¢â€â‚¬Ã¢â€â‚¬ Helper para elegir paleta segÃƒÂºn input$colorMode Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬  
  adv_palette_n <- reactive({
    tipo  <- input$tipo %||% ""
    scope <- input$scope %||% "Por Cepa"
    if (identical(tipo, "Apiladas")) {
      params <- input$stackParams
      if (is.null(params) || !length(params)) {
        ps <- plot_settings()
        params <- if (!is.null(ps)) ps$Parameter else character(0)
      }
      return(length(params))
    }
    if (identical(tipo, "Curvas")) {
      if (is.null(curve_data()) || is.null(datos_combinados())) return(0)
      df_cur <- curve_long_df()
      if (is.null(df_cur) || !nrow(df_cur)) return(0)
      if (scope == "Por Cepa") {
        if (is.null(input$strain)) return(0)
        df_cur <- df_cur %>%
          filter(Strain == input$strain) %>%
          order_filter_strain() %>%
          filter_reps_strain()
        labels <- df_cur$Media
      } else {
        df_cur <- df_cur %>%
          order_filter_group() %>%
          filter_reps_group()
        labels <- if (isTRUE(input$labelMode)) {
          df_cur$Strain
        } else {
          paste(df_cur$Strain, df_cur$Media, sep = "-")
        }
      }
      return(length(unique(as.character(labels))))
    }
    df <- scoped_plot_df()
    if (is.null(df) || !nrow(df)) return(0)
    if (!"Label" %in% names(df)) {
      if (scope == "Por Cepa") {
        df <- df %>% mutate(Label = Media)
      } else {
        df <- df %>% mutate(Label = paste(Strain, Media, sep = "-"))
      }
    }
    if (scope == "Por Cepa") {
      labels <- df$Media
    } else if (isTRUE(input$labelMode)) {
      labels <- df$Strain
    } else {
      labels <- df$Label
    }
    length(unique(as.character(labels)))
  })

  adv_extra_info <- data.frame(
    name = c(
      "Viridis", "Plasma", "Magma", "Inferno", "Cividis",
      "Aqua", "Rose", "Amber", "Slate", "Forest", "Ocean",
      "BlueRed", "PurpleOrange", "GreenBrown",
      "BlueOrange", "TealRed", "PurpleGreen", "CyanMagenta", "BrownTeal",
      "Hue", "OkabeIto", "Tableau", "Kelly", "TolBright", "TolMuted", "TolLight",
      "D3Category10", "D3Category20"
    ),
    category = c(
      "seq", "seq", "seq", "seq", "seq",
      "seq", "seq", "seq", "seq", "seq", "seq",
      "div", "div", "div",
      "div", "div", "div", "div", "div",
      "qual", "qual", "qual", "qual", "qual", "qual", "qual",
      "qual", "qual"
    ),
    maxcolors = c(
      Inf, Inf, Inf, Inf, Inf,
      Inf, Inf, Inf, Inf, Inf, Inf,
      Inf, Inf, Inf,
      Inf, Inf, Inf, Inf, Inf,
      Inf, 8, 10, 22, 7, 9, 9,
      10, 20
    ),
    stringsAsFactors = FALSE
  )

  brewer_palette_choices <- function(type = "seq", filters = character(0), n_classes = 0) {
    info <- RColorBrewer::brewer.pal.info
    cat  <- switch(type, seq = "seq", div = "div", qual = "qual", "seq")
    info <- info[info$category == cat, , drop = FALSE]
    if (length(filters)) {
      if ("colorblind" %in% filters) info <- info[info$colorblind, , drop = FALSE]
      if ("print" %in% filters) info <- info[info$print, , drop = FALSE]
      if ("photocopy" %in% filters) info <- info[info$photocopy, , drop = FALSE]
    }
    choices <- rownames(info)
    extra <- adv_extra_info[adv_extra_info$category == cat, , drop = FALSE]
    if (length(filters)) {
      extra <- extra[0, , drop = FALSE]
    }
    unique(c(choices, extra$name))
  }

  observeEvent(
    list(input$adv_pal_type, input$adv_pal_filters, adv_palette_n()),
    {
      type      <- input$adv_pal_type %||% "seq"
      filters   <- input$adv_pal_filters %||% character(0)
      n_classes <- adv_palette_n()
      choices <- brewer_palette_choices(type, filters, n_classes)
      if (!length(choices)) {
        choices <- brewer_palette_choices(type, character(0), n_classes)
      }
      if (!length(choices)) {
        choices <- brewer_palette_choices(type, character(0), 0)
      }
      selected <- isolate(input$adv_pal_name)
      if (is.null(selected) || !selected %in% choices) {
        selected <- choices[1]
      }
      updateSelectInput(session, "adv_pal_name",
                        choices = choices, selected = selected)
    },
    ignoreInit = FALSE
  )

  get_palette <- function(n) {  
    tone_down <- function(cols, amount = 0.35) {  
      amt <- pmin(pmax(amount, 0), 1)  
      m   <- grDevices::col2rgb(cols)  
      m2  <- m + (255 - m) * amt  
      grDevices::rgb(m2[1, ]/255, m2[2, ]/255, m2[3, ]/255)  
    }  
    if (n <= 0) return(character(0))
    okabe_cols <- c(
      "#E69F00", "#56B4E9", "#009E73", "#F0E442",
      "#0072B2", "#D55E00", "#CC79A7", "#999999"
    )
    tableau_cols <- c(
      "#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F",
      "#EDC949", "#AF7AA1", "#FF9DA7", "#9C755F", "#BAB0AC"
    )
    kelly_cols <- c(
      "#F3C300", "#875692", "#F38400", "#A1CAF1", "#BE0032", "#C2B280",
      "#848482", "#008856", "#E68FAC", "#0067A5", "#F99379", "#604E97",
      "#F6A600", "#B3446C", "#DCD300", "#882D17", "#8DB600", "#654522",
      "#E25822", "#2B3D26", "#F2F3F4", "#222222"
    )
    tol_bright <- c("#4477AA", "#EE6677", "#228833", "#CCBB44",
                    "#66CCEE", "#AA3377", "#BBBBBB")
    tol_muted  <- c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933",
                    "#DDCC77", "#CC6677", "#882255", "#AA4499")
    tol_light  <- c("#77AADD", "#99DDFF", "#44BB99", "#BBCC33", "#AAAA00",
                    "#EEDD88", "#EE8866", "#FFAABB", "#DDDDDD")
    d3_cat10 <- c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD",
                  "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF")
    d3_cat20 <- c("#1F77B4", "#AEC7E8", "#FF7F0E", "#FFBB78", "#2CA02C",
                  "#98DF8A", "#D62728", "#FF9896", "#9467BD", "#C5B0D5",
                  "#8C564B", "#C49C94", "#E377C2", "#F7B6D2", "#7F7F7F",
                  "#C7C7C7", "#BCBD22", "#DBDB8D", "#17BECF", "#9EDAE5")
    take_n <- function(cols, n) {
      if (n <= 0) return(character(0))
      if (n > length(cols)) {
        return(grDevices::colorRampPalette(cols)(n))
      }
      cols[seq_len(n)]
    }
    okabe <- function(n) rep(okabe_cols, length.out = n)
    tableau <- function(n) rep(tableau_cols, length.out = n)
    brew <- function(n, name){  
      info <- RColorBrewer::brewer.pal.info[name, "maxcolors"]  
      pal  <- RColorBrewer::brewer.pal(info, name)  
      rep(pal, length.out = n)  
    }  
    brew_adv <- function(n, name) {
      info <- RColorBrewer::brewer.pal.info
      if (is.null(name) || !nzchar(name) || !name %in% rownames(info)) {
        return(NULL)
      }
      max_col <- info[name, "maxcolors"]
      if (n < 3) {
        pal <- RColorBrewer::brewer.pal(3, name)
        return(pal[seq_len(n)])
      }
      if (n > max_col) {
        base <- RColorBrewer::brewer.pal(max_col, name)
        return(grDevices::colorRampPalette(base)(n))
      }
      RColorBrewer::brewer.pal(n, name)
    }
    adv_palette <- function(n, name) {
      if (is.null(name) || !nzchar(name)) return(NULL)
      if (name %in% rownames(RColorBrewer::brewer.pal.info)) {
        return(brew_adv(n, name))
      }
      switch(name,
             "Viridis"      = viridis::viridis(n),
             "Plasma"       = viridis::plasma(n),
             "Magma"        = viridis::magma(n),
             "Inferno"      = viridis::inferno(n),
             "Cividis"      = viridis::cividis(n),
             "Aqua"         = grDevices::colorRampPalette(
               c("#E8F6F8", "#7FC8D6", "#005B6A")
             )(n),
             "Rose"         = grDevices::colorRampPalette(
               c("#FFF0F3", "#FF8FA3", "#C9184A")
             )(n),
             "Amber"        = grDevices::colorRampPalette(
               c("#FFF3E0", "#FDBA74", "#B45309")
             )(n),
             "Slate"        = grDevices::colorRampPalette(
               c("#F1F5F9", "#94A3B8", "#1E293B")
             )(n),
             "Forest"       = grDevices::colorRampPalette(
               c("#E8F5E9", "#81C784", "#1B5E20")
             )(n),
             "Ocean"        = grDevices::colorRampPalette(
               c("#E0F2FE", "#38BDF8", "#0C4A6E")
             )(n),
             "BlueRed"      = grDevices::colorRampPalette(
               c("#2166AC", "#F7F7F7", "#B2182B")
             )(n),
             "PurpleOrange" = grDevices::colorRampPalette(
               c("#5E3C99", "#F7F7F7", "#E66101")
             )(n),
             "GreenBrown"   = grDevices::colorRampPalette(
               c("#1B7837", "#F7F7F7", "#8C510A")
             )(n),
             "BlueOrange"   = grDevices::colorRampPalette(
               c("#2166AC", "#F7F7F7", "#F4A582")
             )(n),
             "TealRed"      = grDevices::colorRampPalette(
               c("#008080", "#F7F7F7", "#D73027")
             )(n),
             "PurpleGreen"  = grDevices::colorRampPalette(
               c("#762A83", "#F7F7F7", "#1B7837")
             )(n),
             "CyanMagenta"  = grDevices::colorRampPalette(
               c("#00A6D6", "#F7F7F7", "#D81B60")
             )(n),
             "BrownTeal"    = grDevices::colorRampPalette(
               c("#8C510A", "#F7F7F7", "#01665E")
             )(n),
             "Hue"          = safe_hue(n),
             "OkabeIto"     = take_n(okabe_cols, n),
             "Tableau"      = take_n(tableau_cols, n),
             "Kelly"        = take_n(kelly_cols, n),
             "TolBright"    = take_n(tol_bright, n),
             "TolMuted"     = take_n(tol_muted, n),
             "TolLight"     = take_n(tol_light, n),
             "D3Category10" = take_n(d3_cat10, n),
             "D3Category20" = take_n(d3_cat20, n),
             NULL)
    }

    if (isTRUE(input$adv_pal_enable)) {
      pal_adv <- adv_palette(n, input$adv_pal_name)
      if (!is.null(pal_adv)) {
        if (isTRUE(input$adv_pal_reverse)) {
          pal_adv <- rev(pal_adv)
        }
        return(pal_adv)
      }
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
  # Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬  
  
  # Barra de significancia estilo Ã¢â‚¬Å“TÃ¢â‚¬Â (baseÃ¢â‚¬â€˜ggplot2)  
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

  # 1-A  Dibuja UNA barra de significancia tipo Ã¢â‚¬Å“TÃ¢â‚¬Â
  add_sigline <- function(p, group1, group2, label = "*",
                          height   = .05,  # separaciÃƒÂ³n barraÃ¢â‚¬â€˜datos  (proporciÃƒÂ³n del rango Y)
                          vsize    = .02,  # largo de los Ã¢â‚¬Å“postesÃ¢â‚¬Â
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
      get_x   <- function(g) {
        if (is.numeric(g)) return(g)
        g_chr <- wrap_sig_group(g, xbreaks)
        match(g_chr, xbreaks)
      }
    x1 <- get_x(group1);  x2 <- get_x(group2)
    if (any(is.na(c(x1, x2)))) return(p)
    
    yrng_use <- if (is.finite(info$yrng) && info$yrng > 0) info$yrng else 1
    tpad_use <- tpad
    ybar  <- info$ytop + height * yrng_use
    ycap  <- ybar - vsize * yrng_use
    ytxt  <- ybar + tpad_use * yrng_use          # texto un poco mÃƒÂ¡s arriba

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
  # 1-B  Coloca MUCHAS barras sin que se choquen entre sÃƒÂ­
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
        g1 <- if (is.numeric(cmp$g1)) cmp$g1 else wrap_sig_group(cmp$g1, xranks)
        g2 <- if (is.numeric(cmp$g2)) cmp$g2 else wrap_sig_group(cmp$g2, xranks)
        x1 <- if (is.numeric(g1)) g1 else match(g1, xranks)
        x2 <- if (is.numeric(g2)) g2 else match(g2, xranks)
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
    # margen superior del grÃƒÂ¡fico (no del eje) para que se vean barras fuera del panel
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
      title_el <- p_out$theme$plot.title %||% theme_get()$plot.title
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

  add_siglabels <- function(p, sigs, group_tops,
                            tpad        = .01,
                            tsize       = 5,
                            margin_base = NULL,
                            plot_height = NULL,
                            default_param  = NULL) {
    if (length(sigs) == 0) return(p)
    if (is.null(group_tops) || !nrow(group_tops)) return(p)

    build  <- ggplot_build(p)
    if (!length(build$data)) return(p)
    panel  <- build$layout$panel_params[[1]]
    y_range <- panel$y.range %||% panel$y$range
    if (is.null(y_range)) y_range <- c(0, 1)
    y_span <- diff(range(y_range))
    if (!is.finite(y_span) || y_span <= 0) y_span <- 1

    xbreaks <- panel$x$breaks
    if (is.null(xbreaks) || anyNA(xbreaks)) {
      xbreaks <- unique(unlist(lapply(build$data, function(d) d$x)))
    }
    if (is.null(xbreaks) || !length(xbreaks)) return(p)

    gt <- group_tops
    if (!all(c("group", "y_top") %in% names(gt))) return(p)
    gt$group <- as.character(gt$group)
    if ("param" %in% names(gt)) gt$param <- as.character(gt$param)

    label_rows <- list()
    label_info <- list()
    y_txt_vals <- numeric(length(sigs))

    for (i in seq_along(sigs)) {
      cmp <- sigs[[i]]
      grp <- cmp$g2
      if (is.null(grp) || !nzchar(as.character(grp))) {
        y_txt_vals[i] <- NA_real_
        next
      }
        grp_chr <- wrap_sig_group(grp, gt$group)

      if ("param" %in% names(gt)) {
        param_sel <- cmp$param %||% default_param %||% ""
        if (is.na(param_sel)) param_sel <- ""
        if (nzchar(param_sel)) {
          row <- gt[gt$group == grp_chr & gt$param == param_sel, , drop = FALSE]
          if (!nrow(row)) {
            row <- gt[gt$group == grp_chr, , drop = FALSE]
            y_top <- if (nrow(row)) max(row$y_top, na.rm = TRUE) else NA_real_
          } else {
            y_top <- row$y_top[1]
          }
        } else {
          row <- gt[gt$group == grp_chr, , drop = FALSE]
          y_top <- if (nrow(row)) max(row$y_top, na.rm = TRUE) else NA_real_
        }
      } else {
        row <- gt[gt$group == grp_chr, , drop = FALSE]
        y_top <- if (nrow(row)) row$y_top[1] else NA_real_
      }
      if (!is.finite(y_top)) {
        y_txt_vals[i] <- NA_real_
        next
      }
      tpad_use <- sig_tpad(cmp$lab, tpad)
      ytxt <- y_top + tpad_use * y_span
          x_val <- if (is.numeric(grp)) grp else match(grp_chr, xbreaks)
      if (is.na(x_val)) {
        y_txt_vals[i] <- NA_real_
        next
      }
      label_rows[[length(label_rows) + 1]] <- data.frame(
        x = x_val,
        y = ytxt,
        label = cmp$lab,
        .sig_layer = TRUE,
        stringsAsFactors = FALSE
      )
      label_info[[length(label_info) + 1]] <- list(
        x = x_val,
        y = ytxt,
        label = cmp$lab,
        textsize = tsize
      )
      y_txt_vals[i] <- ytxt
    }

    if (!length(label_rows)) return(p)
    txt_df <- do.call(rbind, label_rows)
    p_out <- p +
      geom_text(
        data = txt_df,
        inherit.aes = FALSE,
        aes(x = x, y = y, label = label),
        size = tsize,
        vjust = 0
      )

    overflow   <- max(c(0, y_txt_vals - max(y_range)), na.rm = TRUE)
    ph         <- plot_height %||% input$plot_h %||% 700
    if (!is.numeric(ph) || !is.finite(ph) || ph <= 0) ph <- 700
    overflow_pt <- (overflow / y_span) * ph
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
      title_el <- p_out$theme$plot.title %||% theme_get()$plot.title
      if (!is.null(title_el)) {
        p_out <- p_out + theme(plot.title = bump_title_margin(title_el, title_extra))
      }
    }

    attr(p_out, "sig_plotly") <- list(
      labels = label_info,
      y_range = y_range,
      xbreaks = xbreaks,
      show_caps = FALSE
    )

    p_out <- p_out + coord_cartesian(clip = 'off')
    p_out
  }

  apply_sig_layers <- function(p, group_tops = NULL, margin_base = NULL,
                               plot_height = NULL, default_param = NULL) {
    sigs <- sig_list()
    if (!length(sigs)) return(p)
    if (identical(input$sig_mode, "labels")) {
      return(add_siglabels(
        p, sigs, group_tops,
        tpad = input$sig_textpad,
        tsize = input$sig_textsize,
        margin_base = margin_base,
        plot_height = plot_height,
        default_param = default_param
      ))
    }
    stack_siglines(p, sigs,
                   sep         = input$sig_sep,
                   base_height = input$sig_offset,
                   linewidth   = input$sig_linewidth,
                   vsize       = .02,
                   tpad        = input$sig_textpad,
                   tsize       = input$sig_textsize,
                   margin_base = margin_base,
                   plot_height = plot_height,
                   show_caps   = !isTRUE(input$sig_hide_caps))
  }

  export_plotly_image <- function(p, file,
                                  width, height,
                                  delay = 0.5,   # deja que Plotly acabe de renderizar
                                  zoom  = 3) {    # 3 Ãƒâ€” Ã¢â€¡â€™ 300 dpi aprox. si usas 100 px = 1 in
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
      zoom     = zoom          # Ã¢â€ â€˜ resoluciÃƒÂ³n final  = zoom Ãƒâ€” vwidth
    )
  }



  ###############################################################################  
  # build_plotly_stack() Ã¢â‚¬â€œ 100 % reactiva a todos los controles del panel  
  ###############################################################################  
  build_plotly_stack <- function(scope, strain = NULL, width = NULL, height = NULL) {
    
    lang <- input$app_lang %||% i18n_lang
    num <- function(x) as.numeric(gsub(",", ".", x))  
    
    params_apilar <- input$stackParams  
    validate(need(
      length(params_apilar) > 0,
      sprintf(tr_text("select_param_at_least_one", lang), tr_text("stack_params", lang))
    ))  
    
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
    # --- ÃƒÂ¡ngulo de las etiquetas del eje X -------------------------------
    x_ang <- get_x_angle(
      n           = length(eje_levels),
      angle_input = input$x_angle
    )
    b_mar <- get_bottom_margin(x_ang, input$x_wrap, input$x_wrap_lines)
    extra_bottom <- ceiling(input$fs_axis * 0.8)
    b_mar <- b_mar + extra_bottom
    
    
    ## 3 Ã‚Â· Paleta --------------------------------------------------------------  
    pal <- get_palette(length(params_apilar))  
    names(pal) <- params_apilar  
    
    ## 4 Ã‚Â· Trazas de barras -----------------------------------------------------  
    outline_only <- isTRUE(input$stack_outline_only)
    seg_line_w <- if (outline_only) 0 else 1
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
                        line  = list(color = "black", width = seg_line_w))  
      )  
    }  
    
    ## 5 Ã‚Â· Trazas Ã¢â‚¬Å“fantasmaÃ¢â‚¬Â con barras de error --------------------------------  
    if (isTRUE(input$showErrBars)) {
      
      err_df <- df_long %>%                       # tope de cada tramo
        group_by(.data[[eje_x]]) %>%
        arrange(factor(Parametro, levels = stack_levels), .by_group = TRUE) %>%
        mutate(y_top = cumsum(Mean)) %>%
        ungroup()
      
      thick <- num(input$errbar_size) * 1.6       # grosor cabeza
      use_param_color <- isTRUE(input$errbar_param_color)
      
      for (p in stack_levels) {                   # una traza fantasma por tramo
        sub <- err_df[err_df$Parametro == p, ]
        err_color <- if (use_param_color) pal[[p]] else "black"
        if (is.null(err_color) || is.na(err_color) || !nzchar(as.character(err_color))) {
          err_color <- "black"
        }
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
            color      = err_color,
            thickness  = thick,
            width      = 20                       # longitud de la "cabeza"
          )
        )
      }
    }
    
    outline_shapes <- NULL
    if (outline_only) {
      border_df <- df_long %>%
        group_by(.data[[eje_x]]) %>%
        summarise(total = sum(Mean, na.rm = TRUE), .groups = "drop")
      pos_map <- seq_along(eje_levels) - 1
      names(pos_map) <- eje_levels
      border_df <- border_df %>%
        mutate(xpos = pos_map[as.character(.data[[eje_x]])]) %>%
        filter(is.finite(xpos), is.finite(total))
      bar_w <- 0.8
      outline_shapes <- lapply(seq_len(nrow(border_df)), function(i) {
        list(
          type = "rect",
          layer = "above",
          xref = "x",
          yref = "y",
          x0 = border_df$xpos[i] - bar_w / 2,
          x1 = border_df$xpos[i] + bar_w / 2,
          y0 = 0,
          y1 = border_df$total[i],
          line = list(color = "black", width = 1),
          fillcolor = "rgba(0,0,0,0)"
        )
      })
    }
    
    ## 6 Ã‚Â· Layout (ejes y cuadrÃƒÂ­cula) ------------------------------------------  
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
          y    = 0.95                     # opcional: tambiÃƒÂ©n puedes mover el tÃƒÂ­tulo un poco hacia abajo
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
        ),
        shapes = outline_shapes
      )

    if (identical(input$sig_mode, "labels")) {
      sigs <- sig_list()
      if (length(sigs)) {
        label_tops <- df_long %>%
          mutate(
            group = .data[[eje_x]],
            Parametro = factor(Parametro, levels = stack_levels)
          ) %>%
          arrange(group, Parametro) %>%
          group_by(group) %>%
          mutate(
            ybottom = cumsum(Mean) - Mean,
            ytop    = ybottom + Mean
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
          transmute(group = as.character(group),
                    param = as.character(Parametro),
                    y_top = y_top)

        y_max <- input$ymax
        y_max_data <- max(label_tops$y_top, na.rm = TRUE)
        if (!is.numeric(y_max) || !is.finite(y_max) || y_max <= 0) {
          y_max <- y_max_data
        } else if (is.finite(y_max_data) && y_max < y_max_data) {
          y_max <- y_max_data
        }
        if (!is.finite(y_max) || y_max <= 0) y_max <- 1
        y_range <- c(0, y_max)
        y_span <- diff(range(y_range))
        if (!is.finite(y_span) || y_span <= 0) y_span <- 1

        label_info <- list()
        y_txt_vals <- numeric(length(sigs))
        default_param <- input$sig_param
        use_param_color <- isTRUE(input$sig_label_param_color)

        for (i in seq_along(sigs)) {
          cmp <- sigs[[i]]
          grp <- cmp$g2
          if (is.null(grp) || !nzchar(as.character(grp))) {
            y_txt_vals[i] <- NA_real_
            next
          }
          grp_chr <- wrap_sig_group_html(grp, label_tops$group)
          param_sel <- cmp$param %||% default_param %||% ""
          if (is.na(param_sel)) param_sel <- ""
          if (nzchar(param_sel)) {
            row <- label_tops[label_tops$group == grp_chr &
                                label_tops$param == as.character(param_sel), , drop = FALSE]
            if (!nrow(row)) {
              row <- label_tops[label_tops$group == grp_chr, , drop = FALSE]
              y_top <- if (nrow(row)) max(row$y_top, na.rm = TRUE) else NA_real_
            } else {
              y_top <- row$y_top[1]
            }
          } else {
            row <- label_tops[label_tops$group == grp_chr, , drop = FALSE]
            y_top <- if (nrow(row)) max(row$y_top, na.rm = TRUE) else NA_real_
          }
          if (!is.finite(y_top)) {
            y_txt_vals[i] <- NA_real_
            next
          }
          tpad_use <- sig_tpad(cmp$lab, input$sig_textpad)
          ytxt <- y_top + tpad_use * y_span
          param_color <- NULL
          if (use_param_color && nzchar(param_sel)) {
            pal_val <- pal[[param_sel]]
            if (!is.null(pal_val) && nzchar(as.character(pal_val))) {
              param_color <- unname(as.character(pal_val))
            }
          }
          label_info[[length(label_info) + 1]] <- list(
            x = grp_chr,
            y = ytxt,
            label = cmp$lab,
            textsize = input$sig_textsize,
            color = param_color
          )
          y_txt_vals[i] <- ytxt
        }

        if (length(label_info)) {
          sig_info <- list(
            labels = label_info,
            y_range = y_range
          )
          plt <- add_sig_shapes_plotly(plt, sig_info)

          overflow   <- max(c(0, y_txt_vals - max(y_range)), na.rm = TRUE)
          ph         <- canvas_h %||% input$plot_h %||% 700
          if (!is.numeric(ph) || !is.finite(ph) || ph <= 0) ph <- 700
          overflow_pt <- (overflow / y_span) * ph
          extra_top   <- if (overflow > 0) max(60, overflow_pt * 1.2 + input$sig_textsize * 2) else 0
          if (is.finite(extra_top) && extra_top > 0) {
            pt_to_px <- 96 / 72
            extra_px <- extra_top * pt_to_px
            cur_h <- plt$x$layout$height %||% ph
            if (!is.numeric(cur_h) || !is.finite(cur_h) || cur_h <= 0) cur_h <- ph
            cur_margin <- plt$x$layout$margin %||% list(t = 0, r = 0, b = 0, l = 0)
            plt <- plt %>% layout(
              height = cur_h + extra_px,
              margin = modifyList(cur_margin,
                                  list(t = (cur_margin$t %||% 0) + extra_px))
            )
          }
        }
      }
    }
    plt  
  }  
  ###############################################################################  




  # --------------------------------------------------------------------  
  # FunciÃƒÂ³n auxiliar para exportar PNG sin tocar <input> (para ZIP)  
  #         Ã¢â€“Âº MISMO LOOK que plot_base()  
  # --------------------------------------------------------------------  

  build_plot <- function(scope, strain = NULL, tipo) {  
    lang <- input$app_lang %||% i18n_lang
    req(plot_settings(), input$param)  
    
    # Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬ Nuevo bloque para normalizaciÃƒÂ³n Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬  
    rawParam <- enc2utf8(input$param)  
    is_norm  <- isTRUE(input$doNorm)  
    msg_no_data_sel <- tr_text("no_data_selection", lang)
    choose_param_column <- function(df) {
      # Prioriza la columna normalizada; si no hay valores finitos, usa la cruda.
      cand <- list(param_sel, rawParam)
      for (nm in cand) {
        if (!nm %in% names(df)) next
        vec <- suppressWarnings(as.numeric(df[[nm]]))
        if (any(is.finite(vec))) return(list(col = nm, values = vec, fallback = (nm == rawParam)))
      }
      # Sin columnas válidas: notifica y devuelve vector vacío para gatillar mensaje "no data".
      showNotification(tr_text("norm_failed_generic", lang), type = "warning", duration = 5)
      list(col = param_sel, values = numeric(0), fallback = TRUE)
    }
    # -------------------------------------------------------------------
    # -------------------------------------------------------------------
    
    param_sel <- if (is_norm) paste0(rawParam, "_Norm") else rawParam  
    
    # toma siempre la config del parÃƒÂ¡metro sin Ã¢â‚¬Å“_NormÃ¢â‚¬Â  
    # Ã¢â€ â€™ calcula el tÃƒÂ­tulo por defecto  
    ps       <- plot_settings() %>% filter(Parameter == rawParam)  
    norm_suffix <- tr_text("normalized_suffix")
    defaultY <- if (is_norm)  
      paste0(ps$Y_Title, " ", norm_suffix)  
    else  
      ps$Y_Title  
    if (is.null(defaultY) || !length(defaultY) || is.na(defaultY)) defaultY <- rawParam  
    
    # Ã¢â€ â€™ si el usuario puso algo en input$yLab, ÃƒÂºsalo; sino el default  
    ylab     <- if (nzchar(input$yLab)) input$yLab else defaultY  
    
    
    # lÃƒÂ­mites segÃƒÂºn param_sel  
    lims    <- get_ylim(param_sel)  
    ymax    <- lims$ymax  
    ybreak  <- lims$ybreak  
    # Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬  
    
    ## ---- parche: si ymax o ybreak no son finitos, asigna valores seguros  
    if (!is.finite(ymax)  || ymax  <= 0) ymax  <- 1  
    if (!is.finite(ybreak) || ybreak <= 0) ybreak <- ymax / 5  
    
    # Ã¢â‚¬â€Ã¢â‚¬â€Ã¢â‚¬â€ 2) Estilos comunes Ã¢â‚¬â€Ã¢â‚¬â€Ã¢â‚¬â€  
    colourMode <- input$colorMode  
    fs_title   <- input$fs_title  
    fs_axis    <- input$fs_axis  
    fs_legend  <- input$fs_legend      # Ã¢â€ Â nuevo  
    axis_size  <- input$axis_line_size  
    # coef = Inf produce outliers NA cuando los datos son planos; usamos un valor finito grande
    # para mantener los bigotes en min/max sin romper ggplotly.
    box_coef   <- 1e6
    scope_df <- get_scope_df(scope, strain)
    box_stats <- NULL
    
    if (tipo %in% c("Boxplot", "Barras", "Violin")) {
      if (is.null(param_sel) || !nzchar(param_sel) || !param_sel %in% names(scope_df)) {
        return(
          ggplot() +
            theme_void() +
            annotate("text", 0, 0, label = tr_text("param_no_data_selection", lang))
        )
      }
    }
    
    
    # 0) Si es Curvas, lo procesamos aquÃƒÂ­ y devolvemos  
    
    # Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬ 3.x) NUEVO: Barras apiladas Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬  
    if (tipo == "Apiladas") {  
      params_apilar <- input$stackParams  
      if (length(params_apilar) == 0) {  
        return(  
          ggplot() + theme_void() +  
            annotate("text", 0, 0, label = tr_text("select_params_prompt", lang))  
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
                       sprintf(tr_text("missing_params_list", lang),  
                               paste(missing, collapse = ", "))  
            )  
        )  
      }  
      
      # 3) Prepara df_long con medias y SD  
      # -----------------------------
      # Tomo el orden pedido por el usuario,
      # pero me quedo sÃƒÂ³lo con los parÃƒÂ¡metros que estÃƒÂ¡n seleccionados
      order_stack_input <- trimws(strsplit(input$orderStack, ",")[[1]])
      order_levels      <- intersect(order_stack_input, params_apilar)
      stack_levels      <- if (length(order_levels)) order_levels else params_apilar
      # -----------------------------
      
      # 1) Elige la variable de eje X segÃƒÂºn el scope  
      eje_x <- if (scope == "Por Cepa") {
        "Media"
      } else if (isTRUE(input$labelMode)) {
        # si piden sÃƒÂ³lo cepa, usamos directamente la columna Strain
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
      # --- ÃƒÂ¡ngulo de las etiquetas del eje X -------------------------------
      x_ang <- get_x_angle(
        n           = length(unique(df_long[[eje_x]])),
        angle_input = input$x_angle
      )
      b_mar <- get_bottom_margin(x_ang, input$x_wrap, input$x_wrap_lines)
      extra_bottom <- ceiling(input$fs_axis * 0.8)
      b_mar <- b_mar + extra_bottom
      
      
      # 4) GrÃƒÂ¡fico base  
      pal <- get_palette(length(params_apilar))  
      names(pal) <- params_apilar  
      legend_breaks <- stack_levels
      tone_down_cols <- function(cols, amount = 0.25) {
        amt <- pmin(pmax(amount, 0), 1)
        m   <- grDevices::col2rgb(cols)
        m2  <- m + (255 - m) * amt
        grDevices::rgb(m2[1, ]/255, m2[2, ]/255, m2[3, ]/255)
      }
      outline_only <- isTRUE(input$stack_outline_only)
      seg_line_col <- if (outline_only) NA else "black"
      p <- ggplot(df_long,
                  aes(x = .data[[eje_x]], y = Mean, fill = Parametro)) +
        geom_col(
          position = "stack",
          width    = 0.7,
          colour   = seg_line_col,
          linewidth = 0.6,
          alpha    = 0.85
        ) +
        scale_fill_manual(
          values  = tone_down_cols(pal[stack_levels], amount = 0.25),   # respeta el orden del usuario
          breaks  = legend_breaks,
          guide   = guide_legend(title = NULL, reverse = FALSE) # leyenda sigue el orden definido
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
      
      
      # 5 Ã‚Â·  BARRAS DE DESVIACIÃƒâ€œN Ã¢â‚¬â€œ versiÃƒÂ³n auto-contenida -------------------
      if (isTRUE(input$showErrBars)) {
        
        ## ancho de la barra Ã¢â€ â€™ 0.7  Ã¢â€ â€™ mitad = 0.35
        cap_half_width <- 0.7 / 2   # Ã¢â€ Â NUEVO
        
        err_df <- df_long %>%                        # Ã¢â‚¬Â¦ (cÃƒÂ³digo idÃƒÂ©ntico)
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
        
        if (isTRUE(input$errbar_param_color)) {
          err_df$err_color <- pal[as.character(err_df$Parametro)]
          err_df$err_color[is.na(err_df$err_color) | !nzchar(err_df$err_color)] <- "black"
          p <- p +
            geom_segment(
              data        = err_df,
              inherit.aes = FALSE,
              aes(x = xnum, xend = xnum, y = ystart, yend = yend, color = err_color),
              size        = input$errbar_size,
              show.legend = FALSE
            ) +
            geom_segment(
              data        = err_df,
              inherit.aes = FALSE,
              aes(
                x    = xnum - cap_half_width,
                xend = xnum + cap_half_width,
                y    = yend,
                yend = yend,
                color = err_color
              ),
              size        = input$errbar_size,
              show.legend = FALSE
            ) +
            scale_color_identity()
        } else {
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
      }
      
      
      # 6) Etiquetas, lÃƒÂ­mites y tema final  
      label_tops <- df_long %>%
        mutate(
          group = .data[[eje_x]],
          Parametro = factor(Parametro, levels = stack_levels)
        ) %>%
        arrange(group, Parametro) %>%
        group_by(group) %>%
        mutate(
          ybottom = cumsum(Mean) - Mean,
          ytop    = ybottom + Mean
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
      base_margin <- margin_adj(12, 18, b_mar, 28)
      p <- p +  
        labs(title = input$plotTitle, x = NULL, y = if (nzchar(input$yLab)) input$yLab else ps$Y_Title) +  
        scale_y_continuous(limits = c(0, input$ymax), breaks = seq(0, input$ymax, by = input$ybreak), expand = c(0,0)) +  
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
          legend.text     = element_text(size = fs_legend, colour = "black"),
          legend.key      = element_blank(),
          legend.key.size = unit(1.4, "lines")
        )  

      p <- apply_sig_layers(p,
                            group_tops = label_tops,
                            margin_base = base_margin,
                            plot_height = input$plot_h,
                            default_param = input$sig_param)

      return(p)  
    }  
    
    # --- 3.x) Correlacion -------------------------------------------------
    if (tipo == "Correlacion") {
      
      ## 1) nombres reales de las columnas (con o sin _Norm)
      raw_x   <- input$corr_param_x
      raw_y   <- input$corr_param_y
      norm_mode <- input$corr_norm_target %||% "both"
      use_norm_x <- isTRUE(input$doNorm) && !is.null(input$ctrlMedium) &&
        norm_mode %in% c("both", "x_only")
      use_norm_y <- isTRUE(input$doNorm) && !is.null(input$ctrlMedium) &&
        norm_mode %in% c("both", "y_only")
      param_x <- if (use_norm_x) paste0(raw_x, "_Norm") else raw_x
      param_y <- if (use_norm_y) paste0(raw_y, "_Norm") else raw_y
      
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
      
      ## 4) chequeo de puntos
      validate(need(
        nrow(df) >= 3,
        tr_text("corr_min_points", input$app_lang %||% i18n_lang)
      ))
      
      cor_res <- cor.test(df$X, df$Y, method = input$corr_method)
      fit     <- NULL
      r2_val  <- NULL
      if (isTRUE(input$corr_show_eq) || isTRUE(input$corr_show_r2)) {
        fit    <- lm(Y ~ X, data = df)
        r2_val <- summary(fit)$r.squared
      }
      
      ## 5) Ecuacion de la recta (opcional) y estadisticos
      eq_lab <- NULL
      if (isTRUE(input$corr_show_eq)) {
        fit       <- fit %||% lm(Y ~ X, data = df)
        slope     <- coef(fit)[2]
        intercept <- coef(fit)[1]
        eq_lab    <- sprintf("y = %.3f x %+.3f", slope, intercept)
      }
      stats_lines <- character()
      if (isTRUE(input$corr_show_r))  stats_lines <- c(stats_lines, sprintf("r = %.3f", cor_res$estimate))
      if (isTRUE(input$corr_show_p))  stats_lines <- c(stats_lines, sprintf("p = %.3g", cor_res$p.value))
      if (isTRUE(input$corr_show_r2)) {
        r2_use <- if (!is.null(r2_val)) r2_val else summary(lm(Y ~ X, data = df))$r.squared
        stats_lines <- c(stats_lines, sprintf("R^2 = %.3f", r2_use))
      }
      stats_lab <- if (length(stats_lines)) paste(stats_lines, collapse = "\n") else NULL
      
      ## 6) saneo de ejes y posiciones de los textos
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
      eq_y <- if (is.null(stats_lab)) y_t else y_t - dy*2.3

      ## 7) grafico --------------------------------------------------------
      p <- ggplot(df, aes(X, Y)) +
        geom_point(size = 3, colour = "black") +
        { if (isTRUE(input$corr_show_line))
          geom_smooth(method = "lm", se = FALSE,
                      colour = "black", linetype = "dashed") } +
        { if (isTRUE(input$corr_show_labels))
          geom_text(aes(label = Label),
                    nudge_y = 0.05 * (ymax - ymin),
                    size    = input$corr_label_size) } +
        { if (!is.null(stats_lab))
          annotate("text",
                   x = x_t, y = y_t, hjust = 1, vjust = 1,
                   label = stats_lab,
                   size = 5) } +
        { if (!is.null(eq_lab))
          annotate("text",
                   x = x_t, y = eq_y,
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
        coord_cartesian(clip = "off") +     # solo una vez
        theme(
          plot.margin = margin_adj(20, 50, 10, 10),
          plot.title  = element_text(size = input$fs_title, face = "bold"),
          axis.title  = element_text(size = input$fs_axis,  face = "bold", colour = "black"),
          axis.text   = element_text(size = input$fs_axis, colour = "black"),
          axis.line   = element_line(linewidth = input$axis_line_size, colour = "black"),
          axis.ticks  = element_line(linewidth = input$axis_line_size, colour = "black"),
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
      # 2) Filtrar y ordenar segÃƒÂºn ÃƒÂ¡mbito
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
      
      # 3) Extraer etiquetas de ejes desde la configuraciÃƒÂ³n (ahora editables)
        cfg_cur <- curve_settings()[1, ]
        x_lab <- if (nzchar(input$cur_xlab)) input$cur_xlab else cfg_cur$X_Title
        y_lab <- if (nzchar(input$cur_ylab)) input$cur_ylab else cfg_cur$Y_Title
        b_mar <- get_bottom_margin(0)
        
        x_max <- if (!is.null(input$xmax_cur) && is.finite(input$xmax_cur)) input$xmax_cur else cfg_cur$X_Max
        y_max <- if (!is.null(input$ymax_cur) && is.finite(input$ymax_cur)) input$ymax_cur else cfg_cur$Y_Max
        x_break <- if (!is.null(input$xbreak_cur) && is.finite(input$xbreak_cur) && input$xbreak_cur > 0) input$xbreak_cur else cfg_cur$Interval_X
        y_break <- if (!is.null(input$ybreak_cur) && is.finite(input$ybreak_cur) && input$ybreak_cur > 0) input$ybreak_cur else cfg_cur$Interval_Y
        data_x_max <- suppressWarnings(max(df_sum$Time, na.rm = TRUE))
        data_y_max <- suppressWarnings(max(df_sum$Avg, na.rm = TRUE))
        if (!is.finite(x_max) || x_max <= 0) x_max <- data_x_max
        if (!is.finite(y_max) || y_max <= 0) y_max <- data_y_max
        if (!is.finite(x_max) || x_max <= 0) x_max <- 1
        if (!is.finite(y_max) || y_max <= 0) y_max <- 1
        if (!is.finite(x_break) || x_break <= 0) x_break <- x_max / 3
        if (!is.finite(y_break) || y_break <= 0) y_break <- y_max / 3
        if (!is.finite(x_break) || x_break <= 0) x_break <- 1
        if (!is.finite(y_break) || y_break <= 0) y_break <- 1
      
      # 4) Crear grÃƒÂ¡fico con ggplot2
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
    
    
    
    
    
    
    
    # Ã¢â‚¬â€Ã¢â‚¬â€Ã¢â‚¬â€ 3) LÃƒÂ³gica principal Ã¢â‚¬â€Ã¢â‚¬â€Ã¢â‚¬â€  
    if (scope == "Combinado") {  
      
      # --- 3.1) Boxplot combinado ---  
      if (scope == "Combinado" && tipo == "Boxplot") {
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
          df_plot$Label   <- wrap_label(df_plot$Label,   lines = input$x_wrap_lines)
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
            q1     = stats::quantile(Valor, 0.25, na.rm = TRUE, names = FALSE),
            median = stats::median(Valor, na.rm = TRUE),
            q3     = stats::quantile(Valor, 0.75, na.rm = TRUE, names = FALSE),
            lower  = min(Valor, na.rm = TRUE),
            upper  = max(Valor, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(group = as.character(Label)) %>%
          dplyr::select(group, q1, median, q3, lower, upper)
        pal  <- palette_for_labels(df_labels, levels(df_plot$Label))
        x_ang <- get_x_angle(
          n            = nlevels(df_plot$Label),   # Ã¢â€ Â categorÃƒÂ­as reales
          angle_input  = input$x_angle
        )
        b_mar <- get_bottom_margin(x_ang, input$x_wrap, input$x_wrap_lines)
        p <- ggplot(df_plot, aes(Label, Valor))
        
        if (input$colorMode == "Blanco y Negro") {  
          p <- p +
            geom_boxplot(fill = "white", colour = "black", width = input$box_w,
                         linewidth = input$errbar_size,
                         coef = box_coef,
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
                         coef = box_coef,
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
            limits = c(0, ymax_plot),
            breaks = seq(0, ymax_plot, by = ybreak),
            expand = c(0,0),
            oob    = scales::oob_keep
          ) +  
          theme_minimal(base_size = input$base_size, base_family = "Helvetica") +
          coord_cartesian(clip = "off") +
          theme(
            plot.margin = base_margin,   # margen izq. +20 px aprox.
            plot.title      = element_text(size = fs_title, face = "bold"),  
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
            panel.grid      = element_blank(),  
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
        p <- apply_sig_layers(p,
                              group_tops = sig_tops,
                              margin_base = base_margin,
                              plot_height = input$plot_h)
        if (!is.null(box_stats)) {
          attr(p, "box_stats") <- box_stats
        }
        
        return(p)  
      }  
      
      # --- 3.2) ViolÃƒÂ­n combinado ---
      if (scope == "Combinado" && tipo == "Violin") {
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

        sig_tops <- df_plot %>%
          group_by(Label) %>%
          summarise(y_top = max(Valor, na.rm = TRUE), .groups = "drop") %>%
          mutate(y_top = ifelse(is.finite(y_top), y_top, NA_real_)) %>%
          rename(group = Label)
        p <- apply_sig_layers(p,
                              group_tops = sig_tops,
                              margin_base = base_margin,
                              plot_height = input$plot_h)

        return(p)
      }
      
      # --- 3.2) Barras combinado ---  
      if (scope == "Combinado" && tipo == "Barras") {  
        df_raw <- scope_df %>%
          filter(is.finite(.data[[param_sel]]))
        df_labels <- df_raw %>% distinct(Label, Strain, Media)

        if (isTRUE(input$x_wrap)) {
          df_raw$Label    <- wrap_label(df_raw$Label,    lines = input$x_wrap_lines)
          df_labels$Label <- wrap_label(df_labels$Label, lines = input$x_wrap_lines)
        }
        if (nrow(df_raw) == 0) {  
          return(  
            ggplot() +  
              theme_void() +  
              annotate("text", 0, 0, label = msg_no_data_sel)  
          )  
        }  
        ## 1) RESUMEN (Ã‚Â¡creado ANTES de usarlo!)
        resumen <- df_raw %>%
          group_by(Label) %>%
          summarise(
            Mean = mean(.data[[param_sel]], na.rm = TRUE),
            SD   = sd  (.data[[param_sel]], na.rm = TRUE),
            .groups = "drop"
          )
        
        ## 2) ÃƒÂngulo adaptativo de las etiquetas X
        x_ang <- get_x_angle(
          n           = nlevels(resumen$Label),      # ahora sÃƒÂ­ existe
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
        # Ã¢â€â‚¬Ã¢â€â‚¬ AÃƒÂ±adir barras de significancia seleccionadas Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬  
        sig_tops <- resumen %>%
          mutate(y_top = Mean + ifelse(is.na(SD) | !is.finite(SD), 0, SD)) %>%
          transmute(group = Label, y_top = y_top)
        p <- apply_sig_layers(p,
                              group_tops = sig_tops,
                              margin_base = base_margin,
                              plot_height = input$plot_h)
        
        
        return(p)  
      }  
      
            
      
    } else {  
      
      # --- 3.3) Boxplot por cepa ---  
      if (tipo == "Boxplot") {
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
            q1     = stats::quantile(.data[[param_sel]], 0.25, na.rm = TRUE, names = FALSE),
            median = stats::median(.data[[param_sel]], na.rm = TRUE),
            q3     = stats::quantile(.data[[param_sel]], 0.75, na.rm = TRUE, names = FALSE),
            lower  = min(.data[[param_sel]], na.rm = TRUE),
            upper  = max(.data[[param_sel]], na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(group = as.character(Media)) %>%
          dplyr::select(group, q1, median, q3, lower, upper)
        x_ang <- get_x_angle(
          n            = nlevels(factor(df$Media)),
          angle_input  = input$x_angle
        )
        b_mar <- get_bottom_margin(x_ang, input$x_wrap, input$x_wrap_lines)
        p <- if (colourMode == "Blanco y Negro") {
          ggplot(df, aes(Media, .data[[param_sel]])) +
            geom_boxplot(fill = "white", colour = "black", width = input$box_w,
                         linewidth = input$errbar_size,
                         coef = box_coef,
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
                         coef = box_coef,
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
            limits = c(0, ymax_plot),  
            breaks = seq(0, ymax_plot, by = ybreak),  
            expand = c(0, 0),
            oob    = scales::oob_keep
          ) +  
          theme_minimal(base_size = input$base_size, base_family = "Helvetica") +
          coord_cartesian(clip = "off") +
          theme(
            plot.margin = base_margin,   # margen izq. +20 px aprox.
            plot.title      = element_text(size = fs_title, face = "bold"),  
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
            panel.grid      = element_blank(),  
            legend.position = "none"  
          )  
        # Ã¢â€â‚¬Ã¢â€â‚¬ AÃƒÂ±adir barras de significancia seleccionadas Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬  
        sig_tops <- df %>%
          group_by(Media) %>%
          summarise(y_top = max(.data[[param_sel]], na.rm = TRUE), .groups = "drop") %>%
          mutate(y_top = ifelse(is.finite(y_top), y_top, NA_real_)) %>%
          rename(group = Media)
        p <- apply_sig_layers(p,
                              group_tops = sig_tops,
                              margin_base = base_margin,
                              plot_height = input$plot_h)
        
        
        return(p)  
      }  
      
      
      if (tipo == "Violin") {
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

        sig_tops <- df_raw %>%
          group_by(Media) %>%
          summarise(y_top = max(.data[[param_sel]], na.rm = TRUE), .groups = "drop") %>%
          mutate(y_top = ifelse(is.finite(y_top), y_top, NA_real_)) %>%
          rename(group = Media)
        p <- apply_sig_layers(p,
                              group_tops = sig_tops,
                              margin_base = base_margin,
                              plot_height = input$plot_h)
        if (!is.null(box_stats)) {
          attr(p, "box_stats") <- box_stats
        }
        
        return(p)  
      }  

      # --- 3.4) Barras por cepa ---  
      if (tipo == "Barras") {  
        df_raw <- scope_df %>%
          filter(is.finite(.data[[param_sel]]))

        if (isTRUE(input$x_wrap)) {
          df_raw$Media <- wrap_label(df_raw$Media, lines = input$x_wrap_lines)
        }
        if (nrow(df_raw) == 0) {  
          return(ggplot() + theme_void() +  
                   annotate("text", x = 0, y = 0, label = msg_no_data_sel))  
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
        # Ã¢â€â‚¬Ã¢â€â‚¬ AÃƒÂ±adir barras de significancia seleccionadas Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬  
        sig_tops <- resumen %>%
          mutate(y_top = Mean + ifelse(is.na(SD) | !is.finite(SD), 0, SD)) %>%
          transmute(group = Media, y_top = y_top)
        p <- apply_sig_layers(p,
                              group_tops = sig_tops,
                              margin_base = base_margin,
                              plot_height = input$plot_h)
        
        return(p)  
      }  
      
      # Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬  
    }  
    
    # Ã¢â‚¬â€Ã¢â‚¬â€Ã¢â‚¬â€ fallback para nunca retornar NULL Ã¢â‚¬â€Ã¢â‚¬â€Ã¢â‚¬â€  
    return( ggplot() + theme_void() )  
  }  
  
  # Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬
  # Helper: versiÃƒÂ³n protegida de ggplotly que descarta capas vacÃƒÂ­as
  # (corregido: asigna y devuelve plt)
  # Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬
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
    if (is.null(sig_info)) return(plt)
    bars <- sig_info$bars %||% list()
    labels <- sig_info$labels %||% list()
    if (!length(bars) && !length(labels)) return(plt)
    y_range <- sig_info$y_range
    if (is.null(y_range) || !is.numeric(y_range) || length(y_range) < 2) return(plt)
    y_min <- min(y_range, na.rm = TRUE)
    y_max <- max(y_range, na.rm = TRUE)
    y_span <- y_max - y_min
    if (!is.finite(y_span) || y_span <= 0) y_span <- 1

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
      if (is.null(x_val)) return(NA)
      if (is.factor(x_val)) x_val <- as.character(x_val)
      if (is.character(x_val)) return(x_val)
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

    for (lbl in labels) {
      label_txt <- as.character(lbl$label %||% "")
      if (!nzchar(label_txt)) next
      x_val <- map_x(lbl$x)
      if (is.null(x_val) ||
          (is.numeric(x_val) && any(!is.finite(x_val)))) next
      ytxt <- (lbl$y - y_min) / y_span
      if (!is.finite(ytxt)) next
      text_px <- (lbl$textsize %||% 5) * mm_to_px
      text_px <- if (is.finite(text_px) && text_px > 0) text_px else 12
      yshift_val <- if (is_star_label(label_txt)) -text_px * 0.35 else 0
      label_color <- lbl$color %||% "black"
      if (is.na(label_color) || !nzchar(as.character(label_color))) label_color <- "black"
      annots <- append(annots, list(list(
        x = x_val, y = ytxt,
        xref = "x", yref = "paper",
        text = label_txt,
        showarrow = FALSE,
        yanchor = "bottom",
        yshift = yshift_val,
        font = list(size = text_px, color = label_color)
      )))
    }

    if (length(line_annots) || length(annots)) {
      cur_annots <- plt$x$layout$annotations %||% list()
      if (!is.list(cur_annots)) cur_annots <- list(cur_annots)
      plt$x$layout$annotations <- c(cur_annots, line_annots, annots)
    }
    plt
  }

  safe_ggplotly <- function(p, ...) {
    # Si el objeto ya es plotly, simplemente devuÃƒÂ©lvelo
    if (!inherits(p, "ggplot")) return(p)

    sig_info <- attr(p, "sig_plotly", exact = TRUE)
    sig_margin_pt <- attr(p, "sig_plot_margin_pt", exact = TRUE)
    sig_extra_top_pt <- attr(p, "sig_extra_top_pt", exact = TRUE)
    box_stats <- attr(p, "box_stats", exact = TRUE)
    p_clean <- if (!is.null(sig_info)) strip_sig_layers(p) else p

    # Convierte a plotly directamente
    last_err <- NULL
    plt <- tryCatch(
      ggplotly(p_clean, ...),
      error = function(e) { last_err <<- e; NULL }
    )
    if (is.null(plt)) {
      plt <- tryCatch(
        ggplotly(p, ...),
        error = function(e) { last_err <<- e; NULL }
      )
    }
    if (is.null(plt)) {
      if (!is.null(last_err)) {
        message("Plotly conversion failed: ", conditionMessage(last_err))
      }
      return(NULL)
    }

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
    if (!is.null(box_stats) && is.data.frame(box_stats) && nrow(box_stats)) {
      stats_map <- box_stats
      stats_map$group <- as.character(stats_map$group)
      plt$x$data <- lapply(plt$x$data, function(tr) {
        tr_type <- tolower(tr$type %||% "")
        if (tr_type != "box") return(tr)
        tr_name <- tr$name %||% ""
        if (!nzchar(tr_name) && !is.null(tr$x) && length(tr$x)) {
          tr_name <- as.character(tr$x[[1]])
        }
        row <- stats_map[stats_map$group == tr_name, , drop = FALSE]
        if (!nrow(row)) return(tr)
        tr$boxpoints <- FALSE
        tr$q1 <- row$q1
        tr$median <- row$median
        tr$q3 <- row$q3
        tr$lowerfence <- row$lower
        tr$upperfence <- row$upper
        tr$y <- NULL
        tr
      })
    }
    # Evita que plotly recorte segmentos/texto fuera del panel
    if (!is.null(plt$x$data)) {
      plt$x$data <- lapply(plt$x$data, function(tr) {
        tr_type <- tolower(tr$type %||% "")
        if (tr_type %in% c("box", "violin")) return(tr)
        tr$cliponaxis <- FALSE
        tr
      })
    }
    
    # 4. Devuelve SIEMPRE el objeto plotly resultante
    return(plt)
  }
  
  
  # ---- plot_base: la versiÃƒÂ³n Ã¢â‚¬Å“reactiveÃ¢â‚¬Â que usa la interfaz actual ----  
  plot_base <- reactive({  
    scope_sel  <- if (input$scope == "Combinado") "Combinado" else "Por Cepa"  
    strain_sel <- if (scope_sel == "Por Cepa")    input$strain else NULL  
    
    if (input$tipo == "Apiladas") {  
      # Devuelve un objeto *plotly* listo  
      build_plotly_stack(scope_sel, strain_sel)  
    } else {  
      # Todo lo demÃƒÂ¡s sigue con tu funciÃƒÂ³n ggplot2  
      build_plot(scope_sel, strain_sel, input$tipo)  
    }  
  })  
  
  # --- Salidas ---  
  output$plotInteractivo <- renderPlotly({
    req(input$dataFile)
    if (input$tipo == "Curvas") {
      req(cur_data_box(), cur_cfg_box())
    }
    
    lang <- input$app_lang %||% i18n_lang
    msg_no_data <- tr_text("no_data_plot", lang)
    p <- plot_base()
    validate(need(!is.null(p), msg_no_data))
    if (inherits(p, "ggplot")) {
      ok_build <- tryCatch({
        suppressWarnings(ggplot_build(p))
        TRUE
      }, error = function(e) FALSE)
      validate(need(ok_build, msg_no_data))
      plt <- tryCatch(
        suppressWarnings(
          safe_ggplotly(
            p,
            tooltip      = "all",
            width        = input$plot_w,
            height       = input$plot_h,
            originalData = FALSE
          )
        ),
        error = function(e) NULL
      )
      validate(need(!is.null(plt), msg_no_data))
      plt %>% config(responsive = TRUE)
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
    showNotification(tr_text("notify_copy_success"), type = "message", duration = 3)
  }, ignoreNULL = TRUE)

  observeEvent(input$plot_copy_error, {
    msg <- input$plot_copy_error$message %||% "Error desconocido."
    showNotification(
      paste(tr_text("notify_copy_error_prefix"), msg),
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
        description = "Archivo de parÃƒÂ¡metros exportado"
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
  ## DESCARGAR RESULTADOS ESTADÃƒÂSTICOS (normalidad + significancia)
  ##############################################################################
  download_stats_content <- function(file){
      
      req(input$dataFile)                # <-- asegÃƒÂºrate de que hay datos cargados
      lang <- input$app_lang %||% i18n_lang
      
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
      
      ## helper interno (idÃƒÂ©ntico al usado arriba)
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
        
        # Ã¢â€â‚¬Ã¢â€â‚¬ si no hay datos suficientes se elimina la hoja y se pasa al sig.
        if (nrow(df_param) < 3 || dplyr::n_distinct(df_param$Label) < 2){
          removeWorksheet(wb_tests, sheet)
          next
        }
        
        norm_tbl <- df_param |>
          dplyr::group_by(Label) |>
          dplyr::summarise(
            Shapiro.stat = stats::shapiro.test(Valor)$statistic,
            Shapiro.p    = stats::shapiro.test(Valor)$p.value,
            Normal       = dplyr::if_else(Shapiro.p > 0.05, "SÃƒÂ­", "No"),
            .groups      = "drop"
          )
        
        writeData(wb_tests, sheet, "Normalidad",
                  startRow = 1, startCol = 1,
                  headerStyle = createStyle(textDecoration = "bold"))
        writeData(wb_tests, sheet, norm_tbl,
                  startRow = 2, startCol = 1,
                  headerStyle = createStyle(textDecoration = "bold"))
        
        ## Ã¢â€“Âº helpers Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬
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
        ## Ã¢â€“Âº cÃƒÂ¡lculo Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬
        sig_raw <- switch(sigTest_sel,
                          "ANOVA"          = do_anova(df_param),
                         "Kruskal-Wallis" = do_kw(df_param),
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
                                showNotification(
                                  sprintf(tr_text("sig_insufficient_obs", lang), paste(faltantes, collapse = ", ")),
                                  type = "error", duration = 5
                                )
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
                                showNotification(
                                  sprintf(tr_text("sig_insufficient_obs", lang), paste(faltantes, collapse = ", ")),
                                  type = "error", duration = 5
                                )
                                tibble()
                              } else {
                                rstatix::wilcox_test(sub, Valor ~ Label, paired = can_paired(sub))
                              }
                            }
                          }
        )
        
        # armoniza columnas de comparaciÃƒÂ³n
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
            Significativo = dplyr::if_else(P_valor < 0.05, "SÃƒÂ­", "No"),
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
        
        if (sigTest_sel %in% c("ANOVA","Kruskal-Wallis")){
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
    lang <- input$app_lang %||% i18n_lang

    plot_raw <- capture_to_raw(".png", write_current_plot_png)
    if (length(plot_raw) == 0) {
      showNotification(tr_text("plot_capture_failed", lang), type = "error", duration = 5)
      return()
    }
    plot_pdf_raw <- capture_to_raw(".pdf", write_current_plot_pdf)
    if (length(plot_pdf_raw) == 0) {
      showNotification(tr_text("plot_pdf_failed", lang), type = "error", duration = 5)
      return()
    }
    metadata_raw <- capture_to_raw(".xlsx", write_metadata_xlsx)
    if (length(metadata_raw) == 0) {
      showNotification(tr_text("metadata_capture_failed", lang), type = "error", duration = 5)
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
    showNotification(tr_text("notify_version_saved"), type = "message", duration = 4)
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
          paste("ÃƒÂmbito:", v$scope)
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
          info_lines <- c(info_lines, paste("EstadÃƒÂ­sticas:", stats_ref))
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
    input$app_lang
    req(input$growthFiles)
    if (length(input$growthFiles$name) == 1)
      actionButton("importToPlots", tr("growth_import"), class = "btn btn-primary")
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
    if (!is.null(v <- get_val("sig_mode")))     updateRadioButtons(session, "sig_mode", selected = v)
    if (!is.null(v <- get_val("sig_label_param_color"))) updateCheckboxInput(session, "sig_label_param_color", value = tolower(v) == "true")
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
    if (!is.null(v <- get_val("stack_outline_only"))) updateCheckboxInput(session, "stack_outline_only", value = tolower(v) == "true")
    if (!is.null(v <- get_val("errbar_param_color"))) updateCheckboxInput(session, "errbar_param_color", value = tolower(v) == "true")
    if (!is.null(v <- get_val("xmax_cur")))   updateNumericInput(session, "xmax_cur",   value = as.numeric(v))
    if (!is.null(v <- get_val("xbreak_cur"))) updateNumericInput(session, "xbreak_cur", value = as.numeric(v))
    if (!is.null(v <- get_val("ymax_cur")))   updateNumericInput(session, "ymax_cur",   value = as.numeric(v))
    if (!is.null(v <- get_val("ybreak_cur"))) updateNumericInput(session, "ybreak_cur", value = as.numeric(v))
    if (!is.null(v <- get_val("corr_param_x"))) updateSelectInput(session, "corr_param_x", selected = v)
    if (!is.null(v <- get_val("corr_param_y"))) updateSelectInput(session, "corr_param_y", selected = v)
    if (!is.null(v <- get_val("corr_method")))  updateRadioButtons(session, "corr_method", selected = v)
    if (!is.null(v <- get_val("corr_norm_target"))) updateRadioButtons(session, "corr_norm_target", selected = v)
    if (!is.null(v <- get_val("corr_show_line")))   updateCheckboxInput(session, "corr_show_line",   value = tolower(v) == "true")
    if (!is.null(v <- get_val("corr_show_labels"))) updateCheckboxInput(session, "corr_show_labels", value = tolower(v) == "true")
    if (!is.null(v <- get_val("corr_show_r")))      updateCheckboxInput(session, "corr_show_r",      value = tolower(v) == "true")
    if (!is.null(v <- get_val("corr_show_p")))      updateCheckboxInput(session, "corr_show_p",      value = tolower(v) == "true")
    if (!is.null(v <- get_val("corr_show_r2")))     updateCheckboxInput(session, "corr_show_r2",     value = tolower(v) == "true")
    if (!is.null(v <- get_val("corr_show_eq")))     updateCheckboxInput(session, "corr_show_eq",     value = tolower(v) == "true")
    if (!is.null(v <- get_val("corr_xlab")))        updateTextInput(session, "corr_xlab", value = v)
    if (!is.null(v <- get_val("corr_ylab")))        updateTextInput(session, "corr_ylab", value = v)
    if (!is.null(v <- get_val("xmin_corr")))        updateNumericInput(session, "xmin_corr", value = as.numeric(v))
    if (!is.null(v <- get_val("xmax_corr")))        updateNumericInput(session, "xmax_corr", value = as.numeric(v))
    if (!is.null(v <- get_val("xbreak_corr")))      updateNumericInput(session, "xbreak_corr", value = as.numeric(v))
    if (!is.null(v <- get_val("ymin_corr")))        updateNumericInput(session, "ymin_corr", value = as.numeric(v))
    if (!is.null(v <- get_val("ymax_corr")))        updateNumericInput(session, "ymax_corr", value = as.numeric(v))
    if (!is.null(v <- get_val("ybreak_corr")))      updateNumericInput(session, "ybreak_corr", value = as.numeric(v))
    if (!is.null(v <- get_val("corr_label_size")))  updateNumericInput(session, "corr_label_size", value = as.numeric(v))
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

  # --- Carga de metadata de diseno -----------------------------------
  observeEvent(input$metaFiles, {
    req(input$metaFiles)
    valid_types <- c("Boxplot", "Barras", "Violin", "Curvas", "Apiladas", "Correlacion")
    extract_meta_type <- function(tbl, fname = NULL) {
      campos <- trimws(tolower(tbl$Campo))
      valores <- trimws(as.character(tbl$Valor))
      by_name <- function(key) {
        v <- valores[match(key, campos)]
        v <- v[!is.na(v)]
        v <- if (length(v)) v[1] else NULL
        if (!is.null(v) && nzchar(v)) v else NULL
      }
      out <- by_name("tipo") %||% by_name("tipo_grafico") %||% by_name("plot_type")
      if (!is.null(out)) return(out)
      if (!is.null(fname) && startsWith(fname, "metadata_")) {
        maybe <- sub("^metadata_", "", sub("\\.xlsx$", "", fname))
        if (nzchar(maybe)) return(maybe)
      }
      NULL
    }
    for (i in seq_len(nrow(input$metaFiles))) {
      lang <- input$app_lang %||% i18n_lang
      path  <- input$metaFiles$datapath[i]
      fname <- input$metaFiles$name[i]

      # Intentar lectura de la hoja 'Metadata' de forma robusta
      meta <- tryCatch(read_excel_tmp(path, sheet = "Metadata"),
                       error = function(e) NULL)
      if (is.null(meta)) {
        showNotification(sprintf(tr_text("metadata_invalid_sheet", lang), fname),
                         type = "error", duration = 7)
        next
      }

      # Validacion de columnas minimas requeridas
      required_cols <- c("Campo", "Valor")
      if (!all(required_cols %in% names(meta))) {
        showNotification(
          sprintf(tr_text("metadata_invalid_columns", lang), fname),
          type = "error", duration = 7
        )
        next
      }

      meta$Campo <- as.character(meta$Campo)
      meta$Valor <- as.character(meta$Valor)
      meta_tipo  <- extract_meta_type(meta)
      name_tipo  <- if (startsWith(fname, "metadata_"))
        sub("^metadata_", "", sub("\\.xlsx$", "", fname)) else NULL
      tipo       <- meta_tipo %||% name_tipo
      if (!is.null(tipo) && identical(tipo, "Correlación")) tipo <- "Correlacion"

      if (is.null(tipo) || !tipo %in% valid_types) {
        showNotification(
          sprintf(
            tr_text("metadata_type_unknown", lang),
            fname,
            paste(valid_types, collapse = ", ")
          ),
          type = "error", duration = 7
        )
        next
      }

      if (!is.null(meta_tipo) && !is.null(name_tipo) && !identical(meta_tipo, name_tipo)) {
        showNotification(
          sprintf(tr_text("metadata_type_mismatch", lang), fname, name_tipo, meta_tipo, meta_tipo),
          type = "warning", duration = 6
        )
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
    # 0) Asegurarnos de que ya hay datos cargados en GrÃƒÂ¡ficos & Stats
    lang <- input$app_lang %||% i18n_lang
    if (is.null(datos_box())) {
      showNotification(
        tr_text("import_metadata_first", lang),
        type = "error", duration = 5
      )
      return()
    }
    # 1) Localiza los dos archivos que generÃƒÂ³ runGrowth
    curvas_f <- list.files(growth_out_dir, pattern = "^Curvas_.*\\.xlsx$", full.names = TRUE)
    params_f <- list.files(growth_out_dir, pattern = "^Parametros_.*\\.xlsx$", full.names = TRUE)
    
    # ValidaciÃƒÂ³n
    if (length(curvas_f) != 1 || length(params_f) != 1) {
      showNotification(tr_text("need_curves_params", lang),
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
    
    # 4) Hacer el join de parÃƒÂ¡metros sobre los datos originales
    df1 <- df0 %>% left_join(params, by = "Well")
    datos_box(df1)
    # 5) Sincroniza PlotSettings con los parÃƒÂ¡metros que SÃƒÂ existen
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
    
    # 6) Volvemos a la pestaÃƒÂ±a de resultados
    updateTabsetPanel(session, "mainTabs", selected = "tab_plots")
  })
  
}  


