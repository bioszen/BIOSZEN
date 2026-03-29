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
  session_closing <- reactiveVal(FALSE)

  active_sessions <<- active_sessions + 1
  session$onSessionEnded(function() {
    session_closing(TRUE)
    active_sessions <<- max(0, active_sessions - 1)
    if (active_sessions == 0) shiny::stopApp()
  })

  is_session_closing <- function() {
    if (isTRUE(session_closing())) return(TRUE)
    by_closed <- tryCatch(isTRUE(session$closed), error = function(e) FALSE)
    if (isTRUE(by_closed)) return(TRUE)
    tryCatch(isTRUE(session$isClosed()), error = function(e) FALSE)
  }

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
  cur_meta_box  <- reactiveVal(NULL)   # metadata curvas (summary mode)
  cur_sum_box   <- reactiveVal(NULL)   # curvas summary (mean/sd/n)
  sig_list      <- reactiveVal(list()) # guardarÃƒÂ¡ comparaciones
  sig_preselect <- reactiveVal(NULL)   # selecciÃƒÂ³n pendiente en el manejador de barras
  meta_store    <- reactiveValues()    # metadata por tipo de grÃƒÂ¡fico
  is_group_data <- reactiveVal(FALSE)
  summary_input_mode <- reactiveVal(FALSE)
  curve_summary_mode <- reactiveVal(FALSE)
  bundle_store  <- reactiveValues(datasets = list(), versions = list())
  export_cache  <- reactiveValues(
    plot_png = list(),
    plot_pdf = list(),
    params = list(),
    stats = list(),
    metadata = list(),
    bundle = list()
  )
  plot_payload_cache <- new.env(parent = emptyenv())
  plot_payload_cache$heatmap <- list()
  plot_payload_cache$corrm <- list()
  plot_payload_cache$order <- list(heatmap = character(0), corrm = character(0))
  current_dataset_key <- reactiveVal(NULL)
  reps_strain_selected <- reactiveVal(list())
  reps_group_selected <- reactiveVal(list())
  qc_tech_selected <- reactiveVal(list())
  qc_tech_selected_by_param <- reactiveVal(list())
  qc_tech_bulk_updating <- reactiveVal(FALSE)
  qc_tech_tab_inserted <- reactiveVal(FALSE)
  qc_tech_tab_lang <- reactiveVal(NULL)
  heat_force_empty <- reactiveVal(FALSE)
  merged_platemap_path <- reactiveVal(NULL)
  merged_platemap_name <- reactiveVal(NULL)
  merged_well_map <- reactiveVal(NULL)
  merge_status <- reactiveVal("")
  merged_curve_path <- reactiveVal(NULL)
  merged_curve_name <- reactiveVal(NULL)
  curve_merge_status <- reactiveVal("")

  ylims <- reactiveValues()
  strain_label_ui <- reactiveVal(NULL)
  media_label_ui  <- reactiveVal(NULL)
  strain_label_source <- reactiveVal("default")
  media_label_source  <- reactiveVal("default")

  default_label_text <- function(lang, key) {
    base <- tr_text(key, lang)
    sub(":$", "", base)
  }

  set_default_labels <- function(lang, force = FALSE) {
    if (force || identical(strain_label_source(), "default")) {
      strain_label_ui(default_label_text(lang, "strain_label"))
      strain_label_source("default")
    }
    if (force || identical(media_label_source(), "default")) {
      media_label_ui(default_label_text(lang, "media_label"))
      media_label_source("default")
    }
  }

  apply_data_labels <- function(labels, lang) {
    if (!is.null(labels$Strain) && nzchar(labels$Strain)) {
      strain_label_ui(labels$Strain)
      strain_label_source("data")
    } else {
      strain_label_ui(default_label_text(lang, "strain_label"))
      strain_label_source("default")
    }
    if (!is.null(labels$Media) && nzchar(labels$Media)) {
      media_label_ui(labels$Media)
      media_label_source("data")
    } else {
      media_label_ui(default_label_text(lang, "media_label"))
      media_label_source("default")
    }
  }

  scope_by_label <- function(lang) {
    prefix <- if (identical(lang, "en")) "By" else "Por"
    strain_text <- strain_label_ui() %||% default_label_text(lang, "strain_label")
    paste(prefix, strain_text)
  }

  filter_media_label <- function(lang) {
    prefix <- if (identical(lang, "en")) "Filter" else "Filtro de"
    media_text <- media_label_ui() %||% default_label_text(lang, "media_label")
    paste(prefix, media_text)
  }

  reps_by_media_label <- function(lang) {
    prefix <- if (identical(lang, "en")) "Replicates by" else "Replicas por"
    media_text <- media_label_ui() %||% default_label_text(lang, "media_label")
    paste(prefix, media_text)
  }

  norm_medium_label <- function(lang) {
    base <- tr_text("norm_medium_label", lang)
    base <- sub(":$", "", base)
    media_text <- media_label_ui() %||% default_label_text(lang, "media_label")
    show_media <- identical(media_label_source(), "data") && nzchar(media_text)
    if (show_media) {
      paste0(base, " (", media_text, "):")
    } else {
      paste0(base, ":")
    }
  }
  has_ctrl_selected <- function(ctrl_val = input$ctrlMedium) {
    if (is.null(ctrl_val) || !length(ctrl_val)) return(FALSE)
    raw <- ctrl_val[[1]]
    if (is.null(raw) || is.na(raw)) return(FALSE)
    ctrl_chr <- trimws(as.character(raw))
    nzchar(ctrl_chr)
  }

  normalize_rep_selection <- function(values) {
    normalize_replicate_selection(values %||% character(0))
  }

  current_strain_value <- function() {
    if (is.null(input$strain) || !length(input$strain)) return("")
    as.character(input$strain[[1]])
  }

  get_strain_selection_map <- function(full_map, strain = current_strain_value()) {
    if (!is.list(full_map)) return(list())
    strain_chr <- as.character(strain %||% "")
    if (!nzchar(strain_chr)) return(list())
    out <- full_map[[strain_chr]]
    if (is.list(out)) out else list()
  }

  set_strain_selection_map <- function(full_map, strain = current_strain_value(), strain_map = list()) {
    if (!is.list(full_map)) full_map <- list()
    strain_chr <- as.character(strain %||% "")
    if (!nzchar(strain_chr)) return(full_map)
    if (!is.list(strain_map)) strain_map <- list()
    full_map[[strain_chr]] <- strain_map
    full_map
  }

  get_strain_media_selection <- function(full_map, strain = current_strain_value(), media) {
    if (is.null(media) || !length(media)) return(NULL)
    strain_map <- get_strain_selection_map(full_map, strain)
    strain_map[[as.character(media[[1]])]]
  }

  set_strain_media_selection <- function(full_map, strain = current_strain_value(), media, selected) {
    if (is.null(media) || !length(media)) return(full_map)
    media_chr <- as.character(media[[1]])
    strain_map <- get_strain_selection_map(full_map, strain)
    strain_map[[media_chr]] <- normalize_rep_selection(selected)
    set_strain_selection_map(full_map, strain, strain_map)
  }

  get_group_media_selection <- function(group_map, strain, media) {
    replicate_selection_get_group(group_map, strain = strain, media = media)
  }

  set_group_media_selection <- function(group_map, strain, media, selected) {
    replicate_selection_set_group(
      reps_group_map = group_map,
      strain = strain,
      media = media,
      selected = selected
    )
  }

  get_synced_media_selection <- function(strain_map, group_map, strain, media) {
    replicate_selection_get_synced(
      reps_strain_map = strain_map,
      reps_group_map = group_map,
      strain = strain,
      media = media
    )
  }

  set_synced_media_selection <- function(strain_map, group_map, strain, media, selected) {
    replicate_selection_set_synced(
      reps_strain_map = strain_map,
      reps_group_map = group_map,
      strain = strain,
      media = media,
      selected = selected
    )
  }

  qc_tech_selection_key <- function(group, biorep, strain = NULL, media = NULL) {
    qc_build_tech_selection_key(
      group = group,
      biorep = biorep,
      strain = strain,
      media = media
    )
  }

  apply_qc_tech_filter_raw <- function(df) {
    qc_filter_by_technical_selection(
      df = df,
      tech_map = qc_tech_selected(),
      group_col = NULL,
      biorep_col = "BiologicalReplicate",
      tech_col = "TechnicalReplicate",
      strain_col = "Strain",
      media_col = "Media"
    )
  }


  clear_reactive_values <- function(rv) {
    current <- shiny::reactiveValuesToList(rv)
    if (!length(current)) return(invisible(NULL))
    for (nm in names(current)) rv[[nm]] <- NULL
    invisible(NULL)
  }

  reset_curve_state <- function() {
    cur_data_box(NULL)
    cur_cfg_box(NULL)
    cur_meta_box(NULL)
    cur_sum_box(NULL)
    curve_summary_mode(FALSE)
    merged_curve_path(NULL)
    merged_curve_name(NULL)
    curve_merge_status("")
    ylims$Curvas <- NULL
  }

  reset_dataset_state <- function() {
    datos_box(NULL)
    plot_cfg_box(NULL)
    sig_list(list())
    sig_preselect(NULL)
    reset_curve_state()
    merged_well_map(NULL)
    clear_reactive_values(ylims)
    clear_reactive_values(meta_store)
    summary_input_mode(FALSE)
    is_group_data(FALSE)
    reps_strain_selected(list())
    reps_group_selected(list())
    qc_tech_selected(list())
    qc_tech_selected_by_param(list())
    qc_tech_tab_inserted(FALSE)
    qc_tech_tab_lang(NULL)
    heat_force_empty(FALSE)
    plot_payload_cache$heatmap <- list()
    plot_payload_cache$corrm <- list()
    plot_payload_cache$order <- list(heatmap = character(0), corrm = character(0))
    set_default_labels(input$app_lang %||% i18n_lang, force = TRUE)
  }
  dataset_loading <- reactiveVal(FALSE)

  plot_payload_cache_get <- function(slot, key) {
    slot <- as.character(slot %||% "")
    if (!length(slot) || is.na(slot[[1]]) || !nzchar(slot[[1]])) return(NULL)
    slot <- slot[[1]]
    key_chr <- as.character(key %||% "")
    if (!length(key_chr) || is.na(key_chr[[1]]) || !nzchar(key_chr[[1]])) return(NULL)
    key_chr <- key_chr[[1]]
    cache_slot <- plot_payload_cache[[slot]]
    if (!is.list(cache_slot)) return(NULL)
    cache_slot[[key_chr]]
  }

  plot_payload_cache_set <- function(slot, key, payload, max_entries = 10L) {
    slot <- as.character(slot %||% "")
    if (!length(slot) || is.na(slot[[1]]) || !nzchar(slot[[1]])) return(invisible(NULL))
    slot <- slot[[1]]
    key_chr <- as.character(key %||% "")
    if (!length(key_chr) || is.na(key_chr[[1]]) || !nzchar(key_chr[[1]]) || is.null(payload)) {
      return(invisible(NULL))
    }
    key_chr <- key_chr[[1]]
    if (!is.list(plot_payload_cache[[slot]])) plot_payload_cache[[slot]] <- list()
    plot_payload_cache[[slot]][[key_chr]] <- payload
    order_vec <- as.character(plot_payload_cache$order[[slot]] %||% character(0))
    order_vec <- c(setdiff(order_vec, key_chr), key_chr)
    max_entries <- suppressWarnings(as.integer(max_entries[[1]]))
    if (!is.finite(max_entries) || max_entries < 1L) max_entries <- 10L
    if (length(order_vec) > max_entries) {
      drop_keys <- head(order_vec, length(order_vec) - max_entries)
      for (dk in drop_keys) plot_payload_cache[[slot]][[dk]] <- NULL
      order_vec <- tail(order_vec, max_entries)
    }
    plot_payload_cache$order[[slot]] <- order_vec
    invisible(NULL)
  }

  curve_data     <- reactive(cur_data_box())
  curve_settings <- reactive(cur_cfg_box())
  curve_meta     <- reactive(cur_meta_box())
  curve_summary  <- reactive(cur_sum_box())
  is_summary_mode <- reactive(isTRUE(summary_input_mode()))

  selectize_server_threshold <- 30L
  large_param_threshold <- 100L

  normalize_selectize_values <- function(choices) {
    if (is.null(choices)) return(character(0))
    vals <- choices
    if (is.list(vals) && !is.data.frame(vals)) {
      vals <- unlist(vals, use.names = FALSE)
    }
    vals <- as.character(vals %||% character(0))
    vals <- vals[!is.na(vals) & nzchar(vals)]
    unique(vals)
  }

  should_use_server_selectize <- function(choices) {
    length(normalize_selectize_values(choices)) > selectize_server_threshold
  }

  update_selectize_adaptive <- function(input_id,
                                        choices = NULL,
                                        selected = NULL,
                                        label = NULL,
                                        options = NULL) {
    args <- list(
      session = session,
      inputId = input_id,
      choices = choices,
      selected = selected,
      server = isTRUE(should_use_server_selectize(choices))
    )
    if (!is.null(label)) args$label <- label
    if (!is.null(options)) args$options <- options
    do.call(updateSelectizeInput, args)
    invisible(NULL)
  }

  # Heatmap/correlation now render synchronously to avoid async lockups.
  output$heatmapLoadingUI <- renderUI(NULL)

  observe({
    flag <- if (isTRUE(summary_input_mode())) "true" else "false"
    shinyjs::runjs(sprintf(
      "Shiny.setInputValue('summary_mode_flag', %s, {priority: 'event'});",
      flag
    ))
  })

  observe({
    hide_curve_upload <- isTRUE(is_group_data()) || isTRUE(summary_input_mode())
    if (hide_curve_upload) {
      shinyjs::hide("curve_upload_section")
    } else {
      shinyjs::show("curve_upload_section")
    }
  })

  output$mergeStatus <- renderText({
    msg <- merge_status()
    if (!nzchar(msg)) {
      tr_text("merge_status_idle", input$app_lang %||% i18n_lang)
    } else {
      msg
    }
  })

  output$mergedPlatemapDownloadUI <- renderUI({
    src <- merged_platemap_path()
    if (is.null(src) || !nzchar(src) || !file.exists(src)) return(NULL)

    tags$div(
      style = "margin-top: 8px;",
      downloadButton(
        "downloadMergedPlatemapLatest",
        tr("merge_download"),
        class = "btn btn-outline-secondary"
      )
    )
  })

  output$curveMergeArrowUI <- renderUI({
    map_tbl <- merged_well_map()
    has_map <- is.data.frame(map_tbl) && nrow(map_tbl) > 0
    if (!has_map) return(NULL)

    actionButton(
      "openCurveMergeModal",
      label = NULL,
      icon = icon("chevron-right"),
      class = "btn btn-outline-secondary",
      title = tr("curve_merge_open"),
      `aria-label` = as.character(tr("curve_merge_open"))
    )
  })

  output$curveMergeStatus <- renderText({
    msg <- curve_merge_status()
    if (!nzchar(msg)) {
      tr_text("curve_merge_status_idle", input$app_lang %||% i18n_lang)
    } else {
      msg
    }
  })

  output$mergedCurveDownloadUI <- renderUI({
    src <- merged_curve_path()
    if (is.null(src) || !nzchar(src) || !file.exists(src)) return(NULL)

    tags$div(
      style = "margin-top: 8px;",
      downloadButton(
        "downloadMergedCurvesLatest",
        tr("curve_merge_download"),
        class = "btn btn-outline-secondary"
      )
    )
  })

  observeEvent(input$openMergeModal, {
    showModal(modalDialog(
      title = tr("merge_title"),
      easyClose = TRUE,
      size = "m",
      tags$p(class = "qc-help", tr("merge_help")),
      fileInput("mergeFiles", tr("merge_files"), accept = ".xlsx", multiple = TRUE),
      actionButton(
        "mergePlatemaps",
        tr("merge_run"),
        class = "btn btn-secondary"
      ),
      br(), br(),
      downloadButton(
        "downloadMergedPlatemap",
        tr("merge_download"),
        class = "btn btn-outline-secondary"
      ),
      tags$div(
        style = "margin-top: 10px; font-size: 13px; color: #444;",
        textOutput("mergeStatus")
      ),
      footer = tagList(
        modalButton(tr("merge_close"))
      )
    ))
  })

  observeEvent(input$openCurveMergeModal, {
    showModal(modalDialog(
      title = tr("curve_merge_title"),
      easyClose = TRUE,
      size = "m",
      tags$p(class = "qc-help", tr("curve_merge_help")),
      fileInput("mergeCurveFiles", tr("curve_merge_files"), accept = c(".xlsx", ".xls", ".csv"), multiple = TRUE),
      actionButton(
        "mergeCurves",
        tr("curve_merge_run"),
        class = "btn btn-secondary"
      ),
      br(), br(),
      downloadButton(
        "downloadMergedCurves",
        tr("curve_merge_download"),
        class = "btn btn-outline-secondary"
      ),
      tags$div(
        style = "margin-top: 10px; font-size: 13px; color: #444;",
        textOutput("curveMergeStatus")
      ),
      footer = tagList(
        modalButton(tr("merge_close"))
      )
    ))
  })

  encode_named_metadata <- function(x) {
    if (is.null(x) || !length(x)) return("")
    vals <- as.character(x)
    nms <- names(x)
    if (is.null(nms) || !length(nms)) return("")
    keep <- !is.na(nms) & nzchar(nms) & !is.na(vals)
    if (!any(keep)) return("")
    idx <- which(keep)
    idx <- idx[order(nms[idx])]
    pairs <- vapply(
      idx,
      function(i) {
        paste0(
          utils::URLencode(nms[[i]], reserved = TRUE),
          "=",
          utils::URLencode(vals[[i]], reserved = TRUE)
        )
      },
      character(1)
    )
    paste(pairs, collapse = ";")
  }

  decode_named_metadata <- function(x) {
    raw <- as.character(x %||% "")
    if (!length(raw) || is.na(raw[[1]]) || !nzchar(trimws(raw[[1]]))) {
      return(setNames(character(0), character(0)))
    }
    parts <- strsplit(raw[[1]], ";", fixed = TRUE)[[1]]
    out <- setNames(character(0), character(0))
    for (part in parts) {
      if (!nzchar(part)) next
      kv <- strsplit(part, "=", fixed = TRUE)[[1]]
      if (length(kv) < 2) next
      key <- utils::URLdecode(kv[[1]])
      val <- utils::URLdecode(paste(kv[-1], collapse = "="))
      if (nzchar(key)) out[[key]] <- val
    }
    out
  }

  is_valid_reactive_key <- function(key) {
    if (is.null(key)) return(FALSE)
    key_chr <- as.character(key)
    if (!length(key_chr)) return(FALSE)
    first <- key_chr[[1]]
    !is.na(first) && nzchar(trimws(first))
  }

  # --- Helper: recopilar metadata actual para reproducibilidad ---
  collect_metadata_tbl <- function() {
    base_vals <- list(
      scope          = input$scope,
      strain         = {
        val <- as.character(input$strain %||% "")
        if (!length(val) || is.na(val[[1]])) "" else val[[1]]
      },
      tipo           = input$tipo,
      colorMode      = input$colorMode,
      repeat_colors_combined = as.character(input$repeat_colors_combined %||% FALSE),
      adv_pal_enable = as.character(input$adv_pal_enable %||% FALSE),
      adv_pal_type   = as.character(input$adv_pal_type %||% "seq"),
      adv_pal_reverse = as.character(input$adv_pal_reverse %||% FALSE),
      adv_pal_filters = paste(input$adv_pal_filters %||% character(0), collapse = ","),
      adv_pal_name   = as.character(input$adv_pal_name %||% ""),
      adv_pal_group_enable = as.character(input$adv_pal_group_enable %||% FALSE),
      adv_pal_group_sel = paste(input$adv_pal_group_sel %||% character(0), collapse = ","),
      adv_pal_group_color = as.character(input$adv_pal_group_color %||% "#E15759"),
      adv_pal_group_overrides = encode_named_metadata(
        tryCatch(
          adv_pal_group_overrides(),
          error = function(e) setNames(character(0), character(0))
        )
      ),
      doNorm         = as.character(input$doNorm %||% FALSE),
      ctrlMedium     = {
        val <- as.character(input$ctrlMedium %||% "")
        if (!length(val) || is.na(val[[1]]) || !nzchar(val[[1]])) "NULL" else val[[1]]
      },
      plot_w         = input$plot_w,
      plot_h         = input$plot_h,
      base_size      = input$base_size,
      fs_title       = input$fs_title,
      fs_axis        = input$fs_axis,
      fs_legend      = input$fs_legend,
      axis_line_size = input$axis_line_size,
      yLab           = as.character(input$yLab %||% ""),
      plotTitle      = as.character(input$plotTitle %||% ""),
      labelMode      = as.character(input$labelMode %||% FALSE),
      margin_top_adj = as.character(input$margin_top_adj %||% 0),
      margin_right_adj = as.character(input$margin_right_adj %||% 0),
      margin_bottom_adj = as.character(input$margin_bottom_adj %||% 0),
      margin_left_adj = as.character(input$margin_left_adj %||% 0)
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
    if (input$tipo %in% c("Boxplot", "Barras", "Violin")) {
      meta <- add_row(
        meta,
        Campo = "legend_right",
        Valor = as.character(input$legend_right)
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
                                "sig_label_param_color",
                                "sig_auto_include", "sig_auto_label_mode", "sig_auto_replace",
                                "multitest_method"),
                      Valor = c(as.character(input$sig_linewidth),
                                as.character(input$sig_textsize),
                                as.character(input$sig_sep),
                                as.character(input$sig_textpad),
                                as.character(input$sig_offset),
                                as.character(input$sig_hide_caps),
                                as.character(input$sig_mode),
                                as.character(input$sig_label_param_color),
                                as.character(input$sig_auto_include %||% "significant"),
                                as.character(input$sig_auto_label_mode %||% "stars"),
                                as.character(input$sig_auto_replace %||% TRUE),
                                as.character(input$multitest_method %||% "none")))
    }

    if (input$tipo %in% c("Boxplot", "Barras", "Violin")) {
      meta <- add_row(
        meta,
        Campo = c("param",
                  "errbar_size", "ymax", "ybreak"),
        Valor = c(
          safe_param(),
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
    } else if (input$tipo == "Heatmap") {
      meta <- add_row(
        meta,
        Campo = c(
          "heat_params", "heat_scale_mode", "heat_hclust_method",
          "heat_cluster_rows", "heat_cluster_cols",
          "heat_k_rows", "heat_k_cols",
          "heat_show_side_dend", "heat_show_top_dend",
          "heat_show_param_labels", "heat_orientation",
          "heat_show_values",
          "heat_norm_z"
        ),
        Valor = c(
          paste(input$heat_params %||% character(0), collapse = ","),
          as.character(input$heat_scale_mode %||% "none"),
          as.character(input$heat_hclust_method %||% "ward.D2"),
          as.character(input$heat_cluster_rows %||% FALSE),
          as.character(input$heat_cluster_cols %||% FALSE),
          as.character(input$heat_k_rows %||% 2L),
          as.character(input$heat_k_cols %||% 2L),
          as.character(input$heat_show_side_dend %||% FALSE),
          as.character(input$heat_show_top_dend %||% FALSE),
          as.character(input$heat_show_param_labels %||% TRUE),
          as.character(input$heat_orientation %||% "params_rows"),
          as.character(input$heat_show_values %||% FALSE),
          as.character(identical(input$heat_scale_mode %||% "none", "row"))
        )
      )
    } else if (input$tipo == "MatrizCorrelacion") {
      meta <- add_row(
        meta,
        Campo = c("corrm_params", "corrm_method", "corrm_adjust", "corrm_show_sig"),
        Valor = c(
          paste(input$corrm_params %||% character(0), collapse = ","),
          input$corrm_method %||% "spearman",
          input$corrm_adjust %||% "none",
          as.character(input$corrm_show_sig %||% TRUE)
        )
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
        Campo = c(
          "xmax_cur", "xbreak_cur",
          "ymax_cur", "ybreak_cur",
          "cur_xlab", "cur_ylab",
          "cur_show_ci", "cur_show_reps", "cur_rep_alpha",
          "curve_geom", "curve_color_mode", "curve_single_color",
          "cur_ci_style", "curve_stats_methods"
        ),
        Valor = c(as.character(input$xmax_cur), as.character(input$xbreak_cur),
                  as.character(input$ymax_cur), as.character(input$ybreak_cur),
                  as.character(input$cur_xlab %||% ""),
                  as.character(input$cur_ylab %||% ""),
                  as.character(input$cur_show_ci %||% FALSE),
                  as.character(input$cur_show_reps %||% FALSE),
                  as.character(input$cur_rep_alpha %||% 0.25),
                  as.character(input$curve_geom %||% "line_points"),
                  as.character(input$curve_color_mode %||% "by_group"),
                  as.character(input$curve_single_color %||% "#000000"),
                  as.character(input$cur_ci_style %||% "ribbon"),
                  paste(input$curve_stats_methods %||% character(0), collapse = ","))
      )
    } else if (input$tipo == "Correlacion") {
      meta <- add_row(
        meta,
        Campo = c(
          "corr_param_x", "corr_param_y",
          "corr_method",
          "corr_norm_target",
          "corr_show_line", "corr_show_labels",
          "corr_show_ci", "corr_ci_style", "corr_ci_level",
          "corr_show_r", "corr_show_p", "corr_show_r2", "corr_show_eq",
           "corr_xlab", "corr_ylab",
           "xmin_corr", "xmax_corr", "xbreak_corr",
           "ymin_corr", "ymax_corr", "ybreak_corr",
           "corr_label_size",
           "corr_adv_anchor", "corr_adv_method", "corr_adv_data_mode",
           "corr_adv_sig_only", "corr_adv_pvalue_max", "corr_adv_direction",
           "corr_adv_r_min", "corr_adv_r_max"
         ),
         Valor = c(
           input$corr_param_x %||% "",
           input$corr_param_y %||% "",
           input$corr_method %||% "",
          input$corr_norm_target %||% "both",
          as.character(input$corr_show_line),
          as.character(input$corr_show_labels),
          as.character(input$corr_show_ci %||% FALSE),
          input$corr_ci_style %||% "band",
          as.character(input$corr_ci_level %||% 0.95),
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
           as.character(input$corr_label_size),
           input$corr_adv_anchor %||% "",
           input$corr_adv_method %||% "pearson",
           input$corr_adv_data_mode %||% "raw",
           as.character(input$corr_adv_sig_only %||% FALSE),
           as.character(input$corr_adv_pvalue_max %||% 0.05),
           input$corr_adv_direction %||% "all",
           as.character((input$corr_adv_r_filter %||% c(-1, 1))[1]),
           as.character((input$corr_adv_r_filter %||% c(-1, 1))[2])
         )
       )
     }
    metadata_filter_design_only(meta)
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

  observeEvent(input$mode, {
    mode <- input$mode %||% "light"
    if (identical(mode, "dark")) {
      session$setCurrentTheme(theme_dark)
    } else {
      session$setCurrentTheme(theme_light)
    }
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

  build_current_plotly_for_export <- function(scope_sel, strain_sel, width, height) {
    if (identical(input$tipo, "Apiladas")) {
      plt <- build_plotly_stack(scope_sel, strain_sel, width = width, height = height)
      plt <- sanitize_plotly_display_labels(plt)
      plt <- apply_margin_inputs_to_plotly(plt, legend_in_margin = TRUE)
      return(plt %>% config(responsive = FALSE))
    }

    p <- build_plot(scope_sel, strain_sel, input$tipo, for_interactive = TRUE)
    if (inherits(p, "ggplot")) {
      tooltip_fields <- tryCatch(
        plotly_tooltip_fields(input$tipo %||% ""),
        error = function(e) c("x", "y", "name", "text")
      )
      plt <- tryCatch(
        suppressWarnings(
          safe_ggplotly(
            p,
            tooltip = tooltip_fields,
            width = width,
            height = height,
            originalData = FALSE
          )
        ),
        error = function(e) NULL
      )
    } else {
      plt <- p
    }

    if (is.null(plt)) stop("Unable to build preview-aligned plotly object for export.")
    plt <- sanitize_plotly_display_labels(plt)
    plt <- apply_margin_inputs_to_plotly(plt, legend_in_margin = TRUE, expand_canvas = FALSE)
    plt %>% config(responsive = FALSE)
  }

  write_current_plot_png <- function(file, width = NULL, height = NULL){
    width  <- width  %||% input$plot_w
    height <- height %||% input$plot_h
    eff_width <- effective_plot_width(width)
    eff_height <- effective_plot_height(height)
    scope_sel  <- if (input$scope == "Combinado") "Combinado" else "Por Cepa"
    strain_sel <- if (scope_sel == "Por Cepa") input$strain else NULL

    ok <- tryCatch({
      plt <- build_current_plotly_for_export(scope_sel, strain_sel, eff_width, eff_height)
      export_plotly_image(
        p = plt,
        file = file,
        width = eff_width,
        height = eff_height,
        zoom = 1,
        background = "white"
      )
      TRUE
    }, error = function(e) FALSE)

    if (isTRUE(ok)) return(invisible(TRUE))

    p_fallback <- build_plot(scope_sel, strain_sel, input$tipo, for_interactive = TRUE)
    if (inherits(p_fallback, "ggplot")) {
      ggplot2::ggsave(
        filename = file,
        plot = p_fallback,
        width = eff_width / 96,
        height = eff_height / 96,
        units = "in",
        dpi = 96,
        limitsize = FALSE,
        bg = "white"
      )
      return(invisible(TRUE))
    }
    stop("Plot export failed.")
  }
  
  write_current_plot_pdf <- function(file, width = NULL, height = NULL){
    width  <- width  %||% input$plot_w
    height <- height %||% input$plot_h
    eff_width <- effective_plot_width(width)
    eff_height <- effective_plot_height(height)
    scope_sel  <- if (input$scope == "Combinado") "Combinado" else "Por Cepa"
    strain_sel <- if (scope_sel == "Por Cepa") input$strain else NULL

    ok <- tryCatch({
      plt <- build_current_plotly_for_export(scope_sel, strain_sel, eff_width, eff_height)
      export_plotly_image(
        p = plt,
        file = file,
        width = eff_width,
        height = eff_height,
        zoom = 1,
        background = "white"
      )
      TRUE
    }, error = function(e) FALSE)

    if (isTRUE(ok)) return(invisible(TRUE))

    p_fallback <- build_plot(scope_sel, strain_sel, input$tipo, for_interactive = TRUE)
    if (inherits(p_fallback, "ggplot")) {
      ggplot2::ggsave(
        filename = file,
        plot = p_fallback,
        width = eff_width / 96,
        height = eff_height / 96,
        units = "in",
        limitsize = FALSE,
        device = cairo_pdf,
        bg = "white"
      )
      return(invisible(TRUE))
    }
    stop("Plot export failed.")
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
      choices = named_choices(
        c("both", "x_only", "y_only"),
        tr_text(c("corr_norm_both", "corr_norm_x", "corr_norm_y"), lang)
      ),
      selected = input$corr_norm_target %||% "both"
    )
  }, ignoreInit = TRUE)

    refresh_static_choices <- function() {
    lang <- input$app_lang %||% i18n_lang
    updateRadioButtons(
      session,
      "scope",
      choices  = named_choices(
        c("Por Cepa", "Combinado"),
        list(scope_by_label(lang), tr_text("scope_combined", lang))
      ),
      selected = input$scope %||% "Por Cepa"
    )

    params_now <- plot_cfg_box()$Parameter %||% character(0)
    if (length(params_now) == 0 || identical(params_now, "Parametro_dummy")) {
      type_ids <- c("Curvas")
      type_labels <- list(tr("plot_curves"))
      default_type <- "Curvas"
    } else if (isTRUE(summary_input_mode())) {
      type_ids <- c("Barras", "Curvas", "Apiladas", "Correlacion", "Heatmap", "MatrizCorrelacion")
      type_labels <- list(
        tr("plot_bars"),
        tr("plot_curves"),
        tr("plot_stacked"),
        tr("plot_correlation"),
        tr("plot_heatmap"),
        tr("plot_corr_matrix")
      )
      default_type <- "Barras"
    } else {
      type_ids <- c("Boxplot", "Barras", "Violin", "Curvas", "Apiladas", "Correlacion", "Heatmap", "MatrizCorrelacion")
      type_labels <- list(
        tr("plot_boxplot"),
        tr("plot_bars"),
        tr("plot_violin"),
        tr("plot_curves"),
        tr("plot_stacked"),
        tr("plot_correlation"),
        tr("plot_heatmap"),
        tr("plot_corr_matrix")
      )
      default_type <- "Boxplot"
    }
    selected_type <- input$tipo %||% default_type
    if (!selected_type %in% type_ids) selected_type <- default_type
    updateRadioButtons(
      session,
      "tipo",
      choices = named_choices(type_ids, type_labels),
      selected = selected_type
    )

    updateRadioButtons(
      session,
      "corr_method",
      choices  = named_choices(
        c("pearson", "spearman", "kendall"),
        list(tr("corr_method_pearson"), tr("corr_method_spearman"), tr("corr_method_kendall"))
      ),
      selected = input$corr_method %||% "pearson"
    )

    updateRadioButtons(
      session,
      "corr_adv_method",
      choices  = named_choices(
        c("pearson", "spearman", "kendall"),
        list(tr("corr_method_pearson"), tr("corr_method_spearman"), tr("corr_method_kendall"))
      ),
      selected = input$corr_adv_method %||% "pearson"
    )

    updateRadioButtons(
      session,
      "corr_adv_data_mode",
      choices = named_choices(
        c("raw", "norm_both", "norm_x", "norm_y"),
        list(
          tr("corr_adv_data_raw"),
          tr("corr_adv_data_norm_both"),
          tr("corr_adv_data_norm_x"),
          tr("corr_adv_data_norm_y")
        )
      ),
      selected = input$corr_adv_data_mode %||% "raw"
    )

    updateSelectInput(
      session,
      "corr_adv_direction",
      choices = named_choices(
        c("all", "positive", "negative"),
        list(
          tr("corr_adv_direction_all"),
          tr("corr_adv_direction_pos"),
          tr("corr_adv_direction_neg")
        )
      ),
      selected = input$corr_adv_direction %||% "all"
    )

    updateRadioButtons(
      session,
      "corrm_method",
      choices = named_choices(
        c("pearson", "spearman", "kendall"),
        list(tr("corr_method_pearson"), tr("corr_method_spearman"), tr("corr_method_kendall"))
      ),
      selected = input$corrm_method %||% "spearman"
    )

    updateRadioButtons(
      session,
      "corrm_adjust",
      choices = named_choices(
        c("holm", "fdr", "bonferroni", "none"),
        list(tr("multitest_holm"), tr("multitest_fdr"), tr("multitest_bonferroni"), tr("multitest_none"))
      ),
      selected = input$corrm_adjust %||% "none"
    )

    updateRadioButtons(
      session,
      "multitest_method",
      choices = named_choices(
        c("holm", "fdr", "bonferroni", "none"),
        list(tr("multitest_holm"), tr("multitest_fdr"), tr("multitest_bonferroni"), tr("multitest_none"))
      ),
      selected = input$multitest_method %||% "none"
    )

    updateRadioButtons(
      session,
      "heat_scale_mode",
      choices = named_choices(
        c("none", "row", "column"),
        list(tr("heatmap_scale_none"), tr("heatmap_scale_row"), tr("heatmap_scale_col"))
      ),
      selected = input$heat_scale_mode %||% "none"
    )
    updateRadioButtons(
      session,
      "heat_orientation",
      choices = named_choices(
        c("params_rows", "params_cols"),
        list(tr("heatmap_orientation_params_rows"), tr("heatmap_orientation_params_cols"))
      ),
      selected = input$heat_orientation %||% "params_rows"
    )

    updateSelectInput(
      session,
      "heat_hclust_method",
      choices = c("ward.D2", "ward.D", "complete", "average", "single", "mcquitty", "median", "centroid"),
      selected = input$heat_hclust_method %||% "ward.D2"
    )

    updateRadioButtons(
      session,
      "corr_ci_style",
      choices = named_choices(
        c("band", "dashed"),
        list(tr("corr_ci_band"), tr("corr_ci_dashed"))
      ),
      selected = input$corr_ci_style %||% "band"
    )

    updateRadioButtons(
      session,
      "cur_ci_style",
      choices = named_choices(
        c("ribbon", "errorbar"),
        list(tr("curves_ci_ribbon"), tr("curves_ci_errorbar"))
      ),
      selected = input$cur_ci_style %||% "ribbon"
    )

    updateRadioButtons(
      session,
      "curve_geom",
      choices = named_choices(
        c("line_points", "line_only"),
        list(tr("curves_geom_line_points"), tr("curves_geom_line_only"))
      ),
      selected = input$curve_geom %||% "line_points"
    )

    updateRadioButtons(
      session,
      "curve_color_mode",
      choices = named_choices(
        c("by_group", "single"),
        list(tr("curves_color_by_group"), tr("curves_color_single"))
      ),
      selected = input$curve_color_mode %||% "by_group"
    )

    updateCheckboxGroupInput(
      session,
      "curve_stats_methods",
      choices = named_choices(
        c("S1", "S2", "S3", "S4"),
        list(
          tr("curves_stats_s1"),
          tr("curves_stats_s2"),
          tr("curves_stats_s3"),
          tr("curves_stats_s4")
        )
      ),
      selected = input$curve_stats_methods %||% c("S1", "S2", "S3", "S4")
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
        tr_text(
          c(
            "palette_default",
            "palette_default_soft",
            "palette_bw",
            "palette_bw_soft",
            "palette_viridis",
            "palette_viridis_soft",
            "palette_plasma",
            "palette_plasma_soft",
            "palette_magma",
            "palette_magma_soft",
            "palette_cividis",
            "palette_cividis_soft",
            "palette_set1",
            "palette_set1_soft",
            "palette_set2",
            "palette_set2_soft",
            "palette_set3",
            "palette_set3_soft",
            "palette_dark2",
            "palette_dark2_soft",
            "palette_accent",
            "palette_accent_soft",
            "palette_paired",
            "palette_paired_soft",
            "palette_pastel1",
            "palette_pastel1_soft",
            "palette_pastel2",
            "palette_pastel2_soft",
            "palette_okabeito",
            "palette_okabeito_soft",
            "palette_tableau",
            "palette_tableau_soft"
          ),
          lang
        )
      ),
      selected = input$colorMode %||% "Default"
    )

    updateRadioButtons(
      session,
      "adv_pal_type",
      choices = named_choices(
        c("seq", "div", "qual"),
        list(tr("palette_type_seq"), tr("palette_type_div"), tr("palette_type_qual"))
      ),
      selected = input$adv_pal_type %||% "seq"
    )

    updateCheckboxGroupInput(
      session,
      "adv_pal_filters",
      choices = named_choices(
        c("colorblind", "print", "photocopy"),
        list(tr("palette_filter_colorblind"), tr("palette_filter_print"), tr("palette_filter_photocopy"))
      ),
      selected = input$adv_pal_filters %||% character(0)
    )

    updateCheckboxGroupInput(
      session,
      "normTests",
      choices = named_choices(
        c("shapiro", "ks", "ad"),
        list(tr("norm_shapiro"), tr("norm_ks"), tr("norm_ad"))
      ),
      selected = input$normTests %||% c("shapiro", "ks", "ad")
    )

    updateRadioButtons(
      session,
      "sigTest",
      choices = named_choices(
        c("ANOVA", "Kruskal-Wallis", "ttest", "wilcox"),
        list(
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
        list(tr("comp_all"), tr("comp_control"), tr("comp_pair"))
      ),
      selected = input$compMode %||% "all"
    )

    updateRadioButtons(
      session,
      "sig_mode",
      choices = named_choices(
        c("bars", "labels"),
        list(tr("sig_mode_bars"), tr("sig_mode_labels"))
      ),
      selected = input$sig_mode %||% "bars"
    )

    updateRadioButtons(
      session,
      "sig_auto_include",
      choices = named_choices(
        c("significant", "all"),
        list(tr("sig_auto_significant"), tr("sig_auto_all"))
      ),
      selected = input$sig_auto_include %||% "significant"
    )

    updateRadioButtons(
      session,
      "sig_auto_label_mode",
      choices = named_choices(
        c("stars", "pvalue"),
        list(tr("sig_auto_label_stars"), tr("sig_auto_label_p"))
      ),
      selected = input$sig_auto_label_mode %||% "stars"
    )

    if (!is.null(input$combo_pal)) {
      updateSelectInput(
        session,
        "combo_pal",
        choices = named_choices(
          c(
            "Original",
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
          tr_text(
            c(
              "palette_original",
              "palette_default",
              "palette_default_soft",
              "palette_bw",
              "palette_bw_soft",
              "palette_viridis",
              "palette_viridis_soft",
              "palette_plasma",
              "palette_plasma_soft",
              "palette_magma",
              "palette_magma_soft",
              "palette_cividis",
              "palette_cividis_soft",
              "palette_set1",
              "palette_set1_soft",
              "palette_set2",
              "palette_set2_soft",
              "palette_set3",
              "palette_set3_soft",
              "palette_dark2",
              "palette_dark2_soft",
              "palette_accent",
              "palette_accent_soft",
              "palette_paired",
              "palette_paired_soft",
              "palette_pastel1",
              "palette_pastel1_soft",
              "palette_pastel2",
              "palette_pastel2_soft",
              "palette_okabeito",
              "palette_okabeito_soft",
              "palette_tableau",
              "palette_tableau_soft"
            ),
            lang
          )
        ),
        selected = input$combo_pal %||% "Original"
      )
    }

    if (!is.null(input$combo_adv_pal_type)) {
      updateRadioButtons(
        session,
        "combo_adv_pal_type",
        choices = named_choices(
          c("seq", "div", "qual"),
          tr_text(c("palette_type_seq", "palette_type_div", "palette_type_qual"), lang)
        ),
        selected = input$combo_adv_pal_type %||% "seq"
      )
    }

    if (!is.null(input$combo_adv_pal_filters)) {
      updateCheckboxGroupInput(
        session,
        "combo_adv_pal_filters",
        choices = named_choices(
          c("colorblind", "print", "photocopy"),
          tr_text(c("palette_filter_colorblind", "palette_filter_print", "palette_filter_photocopy"), lang)
        ),
        selected = input$combo_adv_pal_filters %||% character(0)
      )
    }

    if (!is.null(input$combo_legend_scope)) {
      updateSelectInput(
        session,
        "combo_legend_scope",
        choices = named_choices(
          c("all_plots", "by_type", "collect"),
          tr_text(
            c("combo_legend_scope_all", "combo_legend_scope_type", "combo_legend_scope_collect"),
            lang
          )
        ),
        selected = input$combo_legend_scope %||% "by_type"
      )
    }

    if (!is.null(input$combo_legend_side)) {
      updateRadioButtons(
        session,
        "combo_legend_side",
        choices = named_choices(
          c("right", "left"),
          tr_text(c("combo_legend_side_right", "combo_legend_side_left"), lang)
        ),
        selected = input$combo_legend_side %||% "right"
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
    lang_js <- gsub("\\\\", "\\\\\\\\", as.character(lang))
    lang_js <- gsub("'", "\\\\'", lang_js, fixed = TRUE)
    shinyjs::runjs(sprintf(
      "(function(){var node=document.getElementById('i18n-state');if(!node||!window.jQuery)return;if(!Array.isArray(window.i18n_translations))return;jQuery(node).data('lang','%s').trigger('change');})();",
      lang_js
    ))
    set_default_labels(lang, force = FALSE)
    refresh_static_choices()

    # Keep floating mobile switch labels in sync with current language.
    js_escape <- function(x) {
      y <- as.character(x %||% "")
      y <- gsub("\\\\", "\\\\\\\\", y)
      y <- gsub("'", "\\\\'", y, fixed = TRUE)
      y <- gsub("\\r?\\n", " ", y, perl = TRUE)
      y
    }
    mobile_sidebar_lbl <- js_escape(tr_text("mobile_switch_view_graphics", lang))
    mobile_main_lbl <- js_escape(tr_text("mobile_switch_config_panel", lang))
    shinyjs::runjs(sprintf(
      "if(window.BIOSZEN_setPaneLabels){ window.BIOSZEN_setPaneLabels('%s','%s'); }",
      mobile_sidebar_lbl,
      mobile_main_lbl
    ))
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
  legend_right_enabled <- function(color_mode) {
    isTRUE(input$legend_right) && !identical(color_mode, "Blanco y Negro")
  }

  apply_square_legend_right <- function(p) {
    p +
      guides(
        fill = guide_legend(
          title = NULL,
          override.aes = list(
            shape = 15,
            size = 5,
            colour = "black",
            stroke = 0,
            alpha = 1
          )
        )
      ) +
      theme(
        legend.position = "right",
        legend.text = element_text(size = input$fs_legend, colour = "black"),
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.key.size = unit(1.2, "lines")
      )
  }

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

  sanitize_curve_label <- function(x) {
    if (is.null(x)) return(x)
    out <- as.character(x)
    out[is.na(x)] <- NA_character_
    out <- trimws(out)
    # Handles tuple-like labels from imported data, e.g. "(siSORCS2, 1)".
    out <- sub(
      "^\\(\\s*['\"]?(.+?)['\"]?\\s*,\\s*[-+]?[0-9]+(?:\\.[0-9]+)?\\s*\\)$",
      "\\1",
      out,
      perl = TRUE
    )
    out <- sub("^\\((.+)\\)$", "\\1", out)
    out <- sub(",\\s*[-+]?[0-9]+(?:\\.[0-9]+)?\\s*$", "", out, perl = TRUE)
    out <- sub("^['\"](.+)['\"]$", "\\1", out, perl = TRUE)
    trimws(out)
  }

  sanitize_curve_label_preserve_levels <- function(x) {
    if (is.null(x)) return(x)
    if (!is.factor(x)) return(sanitize_curve_label(x))

    raw_levels <- levels(x)
    clean_levels <- sanitize_curve_label(raw_levels)
    clean_levels <- clean_levels[!is.na(clean_levels) & nzchar(clean_levels)]
    clean_vals <- sanitize_curve_label(as.character(x))
    factor(clean_vals, levels = unique(clean_levels))
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
    if (isTRUE(input$doNorm) && !has_ctrl_selected()) {
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
    out <- as.list(as.character(seq_along(sl)))
    names(out) <- labels
    out
  }

  format_sig_p_value <- function(p, digits = 3L) {
    p <- suppressWarnings(as.numeric(p))
    if (!is.finite(p)) return("NA")
    digits <- suppressWarnings(as.integer(digits))
    if (!is.finite(digits) || digits < 1) digits <- 3L
    if (digits > 6) digits <- 6L
    thr <- 10^(-digits)
    if (p < thr) return(paste0("<", formatC(thr, format = "f", digits = digits)))
    formatC(p, format = "f", digits = digits)
  }

  prepare_sig_results_tbl <- function(df, adjust_method = "none", lang = i18n_lang) {
    if (is.null(df) || !is.data.frame(df) || !nrow(df)) return(tibble::tibble())
    tbl <- tibble::as_tibble(df)

    if ("comparison" %in% names(tbl) || "contrast" %in% names(tbl)) {
      cmp_src <- if ("comparison" %in% names(tbl)) tbl$comparison else tbl$contrast
      cmp <- split_comparison(cmp_src)
      tbl$group1 <- cmp[, 1]
      tbl$group2 <- cmp[, 2]
    }
    if (all(c("grupo1", "grupo2") %in% names(tbl))) {
      tbl$group1 <- tbl$grupo1
      tbl$group2 <- tbl$grupo2
    }
    if (!all(c("group1", "group2") %in% names(tbl))) return(tibble::tibble())

    p_candidates <- intersect(
      c("p", "p.value", "p.adj", "adj.p.value", "p_val", "p.value.adj"),
      names(tbl)
    )
    if (!length(p_candidates)) return(tibble::tibble())
    pcol <- p_candidates[1]

    if (!adjust_method %in% c("holm", "fdr", "bonferroni", "none")) adjust_method <- "none"

    tbl2 <- tbl %>%
      dplyr::mutate(
        group1 = as.character(group1),
        group2 = as.character(group2),
        P_valor = suppressWarnings(as.numeric(.data[[pcol]]))
      ) %>%
      dplyr::filter(
        !is.na(group1), nzchar(group1),
        !is.na(group2), nzchar(group2),
        group1 != group2
      )
    if (!nrow(tbl2)) return(tibble::tibble())

    tbl2 <- apply_multitest_preset(
      tbl2,
      p_col = "P_valor",
      method = adjust_method,
      out_col = "P_ajustado"
    )
    p_ref <- if (identical(adjust_method, "none")) "P_valor" else "P_ajustado"

    tbl2 %>%
      dplyr::mutate(
        P_referencia = .data[[p_ref]],
        is_significant = !is.na(P_referencia) & P_referencia < 0.05,
        Significativo = dplyr::if_else(
          is_significant,
          tr_text("yes_label", lang),
          tr_text("no_label", lang)
        ),
        Estrellas = dplyr::case_when(
          P_referencia < 0.001 ~ "***",
          P_referencia < 0.01  ~ "**",
          P_referencia < 0.05  ~ "*",
          TRUE ~ ""
        )
      )
  }

  sig_pair_key <- function(g1, g2, param = NULL) {
    gg <- sort(c(as.character(g1), as.character(g2)))
    if (is.null(param) || !nzchar(as.character(param))) {
      paste(gg, collapse = "||")
    } else {
      paste(c(gg, paste0("@", as.character(param))), collapse = "||")
    }
  }

  sig_table_processed <- reactive({
    req(input$runSig)
    lang <- input$app_lang %||% i18n_lang
    adjust_method <- input$multitest_method %||% "none"
    prepare_sig_results_tbl(sig_res(), adjust_method = adjust_method, lang = lang)
  })

  observeEvent(list(sig_list(), input$app_lang), {
    choices  <- sig_choice_vec(sig_list())
    current  <- isolate(input$sig_current)
    pre_sel  <- sig_preselect()
    sig_preselect(NULL)
    selected <- intersect(
      if (length(pre_sel)) pre_sel else current,
      unlist(choices, use.names = FALSE)
    )
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

  observeEvent(input$sig_current, {
    ids <- as.integer(input$sig_current)
    if (length(ids) != 1) return()
    sl <- sig_list()
    idx <- ids[1]
    if (!is.finite(idx) || idx < 1 || idx > length(sl)) return()
    cmp <- sl[[idx]]
    if (!is.null(cmp$g1) && nzchar(as.character(cmp$g1))) {
      updateSelectInput(session, "sig_group1", selected = as.character(cmp$g1))
    }
    if (!is.null(cmp$g2) && nzchar(as.character(cmp$g2))) {
      updateSelectInput(session, "sig_group2", selected = as.character(cmp$g2))
    }
    updateTextInput(session, "sig_label", value = as.character(cmp$lab %||% ""))
    if (identical(input$tipo, "Apiladas")) {
      cmp_param <- cmp$param %||% ""
      if (nzchar(cmp_param)) updateSelectizeInput(session, "sig_param", selected = cmp_param, server = TRUE)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$sig_update_label, {
    lang <- input$app_lang %||% i18n_lang
    ids <- as.integer(input$sig_current)
    if (length(ids) != 1) {
      showNotification(tr_text("sig_update_select_one", lang), type = "message", duration = 4)
      return()
    }
    req(nzchar(input$sig_label))
    sl <- sig_list()
    idx <- ids[1]
    if (!is.finite(idx) || idx < 1 || idx > length(sl)) return()
    if (!is.null(input$sig_group1) && nzchar(as.character(input$sig_group1))) {
      sl[[idx]]$g1 <- as.character(input$sig_group1)
    }
    if (!is.null(input$sig_group2) && nzchar(as.character(input$sig_group2))) {
      sl[[idx]]$g2 <- as.character(input$sig_group2)
    }
    sl[[idx]]$lab <- input$sig_label
    if (identical(input$sig_mode, "labels") && identical(input$tipo, "Apiladas")) {
      sl[[idx]]$param <- input$sig_param %||% sl[[idx]]$param
    }
    sig_preselect(as.character(idx))
    sig_list(sl)
  })

  observeEvent(input$sig_auto_apply, {
    lang <- input$app_lang %||% i18n_lang
    tbl <- sig_table_processed()
    validate(need(nrow(tbl) > 0, tr_text("no_sig_results", lang)))

    include_mode <- input$sig_auto_include %||% "significant"
    auto_tbl <- tbl
    if (identical(include_mode, "significant")) {
      auto_tbl <- auto_tbl %>% dplyr::filter(is_significant)
    }
    validate(need(nrow(auto_tbl) > 0, tr_text("sig_auto_no_matches", lang)))

    auto_tbl$.pair_key <- vapply(
      seq_len(nrow(auto_tbl)),
      function(i) sig_pair_key(auto_tbl$group1[i], auto_tbl$group2[i]),
      character(1)
    )
    auto_tbl <- auto_tbl %>%
      dplyr::arrange(P_referencia) %>%
      dplyr::group_by(.pair_key) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup()

    label_mode <- input$sig_auto_label_mode %||% "stars"
    labels <- if (identical(label_mode, "pvalue")) {
      vapply(auto_tbl$P_referencia, format_sig_p_value, character(1), digits = 3L)
    } else {
      stars <- as.character(auto_tbl$Estrellas)
      stars[is.na(stars) | !nzchar(stars)] <- "ns"
      stars
    }

    param_val <- if (identical(input$sig_mode, "labels") && identical(input$tipo, "Apiladas")) {
      input$sig_param %||% NULL
    } else {
      NULL
    }

    auto_sigs <- lapply(seq_len(nrow(auto_tbl)), function(i) {
      list(
        g1 = as.character(auto_tbl$group1[i]),
        g2 = as.character(auto_tbl$group2[i]),
        lab = as.character(labels[[i]]),
        param = param_val
      )
    })
    auto_sigs <- auto_sigs[vapply(auto_sigs, function(x) {
      nzchar(as.character(x$g1)) && nzchar(as.character(x$g2)) && !identical(as.character(x$g1), as.character(x$g2))
    }, logical(1))]
    validate(need(length(auto_sigs) > 0, tr_text("sig_auto_no_matches", lang)))

    replace_mode <- isTRUE(input$sig_auto_replace)
    if (replace_mode) {
      sig_list(auto_sigs)
      sig_preselect(as.character(seq_along(auto_sigs)))
      showNotification(
        sprintf(tr_text("sig_auto_added_n", lang), length(auto_sigs)),
        type = "message",
        duration = 4
      )
      return()
    }

    sl_old <- sig_list()
    param_key <- if (identical(input$sig_mode, "labels") && identical(input$tipo, "Apiladas")) {
      input$sig_param %||% ""
    } else {
      NULL
    }
    keys_old <- if (length(sl_old)) {
      vapply(sl_old, function(cmp) sig_pair_key(cmp$g1, cmp$g2, param_key), character(1))
    } else {
      character(0)
    }
    to_add <- list()
    for (cmp in auto_sigs) {
      key <- sig_pair_key(cmp$g1, cmp$g2, param_key)
      if (!key %in% keys_old) {
        to_add <- append(to_add, list(cmp))
        keys_old <- c(keys_old, key)
      }
    }
    if (!length(to_add)) {
      showNotification(tr_text("sig_auto_no_matches", lang), type = "message", duration = 4)
      return()
    }
    new_sl <- append(sl_old, to_add)
    start_idx <- length(sl_old) + 1
    sig_preselect(as.character(seq.int(start_idx, length(new_sl))))
    sig_list(new_sl)
    showNotification(
      sprintf(tr_text("sig_auto_added_n", lang), length(to_add)),
      type = "message",
      duration = 4
    )
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
      parsed <- load_curve_workbook(
        input$curveFile$datapath,
        file_name = input$curveFile$name %||% ""
      )
      if (!isTRUE(parsed$ok)) {
        msg <- parsed$message
        if (!is.character(msg) || !nzchar(msg)) {
          msg <- "Curve workbook format not recognized."
        }
        stop(msg)
      }

      # 4) Guardar en los reactivos
      cur_data_box(parsed$Sheet1)
      cur_cfg_box(parsed$Sheet2)
      cur_meta_box(parsed$Meta)
      cur_sum_box(parsed$Summary)
      curve_summary_mode(isTRUE(parsed$SummaryMode))
      
      # 5) Inicializar lÃƒÂ­mites de curvas
      ylims$Curvas <- list(
        xmax   = parsed$Sheet2$X_Max[1],
        xbreak = parsed$Sheet2$Interval_X[1],
        ymax   = parsed$Sheet2$Y_Max[1],
        ybreak = parsed$Sheet2$Interval_Y[1]
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
    
    meta <- curve_meta() %||% datos_combinados()
    if (is.null(meta) || !nrow(meta)) return(NULL)
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
      reps <- normalize_rep_selection(stats::na.omit(cfg$BiologicalReplicate[cfg$Well == w]))
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
    dataset_loading(TRUE)
    on.exit(dataset_loading(FALSE), add = TRUE)
    merged_platemap_path(NULL)
    merged_platemap_name(NULL)
    merged_well_map(NULL)
    merge_status("")

    ok <- tryCatch({

      file_path <- input$dataFile$datapath
      file_name <- input$dataFile$name %||% ""
      is_csv_input <- is_csv_filename(file_name)

      df_raw <- NULL
      cfg_raw <- NULL
      is_group <- FALSE
      summary_mode_this <- FALSE
      col_labels <- list(Strain = NULL, Media = NULL)

      if (isTRUE(is_csv_input)) {
        df_raw <- read_csv_tmp(file_path)
        if (!is.null(df_raw) && !is.null(names(df_raw))) {
          mapped <- apply_column_aliases(df_raw, allow_media_alias = TRUE)
          df_raw <- mapped$datos
          col_labels <- mapped$labels
        }

        if (is.null(df_raw) || !all(c("Strain", "Media") %in% names(df_raw))) {
          conv_summary <- build_platemap_from_mean_sd_data(
            raw = df_raw,
            col_labels = col_labels,
            default_profile = "csv"
          )
          if (is.null(conv_summary)) stop("CSV format not recognized.")
          df_raw <- conv_summary$Datos
          cfg_raw <- conv_summary$PlotSettings
          if (!is.null(conv_summary$Labels)) col_labels <- conv_summary$Labels
          summary_mode_this <- TRUE
        }
      } else {
        df_raw <- tryCatch(
          read_excel_tmp(file_path, sheet = "Datos"),
          error = function(e) NULL
        )

        if (!is.null(df_raw) && !is.null(names(df_raw))) {
          mapped <- apply_column_aliases(df_raw, allow_media_alias = TRUE)
          df_raw <- mapped$datos
          col_labels <- mapped$labels
        }

        if (is.null(df_raw) || !all(c("Strain", "Media") %in% names(df_raw))) {
          conv_summary <- build_platemap_from_mean_sd(file_path)
          if (!is.null(conv_summary)) {
            df_raw <- conv_summary$Datos
            cfg_raw <- conv_summary$PlotSettings
            if (!is.null(conv_summary$Labels)) col_labels <- conv_summary$Labels
            summary_mode_this <- TRUE
          } else {
            conv <- build_platemap_from_summary(file_path)
            if (is.null(conv)) stop("Formato de archivo no reconocido.")
            df_raw  <- conv$Datos
            cfg_raw <- conv$PlotSettings
            if (!is.null(conv$Labels)) col_labels <- conv$Labels
            is_group <- TRUE
          }
        } else {
          cfg_raw <- tryCatch(
            read_excel_tmp(file_path, sheet = "PlotSettings"),
            error = function(e) NULL
          )
        }
      }

      prep <- prepare_platemap(
        df_raw,
        cfg_raw,
        defaults_profile = if (isTRUE(is_csv_input)) "csv" else "excel"
      )
      df   <- prep$datos
      cfg  <- prep$cfg
      reset_dataset_state()
      apply_data_labels(col_labels, input$app_lang %||% i18n_lang)

      cfg_params <- as.character(cfg$Parameter %||% character(0))
      cfg_y_max <- suppressWarnings(as.numeric(cfg$Y_Max %||% NA_real_))
      cfg_interval <- suppressWarnings(as.numeric(cfg$Interval %||% NA_real_))
      for (i in seq_along(cfg_params)) {
        p_chr <- trimws(cfg_params[[i]])
        if (is.na(p_chr) || !nzchar(p_chr)) next
        ylims[[p_chr]] <- list(
          ymax   = cfg_y_max[[i]],
          ybreak = cfg_interval[[i]]
        )
        ylims[[paste0(p_chr, "_Norm")]] <- list(
          ymax   = 1,
          ybreak = 0.2
        )
      }
      datos_box(df);            plot_cfg_box(cfg)

      summary_input_mode(isTRUE(summary_mode_this))
      is_group_data(is_group)
      # Keep default plot type in sync with the newly loaded dataset.
      refresh_static_choices()

      if ((isTRUE(summary_mode_this) || isTRUE(is_group)) && !isTRUE(is_csv_input)) {
        curve_conv <- load_curve_workbook(file_path, file_name = file_name)
        if (isTRUE(curve_conv$ok)) {
          cur_data_box(curve_conv$Sheet1)
          cur_cfg_box(curve_conv$Sheet2)
          cur_meta_box(curve_conv$Meta)
          cur_sum_box(curve_conv$Summary)
          curve_summary_mode(isTRUE(curve_conv$SummaryMode))
          ylims$Curvas <- list(
            xmax = curve_conv$Sheet2$X_Max[1],
            xbreak = curve_conv$Sheet2$Interval_X[1],
            ymax = curve_conv$Sheet2$Y_Max[1],
            ybreak = curve_conv$Sheet2$Interval_Y[1]
          )
        } else {
          lang <- input$app_lang %||% i18n_lang
          if (identical(curve_conv$reason, "invalid")) {
            detail <- curve_conv$message
            if (!is.character(detail) || !nzchar(detail)) {
              detail <- "Curve workbook format not recognized."
            }
            showNotification(
              sprintf(tr_text("curve_embedded_invalid", lang), detail),
              type = "warning",
              duration = 7
            )
          } else {
            showNotification(
              tr_text("curve_embedded_missing", lang),
              type = "message",
              duration = 7
            )
          }
        }
      }

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
        choices  = named_choices(c("Curvas"), list(tr("plot_curves"))),
        selected = "Curvas"
      )
    } else if (isTRUE(is_summary_mode())) {
      updateRadioButtons(
        session,
        "tipo",
        choices  = named_choices(
          c("Barras", "Curvas", "Apiladas", "Correlacion", "Heatmap", "MatrizCorrelacion"),
          list(
            tr("plot_bars"),
            tr("plot_curves"),
            tr("plot_stacked"),
            tr("plot_correlation"),
            tr("plot_heatmap"),
            tr("plot_corr_matrix")
          )
        ),
        selected = "Barras"
      )
    } else {
      updateRadioButtons(
        session,
        "tipo",
        choices  = named_choices(
          c("Boxplot", "Barras", "Violin", "Curvas", "Apiladas", "Correlacion",
            "Heatmap", "MatrizCorrelacion"),
          list(
            tr("plot_boxplot"),
            tr("plot_bars"),
            tr("plot_violin"),
            tr("plot_curves"),
            tr("plot_stacked"),
            tr("plot_correlation"),
            tr("plot_heatmap"),
            tr("plot_corr_matrix")
          )
        ),
        selected = "Boxplot"
      )
    }
    
    # 2.b) selector de parÃƒÂ¡metro (lo deje vacÃƒÂ­o si no hay)
      update_selectize_adaptive(
        "param",
        choices = params,
        selected = if (length(params)) params[1] else character(0)
      )

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


  observeEvent(input$mergePlatemaps, {
    lang <- input$app_lang %||% i18n_lang

    if (is.null(input$dataFile) || is.null(input$dataFile$datapath) || !nzchar(input$dataFile$datapath)) {
      msg <- tr_text("merge_need_base", lang)
      merge_status(msg)
      showNotification(msg, type = "warning", duration = 6)
      return()
    }
    if (is.null(input$mergeFiles) || !nrow(input$mergeFiles)) {
      msg <- tr_text("merge_need_files", lang)
      merge_status(msg)
      showNotification(msg, type = "warning", duration = 6)
      return()
    }

    merge_paths <- as.character(input$mergeFiles$datapath %||% character(0))
    merge_paths <- merge_paths[!is.na(merge_paths) & nzchar(merge_paths)]
    merge_names <- as.character(input$mergeFiles$name %||% basename(merge_paths))
    if (!length(merge_paths)) {
      msg <- tr_text("merge_need_files", lang)
      merge_status(msg)
      showNotification(msg, type = "warning", duration = 6)
      return()
    }

    dataset_loading(TRUE)
    on.exit(dataset_loading(FALSE), add = TRUE)

    ok <- tryCatch({
      # If a merged platemap already exists, keep accumulating from it.
      # Otherwise, use the original uploaded Data file as base.
      latest_merged <- merged_platemap_path()
      base_file_for_merge <- if (!is.null(latest_merged) && nzchar(latest_merged) && file.exists(latest_merged)) {
        latest_merged
      } else {
        input$dataFile$datapath
      }

      merged <- merge_platemap_workbooks(
        base_file = base_file_for_merge,
        additional_files = merge_paths,
        additional_labels = merge_names
      )

      prep <- prepare_platemap(merged$Datos, merged$PlotSettings)
      df <- prep$datos
      cfg <- prep$cfg

      reset_dataset_state()
      apply_data_labels(merged$Labels %||% list(Strain = NULL, Media = NULL), lang)

      cfg_params <- as.character(cfg$Parameter %||% character(0))
      cfg_y_max <- suppressWarnings(as.numeric(cfg$Y_Max %||% NA_real_))
      cfg_interval <- suppressWarnings(as.numeric(cfg$Interval %||% NA_real_))
      for (i in seq_along(cfg_params)) {
        p_chr <- trimws(cfg_params[[i]])
        if (is.na(p_chr) || !nzchar(p_chr)) next
        ylims[[p_chr]] <- list(
          ymax = cfg_y_max[[i]],
          ybreak = cfg_interval[[i]]
        )
        ylims[[paste0(p_chr, "_Norm")]] <- list(
          ymax = 1,
          ybreak = 0.2
        )
      }

      datos_box(df)
      plot_cfg_box(cfg)
      summary_input_mode(FALSE)
      is_group_data(FALSE)
      refresh_static_choices()

      params <- plot_cfg_box()$Parameter
      if (length(params) == 0 || identical(params, "Parametro_dummy")) {
        updateRadioButtons(
          session,
          "tipo",
          choices = named_choices(c("Curvas"), list(tr("plot_curves"))),
          selected = "Curvas"
        )
      } else {
        updateRadioButtons(
          session,
          "tipo",
          choices = named_choices(
            c("Boxplot", "Barras", "Violin", "Curvas", "Apiladas", "Correlacion",
              "Heatmap", "MatrizCorrelacion"),
            list(
              tr("plot_boxplot"),
              tr("plot_bars"),
              tr("plot_violin"),
              tr("plot_curves"),
              tr("plot_stacked"),
              tr("plot_correlation"),
              tr("plot_heatmap"),
              tr("plot_corr_matrix")
            )
          ),
          selected = "Boxplot"
        )
      }

      update_selectize_adaptive(
        "param",
        choices = params,
        selected = if (length(params)) params[1] else character(0)
      )

      if (length(params)) {
        first_cfg <- cfg[cfg$Parameter == params[1], , drop = FALSE]
        updateNumericInput(
          session, "ymax",
          label = paste0(tr("y_max"), " (", params[1], "):"),
          value = first_cfg$Y_Max[1]
        )
        updateNumericInput(
          session, "ybreak",
          label = paste0(tr("y_interval"), " (", params[1], "):"),
          value = first_cfg$Interval[1]
        )
      }

      updateRadioButtons(session, "scope", selected = "Por Cepa")
      update_selectize_adaptive("strain", choices = NULL, selected = character(0))

      tmp_xlsx <- tempfile(fileext = ".xlsx")
      writexl::write_xlsx(
        list(Datos = merged$Datos, PlotSettings = merged$PlotSettings),
        path = tmp_xlsx
      )
      merged_platemap_path(tmp_xlsx)
      merged_platemap_name(
        paste0("platemap_merged_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
      )
      merged_well_map(merged$WellMap %||% data.frame())

      info <- merged$Info %||% list()
      merge_status(sprintf(
        tr_text("merge_status_ready", lang),
        info$rows_total %||% nrow(merged$Datos),
        info$parameters_total %||% nrow(merged$PlotSettings),
        info$groups_total %||% 0L,
        info$overlap_groups %||% 0L
      ))
      showNotification(tr_text("merge_loaded", lang), type = "message", duration = 6)
      TRUE
    }, error = function(e) {
      msg <- sprintf(tr_text("merge_failed", lang), conditionMessage(e))
      merge_status(msg)
      showNotification(msg, type = "error", duration = 8)
      FALSE
    })

    if (!ok) return()
  })

  merged_platemap_filename <- function() {
    merged_platemap_name() %||% "platemap_merged.xlsx"
  }

  copy_merged_platemap <- function(file) {
    src <- merged_platemap_path()
    if (is.null(src) || !nzchar(src) || !file.exists(src)) {
      stop(tr_text("merge_no_file", input$app_lang %||% i18n_lang))
    }
    ok <- file.copy(src, file, overwrite = TRUE)
    if (!isTRUE(ok)) {
      stop(tr_text("merge_no_file", input$app_lang %||% i18n_lang))
    }
  }

  output$downloadMergedPlatemap <- downloadHandler(
    filename = merged_platemap_filename,
    content = copy_merged_platemap
  )

  output$downloadMergedPlatemapLatest <- downloadHandler(
    filename = merged_platemap_filename,
    content = copy_merged_platemap
  )

  observeEvent(input$mergeCurves, {
    lang <- input$app_lang %||% i18n_lang

    map_tbl <- merged_well_map()
    if (!is.data.frame(map_tbl) || !nrow(map_tbl)) {
      msg <- tr_text("curve_merge_need_map", lang)
      curve_merge_status(msg)
      showNotification(msg, type = "warning", duration = 6)
      return()
    }
    if (is.null(input$mergeCurveFiles) || !nrow(input$mergeCurveFiles)) {
      msg <- tr_text("curve_merge_need_files", lang)
      curve_merge_status(msg)
      showNotification(msg, type = "warning", duration = 6)
      return()
    }

    merge_curve_paths <- as.character(input$mergeCurveFiles$datapath %||% character(0))
    merge_curve_names <- as.character(input$mergeCurveFiles$name %||% character(0))
    keep_paths <- !is.na(merge_curve_paths) & nzchar(merge_curve_paths)
    if (length(merge_curve_names) < length(keep_paths)) {
      merge_curve_names <- c(
        merge_curve_names,
        rep("", length(keep_paths) - length(merge_curve_names))
      )
    }
    merge_curve_names <- merge_curve_names[seq_len(length(keep_paths))]
    merge_curve_paths <- merge_curve_paths[keep_paths]
    merge_curve_names <- merge_curve_names[keep_paths]
    merge_curve_names <- ifelse(
      is.na(merge_curve_names) | !nzchar(trimws(merge_curve_names)),
      basename(merge_curve_paths),
      merge_curve_names
    )
    if (!length(merge_curve_paths)) {
      msg <- tr_text("curve_merge_need_files", lang)
      curve_merge_status(msg)
      showNotification(msg, type = "warning", duration = 6)
      return()
    }

    base_curve_file <- NULL
    base_curve_name <- NULL
    latest_curve <- merged_curve_path()
    if (!is.null(latest_curve) && nzchar(latest_curve) && file.exists(latest_curve)) {
      base_curve_file <- latest_curve
      base_curve_name <- merged_curve_name() %||% basename(latest_curve)
    } else if (!is.null(input$curveFile) && !is.null(input$curveFile$datapath) && nzchar(input$curveFile$datapath)) {
      base_curve_file <- input$curveFile$datapath
      base_curve_name <- input$curveFile$name %||% basename(input$curveFile$datapath)
    } else if (is.data.frame(cur_data_box()) && nrow(cur_data_box()) && is.data.frame(cur_cfg_box()) && nrow(cur_cfg_box())) {
      tmp_curve_base <- tempfile(fileext = ".xlsx")
      writexl::write_xlsx(
        list(
          Sheet1 = cur_data_box(),
          Sheet2 = cur_cfg_box()
        ),
        path = tmp_curve_base
      )
      base_curve_file <- tmp_curve_base
      base_curve_name <- "curves_current_session.xlsx"
    }

    if (is.null(base_curve_file) || !nzchar(base_curve_file) || !file.exists(base_curve_file)) {
      msg <- tr_text("curve_merge_need_base", lang)
      curve_merge_status(msg)
      showNotification(msg, type = "warning", duration = 6)
      return()
    }

    dataset_loading(TRUE)
    on.exit(dataset_loading(FALSE), add = TRUE)

    ok <- tryCatch({
      merged_curves <- merge_curve_workbooks_by_well_map(
        base_file = base_curve_file,
        additional_files = merge_curve_paths,
        well_map = map_tbl,
        base_name = base_curve_name,
        additional_names = merge_curve_names
      )

      cur_data_box(merged_curves$Sheet1)
      cur_cfg_box(merged_curves$Sheet2)
      cur_meta_box(NULL)
      cur_sum_box(NULL)
      curve_summary_mode(FALSE)

      ylims$Curvas <- list(
        xmax = merged_curves$Sheet2$X_Max[1],
        xbreak = merged_curves$Sheet2$Interval_X[1],
        ymax = merged_curves$Sheet2$Y_Max[1],
        ybreak = merged_curves$Sheet2$Interval_Y[1]
      )

      tmp_curve_xlsx <- tempfile(fileext = ".xlsx")
      writexl::write_xlsx(
        list(
          Sheet1 = merged_curves$Sheet1,
          Sheet2 = merged_curves$Sheet2,
          SourceMap = merged_curves$SourceMap
        ),
        path = tmp_curve_xlsx
      )
      merged_curve_path(tmp_curve_xlsx)
      merged_curve_name(
        paste0("curves_merged_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
      )

      n_curve_cols <- max(ncol(merged_curves$Sheet1) - 1L, 0L)
      n_time_points <- nrow(merged_curves$Sheet1)
      curve_merge_status(sprintf(
        tr_text("curve_merge_status_ready", lang),
        n_curve_cols,
        n_time_points
      ))
      showNotification(tr_text("curve_merge_loaded", lang), type = "message", duration = 6)
      TRUE
    }, error = function(e) {
      msg <- sprintf(tr_text("curve_merge_failed", lang), conditionMessage(e))
      curve_merge_status(msg)
      showNotification(msg, type = "error", duration = 8)
      FALSE
    })

    if (!ok) return()
  })

  merged_curve_filename <- function() {
    merged_curve_name() %||% "curves_merged.xlsx"
  }

  copy_merged_curve <- function(file) {
    src <- merged_curve_path()
    if (is.null(src) || !nzchar(src) || !file.exists(src)) {
      stop(tr_text("curve_merge_no_file", input$app_lang %||% i18n_lang))
    }
    ok <- file.copy(src, file, overwrite = TRUE)
    if (!isTRUE(ok)) {
      stop(tr_text("curve_merge_no_file", input$app_lang %||% i18n_lang))
    }
  }

  output$downloadMergedCurves <- downloadHandler(
    filename = merged_curve_filename,
    content = copy_merged_curve
  )

  output$downloadMergedCurvesLatest <- downloadHandler(
    filename = merged_curve_filename,
    content = copy_merged_curve
  )

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
      # Localizar la carpeta de archivos de referencia en desarrollo o instalado.
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
      ref_candidates <- c("reference_files", "Archivos de referencia")
      ref_dir <- NULL
      for (candidate in ref_candidates) {
        path <- file.path(www_dir, candidate)
        if (dir.exists(path)) {
          ref_dir <- path
          break
        }
      }
      if (is.null(ref_dir)) stop("No se encontro la carpeta de archivos de referencia dentro de 'www'.")

      # Crear zip preservando el nombre de la carpeta
      parent <- dirname(ref_dir)
      base   <- basename(ref_dir)
      zip::zipr(zipfile = file, files = base, root = parent)
    }
  )

  # --- Descargar Manual (PDF) using bundled static files only ---
  output$downloadManual <- downloadHandler(
    filename = function() {
      lang <- input$manual_lang %||% 'en'
      if (lang == 'en') 'MANUAL_EN.pdf' else 'MANUAL_ES.pdf'
    },
    content = function(file) {
      lang <- input$manual_lang %||% 'en'
      fname_pdf  <- if (lang == 'en') 'MANUAL_EN.pdf' else 'MANUAL_ES.pdf'
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
      src_pdf <- file.path(www_dir, fname_pdf)
      if (!file.exists(src_pdf)) {
        stop(sprintf("Missing manual PDF in www: %s", fname_pdf))
      }
      file.copy(src_pdf, file, overwrite = TRUE)
    }
  )

  is_large_param_set <- function(params) {
    length(params) > large_param_threshold
  }

  is_server_side_param_set <- function(params) {
    length(params) > selectize_server_threshold
  }

  default_stack_params <- function(params) {
    if (!length(params)) return(character(0))
    if (is_large_param_set(params)) {
      return(head(params, min(8L, length(params))))
    }
    params
  }

  update_stack_params_input <- function(params, selected = NULL) {
    params <- unique(as.character(params %||% character(0)))
    params <- params[!is.na(params) & nzchar(params)]

    if (is.null(selected)) {
      selected <- isolate(input$stackParams %||% character(0))
    }
    selected <- intersect(as.character(selected %||% character(0)), params)
    if (!length(selected)) selected <- default_stack_params(params)

    if (is_server_side_param_set(params)) {
      update_selectize_adaptive(
        "stackParams",
        choices = params,
        selected = selected
      )
    } else {
      updateCheckboxGroupInput(
        session, "stackParams",
        choices = params,
        selected = selected
      )
    }

    selected
  }

  has_finite_column_values <- function(df, col_name) {
    if (is.null(df) || !is.data.frame(df) || !nrow(df)) return(FALSE)
    if (is.null(col_name) || !length(col_name)) return(FALSE)
    col_chr <- as.character(col_name[[1]])
    if (!nzchar(col_chr) || !col_chr %in% names(df)) return(FALSE)
    vals <- suppressWarnings(as.numeric(df[[col_chr]]))
    any(is.finite(vals))
  }

  normalized_ready_params <- function(df, params) {
    params <- unique(as.character(params %||% character(0)))
    params <- params[!is.na(params) & nzchar(params)]
    if (!length(params)) return(character(0))
    params[vapply(
      params,
      function(p) has_finite_column_values(df, paste0(p, "_Norm")),
      logical(1)
    )]
  }


  observeEvent(
    list(
      plot_settings(), input$app_lang,
      input$doNorm, input$ctrlMedium,
      input$scope, input$strain, input$showGroups, input$showMedios,
      input$corr_adv_data_mode,
      reps_strain_selected(), reps_group_selected(), input$rm_reps_all
    ),
    {
      req(plot_settings())
      params <- unique(as.character(plot_settings()$Parameter %||% character(0)))
      params <- params[!is.na(params) & nzchar(params)]
      high_dim <- is_large_param_set(params)

      current_param <- isolate(input$param %||% "")
      current_x <- isolate(input$corr_param_x %||% "")
      current_y <- isolate(input$corr_param_y %||% "")
      current_adv_anchor <- isolate(input$corr_adv_anchor %||% "")
      current_corrm <- isolate(intersect(as.character(input$corrm_params %||% character(0)), params))
      current_heat <- isolate(intersect(as.character(input$heat_params %||% character(0)), params))

      strict_norm <- isTRUE(input$doNorm) && has_ctrl_selected()
      scope_sel <- input$scope %||% "Por Cepa"
      strain_sel <- if (identical(scope_sel, "Por Cepa")) input$strain %||% NULL else NULL
      has_strain <- !identical(scope_sel, "Por Cepa") || (
        !is.null(strain_sel) && length(strain_sel) &&
          !is.na(strain_sel[[1]]) && nzchar(as.character(strain_sel[[1]]))
      )
      scope_df <- if (has_strain) {
        tryCatch(get_scope_df(scope_sel, strain_sel), error = function(e) NULL)
      } else {
        NULL
      }

      # Strict normalized mode: only allow parameters with at least one finite _Norm
      # in the current selection, so raw-looking plots are never shown under normalization.
      param_choices <- params
      if (strict_norm && !is.null(scope_df)) {
        param_choices <- normalized_ready_params(scope_df, params)
      }

      selected_param <- if (nzchar(current_param) && current_param %in% param_choices) {
        current_param
      } else if (length(param_choices)) {
        param_choices[1]
      } else {
        character(0)
      }

      selected_x <- if (nzchar(current_x) && current_x %in% params) {
        current_x
      } else if (length(params)) {
        params[1]
      } else {
        character(0)
      }

      fallback_y <- if (length(params) >= 2) params[2] else selected_x
      selected_y <- if (nzchar(current_y) && current_y %in% params) {
        current_y
      } else {
        fallback_y
      }

      adv_mode <- input$corr_adv_data_mode %||% "raw"
      strict_adv_norm <- strict_norm && adv_mode %in% c("norm_both", "norm_x", "norm_y")
      adv_anchor_choices <- params
      if (strict_adv_norm && !is.null(scope_df)) {
        adv_anchor_choices <- normalized_ready_params(scope_df, params)
      }

      selected_adv_anchor <- if (nzchar(current_adv_anchor) && current_adv_anchor %in% adv_anchor_choices) {
        current_adv_anchor
      } else if (length(adv_anchor_choices) && selected_x %in% adv_anchor_choices) {
        selected_x
      } else if (length(adv_anchor_choices)) {
        adv_anchor_choices[1]
      } else {
        character(0)
      }

      update_selectize_adaptive(
        "param",
        choices = param_choices,
        selected = selected_param
      )
      stack_selected <- update_stack_params_input(params)

      corrm_default <- if (high_dim) {
        head(params, min(6L, length(params)))
      } else {
        params[seq_len(min(4L, length(params)))]
      }
      heat_default <- params
      corrm_selected <- if (length(current_corrm)) current_corrm else corrm_default
      heat_selected <- if (isTRUE(heat_force_empty())) {
        character(0)
      } else if (length(current_heat)) {
        current_heat
      } else {
        heat_default
      }

      # ---------- Correlacion: selectors server-side -----------------------
      update_selectize_adaptive(
        "corr_param_x",
        choices = params,
        selected = selected_x
      )
      update_selectize_adaptive(
        "corr_param_y",
        choices = params,
        selected = selected_y
      )
      update_selectize_adaptive(
        "corr_adv_anchor",
        choices = adv_anchor_choices,
        selected = selected_adv_anchor
      )
      update_selectize_adaptive(
        "corrm_params",
        choices = params,
        selected = corrm_selected
      )
      update_selectize_adaptive(
        "heat_params",
        choices = params,
        selected = heat_selected
      )
      
      # inicializar tambiÃƒÂ©n el orden de parÃƒÂ¡metros apilados  
      updateTextInput(  
        session, "orderStack",  
        value = paste(stack_selected, collapse = ",")  
      )  
    },
    ignoreInit = FALSE
  )  
  
  observeEvent(input$heat_select_all_params, {
    req(plot_settings())
    heat_force_empty(FALSE)
    params <- unique(as.character(plot_settings()$Parameter %||% character(0)))
    params <- params[!is.na(params) & nzchar(params)]

    if (isTRUE(input$doNorm) && has_ctrl_selected()) {
      scope_sel <- input$scope %||% "Por Cepa"
      strain_sel <- if (identical(scope_sel, "Por Cepa")) input$strain %||% NULL else NULL
      scope_df <- tryCatch(get_scope_df(scope_sel, strain_sel), error = function(e) NULL)
      if (!is.null(scope_df)) {
        params <- normalized_ready_params(scope_df, params)
      }
    }

    update_selectize_adaptive(
      "heat_params",
      choices = params,
      selected = params
    )
  }, ignoreInit = TRUE)

  observeEvent(input$heat_clear_all_params, {
    req(plot_settings())
    heat_force_empty(TRUE)
    params <- unique(as.character(plot_settings()$Parameter %||% character(0)))
    params <- params[!is.na(params) & nzchar(params)]
    update_selectize_adaptive(
      "heat_params",
      choices = params,
      selected = character(0)
    )
  }, ignoreInit = TRUE)

  observeEvent(input$heat_params, {
    vals <- as.character(input$heat_params %||% character(0))
    vals <- vals[!is.na(vals) & nzchar(vals)]
    if (length(vals)) heat_force_empty(FALSE)
  }, ignoreInit = TRUE)

  
  ## accesos rÃƒÂ¡pidos (usan las reactiveVal que acabamos de crear)  
  datos_combinados <- reactive( datos_box() )  
  plot_settings    <- reactive( plot_cfg_box() )  
  
  # --- ParÃƒÂ¡m. seguro: siempre existe en el Excel cargado ----  
  safe_param <- reactive({
    req(input$param)
    if (isTRUE(input$doNorm) && has_ctrl_selected())
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
    cfg <- plot_settings()
    params <- as.character(cfg$Parameter %||% character(0))
    y_max <- suppressWarnings(as.numeric(cfg$Y_Max %||% NA_real_))
    interval <- suppressWarnings(as.numeric(cfg$Interval %||% NA_real_))

    for (i in seq_along(params)) {
      p_chr <- trimws(params[[i]])
      if (is.na(p_chr) || !nzchar(p_chr)) next

      if (is.null(ylims[[p_chr]])) {
        ylims[[p_chr]] <- list(
          ymax   = y_max[[i]],
          ybreak = interval[[i]]
        )
      }
      if (is.null(ylims[[paste0(p_chr, "_Norm")]])) {
        ylims[[paste0(p_chr, "_Norm")]] <- list(
          ymax   = 1,
          ybreak = 0.2
        )
      }
    }
  }, ignoreNULL = FALSE)  
  
  
  # Ã¢â€â‚¬Ã¢â€â‚¬ Reset general al cargar un nuevo archivo Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬  
  observeEvent(input$dataFile, {  
    req(plot_settings())                     # esperamos a tener la hoja  
    
    ## 1Ã‚Â· reiniciar parÃƒÂ¡metro seleccionado  
    update_selectize_adaptive(
      "param",
      choices = plot_settings()$Parameter,
      selected = if (length(plot_settings()$Parameter)) plot_settings()$Parameter[1] else character(0)
    )
    
    ## 2Ã‚Â· reiniciar strain / scope  
    updateRadioButtons(session, "scope", selected = "Por Cepa")  
    update_selectize_adaptive("strain", choices = NULL, selected = character(0))
    
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
      if (!is_valid_reactive_key(input$param)) return()
      # clave correcta:  Ã¢â‚¬Å“PARÃ¢â‚¬Â    o  Ã¢â‚¬Å“PAR_NormÃ¢â‚¬Â  
      tgt <- if (isTRUE(input$doNorm) && has_ctrl_selected())
        paste0(input$param, "_Norm") else input$param
      if (!is_valid_reactive_key(tgt)) return()
      
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
  
  output$stackParamsUI <- renderUI({
    input$app_lang
    req(plot_cfg_box())
    params <- unique(as.character(plot_cfg_box()$Parameter %||% character(0)))
    params <- params[!is.na(params) & nzchar(params)]

    if (is_server_side_param_set(params)) {
      selectizeInput(
        "stackParams",
        tr("stack_params"),
        choices = NULL,
        selected = NULL,
        multiple = TRUE,
        options = list(
          plugins = list("remove_button"),
          placeholder = tr_text("select_params_prompt", input$app_lang %||% i18n_lang)
        )
      )
    } else {
      checkboxGroupInput(
        "stackParams",
        tr("stack_params"),
        choices = params,
        selected = default_stack_params(params)
      )
    }
  })

  
  # dentro de server(), tras definir plot_settings()  
  output$paramSel <- renderUI({
    input$app_lang
    req(plot_cfg_box())                         # cfg ya cargada
    params <- plot_cfg_box()$Parameter
    if (length(params) == 0 || identical(params, "Parametro_dummy")) {
      helpText(tr("no_params_curves"))
    } else {
      selectizeInput("param", tr("param_label"),
                     choices = NULL, selected = NULL)
    }
  })
  
  
  # --- Procesamiento de datos dinÃƒÂ¡mico ---  
  datos_agrupados <- reactive({
    req(datos_combinados(), plot_settings())
    
    # 1) Parametros de configuraciÃƒÂ³n vs columnas reales
    params <- plot_settings()$Parameter
    df     <- apply_qc_tech_filter_raw(datos_combinados())
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
    
    # 3) Agrupar y resumir parametros + columnas SD_/N_ si existen.
    sd_present <- intersect(paste0("SD_", present), names(df))
    n_present  <- intersect(paste0("N_", present), names(df))
    summary_cols <- unique(c(present, sd_present, n_present))

    df %>%
      filter(!is.na(Strain), !is.na(Media), Strain != "C-") %>%
      group_by(Strain, Media, BiologicalReplicate) %>%
      summarise(
        across(all_of(summary_cols), ~ mean(.x, na.rm = TRUE)),
        Orden = dplyr::first(Orden),
        .groups = "drop"
      )
  })
  
  
  
  # --- Inputs dinÃƒÂ¡micos: Por Cepa ---  
  
  observeEvent(list(datos_agrupados(), input$app_lang, strain_label_ui(), media_label_ui()), {  
    
    
    # 1) poblar selector de cepas  
    update_selectize_adaptive(
      "strain",
      label = paste0(strain_label_ui() %||% default_label_text(input$app_lang %||% i18n_lang, "strain_label"), ":"),
      choices = sort(unique(datos_agrupados()$Strain))
    )  
    # 2) poblar filtro de medios  
    medias <- sort(unique(datos_agrupados()$Media))  
    output$showMediosUI <- renderUI({  
      input$app_lang
      media_label <- media_label_ui() %||% default_label_text(input$app_lang %||% i18n_lang, "media_label")
      checkboxGroupInput("showMedios", paste0(media_label, ":"),
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
    reps <- normalize_rep_selection(df$BiologicalReplicate)
    if (!length(reps)) return(NULL)
    reps_chr <- as.character(reps)
    accordion(
      id = "repsGlobalPanel",
      open = FALSE,
      multiple = TRUE,
      accordion_panel_safe(
        tr("exclude_reps_global"),
        checkboxGroupInput(
          "rm_reps_all",
          label = NULL,
          choices = reps_chr,
          selected = isolate(intersect(as.character(input$rm_reps_all %||% character(0)), reps_chr))
        ),
        value = "reps_global",
        style = "default"
      )
    )
  })

  output$filterMediaHeader <- renderUI({
    lang <- input$app_lang %||% i18n_lang
    h4(filter_media_label(lang))
  })

  output$repsByMediaTitle <- renderUI({
    lang <- input$app_lang %||% i18n_lang
    reps_by_media_label(lang)
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
    res <- withProgress(message = tr_text("progress_normalization", lang), value = 0, {
      incProgress(0.25)
      out <- tryCatch(  
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
      incProgress(1)
      out
    })
  
    norm_mode <- as.character(attr(res, "norm_mode") %||% "")
    should_warn_unavailable <- isTRUE(attr(res, "norm_fallback")) &&
      identical(norm_mode, "unavailable")
    if (isTRUE(should_warn_unavailable)) {
      ctrl_chr <- if (!is.null(ctrl) && length(ctrl) && !is.na(ctrl[[1]])) {
        trimws(as.character(ctrl[[1]]))
      } else {
        ""
      }
      msg <- if (nzchar(ctrl_chr)) {
        sprintf(tr_text("norm_unavailable_ctrl", lang), ctrl_chr)
      } else {
        tr_text("norm_unavailable_ctrl_noctrl", lang)
      }
      showNotification(msg, type = "warning", duration = 6)
    }
  
    res  
  })  
  
  
  # Ã¢â€â‚¬Ã¢â€â‚¬ Toggle Ã¢â‚¬Å“Por CepaÃ¢â‚¬Â Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬  
  observeEvent(input$toggleMedios, {  
    df <- datos_agrupados()
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0 || !"Media" %in% names(df)) return()
    if (is.null(input$showMedios)) return()
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
    if (is.null(input$showGroups)) return()
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
    map_sel <- isolate(reps_strain_selected())
    map_grp <- isolate(reps_group_selected())
    
    # construye un "sub-checkbox" por cada medio de esa cepa  
    tagList(
      actionButton(
        "repsStrainSelectAll",
        tr("reps_media_select_all"),
        class = "btn btn-outline-primary w-100",
        style = "white-space: normal; margin-bottom: 8px;"
      ),
      lapply(unique(df$Media), function(m){  
        reps <- normalize_rep_selection(df$BiologicalReplicate[df$Media == m])
        drop_all <- isolate(as.character(input$rm_reps_all %||% character(0)))
        input_id <- paste0("reps_", make.names(m))
        stored_sel <- get_synced_media_selection(
          strain_map = map_sel,
          group_map = map_grp,
          strain = input$strain,
          media = m
        )
        selected <- if (!is.null(stored_sel)) {
          as.character(stored_sel)
        } else {
          setdiff(reps, drop_all)
        }
        selected <- intersect(selected, reps)
        checkboxGroupInput(  
          input_id,                             # id = reps_<medio>  
          paste(tr("reps_prefix"), m),  
          choices  = reps,  
          selected = selected
        )  
      })
    )  
  })
  
  output$ctrlSelUI <- renderUI({  
    lang <- input$app_lang %||% i18n_lang
    req(input$doNorm)                          # sÃƒÂ³lo cuando se active el check  
    
    # Ã¢â‚¬â€˜Ã¢â‚¬â€˜Ã‚Â si la cepa aÃƒÂºn no estÃƒÂ¡ elegida, muestra TODOS los medios  
    strain_val <- if (is.null(input$strain) || !length(input$strain)) {
      ""
    } else {
      as.character(input$strain[[1]])
    }
    has_strain <- nzchar(strain_val)
    if (input$scope == "Por Cepa" && has_strain) {
      opts <- sort(unique(
        datos_agrupados()$Media[
          datos_agrupados()$Strain == strain_val]))
    } else {
      opts <- sort(unique(datos_agrupados()$Media))
    }
    opts <- trimws(as.character(opts))
    opts <- opts[!is.na(opts) & nzchar(opts)]
    prev_sel <- isolate(input$ctrlMedium)
    selected_ctrl <- if (!is.null(prev_sel) && length(prev_sel) &&
                           as.character(prev_sel[[1]]) %in% opts) {
      as.character(prev_sel[[1]])
    } else if (length(opts)) {
      opts[1]
    } else {
      character(0)
    }
    selectInput("ctrlMedium", norm_medium_label(lang),    # Ã¢â€ Â etiqueta genÃƒÂ©rica
                choices = opts,
                selected = selected_ctrl)
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
    map_sel <- isolate(reps_group_selected())
    map_str <- isolate(reps_strain_selected())
    
    accordion(
      id       = "repsGrpPanel",
      open     = FALSE,   # empieza cerrada
      multiple = TRUE,
      accordion_panel_safe(
        tr("reps_by_group"),
        actionButton(
          "repsGrpSelectAll",
          tr("reps_group_select_all"),
          class = "btn btn-outline-primary w-100",
          style = "white-space: normal; margin-bottom: 8px;"
        ),
        tagList(                       # envolver lapply
          lapply(grps, function(g){
            grp_idx <- paste(df$Strain, df$Media, sep = "-") == g
            reps <- normalize_rep_selection(
              as.character(df$BiologicalReplicate[grp_idx])
            )
            drop_all <- isolate(as.character(input$rm_reps_all %||% character(0)))
            input_id <- paste0("reps_grp_", make.names(g))
            strain_g <- if (any(grp_idx)) as.character(df$Strain[grp_idx][[1]]) else ""
            media_g <- if (any(grp_idx)) as.character(df$Media[grp_idx][[1]]) else ""
            stored_sel <- map_sel[[as.character(g)]]
            if (is.null(stored_sel)) {
              stored_sel <- get_strain_media_selection(map_str, strain_g, media_g)
            }
            selected <- if (!is.null(stored_sel)) {
              as.character(stored_sel)
            } else {
              setdiff(reps, drop_all)
            }
            selected <- intersect(selected, reps)
            checkboxGroupInput(
              input_id,
              paste(tr("reps_prefix"), g),
              choices  = reps,
              selected = selected
            )
          })
        ),
        style = "default"
      )
    )
  })

  rm_reps_all_debounced <- debounce(
    reactive(as.character(input$rm_reps_all %||% character(0))),
    150
  )

  observeEvent(rm_reps_all_debounced(), {
    drop_all <- rm_reps_all_debounced()
    use_param <- !is.null(input$tipo) && input$tipo %in% c("Boxplot", "Barras", "Violin")
    df <- if (use_param) {
      param_rep_df()
    } else {
      if (isTRUE(input$doNorm)) datos_agrupados_norm() else datos_agrupados()
    }
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return()

    scope_sel <- input$scope %||% "Por Cepa"
    if (scope_sel == "Por Cepa") {
      if (is.null(input$strain)) return()
      df <- df %>% filter(Strain == input$strain)
      if (!is.null(input$showMedios)) {
        df <- df %>% filter(Media %in% input$showMedios)
      }
      map_strain <- reps_strain_selected()
      map_group <- reps_group_selected()
      for (m in unique(df$Media)) {
        reps <- normalize_rep_selection(df$BiologicalReplicate[df$Media == m])
        selected <- setdiff(reps, drop_all)
        sync_maps <- set_synced_media_selection(
          strain_map = map_strain,
          group_map = map_group,
          strain = input$strain,
          media = m,
          selected = selected
        )
        map_strain <- sync_maps$reps_strain_map
        map_group <- sync_maps$reps_group_map
        updateCheckboxGroupInput(
          session,
          inputId  = paste0("reps_", make.names(m)),
          choices  = reps,
          selected = selected
        )
      }
      reps_strain_selected(map_strain)
      reps_group_selected(map_group)
    } else if (scope_sel == "Combinado" && !is.null(input$showGroups)) {
      grps <- input$showGroups
      df <- df %>% filter(paste(Strain, Media, sep = "-") %in% grps)
      map_group <- reps_group_selected()
      map_strain <- reps_strain_selected()
      for (g in grps) {
        grp_idx <- paste(df$Strain, df$Media, sep = "-") == g
        reps <- normalize_rep_selection(
          as.character(df$BiologicalReplicate[grp_idx])
        )
        selected <- setdiff(reps, drop_all)
        strain_g <- if (any(grp_idx)) as.character(df$Strain[grp_idx][[1]]) else ""
        media_g <- if (any(grp_idx)) as.character(df$Media[grp_idx][[1]]) else ""
        sync_maps <- set_synced_media_selection(
          strain_map = map_strain,
          group_map = map_group,
          strain = strain_g,
          media = media_g,
          selected = selected
        )
        map_strain <- sync_maps$reps_strain_map
        map_group <- sync_maps$reps_group_map
        updateCheckboxGroupInput(
          session,
          inputId  = paste0("reps_grp_", make.names(g)),
          choices  = reps,
          selected = selected
        )
      }
      reps_group_selected(map_group)
      reps_strain_selected(map_strain)
    }
  }, ignoreInit = TRUE)
  
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
      update_selectize_adaptive(
        "sig_param",
        choices = character(0),
        selected = character(0)
      )
      return()
    }
    if (is.null(current) || !current %in% params) current <- params[1]
    update_selectize_adaptive(
      "sig_param",
      choices = params,
      selected = current
    )
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
    list(
      input$corr_param_x, input$corr_param_y,
      input$doNorm, input$ctrlMedium, input$corr_norm_target,
      input$scope, input$strain, input$showGroups
    ),
    {
      req(plot_settings())
      raw_x <- trimws(as.character(input$corr_param_x %||% ""))
      raw_y <- trimws(as.character(input$corr_param_y %||% ""))
      if (!nzchar(raw_x) || !nzchar(raw_y)) return()
      updateTextInput(session, "corr_xlab", value = raw_x)
      updateTextInput(session, "corr_ylab", value = raw_y)

      norm_mode <- input$corr_norm_target %||% "both"
      use_norm_x <- isTRUE(input$doNorm) && has_ctrl_selected() &&
        norm_mode %in% c("both", "x_only")
      use_norm_y <- isTRUE(input$doNorm) && has_ctrl_selected() &&
        norm_mode %in% c("both", "y_only")

      col_x <- if (use_norm_x) paste0(raw_x, "_Norm") else raw_x
      col_y <- if (use_norm_y) paste0(raw_y, "_Norm") else raw_y

      scope_sel <- input$scope %||% "Por Cepa"
      strain_sel <- if (identical(scope_sel, "Por Cepa")) input$strain %||% NULL else NULL
      df <- tryCatch(
        get_scope_df(scope = scope_sel, strain = strain_sel),
        error = function(e) NULL
      )
      if (is.null(df) || !is.data.frame(df) || !nrow(df)) return()
      if (!all(c(col_x, col_y) %in% names(df))) return()

      df_points <- if (identical(scope_sel, "Por Cepa")) {
        df %>%
          dplyr::group_by(Media) %>%
          dplyr::summarise(
            X = mean(.data[[col_x]], na.rm = TRUE),
            Y = mean(.data[[col_y]], na.rm = TRUE),
            .groups = "drop"
          )
      } else {
        df %>%
          dplyr::group_by(Strain, Media) %>%
          dplyr::summarise(
            X = mean(.data[[col_x]], na.rm = TRUE),
            Y = mean(.data[[col_y]], na.rm = TRUE),
            .groups = "drop"
          )
      }
      df_points <- df_points %>%
        dplyr::mutate(
          X = suppressWarnings(as.numeric(X)),
          Y = suppressWarnings(as.numeric(Y))
        ) %>%
        dplyr::filter(is.finite(X), is.finite(Y))
      if (!is.data.frame(df_points) || !nrow(df_points)) return()

      cfg_x <- plot_settings() %>% filter(Parameter == raw_x)
      cfg_y <- plot_settings() %>% filter(Parameter == raw_y)

      axis_limits <- function(vals, fallback_max = NA_real_, fallback_break = NA_real_) {
        vals <- suppressWarnings(as.numeric(vals))
        vals <- vals[is.finite(vals)]
        if (!length(vals)) return(list(min = 0, max = 1, axis_break = 0.2))

        snap_down_1 <- function(x) floor(round(as.numeric(x), 8) * 10) / 10
        snap_up_1 <- function(x) ceiling(round(as.numeric(x), 8) * 10) / 10
        nice_step_1 <- function(span, preferred = NA_real_) {
          step <- suppressWarnings(as.numeric(preferred))
          if (!is.finite(step) || step <= 0) step <- span / 5
          if (!is.finite(step) || step <= 0) step <- 0.1

          p <- floor(log10(step))
          base <- step / (10 ^ p)
          mult <- c(1, 2, 5, 10)
          step <- mult[which.min(abs(mult - base))] * (10 ^ p)
          step <- round(step, 1)
          if (!is.finite(step) || step <= 0) step <- 0.1
          step
        }

        lo_raw <- suppressWarnings(min(vals, na.rm = TRUE))
        hi_raw <- suppressWarnings(max(vals, na.rm = TRUE))
        if (is.finite(fallback_max)) hi_raw <- max(hi_raw, as.numeric(fallback_max))

        if (!is.finite(lo_raw) || !is.finite(hi_raw)) return(list(min = 0, max = 1, axis_break = 0.2))

        pad <- if (hi_raw <= lo_raw) {
          if (hi_raw == 0) 1 else abs(hi_raw) * 0.1
        } else {
          (hi_raw - lo_raw) * 0.04
        }
        min_out <- if (lo_raw >= 0) 0 else snap_down_1(lo_raw - pad)
        max_out <- snap_up_1(hi_raw + pad)
        if (!is.finite(min_out)) min_out <- 0
        if (!is.finite(max_out) || max_out <= min_out) max_out <- min_out + 0.1

        br <- nice_step_1(max_out - min_out, preferred = fallback_break)
        list(min = min_out, max = max_out, axis_break = br)
      }

      xmax_cfg <- if (!use_norm_x && nrow(cfg_x)) cfg_x$Y_Max[1] else NA_real_
      xbreak_cfg <- if (!use_norm_x && nrow(cfg_x)) cfg_x$Interval[1] else NA_real_
      ymax_cfg <- if (!use_norm_y && nrow(cfg_y)) cfg_y$Y_Max[1] else NA_real_
      ybreak_cfg <- if (!use_norm_y && nrow(cfg_y)) cfg_y$Interval[1] else NA_real_

      x_lim <- axis_limits(df_points$X, fallback_max = xmax_cfg, fallback_break = xbreak_cfg)
      y_lim <- axis_limits(df_points$Y, fallback_max = ymax_cfg, fallback_break = ybreak_cfg)

      updateNumericInput(session, "xmin_corr", value = x_lim$min)
      updateNumericInput(session, "xmax_corr", value = x_lim$max, max = Inf)
      updateNumericInput(session, "xbreak_corr", value = x_lim$axis_break, min = 0.0001)
      updateNumericInput(session, "ymin_corr", value = y_lim$min)
      updateNumericInput(session, "ymax_corr", value = y_lim$max, max = Inf)
      updateNumericInput(session, "ybreak_corr", value = y_lim$axis_break, min = 0.0001)
    },
    ignoreInit = FALSE
  )
  
  
  
  ## Ã¢â€â‚¬Ã¢â€â‚¬ reajustar YÃ¢â‚¬â€˜axis cuando se (des)activa la normalizaciÃƒÂ³n Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬  
  observeEvent(
    list(input$doNorm, input$ctrlMedium, input$param),
    {
      req(plot_settings(), input$param)

      tgt <- if (isTRUE(input$doNorm) && has_ctrl_selected())
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
    if (is.null(df) || !nrow(df)) return(df)

    reps_chr <- as.character(df$BiologicalReplicate)
    drop_all <- as.character(input$rm_reps_all %||% character(0))
    if (length(drop_all)) {
      keep_global <- !(reps_chr %in% drop_all)
      if (any(!keep_global)) {
        df <- df[keep_global, , drop = FALSE]
        if (!nrow(df)) return(df)
        reps_chr <- reps_chr[keep_global]
      }
    }

    media_chr <- as.character(df$Media)
    media_chr <- trimws(media_chr)
    valid_media <- !is.na(media_chr) & nzchar(media_chr)
    medias <- unique(media_chr[valid_media])
    if (!length(medias)) return(df)
    full_map <- reps_strain_selected()
    group_map <- reps_group_selected()
    strain_sel <- current_strain_value()
    if (!nzchar(strain_sel) && "Strain" %in% names(df) && nrow(df)) {
      strain_sel <- as.character(df$Strain[[1]])
    }

    keep <- rep(TRUE, nrow(df))
    for (m in medias) {
      idx <- valid_media & media_chr == m
      if (!any(idx)) next
      sel <- get_synced_media_selection(
        strain_map = full_map,
        group_map = group_map,
        strain = strain_sel,
        media = m
      )
      if (is.null(sel)) {
        sel <- setdiff(normalize_rep_selection(reps_chr[idx]), drop_all)
      }
      sel_chr <- normalize_rep_selection(sel)
      keep[idx] <- reps_chr[idx] %in% sel_chr
    }
    df[keep, , drop = FALSE]
  }  
  
  filter_reps_group <- function(df){  
    grps <- input$showGroups  
    if (is.null(grps)) return(df[0, ])  
    if (is.null(df) || !nrow(df)) return(df[0, , drop = FALSE])

    grp_id <- paste(df$Strain, df$Media, sep = "-")
    keep_groups <- grp_id %in% grps
    df <- df[keep_groups, , drop = FALSE]
    if (!nrow(df)) return(df)

    reps_chr <- as.character(df$BiologicalReplicate)
    drop_all <- as.character(input$rm_reps_all %||% character(0))
    if (length(drop_all)) {
      keep_global <- !(reps_chr %in% drop_all)
      if (any(!keep_global)) {
        df <- df[keep_global, , drop = FALSE]
        if (!nrow(df)) return(df)
        reps_chr <- reps_chr[keep_global]
      }
    }

    grp_id <- paste(df$Strain, df$Media, sep = "-")
    sel_map <- setNames(vector("list", length(grps)), grps)
    has_override <- rep(FALSE, length(grps))
    map_group <- reps_group_selected()
    map_strain <- reps_strain_selected()
    for (i in seq_along(grps)) {
      sel <- map_group[[as.character(grps[[i]])]]
      if (is.null(sel)) {
        idx <- grp_id == as.character(grps[[i]])
        if (any(idx)) {
          strain_g <- as.character(df$Strain[idx][[1]])
          media_g <- as.character(df$Media[idx][[1]])
          sel <- get_strain_media_selection(map_strain, strain_g, media_g)
        }
      }
      if (!is.null(sel)) {
        sel_map[[i]] <- normalize_rep_selection(sel)
        has_override[[i]] <- TRUE
      }
    }
    if (!any(has_override)) return(df)

    keep <- rep(TRUE, nrow(df))
    for (g in grps[has_override]) {
      idx <- grp_id == g
      if (!any(idx)) next
      keep[idx] <- reps_chr[idx] %in% sel_map[[g]]
    }
    df[keep, , drop = FALSE]
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
    df <- if (isTRUE(input$doNorm)) datos_agrupados_norm() else datos_agrupados()
    if (!is.data.frame(df) || !nrow(df)) return(df)
    if ("Strain" %in% names(df)) df$Strain <- sanitize_curve_label(df$Strain)
    if ("Media" %in% names(df)) df$Media <- sanitize_curve_label(df$Media)
    df
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
    req(curve_data())
    meta_df <- curve_meta() %||% datos_combinados()
    req(meta_df)

    out <- curve_data() %>%
      mutate(across(-Time, ~ suppressWarnings(as.numeric(.x)))) %>%
      pivot_longer(cols = -Time, names_to = "Well", values_to = "Value") %>%
      mutate(
        Time = suppressWarnings(as.numeric(Time)),
        Well = as.character(Well)
      ) %>%
      left_join(meta_df %>% mutate(Well = as.character(Well)), by = "Well") %>%
      mutate(
        Strain = sanitize_curve_label(Strain),
        Media = sanitize_curve_label(Media)
      )

    cs <- curve_summary()
    if (!is.null(cs) && is.data.frame(cs) && nrow(cs)) {
      cs2 <- cs %>%
        transmute(
          Time = suppressWarnings(as.numeric(Time)),
          Well = as.character(Well),
          SD_Input = suppressWarnings(as.numeric(SD)),
          N_Input = suppressWarnings(as.numeric(N))
        )
      out <- out %>% left_join(cs2, by = c("Time", "Well"))
    } else {
      out <- out %>% mutate(SD_Input = NA_real_, N_Input = NA_real_)
    }
    out
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
      user_order <- trimws(strsplit(input$orderMedios, ",", fixed = TRUE)[[1]])
      user_order <- sanitize_curve_label(user_order)
      user_order <- user_order[!is.na(user_order) & nzchar(user_order)]
      user_order <- unique(user_order[user_order %in% final_levels])
      if (length(user_order)) {
        final_levels <- c(user_order, setdiff(final_levels, user_order))
      }
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
      mutate(
        Strain = sanitize_curve_label(Strain),
        Media = sanitize_curve_label(Media)
      ) %>%
      distinct(Strain, Media, Orden) %>%
      mutate(Label = paste(Strain, Media, sep = "-")) %>%
      arrange(Orden) %>%
      pull(Label) %>%
      intersect(available)
    # 4) Si el usuario escribiÃƒÂ³ un CSV en orderGroups, lo prioriza
    user_order <- NULL
    if (!is.null(input$orderGroups) && nzchar(input$orderGroups)) {
      user_order_raw <- trimws(strsplit(input$orderGroups, ",", fixed = TRUE)[[1]])
      user_order_raw <- sanitize_curve_label(user_order_raw)
      user_order_raw <- user_order_raw[!is.na(user_order_raw) & nzchar(user_order_raw)]
      user_order <- unique(user_order_raw[user_order_raw %in% available])
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
                  choices = named_choices(  
                    c("Tukey", "Bonferroni", "Sidak", "Dunnett", "Scheffe", "GamesHowell"),
                    c("Tukey", "Bonferroni", "Sidak", "Dunnett", "Scheffe", "Games-Howell")
                  ),
                  selected = "Tukey"  
      )  
    } else if (input$sigTest == "Kruskal-Wallis") {  
      selectInput("postHoc", tr("posthoc_label"),  
                  choices = named_choices(
                    c("Dunn", "Conover", "Nemenyi", "DSCF"),
                    c("Dunn (Bonf.)", "Conover", "Nemenyi", "DSCF")
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
    empty_df <- tibble::tibble(Label = character(), Valor = numeric(), BiologicalReplicate = character())
    if (isTRUE(is_summary_mode())) return(empty_df)
    p <- input$param
    if (is.null(p) || !length(p) || is.na(p[1]) || !nzchar(p[1])) return(empty_df)
    if (isTRUE(input$doNorm) && has_ctrl_selected()) p <- paste0(p, "_Norm")

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
                  Valor = .data[[p]],
                  BiologicalReplicate = as.character(BiologicalReplicate)) %>%
        filter(is.finite(Valor))
    } else {
      src %>%
        order_filter_group() %>%
        transmute(Label,
                  Valor = .data[[p]],
                  BiologicalReplicate = as.character(BiologicalReplicate)) %>%
        filter(is.finite(Valor))
    }
  }

  make_summary_test_df <- function() {
    empty_df <- tibble::tibble(Label = character(), Mean = numeric(), SD = numeric(), N = numeric())
    if (!isTRUE(is_summary_mode())) return(empty_df)
    p <- input$param
    if (is.null(p) || !length(p) || is.na(p[1]) || !nzchar(p[1])) return(empty_df)
    if (isTRUE(input$doNorm) && has_ctrl_selected()) p <- paste0(p, "_Norm")

    src <- if (isTRUE(input$doNorm)) datos_agrupados_norm() else datos_agrupados()
    if (!is.data.frame(src) || !nrow(src) || !p %in% names(src)) return(empty_df)
    if (input$scope == "Por Cepa" &&
        (is.null(input$strain) || is.na(input$strain) || !nzchar(input$strain))) {
      return(empty_df)
    }

    if (input$scope == "Por Cepa") {
      src <- src %>%
        filter(Strain == input$strain) %>%
        order_filter_strain() %>%
        filter_reps_strain() %>%
        mutate(Label = as.character(Media))
    } else {
      src <- src %>%
        order_filter_group() %>%
        mutate(Label = as.character(Label))
    }
    if (!nrow(src)) return(empty_df)

    sd_col <- resolve_prefixed_param_col(src, "SD_", p)
    n_col  <- resolve_prefixed_param_col(src, "N_", p)

    src %>%
      group_by(Label) %>%
      summarise(
        Mean = mean(.data[[p]], na.rm = TRUE),
        SD = if (!is.null(sd_col) && sd_col %in% names(src)) {
          mean(.data[[sd_col]], na.rm = TRUE)
        } else {
          sd(.data[[p]], na.rm = TRUE)
        },
        N = if (!is.null(n_col) && n_col %in% names(src)) {
          mean(.data[[n_col]], na.rm = TRUE)
        } else {
          dplyr::n_distinct(BiologicalReplicate)
        },
        .groups = "drop"
      ) %>%
      mutate(
        Mean = suppressWarnings(as.numeric(Mean)),
        SD = suppressWarnings(as.numeric(SD)),
        N = suppressWarnings(as.numeric(N))
      ) %>%
      filter(is.finite(Mean), is.finite(N), N > 1)
  }

  summary_welch_pair <- function(m1, s1, n1, m2, s2, n2) {
    m1 <- suppressWarnings(as.numeric(m1)); s1 <- suppressWarnings(as.numeric(s1)); n1 <- suppressWarnings(as.numeric(n1))
    m2 <- suppressWarnings(as.numeric(m2)); s2 <- suppressWarnings(as.numeric(s2)); n2 <- suppressWarnings(as.numeric(n2))
    if (!is.finite(m1) || !is.finite(m2) || !is.finite(n1) || !is.finite(n2) || n1 <= 1 || n2 <= 1) {
      return(list(estimate = NA_real_, p_value = NA_real_))
    }
    if (!is.finite(s1) || s1 < 0 || !is.finite(s2) || s2 < 0) {
      return(list(estimate = m1 - m2, p_value = NA_real_))
    }
    v1 <- (s1^2) / n1
    v2 <- (s2^2) / n2
    se2 <- v1 + v2
    if (!is.finite(se2) || se2 <= 0) {
      return(list(estimate = m1 - m2, p_value = NA_real_))
    }
    t_val <- (m1 - m2) / sqrt(se2)
    df_num <- se2^2
    df_den <- ((v1^2) / (n1 - 1)) + ((v2^2) / (n2 - 1))
    df <- if (is.finite(df_den) && df_den > 0) df_num / df_den else NA_real_
    p_val <- if (is.finite(df) && df > 0) {
      2 * stats::pt(-abs(t_val), df = df)
    } else {
      2 * stats::pnorm(-abs(t_val))
    }
    list(estimate = m1 - m2, p_value = p_val)
  }

  run_summary_significance <- function(df_sum, comp_mode = "all", control_group = NULL, group1 = NULL, group2 = NULL) {
    df_sum <- df_sum %>%
      mutate(Label = as.character(Label)) %>%
      filter(!is.na(Label), nzchar(Label), is.finite(Mean), is.finite(SD), is.finite(N), N > 1)
    if (nrow(df_sum) < 2 || dplyr::n_distinct(df_sum$Label) < 2) return(tibble::tibble())

    build_row <- function(a, b) {
      r1 <- df_sum %>% filter(Label == a) %>% slice(1)
      r2 <- df_sum %>% filter(Label == b) %>% slice(1)
      if (!nrow(r1) || !nrow(r2)) return(NULL)
      tst <- summary_welch_pair(r1$Mean, r1$SD, r1$N, r2$Mean, r2$SD, r2$N)
      tibble::tibble(
        group1 = as.character(a),
        group2 = as.character(b),
        estimate = suppressWarnings(as.numeric(tst$estimate)),
        p = suppressWarnings(as.numeric(tst$p_value))
      )
    }

    labels <- unique(as.character(df_sum$Label))
    rows <- list()
    if (identical(comp_mode, "control")) {
      ctrl <- as.character(control_group %||% "")
      if (!nzchar(ctrl) || !ctrl %in% labels) return(tibble::tibble())
      others <- setdiff(labels, ctrl)
      rows <- lapply(others, function(lbl) build_row(ctrl, lbl))
    } else if (identical(comp_mode, "pair")) {
      g1 <- as.character(group1 %||% "")
      g2 <- as.character(group2 %||% "")
      if (!nzchar(g1) || !nzchar(g2) || identical(g1, g2)) return(tibble::tibble())
      rows <- list(build_row(g1, g2))
    } else {
      pairs <- utils::combn(labels, 2, simplify = FALSE)
      rows <- lapply(pairs, function(pp) build_row(pp[[1]], pp[[2]]))
    }

    rows <- rows[!vapply(rows, is.null, logical(1))]
    if (!length(rows)) return(tibble::tibble())
    dplyr::bind_rows(rows) %>% filter(is.finite(p))
  }
  
  
  
  # Ã¢â€â‚¬Ã¢â€â‚¬ Reusar el mismo para Normalidad Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬  
  make_norm_df <- make_test_df
  safe_shapiro_test <- function(v) {
    vals <- suppressWarnings(as.numeric(v))
    vals <- vals[is.finite(vals)]
    if (length(vals) < 3 || length(vals) > 5000) {
      return(list(stat = NA_real_, p = NA_real_))
    }
    if (!is.finite(stats::sd(vals)) || stats::sd(vals) == 0) {
      return(list(stat = NA_real_, p = NA_real_))
    }
    out <- tryCatch(stats::shapiro.test(vals), error = function(e) NULL)
    if (is.null(out)) return(list(stat = NA_real_, p = NA_real_))
    list(stat = unname(as.numeric(out$statistic)), p = as.numeric(out$p.value))
  }
  safe_ks_test <- function(v) {
    vals <- suppressWarnings(as.numeric(v))
    vals <- vals[is.finite(vals)]
    if (length(vals) < 2) return(list(stat = NA_real_, p = NA_real_))
    sdv <- stats::sd(vals)
    if (!is.finite(sdv) || sdv == 0) return(list(stat = NA_real_, p = NA_real_))
    out <- tryCatch(
      stats::ks.test(vals, "pnorm", mean(vals), sdv),
      error = function(e) NULL
    )
    if (is.null(out)) return(list(stat = NA_real_, p = NA_real_))
    list(stat = unname(as.numeric(out$statistic)), p = as.numeric(out$p.value))
  }
  safe_ad_test <- function(v) {
    vals <- suppressWarnings(as.numeric(v))
    vals <- vals[is.finite(vals)]
    if (length(vals) < 8) return(list(stat = NA_real_, p = NA_real_))
    if (!is.finite(stats::sd(vals)) || stats::sd(vals) == 0) {
      return(list(stat = NA_real_, p = NA_real_))
    }
    out <- tryCatch(nortest::ad.test(vals), error = function(e) NULL)
    if (is.null(out)) return(list(stat = NA_real_, p = NA_real_))
    list(stat = unname(as.numeric(out$statistic)), p = as.numeric(out$p.value))
  }
  
  observe({  
    # aseguramos que ya hay configuraciÃƒÂ³n y datos  
    req(plot_settings(), nrow(datos_agrupados()) > 0)  
    
    df_test <- make_test_df()  
    if (nrow(df_test) == 0 && isTRUE(is_summary_mode())) {
      df_sum <- make_summary_test_df()
      if (nrow(df_sum) > 0) {
        grupos <- unique(as.character(df_sum$Label))
        updateSelectInput(session, "controlGroup",
                          choices = grupos,
                          selected = if ("Control" %in% grupos) "Control" else grupos[1])
        updateSelectInput(session, "group1",
                          choices = grupos,
                          selected = grupos[1])
        updateSelectInput(session, "group2",
                          choices = grupos,
                          selected = if (length(grupos) >= 2) grupos[2] else grupos[1])
      }
      return()
    }
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
    withProgress(message = tr_text("progress_normality", lang), value = 0, {
      if (isTRUE(is_summary_mode())) {
        df_sum <- make_summary_test_df()
        if (nrow(df_sum) == 0 || dplyr::n_distinct(df_sum$Label) < 2) {
          showNotification(tr_text("norm_min_groups", lang), type = "error", duration = 4)
          return(tibble::tibble(
            Label = character(), shapiro.stat = numeric(), shapiro.p = numeric(),
            ks.stat = numeric(), ks.p = numeric(), ad.stat = numeric(), ad.p = numeric()
          ))
        }
        showNotification(
          "Normality tests are not computable from summary input (Mean/SD/N only).",
          type = "message",
          duration = 6
        )
        return(df_sum %>%
                 transmute(
                   Label = as.character(Label),
                   shapiro.stat = NA_real_,
                   shapiro.p = NA_real_,
                   ks.stat = NA_real_,
                   ks.p = NA_real_,
                   ad.stat = NA_real_,
                   ad.p = NA_real_
                 ))
      }

      df <- make_norm_df()
      if (nrow(df) == 0 || dplyr::n_distinct(df$Label) < 2) {
        showNotification(tr_text("norm_min_groups", lang), type = "error", duration = 4)
        return(tibble::tibble(
          Label = character(), shapiro.stat = numeric(), shapiro.p = numeric(),
          ks.stat = numeric(), ks.p = numeric(), ad.stat = numeric(), ad.p = numeric()
        ))
      }
      labels <- unique(as.character(df$Label))
      n_labels <- max(length(labels), 1)
      out <- dplyr::bind_rows(lapply(seq_along(labels), function(i) {
        lbl <- labels[[i]]
        vals <- df$Valor[df$Label == lbl]
        sw <- safe_shapiro_test(vals)
        ks <- if ("ks" %in% input$normTests) safe_ks_test(vals) else list(stat = NA_real_, p = NA_real_)
        ad <- if ("ad" %in% input$normTests) safe_ad_test(vals) else list(stat = NA_real_, p = NA_real_)
        incProgress(1 / n_labels)
        tibble::tibble(
          Label = lbl,
          shapiro.stat = sw$stat,
          shapiro.p = sw$p,
          ks.stat = ks$stat,
          ks.p = ks$p,
          ad.stat = ad$stat,
          ad.p = ad$p
        )
      }))
      out
    })
  })
  
  
  # Ã¢â€â‚¬Ã¢â€â‚¬ Significancia Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬  
  sig_res <- eventReactive(input$runSig, {
    lang <- input$app_lang %||% i18n_lang
    adjust_method <- input$multitest_method %||% "none"
    if (!adjust_method %in% c("holm", "fdr", "bonferroni", "none")) adjust_method <- "none"
    if (isTRUE(is_summary_mode())) {
      df_sum <- make_summary_test_df()
      if (nrow(df_sum) < 2 || dplyr::n_distinct(df_sum$Label) < 2) {
        showNotification(tr_text("norm_groups_insufficient", lang), type = "error", duration = 5)
        return(tibble())
      }
      if (input$sigTest %in% c("Kruskal-Wallis", "wilcox")) {
        showNotification(
          "Kruskal-Wallis and Wilcoxon require raw observations and are not available for Mean/SD/N input.",
          type = "warning",
          duration = 6
        )
        return(tibble())
      }
      if (!identical(input$sigTest, "ttest")) {
        showNotification(
          "Summary input detected: using Welch-style comparisons from Mean/SD/N.",
          type = "message",
          duration = 5
        )
      }
      out <- run_summary_significance(
        df_sum,
        comp_mode = input$compMode %||% "all",
        control_group = input$controlGroup,
        group1 = input$group1,
        group2 = input$group2
      )
      if (!nrow(out)) {
        showNotification(tr_text("no_sig_results", lang), type = "error", duration = 5)
        return(tibble())
      }
      return(out)
    }
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
               "Tukey"      = rstatix::tukey_hsd(df, Valor ~ Label),  
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
                safe_pairwise_t(df, adjust_method)
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
                 sub <- df %>% filter(Label %in% grupos)
                 if (is.factor(sub$Label)) {
                   sub$Label <- droplevels(sub$Label)
                 } else {
                   sub$Label <- factor(as.character(sub$Label), levels = unique(as.character(sub$Label)))
                 }
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
                safe_pairwise_wilcox(df, adjust_method)
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
                 sub <- df %>% filter(Label %in% grupos)
                 if (is.factor(sub$Label)) {
                   sub$Label <- droplevels(sub$Label)
                 } else {
                   sub$Label <- factor(as.character(sub$Label), levels = unique(as.character(sub$Label)))
                 }
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
        Shapiro = dplyr::case_when(
          is.na(shapiro.p) ~ "N/A",
          shapiro.p > 0.05 ~ tr_text("yes_label", lang),
          TRUE ~ tr_text("no_label", lang)
        ),
        KS = dplyr::case_when(
          is.na(ks.p) ~ "N/A",
          ks.p > 0.05 ~ tr_text("yes_label", lang),
          TRUE ~ tr_text("no_label", lang)
        ),
        AD = dplyr::case_when(
          is.na(ad.p) ~ "N/A",
          ad.p > 0.05 ~ tr_text("yes_label", lang),
          TRUE ~ tr_text("no_label", lang)
        )
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
    df2 <- sig_table_processed()
    validate(need(nrow(df2) > 0, tr_text("no_valid_comparisons", lang)))
    tbl_view <- df2 %>% dplyr::select(-dplyr::any_of(c("is_significant", ".pair_key")))
    datatable(tbl_view, options = list(pageLength = 10, scrollX = TRUE))
  }, server = FALSE)  

  advanced_stats_res <- eventReactive(input$runAdvancedStats, {
    lang <- input$app_lang %||% i18n_lang
    df_raw <- make_test_df()
    df <- filter_min_obs(df_raw)
    if (dplyr::n_distinct(df$Label) < 2) {
      showNotification(tr_text("norm_groups_insufficient", lang), type = "error", duration = 5)
      return(list(pairwise = tibble::tibble(), mixed = tibble::tibble()))
    }

    n_iter <- suppressWarnings(as.integer(input$resample_iter %||% 2000))
    if (!is.finite(n_iter) || n_iter < 200) n_iter <- 200L
    resample_method <- input$resample_method %||% "bootstrap"
    adjust_method <- input$multitest_method %||% "none"
    if (!adjust_method %in% c("holm", "fdr", "bonferroni", "none")) adjust_method <- "none"

    withProgress(message = tr_text("progress_advanced_stats", lang), value = 0, {
      incProgress(0.2)
      eff <- pairwise_effect_sizes(
        df %>% dplyr::select(Label, Valor),
        n_boot = n_iter,
        adjust_method = adjust_method
      )
      incProgress(0.65)
      resamp <- pairwise_resampling_tests(
        df %>% dplyr::select(Label, Valor),
        method = resample_method,
        n_iter = n_iter,
        adjust_method = adjust_method
      ) %>%
        dplyr::rename(
          resample_mean_diff = mean_diff,
          resample_ci_low = ci_low,
          resample_ci_high = ci_high,
          p.resample = p.value,
          p.resample.adj = p.adj
        )

      pairwise_tbl <- dplyr::full_join(eff, resamp, by = c("group1", "group2"))
      mixed_tbl <- if (isTRUE(input$run_mixed_model)) {
        mixed_model_summary(df, replicate_col = "BiologicalReplicate")
      } else {
        tibble::tibble()
      }
      incProgress(1)
      list(
        pairwise = pairwise_tbl,
        mixed = mixed_tbl,
        method = resample_method,
        adjust = adjust_method
      )
    })
  })

  output$advStatsTable <- renderDT({
    req(input$runAdvancedStats)
    lang <- input$app_lang %||% i18n_lang
    tbl <- advanced_stats_res()$pairwise
    validate(need(nrow(tbl) > 0, tr_text("no_sig_results", lang)))
    keep_cols <- c(
      "group1", "group2", "n1", "n2",
      "mean_diff", "mean_diff_ci_low", "mean_diff_ci_high",
      "cohen_d", "cohen_d_ci_low", "cohen_d_ci_high",
      "cliffs_delta", "cliffs_delta_ci_low", "cliffs_delta_ci_high",
      "fold_change", "fold_change_ci_low", "fold_change_ci_high",
      "p.bootstrap", "p.bootstrap.adj",
      "p.resample", "p.resample.adj", "resample_method"
    )
    show_tbl <- tbl %>% dplyr::select(dplyr::any_of(keep_cols))
    num_cols <- names(show_tbl)[vapply(show_tbl, is.numeric, logical(1))]
    if (length(num_cols)) {
      show_tbl <- show_tbl %>%
        dplyr::mutate(dplyr::across(dplyr::all_of(num_cols), ~ round(.x, 4)))
    }
    pretty_names <- c(
      group1 = "Group 1",
      group2 = "Group 2",
      n1 = "N 1",
      n2 = "N 2",
      mean_diff = "Mean Diff",
      mean_diff_ci_low = "Mean Diff CI Low",
      mean_diff_ci_high = "Mean Diff CI High",
      cohen_d = "Cohen d",
      cohen_d_ci_low = "Cohen d CI Low",
      cohen_d_ci_high = "Cohen d CI High",
      cliffs_delta = "Cliffs Delta",
      cliffs_delta_ci_low = "Cliffs Delta CI Low",
      cliffs_delta_ci_high = "Cliffs Delta CI High",
      fold_change = "Fold Change",
      fold_change_ci_low = "Fold Change CI Low",
      fold_change_ci_high = "Fold Change CI High",
      "p.bootstrap" = "P Bootstrap",
      "p.bootstrap.adj" = "P Bootstrap Adj",
      "p.resample" = "P Resample",
      "p.resample.adj" = "P Resample Adj",
      resample_method = "Resample Method"
    )
    for (nm in names(pretty_names)) {
      if (nm %in% names(show_tbl)) {
        names(show_tbl)[names(show_tbl) == nm] <- pretty_names[[nm]]
      }
    }
    datatable(show_tbl, options = list(pageLength = 10, scrollX = TRUE))
  }, server = FALSE)

  output$mixedModelTable <- renderDT({
    req(input$runAdvancedStats)
    lang <- input$app_lang %||% i18n_lang
    tbl <- advanced_stats_res()$mixed
    validate(need(nrow(tbl) > 0, tr_text("mixed_model_not_available", lang)))
    num_cols <- names(tbl)[vapply(tbl, is.numeric, logical(1))]
    if (length(num_cols)) {
      tbl <- tbl %>% dplyr::mutate(dplyr::across(dplyr::all_of(num_cols), ~ round(.x, 4)))
    }
    datatable(tbl, options = list(pageLength = 5, scrollX = TRUE))
  }, server = FALSE)

  curve_stats_df <- reactive({
    req(curve_data(), curve_settings())
    df_cur <- curve_long_df()
    use_summary_curve <- isTRUE(curve_summary_mode())

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

    if (input$scope == "Por Cepa") {
      req(input$strain)
      df_cur <- df_cur %>%
        filter(Strain == input$strain) %>%
        order_filter_strain() %>%
        filter_reps_strain()
      out <- summarise_curve(df_cur, "Media") %>%
        mutate(Label = as.character(Media))
    } else {
      df_cur <- df_cur %>%
        order_filter_group() %>%
        filter_reps_group()
      if (isTRUE(input$labelMode)) {
        out <- summarise_curve(df_cur, "Strain") %>%
          mutate(Label = as.character(Strain))
      } else {
        out <- summarise_curve(df_cur, c("Strain", "Media")) %>%
          mutate(Label = paste(Strain, Media, sep = "-"))
      }
    }

    out %>%
      filter(is.finite(Time), is.finite(Avg), !is.na(Label)) %>%
      mutate(
        Label = sanitize_curve_label(Label),
        SD = ifelse(is.finite(SD), SD, 0),
        N = ifelse(is.finite(N) & N > 0, N, 1)
      )
  })

  curve_signif_stars <- function(p) {
    p <- suppressWarnings(as.numeric(p))
    dplyr::case_when(
      !is.finite(p) ~ "",
      p < 0.001 ~ "***",
      p < 0.01 ~ "**",
      p < 0.05 ~ "*",
      TRUE ~ "ns"
    )
  }

  curve_stats_res <- eventReactive(input$runCurveStats, {
    methods <- input$curve_stats_methods %||% character(0)
    df <- curve_stats_df()
    out <- list()

    if (!nrow(df) || dplyr::n_distinct(df$Label) < 2) {
      return(tibble::tibble(
        Method = character(),
        Comparison = character(),
        Estimate = numeric(),
        P_value = numeric(),
        P_adjusted = numeric(),
        Stars = character()
      ))
    }

    df <- df %>%
      mutate(
        Label = as.character(Label),
        Time = suppressWarnings(as.numeric(Time)),
        Avg = suppressWarnings(as.numeric(Avg)),
        SD = suppressWarnings(as.numeric(SD)),
        N = suppressWarnings(as.numeric(N))
      ) %>%
      filter(is.finite(Time), is.finite(Avg), !is.na(Label), nzchar(Label))

    make_pairwise_rows <- function(method_code, prefix, pair_fun) {
      labels <- unique(as.character(df$Label))
      if (length(labels) < 2) return(tibble::tibble())
      pairs <- utils::combn(labels, 2, simplify = FALSE)
      rows <- lapply(pairs, function(pp) {
        d1 <- df %>% filter(Label == pp[1]) %>% arrange(Time)
        d2 <- df %>% filter(Label == pp[2]) %>% arrange(Time)
        res <- tryCatch(pair_fun(d1, d2), error = function(e) NULL)
        if (is.null(res) || !is.list(res)) return(NULL)
        est <- suppressWarnings(as.numeric(res$estimate %||% NA_real_))
        pv <- suppressWarnings(as.numeric(res$p_value %||% NA_real_))
        comp_suffix <- as.character(res$comparison_suffix %||% "")
        comp <- paste0(prefix, ": ", pp[1], " vs ", pp[2], comp_suffix)
        tibble::tibble(
          Method = method_code,
          Comparison = comp,
          Estimate = est,
          P_value = pv
        )
      })
      rows <- rows[!vapply(rows, is.null, logical(1))]
      if (!length(rows)) return(tibble::tibble())
      dplyr::bind_rows(rows)
    }

    if ("S1" %in% methods && dplyr::n_distinct(df$Time) >= 4) {
      df_s1 <- df %>%
        mutate(
          Label = as.factor(Label),
          w = ifelse(is.finite(SD) & SD > 0, 1 / (SD^2), 1)
        )
      fit_null <- tryCatch(
        stats::lm(Avg ~ Label + splines::ns(Time, df = 4), data = df_s1, weights = w),
        error = function(e) NULL
      )
      fit_alt <- tryCatch(
        stats::lm(Avg ~ Label * splines::ns(Time, df = 4), data = df_s1, weights = w),
        error = function(e) NULL
      )
      p_s1 <- NA_real_
      if (!is.null(fit_null) && !is.null(fit_alt)) {
        a <- tryCatch(stats::anova(fit_null, fit_alt), error = function(e) NULL)
        if (!is.null(a) && nrow(a) >= 2 && "Pr(>F)" %in% names(a)) {
          p_s1 <- suppressWarnings(as.numeric(a[2, "Pr(>F)"]))
        }
      }
      out[[length(out) + 1]] <- tibble::tibble(
        Method = "S1",
        Comparison = "Global curve shape difference",
        Estimate = NA_real_,
        P_value = p_s1
      )
    }

    if ("S2" %in% methods) {
      s2_tbl <- make_pairwise_rows("S2", "Pointwise Fisher", function(d1, d2) {
        curve_pointwise_fisher(d1, d2)
      })
      if (nrow(s2_tbl)) out[[length(out) + 1]] <- s2_tbl
    }

    if ("S3" %in% methods) {
      s3_tbl <- make_pairwise_rows("S3", "Endpoint difference", function(d1, d2) {
        t_end <- suppressWarnings(max(intersect(d1$Time, d2$Time), na.rm = TRUE))
        if (!is.finite(t_end)) {
          return(list(estimate = NA_real_, p_value = NA_real_))
        }
        r1 <- d1 %>% filter(Time == t_end) %>% slice(1)
        r2 <- d2 %>% filter(Time == t_end) %>% slice(1)
        if (!nrow(r1) || !nrow(r2)) {
          return(list(estimate = NA_real_, p_value = NA_real_))
        }
        diff_end <- as.numeric(r1$Avg - r2$Avg)
        se_end <- sqrt(
          pmax(as.numeric(r1$SD)^2 / pmax(as.numeric(r1$N), 1), 0) +
            pmax(as.numeric(r2$SD)^2 / pmax(as.numeric(r2$N), 1), 0)
        )
        z_val <- if (is.finite(se_end) && se_end > 0) diff_end / se_end else NA_real_
        p_val <- if (is.finite(z_val)) 2 * stats::pnorm(-abs(z_val)) else NA_real_
        list(
          estimate = diff_end,
          p_value = p_val,
          comparison_suffix = paste0(" (t=", format(t_end, trim = TRUE, scientific = FALSE), ")")
        )
      })
      if (nrow(s3_tbl)) out[[length(out) + 1]] <- s3_tbl
    }

    if ("S4" %in% methods) {
      raw_auc <- curve_long_df()
      if (input$scope == "Por Cepa") {
        req(input$strain)
        raw_auc <- raw_auc %>%
          filter(Strain == input$strain) %>%
          order_filter_strain() %>%
          filter_reps_strain() %>%
          mutate(Label = as.character(Media))
      } else {
        raw_auc <- raw_auc %>%
          order_filter_group() %>%
          filter_reps_group()
        if (isTRUE(input$labelMode)) {
          raw_auc <- raw_auc %>% mutate(Label = as.character(Strain))
        } else {
          raw_auc <- raw_auc %>% mutate(Label = paste(Strain, Media, sep = "-"))
        }
      }

      raw_auc <- raw_auc %>%
        mutate(
          Time = suppressWarnings(as.numeric(Time)),
          Value = suppressWarnings(as.numeric(Value)),
          Label = sanitize_curve_label(as.character(Label)),
          BiologicalReplicate = as.character(BiologicalReplicate)
        ) %>%
        filter(
          is.finite(Time), is.finite(Value),
          !is.na(Label), nzchar(Label),
          !is.na(BiologicalReplicate), nzchar(BiologicalReplicate)
        )

      auc_rep <- raw_auc %>%
        group_by(Label, BiologicalReplicate) %>%
        group_modify(~ {
          d <- .x %>%
            group_by(Time) %>%
            summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
            arrange(Time)
          if (nrow(d) < 2 || dplyr::n_distinct(d$Time) < 2) {
            return(tibble::tibble(AUC = NA_real_))
          }
          auc_val <- suppressWarnings(gcplyr::auc(x = d$Time, y = d$Value))
          tibble::tibble(AUC = as.numeric(auc_val))
        }) %>%
        ungroup() %>%
        filter(is.finite(AUC))

      has_rep_auc <- nrow(auc_rep) > 0 &&
        dplyr::n_distinct(auc_rep$Label) >= 2 &&
        dplyr::n_distinct(auc_rep$BiologicalReplicate) >= 2

      if (isTRUE(has_rep_auc)) {
        auc_rows <- auc_rep %>%
          group_by(Label) %>%
          summarise(AUC = mean(AUC, na.rm = TRUE), N_Rep = dplyr::n(), .groups = "drop") %>%
          transmute(
            Method = "S4",
            Comparison = paste0("Replicate-level AUC mean: ", Label, " (n=", N_Rep, ")"),
            Estimate = AUC,
            P_value = NA_real_
          )
        if (nrow(auc_rows)) out[[length(out) + 1]] <- auc_rows

        means_auc <- auc_rep %>%
          group_by(Label) %>%
          summarise(mean_auc = mean(AUC, na.rm = TRUE), .groups = "drop")

        norm_auc <- auc_rep %>%
          group_by(Label) %>%
          summarise(
            Shapiro_p = safe_shapiro_test(AUC)$p,
            Is_Normal = is.finite(Shapiro_p) & Shapiro_p > 0.05,
            .groups = "drop"
          )

        n_groups_auc <- nrow(means_auc)
        all_groups_normal <- n_groups_auc >= 2 && all(norm_auc$Is_Normal)

        global_method <- if (n_groups_auc == 2) {
          if (all_groups_normal) "Welch t-test" else "Wilcoxon rank-sum"
        } else {
          if (all_groups_normal) "ANOVA" else "Kruskal-Wallis"
        }

        global_p <- NA_real_
        if (n_groups_auc == 2) {
          two_dat <- auc_rep %>% filter(Label %in% means_auc$Label)
          if (all_groups_normal) {
            tt <- tryCatch(stats::t.test(AUC ~ Label, data = two_dat, var.equal = FALSE), error = function(e) NULL)
            if (!is.null(tt)) global_p <- suppressWarnings(as.numeric(tt$p.value))
          } else {
            wt <- tryCatch(stats::wilcox.test(AUC ~ Label, data = two_dat, exact = FALSE), error = function(e) NULL)
            if (!is.null(wt)) global_p <- suppressWarnings(as.numeric(wt$p.value))
          }
        } else if (all_groups_normal) {
          fit_auc <- tryCatch(stats::aov(AUC ~ Label, data = auc_rep), error = function(e) NULL)
          if (!is.null(fit_auc)) {
            an <- tryCatch(summary(fit_auc), error = function(e) NULL)
            if (!is.null(an) && length(an) >= 1) {
              tab <- suppressWarnings(as.data.frame(an[[1]]))
              if (nrow(tab) >= 1 && "Pr(>F)" %in% names(tab)) {
                global_p <- suppressWarnings(as.numeric(tab[1, "Pr(>F)"]))
              }
            }
          }
        } else {
          kw <- tryCatch(stats::kruskal.test(AUC ~ Label, data = auc_rep), error = function(e) NULL)
          if (!is.null(kw)) global_p <- suppressWarnings(as.numeric(kw$p.value))
        }

        out[[length(out) + 1]] <- tibble::tibble(
          Method = "S4",
          Comparison = if (is.finite(global_p)) {
            paste0(
              "Replicate-level AUC (",
              global_method,
              ", selected by Shapiro normality, raw p=",
              formatC(global_p, format = "e", digits = 2),
              ")"
            )
          } else {
            paste0("Replicate-level AUC (", global_method, ", selected by Shapiro normality)")
          },
          Estimate = NA_real_,
          P_value = NA_real_
        )

        pair_tbl <- {
          pairs <- utils::combn(as.character(means_auc$Label), 2, simplify = FALSE)
          rows <- lapply(pairs, function(pp) {
            d_pair <- auc_rep %>%
              filter(Label %in% pp) %>%
              mutate(Label = factor(Label, levels = pp))

            m1 <- means_auc %>% filter(Label == pp[1]) %>% pull(mean_auc)
            m2 <- means_auc %>% filter(Label == pp[2]) %>% pull(mean_auc)
            est <- if (length(m1) && length(m2)) as.numeric(m1[[1]] - m2[[1]]) else NA_real_

            pair_method <- if (all_groups_normal) "Welch t-test" else "Wilcoxon rank-sum"
            p_pair <- NA_real_

            if (all_groups_normal) {
              tt <- tryCatch(stats::t.test(AUC ~ Label, data = d_pair, var.equal = FALSE), error = function(e) NULL)
              if (!is.null(tt)) p_pair <- suppressWarnings(as.numeric(tt$p.value))
            } else {
              wt <- tryCatch(stats::wilcox.test(AUC ~ Label, data = d_pair, exact = FALSE), error = function(e) NULL)
              if (!is.null(wt)) p_pair <- suppressWarnings(as.numeric(wt$p.value))
            }

            if (!is.finite(p_pair)) {
              wt_fb <- tryCatch(stats::wilcox.test(AUC ~ Label, data = d_pair, exact = FALSE), error = function(e) NULL)
              if (!is.null(wt_fb)) {
                p_pair <- suppressWarnings(as.numeric(wt_fb$p.value))
                pair_method <- "Wilcoxon rank-sum (fallback)"
              }
            }

            tibble::tibble(
              Method = "S4",
              Comparison = paste0("Replicate-level AUC ", pair_method, ": ", pp[1], " vs ", pp[2]),
              Estimate = est,
              P_value = p_pair
            )
          })
          if (length(rows)) dplyr::bind_rows(rows) else tibble::tibble()
        }
        if (nrow(pair_tbl)) out[[length(out) + 1]] <- pair_tbl
      } else {
        auc_stats <- df %>%
          group_by(Label) %>%
          group_modify(~ {
            d <- .x %>% arrange(Time)
            if (nrow(d) < 2 || length(unique(d$Time)) < 2) {
              return(tibble::tibble(AUC = NA_real_, VarAUC = NA_real_))
            }
            auc_val <- suppressWarnings(gcplyr::auc(x = d$Time, y = d$Avg))
            var_y <- (d$SD^2) / pmax(d$N, 1)
            dt <- diff(d$Time)
            var_auc <- if (length(dt) >= 1) {
              sum(((dt / 2)^2) * (var_y[-length(var_y)] + var_y[-1]), na.rm = TRUE)
            } else {
              NA_real_
            }
            tibble::tibble(AUC = as.numeric(auc_val), VarAUC = as.numeric(var_auc))
          }) %>%
          ungroup()

        auc_rows <- auc_stats %>%
          transmute(
            Method = "S4",
            Comparison = paste0("Mean-curve AUC: ", Label),
            Estimate = AUC,
            P_value = NA_real_
          )
        if (nrow(auc_rows)) out[[length(out) + 1]] <- auc_rows

        labs <- as.character(auc_stats$Label)
        pair_tbl <- if (length(labs) >= 2) {
          pairs <- utils::combn(labs, 2, simplify = FALSE)
          rows <- lapply(pairs, function(pp) {
            a <- auc_stats %>% filter(Label == pp[1]) %>% slice(1)
            b <- auc_stats %>% filter(Label == pp[2]) %>% slice(1)
            diff_auc <- as.numeric(a$AUC - b$AUC)
            se_diff <- sqrt(pmax(as.numeric(a$VarAUC), 0) + pmax(as.numeric(b$VarAUC), 0))
            z_val <- if (is.finite(se_diff) && se_diff > 0) diff_auc / se_diff else NA_real_
            p_val <- if (is.finite(z_val)) 2 * stats::pnorm(-abs(z_val)) else NA_real_
            tibble::tibble(
              Method = "S4",
              Comparison = paste0("Mean-curve AUC z-approx: ", pp[1], " vs ", pp[2]),
              Estimate = diff_auc,
              P_value = p_val
            )
          })
          dplyr::bind_rows(rows)
        } else {
          tibble::tibble()
        }
        if (nrow(pair_tbl)) out[[length(out) + 1]] <- pair_tbl
      }
    }

    if (!length(out)) {
      return(tibble::tibble(
        Method = character(),
        Comparison = character(),
        Estimate = numeric(),
        P_value = numeric(),
        P_adjusted = numeric(),
        Stars = character()
      ))
    }

    tbl <- dplyr::bind_rows(out) %>%
      mutate(
        P_adjusted = NA_real_,
        Stars = ""
      )

    for (m in unique(tbl$Method)) {
      idx <- which(tbl$Method == m)
      has_p <- which(tbl$Method == m & is.finite(tbl$P_value))
      if (!length(has_p)) next
      p_adj <- stats::p.adjust(tbl$P_value[has_p], method = "holm")
      tbl$P_adjusted[has_p] <- p_adj
      tbl$Stars[has_p] <- curve_signif_stars(p_adj)
    }

    tbl %>%
      mutate(
        Comparison = as.character(Comparison),
        Stars = ifelse(!is.finite(P_adjusted) & !is.finite(P_value), "", Stars)
      )
  })

  output$curveStatsTable <- renderDT({
    req(input$runCurveStats)
    lang <- input$app_lang %||% i18n_lang
    tbl <- curve_stats_res()
    validate(need(nrow(tbl) > 0, tr_text("no_data_selection", lang)))
    format_p <- function(p) {
      p <- suppressWarnings(as.numeric(p))
      out <- rep(NA_character_, length(p))
      ok <- is.finite(p)
      if (any(ok)) {
        out[ok] <- ifelse(p[ok] < 1e-4, "<1e-4", formatC(p[ok], format = "f", digits = 4))
      }
      out
    }
    method_map <- c(
      S1 = tr_text("curves_stats_s1", lang),
      S2 = tr_text("curves_stats_s2", lang),
      S3 = tr_text("curves_stats_s3", lang),
      S4 = tr_text("curves_stats_s4", lang)
    )
    tbl <- tbl %>%
      mutate(
        Method = {
          code <- as.character(Method)
          mapped <- unname(method_map[code])
          ifelse(is.na(mapped), code, mapped)
        },
        Estimate = ifelse(is.finite(Estimate), round(Estimate, 5), NA_real_),
        P_value = format_p(P_value),
        P_adjusted = format_p(P_adjusted)
      )
    datatable(tbl, options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
  }, server = FALSE)

  corr_adv_table_data <- reactiveVal(tibble::tibble())
  corr_adv_has_run <- reactiveVal(FALSE)
  corr_adv_running <- reactiveVal(FALSE)
  corr_adv_last_pair <- reactiveVal(NULL)
  corr_adv_results <- reactiveVal(tibble::tibble())

  set_corr_adv_button_state <- function(running = FALSE) {
    lang <- isolate(input$app_lang %||% i18n_lang)
    if (isTRUE(running)) {
      shinyjs::disable("corr_adv_run")
      updateActionButton(session, "corr_adv_run", label = tr_text("corr_adv_running", lang))
    } else {
      shinyjs::enable("corr_adv_run")
      updateActionButton(session, "corr_adv_run", label = tr_text("corr_adv_run", lang))
    }
    invisible(NULL)
  }

  build_corr_adv_snapshot <- function() {
    req(plot_settings())
    scope_sel <- input$scope %||% "Por Cepa"
    strain_sel <- if (identical(scope_sel, "Por Cepa")) input$strain %||% NULL else NULL
    if (identical(scope_sel, "Por Cepa")) {
      strain_chr <- as.character(strain_sel %||% "")
      if (!length(strain_chr) || is.na(strain_chr[[1]]) || !nzchar(strain_chr[[1]])) {
        return(NULL)
      }
    }

    df_scope <- get_scope_df(scope = scope_sel, strain = strain_sel)
    if (is.null(df_scope) || !is.data.frame(df_scope) || !nrow(df_scope)) {
      return(NULL)
    }

    params <- unique(as.character(plot_settings()$Parameter %||% character(0)))
    params <- params[!is.na(params) & nzchar(params)]

    method <- input$corr_adv_method %||% "pearson"
    if (!method %in% c("pearson", "spearman", "kendall")) method <- "pearson"

    mode <- input$corr_adv_data_mode %||% "raw"
    if (!mode %in% c("raw", "norm_both", "norm_x", "norm_y")) mode <- "raw"

    strict_norm_mode <- isTRUE(input$doNorm) && has_ctrl_selected() &&
      mode %in% c("norm_both", "norm_x", "norm_y")
    if (isTRUE(strict_norm_mode)) {
      params <- normalized_ready_params(df_scope, params)
    }
    if (length(params) < 2) return(NULL)

    anchor <- as.character(input$corr_adv_anchor %||% input$corr_param_x %||% "")
    if (!nzchar(anchor) || !anchor %in% params) anchor <- params[[1]]

    list(
      scope = scope_sel,
      df_scope = df_scope,
      params = params,
      anchor = anchor,
      method = method,
      mode = mode
    )
  }

  run_corr_adv_scan <- function(snapshot) {
    if (is.null(snapshot) || !is.list(snapshot)) return(tibble::tibble())
    df_scope <- snapshot$df_scope
    params <- snapshot$params
    anchor <- snapshot$anchor
    method <- snapshot$method
    mode <- snapshot$mode
    scope_sel <- snapshot$scope

    resolve_col <- function(param, is_anchor = FALSE) {
      if (identical(mode, "raw")) return(param)
      if (identical(mode, "norm_both")) return(paste0(param, "_Norm"))
      if (identical(mode, "norm_x")) return(if (is_anchor) paste0(param, "_Norm") else param)
      if (identical(mode, "norm_y")) return(if (is_anchor) param else paste0(param, "_Norm"))
      param
    }

    has_finite_col <- function(df, col_name) {
      if (is.null(col_name) || !nzchar(col_name) || !col_name %in% names(df)) return(FALSE)
      vals <- suppressWarnings(as.numeric(df[[col_name]]))
      any(is.finite(vals))
    }

    if (!identical(mode, "raw")) {
      norm_ready <- params[vapply(
        params,
        function(p) has_finite_col(df_scope, paste0(p, "_Norm")),
        logical(1)
      )]
      params <- norm_ready
      if (!length(params)) return(tibble::tibble())
      if (!anchor %in% params) anchor <- params[[1]]
    }

    col_map <- stats::setNames(rep("", length(params)), params)
    for (p in params) {
      col_map[[p]] <- resolve_col(p, is_anchor = identical(p, anchor))
    }

    params_ready <- params[col_map[params] %in% names(df_scope)]
    params_ready <- params_ready[vapply(
      params_ready,
      function(p) has_finite_col(df_scope, col_map[[p]]),
      logical(1)
    )]
    if (!anchor %in% params_ready || length(params_ready) < 2) {
      return(tibble::tibble())
    }

    group_cols <- if (identical(scope_sel, "Por Cepa")) c("Media") else c("Strain", "Media")
    stat_cols <- unique(unname(col_map[params_ready]))
    safe_mean_num <- function(v) {
      num <- suppressWarnings(as.numeric(v))
      num <- num[is.finite(num)]
      if (!length(num)) return(NA_real_)
      mean(num, na.rm = TRUE)
    }
    df_units <- df_scope %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
      dplyr::summarise(
        dplyr::across(dplyr::all_of(stat_cols), safe_mean_num),
        .groups = "drop"
      )

    corr_df <- as.data.frame(
      stats::setNames(
        lapply(params_ready, function(p) df_units[[col_map[[p]]]]),
        params_ready
      ),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    params_ready <- params_ready[vapply(
      params_ready,
      function(p) has_finite_col(corr_df, p),
      logical(1)
    )]
    if (!anchor %in% params_ready || length(params_ready) < 2) {
      return(tibble::tibble())
    }

    anchor_vec <- suppressWarnings(as.numeric(corr_df[[anchor]]))
    others <- setdiff(params_ready, anchor)
    if (!length(others)) return(tibble::tibble())
    pair_ready <- others[vapply(others, function(other) {
      other_vec <- suppressWarnings(as.numeric(corr_df[[other]]))
      ok <- is.finite(anchor_vec) & is.finite(other_vec)
      if (sum(ok) < 3) return(FALSE)
      if (dplyr::n_distinct(anchor_vec[ok]) < 2) return(FALSE)
      if (dplyr::n_distinct(other_vec[ok]) < 2) return(FALSE)
      TRUE
    }, logical(1))]
    params_ready <- c(anchor, pair_ready)
    if (length(params_ready) < 2) return(tibble::tibble())

    out <- correlation_one_vs_all_with_p(
      df = corr_df,
      anchor = anchor,
      params = params_ready,
      method = method,
      adjust_method = "none",
      min_points = 3L
    )
    if (!nrow(out)) return(tibble::tibble())

    out %>%
      dplyr::mutate(
        method = method,
        data_mode = mode,
        x_col = unname(col_map[param_anchor]),
        y_col = unname(col_map[param_other]),
        x_is_norm = grepl("_Norm$", x_col),
        y_is_norm = grepl("_Norm$", y_col),
        significant = is.finite(p.value) & p.value < 0.05,
        direction = dplyr::case_when(
          !is.finite(r) ~ "zero",
          r > 0 ~ "positive",
          r < 0 ~ "negative",
          TRUE ~ "zero"
        )
      ) %>%
      dplyr::arrange(
        dplyr::desc(significant),
        dplyr::if_else(is.finite(p.value), p.value, Inf),
        dplyr::desc(abs(r))
      )
  }

  finalize_corr_adv <- function(result = NULL, err = NULL) {
    isolate(corr_adv_running(FALSE))
    set_corr_adv_button_state(FALSE)

    if (!is.null(err) && nzchar(as.character(err))) {
      lang <- isolate(input$app_lang %||% i18n_lang)
      msg <- sprintf(
        tr_text("global_error_template", lang),
        tr_text("corr_adv_title", lang),
        as.character(err)
      )
      showNotification(msg, type = "error", duration = 8)
      isolate(corr_adv_results(tibble::tibble()))
      return(invisible(NULL))
    }

    if (!is.data.frame(result)) {
      isolate(corr_adv_results(tibble::tibble()))
    } else {
      isolate(corr_adv_results(result))
    }
    invisible(NULL)
  }

  observeEvent(input$corr_adv_run, {
    if (isTRUE(corr_adv_running())) return()
    corr_adv_has_run(TRUE)
    corr_adv_table_data(tibble::tibble())
    corr_adv_last_pair(NULL)

    snapshot <- build_corr_adv_snapshot()
    if (is.null(snapshot)) {
      corr_adv_results(tibble::tibble())
      set_corr_adv_button_state(FALSE)
      corr_adv_running(FALSE)
      return()
    }

    corr_adv_running(TRUE)
    set_corr_adv_button_state(TRUE)

    value <- tryCatch(run_corr_adv_scan(snapshot), error = function(e) e)
    if (inherits(value, "error")) {
      finalize_corr_adv(err = conditionMessage(value))
    } else {
      finalize_corr_adv(result = value)
    }
  }, ignoreInit = TRUE)

  observeEvent(
    list(
      input$corr_adv_anchor, input$corr_adv_method, input$corr_adv_data_mode,
      input$scope, input$strain, input$showGroups,
      plot_settings()
    ),
    {
      if (!isTRUE(corr_adv_has_run())) return()
      corr_adv_has_run(FALSE)
      corr_adv_table_data(tibble::tibble())
      corr_adv_last_pair(NULL)
      corr_adv_results(tibble::tibble())
    },
    ignoreInit = TRUE
  )

  corr_adv_filtered <- reactive({
    if (!isTRUE(corr_adv_has_run())) return(tibble::tibble())
    tbl <- corr_adv_results()
    if (!is.data.frame(tbl) || !nrow(tbl)) return(tibble::tibble())

    p_max <- suppressWarnings(as.numeric(input$corr_adv_pvalue_max %||% 0.05))
    if (!is.finite(p_max)) p_max <- 0.05
    p_max <- min(max(p_max, 0), 1)

    r_range <- suppressWarnings(as.numeric(input$corr_adv_r_filter %||% c(-1, 1)))
    if (length(r_range) != 2 || any(!is.finite(r_range))) r_range <- c(-1, 1)
    r_min <- max(min(r_range), -1)
    r_max <- min(max(r_range), 1)
    if (r_max < r_min) {
      tmp <- r_min
      r_min <- r_max
      r_max <- tmp
    }

    if (isTRUE(input$corr_adv_sig_only)) {
      tbl <- tbl %>% dplyr::filter(is.finite(p.value), p.value <= p_max)
    }

    tbl <- tbl %>%
      dplyr::filter(is.finite(r), r >= r_min, r <= r_max)

    direction_mode <- input$corr_adv_direction %||% "all"
    if (identical(direction_mode, "positive")) {
      tbl <- tbl %>% dplyr::filter(r > 0)
    } else if (identical(direction_mode, "negative")) {
      tbl <- tbl %>% dplyr::filter(r < 0)
    }
    tbl
  })

  corr_adv_summary_counts <- reactive({
    if (!isTRUE(corr_adv_has_run())) return(NULL)
    tbl <- corr_adv_results()
    if (!is.data.frame(tbl) || !nrow(tbl)) return(NULL)

    valid_idx <- is.finite(tbl$r) & is.finite(tbl$p.value)
    sig_idx <- valid_idx & tbl$p.value <= 0.05
    list(
      n_total = nrow(tbl),
      n_valid = sum(valid_idx, na.rm = TRUE),
      n_sig = sum(sig_idx, na.rm = TRUE),
      n_sig_pos = sum(sig_idx & tbl$r > 0, na.rm = TRUE),
      n_sig_neg = sum(sig_idx & tbl$r < 0, na.rm = TRUE),
      n_excluded = sum(!valid_idx, na.rm = TRUE)
    )
  })

  output$corr_adv_summary <- renderUI({
    lang <- input$app_lang %||% i18n_lang
    if (isTRUE(corr_adv_running())) {
      return(tags$p(class = "qc-help", tr_text("corr_adv_loading", lang)))
    }
    if (!isTRUE(corr_adv_has_run())) return(NULL)

    counts <- corr_adv_summary_counts()
    if (is.null(counts)) {
      return(tags$p(class = "qc-help", tr_text("corr_adv_no_results", lang)))
    }
    tagList(
      tags$strong(tr_text("corr_adv_summary_title", lang)),
      tags$p(sprintf("%s: %s", tr_text("corr_adv_summary_sig_total", lang), counts$n_sig)),
      tags$p(sprintf("%s: %s", tr_text("corr_adv_summary_sig_pos", lang), counts$n_sig_pos)),
      tags$p(sprintf("%s: %s", tr_text("corr_adv_summary_sig_neg", lang), counts$n_sig_neg)),
      tags$p(class = "qc-help", sprintf("%s: %s", tr_text("corr_adv_summary_excluded", lang), counts$n_excluded))
    )
  })

  output$download_corr_adv <- downloadHandler(
    filename = function() {
      anchor_name <- tryCatch({
        tbl <- corr_adv_results()
        a <- as.character(tbl$param_anchor[[1]] %||% "ancla")
        if (!nzchar(a)) "ancla" else a
      }, error = function(e) "ancla")
      anchor_name <- gsub("[^A-Za-z0-9_-]", "_", anchor_name)
      paste0("correlaciones_", anchor_name, "_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".xlsx")
    },
    content = function(file) {
      lang <- input$app_lang %||% i18n_lang
      if (!isTRUE(corr_adv_has_run())) {
        msg <- tr_text("corr_adv_download_run_first", lang)
        showNotification(msg, type = "warning", duration = 4)
        stop(msg, call. = FALSE)
      }

      tbl <- corr_adv_results()
      if (!is.data.frame(tbl) || !nrow(tbl)) {
        msg <- tr_text("corr_adv_no_results", lang)
        showNotification(msg, type = "warning", duration = 4)
        stop(msg, call. = FALSE)
      }

      # Correlacion.R-style export: one sheet with correlation and p-value columns.
      export_tbl <- tbl %>%
        dplyr::transmute(
          ancla = param_anchor,
          parametro = param_other,
          p_value = p.value,
          correlacion = r
        )

      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "correlaciones")
      openxlsx::writeData(wb, "correlaciones", export_tbl)
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )

  output$corrAdvDownloadReady <- renderText({
    if (!identical(input$tipo %||% "", "Correlacion")) return("false")
    if (isTRUE(corr_adv_running())) return("false")
    if (!isTRUE(corr_adv_has_run())) return("false")
    tbl <- corr_adv_results()
    if (!is.data.frame(tbl) || !nrow(tbl)) return("false")
    "true"
  })
  outputOptions(output, "corrAdvDownloadReady", suspendWhenHidden = FALSE)

  output$corr_adv_table <- renderDT({
    lang <- input$app_lang %||% i18n_lang
    if (isTRUE(corr_adv_running())) {
      corr_adv_table_data(tibble::tibble())
      loading_tbl <- tibble::tibble(
        !!tr_text("corr_adv_col_parameter", lang) := tr_text("corr_adv_loading", lang)
      )
      return(datatable(
        loading_tbl,
        options = list(dom = "t"),
        rownames = FALSE,
        selection = "none"
      ))
    }

    if (!isTRUE(corr_adv_has_run())) {
      corr_adv_table_data(tibble::tibble())
      empty_tbl <- tibble::tibble(
        !!tr_text("corr_adv_col_parameter", lang) := tr_text("corr_adv_press_run", lang)
      )
      return(datatable(
        empty_tbl,
        options = list(dom = "t"),
        rownames = FALSE,
        selection = "none"
      ))
    }

    filtered_tbl <- corr_adv_filtered()
    base_tbl <- corr_adv_results()

    if (!is.data.frame(filtered_tbl) || !nrow(filtered_tbl)) {
      corr_adv_table_data(tibble::tibble())
      empty_msg <- if (is.data.frame(base_tbl) && nrow(base_tbl)) {
        tr_text("corr_adv_no_filtered_results", lang)
      } else {
        tr_text("corr_adv_no_results", lang)
      }
      empty_tbl <- tibble::tibble(
        !!tr_text("corr_adv_col_parameter", lang) := empty_msg
      )
      return(datatable(
        empty_tbl,
        options = list(dom = "t"),
        rownames = FALSE,
        selection = "none"
      ))
    }

    corr_adv_table_data(filtered_tbl)

    direction_txt <- dplyr::case_when(
      filtered_tbl$direction == "positive" ~ tr_text("corr_adv_direction_pos", lang),
      filtered_tbl$direction == "negative" ~ tr_text("corr_adv_direction_neg", lang),
      TRUE ~ tr_text("corr_adv_direction_zero", lang)
    )
    view_tbl <- filtered_tbl %>%
      dplyr::mutate(
        r = ifelse(is.finite(r), round(r, 4), NA_real_),
        p_view = ifelse(is.finite(p.value), signif(p.value, 4), NA_real_),
        n = suppressWarnings(as.integer(n)),
        significant_view = ifelse(significant, tr_text("yes_label", lang), tr_text("no_label", lang)),
        direction_view = direction_txt
      ) %>%
      dplyr::transmute(
        !!tr_text("corr_adv_col_anchor", lang) := param_anchor,
        !!tr_text("corr_adv_col_parameter", lang) := param_other,
        !!tr_text("corr_adv_col_r", lang) := r,
        !!tr_text("corr_adv_col_pvalue", lang) := p_view,
        !!tr_text("corr_adv_col_n", lang) := n,
        !!tr_text("corr_adv_col_significant", lang) := significant_view,
        !!tr_text("corr_adv_col_direction", lang) := direction_view
      )

    datatable(
      view_tbl,
      rownames = FALSE,
      selection = "single",
      filter = "top",
      options = list(
        pageLength = 8,
        scrollX = TRUE,
        autoWidth = TRUE
      )
    )
  }, server = FALSE)

  observeEvent(input$corr_adv_table_rows_selected, {
    idx <- suppressWarnings(as.integer((input$corr_adv_table_rows_selected %||% integer(0))[1]))
    tbl <- corr_adv_table_data()
    if (!is.data.frame(tbl) || !nrow(tbl) || !is.finite(idx) || idx < 1 || idx > nrow(tbl)) return()

    sel <- tbl[idx, , drop = FALSE]
    anchor <- as.character(sel$param_anchor[[1]] %||% "")
    other <- as.character(sel$param_other[[1]] %||% "")
    mode <- as.character(sel$data_mode[[1]] %||% "raw")
    method <- as.character(sel$method[[1]] %||% "pearson")
    if (!nzchar(anchor) || !nzchar(other)) return()
    corr_adv_last_pair(list(x = anchor, y = other))

    params_all <- unique(as.character(plot_settings()$Parameter %||% character(0)))
    params_all <- params_all[!is.na(params_all) & nzchar(params_all)]

    update_selectize_adaptive(
      "corr_param_x",
      choices = params_all,
      selected = anchor
    )
    update_selectize_adaptive(
      "corr_param_y",
      choices = params_all,
      selected = other
    )
    updateRadioButtons(session, "corr_method", selected = method)

    if (identical(mode, "raw")) {
      updateCheckboxInput(session, "doNorm", value = FALSE)
      updateRadioButtons(session, "corr_norm_target", selected = "both")
    } else {
      norm_target <- switch(
        mode,
        norm_both = "both",
        norm_x = "x_only",
        norm_y = "y_only",
        "both"
      )
      updateCheckboxInput(session, "doNorm", value = TRUE)
      updateRadioButtons(session, "corr_norm_target", selected = norm_target)
    }

    showNotification(
      sprintf(
        tr_text("corr_adv_opened_plot", input$app_lang %||% i18n_lang),
        anchor,
        other
      ),
      type = "message",
      duration = 3
    )
  }, ignoreInit = TRUE)
  
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
          dplyr::mutate(
            BiologicalReplicate = factor(
              as.character(BiologicalReplicate),
              levels = normalize_rep_selection(BiologicalReplicate)
            )
          ) %>%
          arrange(BiologicalReplicate) %>%  
          dplyr::mutate(BiologicalReplicate = as.character(BiologicalReplicate)) %>%  
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
          RepBiol = factor(
            RepBiol,
            levels = c(normalize_rep_selection(df$BiologicalReplicate), "Promedio")
          )
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
        mutate(
          BiologicalReplicate = factor(
            as.character(BiologicalReplicate),
            levels = c(
              normalize_rep_selection(BiologicalReplicate[as.character(BiologicalReplicate) != "Promedio"]),
              "Promedio"
            )
          )
        ) %>%
        arrange(Parametro, Label, BiologicalReplicate) %>%
        mutate(BiologicalReplicate = as.character(BiologicalReplicate))
      return(datatable(tabla, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE))  
    }  
    
    if (tipo == "Correlacion") {
      raw_x <- trimws(as.character(input$corr_param_x %||% ""))
      raw_y <- trimws(as.character(input$corr_param_y %||% ""))
      validate(need(
        nzchar(raw_x) && nzchar(raw_y) && !identical(raw_x, raw_y),
        tr_text("corr_select_two_params", input$app_lang %||% i18n_lang)
      ))
      norm_mode <- input$corr_norm_target %||% "both"
      use_norm_x <- isTRUE(input$doNorm) && has_ctrl_selected() && norm_mode %in% c("both", "x_only")
      use_norm_y <- isTRUE(input$doNorm) && has_ctrl_selected() && norm_mode %in% c("both", "y_only")
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
      validate(need(
        all(c(col_x, col_y) %in% names(df)),
        tr_text("no_data_selection", lang)
      ))

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
        mutate(
          BiologicalReplicate = factor(
            as.character(BiologicalReplicate),
            levels = c(
              normalize_rep_selection(BiologicalReplicate[as.character(BiologicalReplicate) != "Promedio"]),
              "Promedio"
            )
          )
        ) %>%
        arrange(Parametro, Label, BiologicalReplicate) %>%
        mutate(BiologicalReplicate = as.character(BiologicalReplicate))
      return(datatable(tabla, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE))
    }
    
    datatable(
      tibble::tibble(Mensaje = tr_text("table_not_available", input$app_lang %||% i18n_lang)),
      options = list(dom = 't')
    )  
  })  
  # Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬  
  
  # Ã¢â€â‚¬Ã¢â€â‚¬ Helper para elegir paleta segÃƒÂºn input$colorMode Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬  
  qc_scope_base_df <- reactive({
    req(datos_agrupados(), plot_settings())
    df <- if (isTRUE(input$doNorm)) datos_agrupados_norm() else datos_agrupados()
    if (!is.data.frame(df) || !nrow(df)) return(df)

    if (input$scope == "Por Cepa") {
      req(input$strain)
      df <- df %>%
        dplyr::filter(Strain == input$strain) %>%
        order_filter_strain()
    } else {
      grps <- input$showGroups %||% character(0)
      if (!length(grps)) return(df[0, , drop = FALSE])
      df <- df %>%
        dplyr::filter(paste(Strain, Media, sep = "-") %in% grps)
    }

    if (!"Label" %in% names(df)) {
      if (input$scope == "Por Cepa") {
        df <- df %>% dplyr::mutate(Label = Media)
      } else {
        df <- df %>% dplyr::mutate(Label = paste(Strain, Media, sep = "-"))
      }
    }
    df
  })

  qc_scope_bio_df <- reactive({
    df <- qc_scope_base_df()
    if (!is.data.frame(df) || !nrow(df)) return(df)
    if (input$scope == "Por Cepa") {
      df <- filter_reps_strain(df)
    } else {
      df <- filter_reps_group(df)
    }
    df
  })

  qc_param_cols <- reactive({
    req(plot_settings())
    params <- plot_settings()$Parameter
    if (isTRUE(input$doNorm) && has_ctrl_selected()) {
      params <- paste0(params, "_Norm")
    }
    intersect(params, names(qc_scope_base_df()))
  })

  qc_group_var <- reactive({
    if (input$scope == "Por Cepa") "Media" else "Label"
  })

  qc_tech_param_cols <- reactive({
    params <- qc_param_cols()
    if (!length(params)) return(character(0))

    selected_param <- as.character(input$param %||% character(0))
    selected_param <- selected_param[!is.na(selected_param) & nzchar(selected_param)]
    selected_param <- if (length(selected_param)) selected_param[[1]] else ""

    if (nzchar(selected_param)) {
      selected_candidates <- c(selected_param)
      if (isTRUE(input$doNorm) && has_ctrl_selected() && !grepl("_Norm$", selected_param, fixed = TRUE)) {
        selected_candidates <- c(selected_candidates, paste0(selected_param, "_Norm"))
      }
      selected_candidates <- unique(selected_candidates)
      selected_match <- intersect(selected_candidates, params)
      if (length(selected_match)) return(selected_match[[1]])
    }

    params[[1]]
  })

  qc_tech_param_key <- reactive({
    param <- qc_tech_param_cols()
    if (!length(param)) return("")
    as.character(param[[1]])
  })

  qc_tech_get_param_map <- function(store, param_key) {
    if (!is.list(store) || !length(store)) return(list())
    key <- as.character(param_key %||% "")
    if (!length(key) || is.na(key[[1]]) || !nzchar(key[[1]])) return(list())
    map <- store[[key[[1]]]]
    if (is.null(map) || !is.list(map)) list() else map
  }

  qc_tech_set_param_map <- function(store, param_key, map) {
    if (!is.list(store)) store <- list()
    key <- as.character(param_key %||% "")
    if (!length(key) || is.na(key[[1]]) || !nzchar(key[[1]])) return(store)
    map_use <- if (is.list(map)) map else list()
    store[[key[[1]]]] <- map_use
    store
  }

  observeEvent(qc_tech_param_key(), {
    if (isTRUE(qc_tech_bulk_updating())) return()
    param_key <- qc_tech_param_key()
    target_map <- qc_tech_get_param_map(isolate(qc_tech_selected_by_param()), param_key)
    current_map <- isolate(qc_tech_selected())
    if (!identical(current_map, target_map)) {
      qc_tech_selected(target_map)
    }
  }, ignoreInit = FALSE)

  observeEvent(qc_tech_selected(), {
    if (isTRUE(qc_tech_bulk_updating())) return()
    param_key <- isolate(qc_tech_param_key())
    if (!nzchar(param_key)) return()
    current_store <- isolate(qc_tech_selected_by_param())
    next_store <- qc_tech_set_param_map(current_store, param_key, qc_tech_selected())
    if (!identical(next_store, current_store)) {
      qc_tech_selected_by_param(next_store)
    }
  }, ignoreInit = FALSE)

  qc_tech_input_id <- function(group, biorep, strain = NULL, media = NULL, param_key = NULL) {
    key <- qc_tech_selection_key(group, biorep, strain = strain, media = media)
    if (!nzchar(key)) return("")
    pkey <- as.character(param_key %||% "")
    if (!length(pkey) || is.na(pkey[[1]]) || !nzchar(pkey[[1]])) {
      pkey <- isolate(qc_tech_param_key())
    }
    if (!length(pkey) || is.na(pkey[[1]]) || !nzchar(pkey[[1]])) pkey <- "_all"
    paste0("qc_tech_rep_", make.names(pkey[[1]]), "_", make.names(key))
  }

  qc_tech_available <- reactive({
    df <- qc_tech_source_df()
    group_var <- qc_group_var()
    required <- c(group_var, "BiologicalReplicate", "TechnicalReplicate")
    if (!is.data.frame(df) || !nrow(df)) return(FALSE)
    if (!all(required %in% names(df))) return(FALSE)

    tech_counts <- df %>%
      dplyr::transmute(
        Group = as.character(.data[[group_var]]),
        BiologicalReplicate = as.character(BiologicalReplicate),
        TechnicalReplicate = as.character(TechnicalReplicate)
      ) %>%
      dplyr::filter(
        !is.na(Group), nzchar(Group),
        !is.na(BiologicalReplicate), nzchar(BiologicalReplicate),
        !is.na(TechnicalReplicate), nzchar(TechnicalReplicate)
      ) %>%
      dplyr::distinct() %>%
      dplyr::group_by(Group, BiologicalReplicate) %>%
      dplyr::summarise(
        TechnicalReplicates = dplyr::n_distinct(TechnicalReplicate),
        .groups = "drop"
      )

    any(tech_counts$TechnicalReplicates > 1, na.rm = TRUE)
  })

  qc_apply_tech_selection_df <- function(df) {
    group_var <- qc_group_var()
    qc_filter_by_technical_selection(
      df = df,
      tech_map = qc_tech_selected(),
      group_col = group_var,
      biorep_col = "BiologicalReplicate",
      tech_col = "TechnicalReplicate",
      strain_col = "Strain",
      media_col = "Media"
    )
  }

  qc_scope_df <- reactive({
    df <- qc_scope_bio_df()
    if (!is.data.frame(df) || !nrow(df)) return(df)
    qc_apply_tech_selection_df(df)
  })

  qc_outlier_detected <- reactive({
    df <- qc_scope_df()
    params <- qc_param_cols()
    group_var <- qc_group_var()
    if (!is.data.frame(df) || !nrow(df)) {
      return(qc_detect_iqr_outliers(df = data.frame(), params = params, group_col = group_var))
    }
    tryCatch(
      qc_detect_iqr_outliers(
        df = df,
        params = params,
        group_col = group_var,
        rep_col = "BiologicalReplicate",
        iqr_mult = input$qc_outlier_iqr_mult %||% 1.5
      ),
      error = function(e) {
        qc_detect_iqr_outliers(df = data.frame(), params = params, group_col = group_var)
      }
    )
  })

  qc_selector_base_df <- reactive({
    use_param <- !is.null(input$tipo) && input$tipo %in% c("Boxplot", "Barras", "Violin")
    df <- if (use_param) {
      param_rep_df()
    } else {
      if (isTRUE(input$doNorm)) datos_agrupados_norm() else datos_agrupados()
    }
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(df)

    if (input$scope == "Por Cepa") {
      req(input$strain)
      df <- df %>% dplyr::filter(Strain == input$strain)
      if (!is.null(input$showMedios)) {
        df <- df %>% dplyr::filter(Media %in% input$showMedios)
      }
    } else {
      grps <- input$showGroups %||% character(0)
      if (!length(grps)) return(df[0, , drop = FALSE])
      df <- df %>% dplyr::filter(paste(Strain, Media, sep = "-") %in% grps)
    }

    if ("BiologicalReplicate" %in% names(df)) {
      drop_all <- as.character(input$rm_reps_all %||% character(0))
      if (length(drop_all)) {
        df <- df %>% dplyr::filter(!as.character(BiologicalReplicate) %in% drop_all)
      }
    }

    if (!"Label" %in% names(df) && all(c("Strain", "Media") %in% names(df))) {
      df <- df %>% dplyr::mutate(Label = paste(Strain, Media, sep = "-"))
    }
    df
  })

  qc_tech_source_df <- reactive({
    group_var <- qc_group_var()
    required <- c(group_var, "BiologicalReplicate", "TechnicalReplicate")

    df_primary <- qc_scope_bio_df()
    if (is.data.frame(df_primary) && nrow(df_primary) && all(required %in% names(df_primary))) {
      return(df_primary)
    }

    df_fallback <- qc_selector_base_df()
    if (!is.data.frame(df_fallback) || !nrow(df_fallback)) return(df_primary)

    if (!"Label" %in% names(df_fallback) && all(c("Strain", "Media") %in% names(df_fallback))) {
      df_fallback <- df_fallback %>% dplyr::mutate(Label = paste(Strain, Media, sep = "-"))
    }
    if (!all(required %in% names(df_fallback))) return(df_primary)

    if (input$scope == "Por Cepa") {
      df_fallback <- filter_reps_strain(df_fallback)
    } else {
      df_fallback <- filter_reps_group(df_fallback)
    }
    df_fallback
  })

  qc_tech_selector_state <- reactive({
    df <- qc_tech_source_df()
    active_param_key <- qc_tech_param_key()
    group_var <- qc_group_var()
    required <- c(group_var, "BiologicalReplicate", "TechnicalReplicate")
    out <- list()
    if (!isTRUE(qc_tech_available())) return(out)
    if (is.null(df) || !is.data.frame(df) || !nrow(df)) return(out)
    if (!all(required %in% names(df))) return(out)

    combos <- df %>%
      dplyr::transmute(
        Group = as.character(.data[[group_var]]),
        Strain = if ("Strain" %in% names(df)) as.character(Strain) else "",
        Media = if ("Media" %in% names(df)) as.character(Media) else "",
        BiologicalReplicate = as.character(BiologicalReplicate),
        TechnicalReplicate = as.character(TechnicalReplicate)
      ) %>%
      dplyr::filter(
        !is.na(Group), nzchar(Group),
        !is.na(BiologicalReplicate), nzchar(BiologicalReplicate),
        !is.na(TechnicalReplicate), nzchar(TechnicalReplicate)
      ) %>%
      dplyr::distinct() %>%
      dplyr::arrange(Group, Strain, Media, BiologicalReplicate, TechnicalReplicate)
    if (!nrow(combos)) return(out)

    stored_map <- qc_tech_selected()
    split_keys <- unique(paste(combos$Group, combos$Strain, combos$Media, combos$BiologicalReplicate, sep = "||"))
    for (key in split_keys) {
      idx <- paste(combos$Group, combos$Strain, combos$Media, combos$BiologicalReplicate, sep = "||") == key
      if (!any(idx)) next
      group_name <- as.character(combos$Group[idx][[1]])
      strain_name <- as.character(combos$Strain[idx][[1]])
      media_name <- as.character(combos$Media[idx][[1]])
      biorep_name <- as.character(combos$BiologicalReplicate[idx][[1]])
      canonical_key <- qc_tech_selection_key(
        group = group_name,
        biorep = biorep_name,
        strain = strain_name,
        media = media_name
      )
      choices <- normalize_rep_selection(combos$TechnicalReplicate[idx])
      input_id <- qc_tech_input_id(
        group_name,
        biorep_name,
        strain = strain_name,
        media = media_name,
        param_key = active_param_key
      )
      if (!nzchar(input_id) || !length(choices)) next
      current <- input[[input_id]]
      stored <- stored_map[[canonical_key]]
      selected <- if (!is.null(current)) {
        normalize_rep_selection(intersect(as.character(current), choices))
      } else if (!is.null(stored)) {
        normalize_rep_selection(intersect(as.character(stored), choices))
      } else {
        choices
      }
      out[[canonical_key]] <- list(
        key = canonical_key,
        id = input_id,
        group = group_name,
        strain = strain_name,
        media = media_name,
        biorep = biorep_name,
        choices = choices,
        selected = selected
      )
    }

    out
  })

  observe({
    if (isTRUE(qc_tech_bulk_updating())) return()

    selector_state <- qc_tech_selector_state()
    current_map <- isolate(qc_tech_selected())
    next_map <- list()

    if (!length(selector_state)) {
      return()
    }

    for (key in names(selector_state)) {
      info <- selector_state[[key]]
      current_input <- input[[info$id]]
      next_sel <- if (!is.null(current_input)) {
        normalize_rep_selection(intersect(as.character(current_input), info$choices))
      } else if (!is.null(current_map[[key]])) {
        normalize_rep_selection(intersect(as.character(current_map[[key]]), info$choices))
      } else {
        info$choices
      }
      next_map[[key]] <- next_sel
    }

    if (!identical(next_map, current_map)) {
      qc_tech_selected(next_map)
    }
  })

  qc_apply_tech_selection <- function(selector_state, selected_map) {
    if (!length(selector_state)) return(invisible(NULL))
    next_map <- setNames(vector("list", length(selector_state)), names(selector_state))
    current_map <- isolate(qc_tech_selected())

    qc_tech_bulk_updating(TRUE)
    session$onFlushed(function() {
      qc_tech_bulk_updating(FALSE)
    }, once = TRUE)

    for (key in names(selector_state)) {
      info <- selector_state[[key]]
      next_sel <- selected_map[[key]]
      if (is.null(next_sel)) next_sel <- info$selected
      next_sel <- normalize_rep_selection(intersect(as.character(next_sel), info$choices))
      next_map[[key]] <- next_sel
      current_input <- normalize_rep_selection(intersect(as.character(input[[info$id]] %||% character(0)), info$choices))
      if (identical(current_input, next_sel)) next
      shiny::freezeReactiveValue(input, info$id)
      updateCheckboxGroupInput(
        session,
        inputId = info$id,
        choices = info$choices,
        selected = next_sel
      )
    }

    if (!identical(next_map, current_map)) {
      qc_tech_selected(next_map)
    }
    invisible(NULL)
  }

  output$qcTechTabAvailable <- renderText({
    if (isTRUE(qc_tech_available())) "TRUE" else "FALSE"
  })

  qc_tech_tab_panel <- function(lang) {
    tabPanel(
      title = tr_text("qc_tech_title", lang),
      value = "qc_tech",
      tags$p(class = "qc-help", tr_text("qc_tech_help", lang)),
      actionButton(
        "qc_tech_select_all",
        tr_text("qc_tech_select_all_button", lang),
        class = "btn btn-outline-primary w-100",
        style = "white-space: normal; margin-bottom: 6px;"
      ),
      actionButton(
        "qc_tech_deselect_all",
        tr_text("qc_tech_deselect_all_button", lang),
        class = "btn btn-outline-secondary w-100",
        style = "white-space: normal; margin-bottom: 8px;"
      ),
      uiOutput("qcTechSelectorsUI"),
      tags$hr(),
      tags$h5(tr_text("qc_tech_outlier_table", lang)),
      numericInput(
        "qc_tech_outlier_iqr_mult",
        tr_text("qc_tech_outlier_iqr_multiplier_label", lang),
        value = 1.5,
        min = 0.1,
        step = 0.1
      ),
      tags$p(class = "qc-help", tr_text("qc_tech_outlier_iqr_multiplier_help", lang)),
      actionButton(
        "qc_apply_tech_outlier_exclusion",
        tr_text("qc_tech_outlier_apply_button", lang),
        class = "btn btn-outline-primary w-100",
        style = "white-space: normal;"
      ),
      br(),
      numericInput(
        "qc_tech_keep_top_n",
        tr_text("qc_tech_keep_n_label", lang),
        value = 2,
        min = 1,
        step = 1
      ),
      tags$p(class = "qc-help", tr_text("qc_tech_keep_n_help", lang)),
      actionButton(
        "qc_apply_tech_top_n",
        tr_text("qc_tech_apply_topn_button", lang),
        class = "btn btn-outline-secondary w-100",
        style = "white-space: normal;"
      ),
      br(), br(),
      DTOutput("qcTechOutlierTable")
    )
  }

  observe({
    lang <- input$app_lang %||% i18n_lang
    available <- isTRUE(qc_tech_available())
    inserted <- isTRUE(qc_tech_tab_inserted())
    current_tab <- input$qcTabs %||% ""

    if (available && !inserted) {
      insertTab(
        inputId = "qcTabs",
        tab = qc_tech_tab_panel(lang),
        target = "qc_sample",
        position = "after",
        select = FALSE,
        session = session
      )
      qc_tech_tab_inserted(TRUE)
      qc_tech_tab_lang(lang)
      return(invisible(NULL))
    }

    if (!available && inserted) {
      if (identical(current_tab, "qc_tech")) {
        updateTabsetPanel(session, "qcTabs", selected = "qc_sample")
      }
      removeTab(
        inputId = "qcTabs",
        target = "qc_tech",
        session = session
      )
      qc_tech_tab_inserted(FALSE)
      qc_tech_tab_lang(NULL)
      return(invisible(NULL))
    }

    if (available && inserted && !identical(lang, isolate(qc_tech_tab_lang()))) {
      selected_tech <- identical(current_tab, "qc_tech")
      removeTab(
        inputId = "qcTabs",
        target = "qc_tech",
        session = session
      )
      insertTab(
        inputId = "qcTabs",
        tab = qc_tech_tab_panel(lang),
        target = "qc_sample",
        position = "after",
        select = selected_tech,
        session = session
      )
      qc_tech_tab_lang(lang)
    }
    invisible(NULL)
  })

  output$qcTechSelectorsUI <- renderUI({
    lang <- input$app_lang %||% i18n_lang
    selector_state <- qc_tech_selector_state()
    if (!length(selector_state)) return(NULL)

    group_names <- unique(vapply(selector_state, function(info) info$group, character(1)))
    group_panels <- lapply(group_names, function(group_name) {
      group_keys <- names(selector_state)[vapply(selector_state, function(info) identical(info$group, group_name), logical(1))]
      bio_panels <- lapply(group_keys, function(key) {
        info <- selector_state[[key]]
        accordion_panel_safe(
          sprintf(tr_text("qc_tech_biorep_panel", lang), info$biorep),
          checkboxGroupInput(
            inputId = info$id,
            label = tr_text("qc_tech_selector_label", lang),
            choices = info$choices,
            selected = info$selected
          ),
          value = paste0("qc_tech_bio_", make.names(key))
        )
      })
      nested <- do.call(
        accordion,
        c(
          list(
            id = paste0("qcTechBioPanel_", make.names(group_name)),
            open = FALSE,
            multiple = TRUE
          ),
          bio_panels
        )
      )
      accordion_panel_safe(
        group_name,
        nested,
        value = paste0("qc_tech_group_", make.names(group_name))
      )
    })

    tagList(
      tags$p(class = "qc-help", tr_text("qc_tech_selector_help", lang)),
      do.call(
        accordion,
        c(
          list(
            id = "qcTechSelectorAccordion",
            open = FALSE,
            multiple = TRUE
          ),
          group_panels
        )
      )
    )
  })

  qc_tech_outlier_detected <- reactive({
    df <- qc_apply_tech_selection_df(qc_tech_source_df())
    params <- qc_tech_param_cols()
    group_var <- qc_group_var()
    if (!isTRUE(qc_tech_available()) || !is.data.frame(df) || !nrow(df)) {
      return(qc_detect_iqr_outliers(
        df = data.frame(),
        params = params,
        group_col = group_var,
        rep_col = "TechnicalReplicate",
        subgroup_col = "BiologicalReplicate"
      ))
    }
    tryCatch(
      qc_detect_iqr_outliers(
        df = df,
        params = params,
        group_col = group_var,
        rep_col = "TechnicalReplicate",
        subgroup_col = "BiologicalReplicate",
        iqr_mult = input$qc_tech_outlier_iqr_mult %||% 1.5
      ),
      error = function(e) {
        qc_detect_iqr_outliers(
          df = data.frame(),
          params = params,
          group_col = group_var,
          rep_col = "TechnicalReplicate",
          subgroup_col = "BiologicalReplicate"
        )
      }
    )
  })

  qc_format_tech_sample_summary <- function(df, group_var, lang) {
    required <- c(group_var, "BiologicalReplicate", "TechnicalReplicate")
    if (is.null(df) || !is.data.frame(df) || !nrow(df)) return(NULL)
    if (!all(required %in% names(df))) return(NULL)

    tech_counts <- df %>%
      dplyr::transmute(
        Group = as.character(.data[[group_var]]),
        BiologicalReplicate = as.character(BiologicalReplicate),
        TechnicalReplicate = as.character(TechnicalReplicate)
      ) %>%
      dplyr::filter(
        !is.na(Group), nzchar(Group),
        !is.na(BiologicalReplicate), nzchar(BiologicalReplicate)
      ) %>%
      dplyr::group_by(Group, BiologicalReplicate) %>%
      dplyr::summarise(
        TechnicalReplicates = dplyr::n_distinct(TechnicalReplicate[!is.na(TechnicalReplicate) & nzchar(TechnicalReplicate)]),
        .groups = "drop"
      )

    if (!nrow(tech_counts) || !any(tech_counts$TechnicalReplicates > 0, na.rm = TRUE)) {
      return(NULL)
    }

    tech_counts %>%
      dplyr::group_by(Group) %>%
      dplyr::summarise(
        TechnicalReplicates = {
          group_tbl <- dplyr::cur_data_all()
          positive_tbl <- group_tbl[group_tbl$TechnicalReplicates > 0, , drop = FALSE]
          if (!nrow(positive_tbl)) {
            ""
          } else {
            freq <- sort(table(positive_tbl$TechnicalReplicates), decreasing = TRUE)
            baseline <- as.character(names(freq)[1])
            if (length(freq) == 1) {
              sprintf(tr_text("qc_tech_sample_each", lang), baseline)
            } else if (unname(freq[1]) <= 1) {
              paste(
                sprintf(
                  tr_text("qc_tech_sample_exception", lang),
                  positive_tbl$BiologicalReplicate,
                  positive_tbl$TechnicalReplicates
                ),
                collapse = "; "
              )
            } else {
              exceptions <- positive_tbl[as.character(positive_tbl$TechnicalReplicates) != baseline, , drop = FALSE]
              parts <- c(sprintf(tr_text("qc_tech_sample_each", lang), baseline))
              if (nrow(exceptions)) {
                parts <- c(
                  parts,
                  sprintf(
                    tr_text("qc_tech_sample_exception", lang),
                    exceptions$BiologicalReplicate,
                    exceptions$TechnicalReplicates
                  )
                )
              }
              paste(parts, collapse = "; ")
            }
          }
        },
        .groups = "drop"
      )
  }

  qc_replicate_selectors <- function(df) {
    out <- list()
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(out)
    if (!"BiologicalReplicate" %in% names(df)) return(out)
    map_strain <- reps_strain_selected()
    map_group <- reps_group_selected()

    clean_reps <- function(values) {
      vals <- as.character(values)
      vals <- vals[!is.na(vals) & nzchar(vals)]
      unique(vals)
    }

    if (input$scope == "Por Cepa") {
      groups <- sort(unique(as.character(df$Media)))
      for (g in groups) {
        idx <- as.character(df$Media) == g
        choices <- normalize_rep_selection(clean_reps(df$BiologicalReplicate[idx]))
        id <- paste0("reps_", make.names(g))
        current <- input[[id]]
        stored <- get_synced_media_selection(
          strain_map = map_strain,
          group_map = map_group,
          strain = input$strain,
          media = g
        )
        selected <- if (!is.null(current)) {
          intersect(as.character(current), choices)
        } else if (!is.null(stored)) {
          intersect(as.character(stored), choices)
        } else {
          choices
        }
        out[[g]] <- list(
          id = id,
          choices = choices,
          selected = selected,
          strain = as.character(input$strain %||% ""),
          media = as.character(g)
        )
      }
    } else {
      grps <- input$showGroups %||% character(0)
      grp_id <- paste(df$Strain, df$Media, sep = "-")
      available <- unique(grp_id)
      groups <- grps[grps %in% available]
      for (g in groups) {
        idx <- grp_id == g
        choices <- normalize_rep_selection(clean_reps(df$BiologicalReplicate[idx]))
        id <- paste0("reps_grp_", make.names(g))
        current <- input[[id]]
        strain_g <- if (any(idx)) as.character(df$Strain[idx][[1]]) else ""
        media_g <- if (any(idx)) as.character(df$Media[idx][[1]]) else ""
        stored <- map_group[[as.character(g)]]
        if (is.null(stored)) {
          stored <- get_strain_media_selection(map_strain, strain_g, media_g)
        }
        selected <- if (!is.null(current)) {
          intersect(as.character(current), choices)
        } else if (!is.null(stored)) {
          intersect(as.character(stored), choices)
        } else {
          choices
        }
        out[[g]] <- list(
          id = id,
          choices = choices,
          selected = selected,
          strain = strain_g,
          media = media_g
        )
      }
    }
    out
  }

  qc_apply_replicate_selection <- function(selector_state, selected_map) {
    if (!length(selector_state)) return(invisible(NULL))
    map_strain <- reps_strain_selected()
    map_group <- reps_group_selected()
    for (g in names(selector_state)) {
      info <- selector_state[[g]]
      next_sel <- selected_map[[g]]
      if (is.null(next_sel)) next_sel <- info$selected
      next_sel <- intersect(as.character(next_sel), info$choices)
      strain_g <- as.character(info$strain %||% "")
      media_g <- as.character(info$media %||% g)
      sync_maps <- set_synced_media_selection(
        strain_map = map_strain,
        group_map = map_group,
        strain = strain_g,
        media = media_g,
        selected = next_sel
      )
      map_strain <- sync_maps$reps_strain_map
      map_group <- sync_maps$reps_group_map
      updateCheckboxGroupInput(
        session,
        inputId = info$id,
        choices = info$choices,
        selected = next_sel
      )
    }
    reps_strain_selected(map_strain)
    reps_group_selected(map_group)
    invisible(NULL)
  }

  output$qcMissingTable <- renderDT({
    lang <- input$app_lang %||% i18n_lang
    df <- qc_scope_df()
    params <- qc_param_cols()
    validate(need(length(params) > 0, tr_text("no_data_selection", lang)))
    miss_tbl <- tibble::tibble(
      Parameter = params,
      Missing = vapply(params, function(p) {
        vals <- suppressWarnings(as.numeric(df[[p]]))
        sum(!is.finite(vals))
      }, numeric(1)),
      Total = nrow(df)
    ) %>%
      dplyr::mutate(MissingPct = dplyr::if_else(Total > 0, 100 * Missing / Total, NA_real_))
    datatable(miss_tbl, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  }, server = FALSE)

  output$qcSampleTable <- renderDT({
    lang <- input$app_lang %||% i18n_lang
    df <- qc_scope_df()
    validate(need(nrow(df) > 0, tr_text("no_data_selection", lang)))
    group_var <- if (input$scope == "Por Cepa") "Media" else "Label"
    rep_col <- if ("BiologicalReplicate" %in% names(df)) "BiologicalReplicate" else NULL
    smp_tbl <- if (!is.null(rep_col)) {
      df %>%
        dplyr::group_by(Group = .data[[group_var]]) %>%
        dplyr::summarise(
          N = dplyr::n(),
          BiologicalReplicates = dplyr::n_distinct(.data[[rep_col]]),
          .groups = "drop"
        )
    } else {
      df %>%
        dplyr::group_by(Group = .data[[group_var]]) %>%
        dplyr::summarise(N = dplyr::n(), .groups = "drop")
    }
    tech_tbl <- qc_format_tech_sample_summary(
      qc_apply_tech_selection_df(qc_tech_source_df()),
      group_var = group_var,
      lang = lang
    )
    if (!is.null(tech_tbl) && nrow(tech_tbl)) {
      smp_tbl <- smp_tbl %>%
        dplyr::left_join(tech_tbl, by = "Group")
      tech_col_name <- tr_text("qc_tech_sample_column", lang)
      names(smp_tbl)[names(smp_tbl) == "TechnicalReplicates"] <- tech_col_name
    }
    datatable(smp_tbl, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  }, server = FALSE)

  output$qcOutlierTable <- renderDT({
    lang <- input$app_lang %||% i18n_lang
    params <- qc_param_cols()
    validate(need(length(params) > 0, tr_text("no_data_selection", lang)))
    out_tbl <- qc_summarize_outliers(qc_outlier_detected())
    datatable(out_tbl, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  }, server = FALSE)

  output$qcOutlierGroupTable <- renderDT({
    lang <- input$app_lang %||% i18n_lang
    params <- qc_param_cols()
    validate(need(length(params) > 0, tr_text("no_data_selection", lang)))

    out_tbl <- qc_summarize_outliers(qc_outlier_detected())
    group_tbl <- if (nrow(out_tbl)) {
      out_tbl %>%
        dplyr::group_by(Group) %>%
        dplyr::summarise(
          Outliers = sum(Outliers, na.rm = TRUE),
          ParametersWithOutliers = dplyr::n_distinct(Parameter),
          .groups = "drop"
        ) %>%
        dplyr::arrange(Group)
    } else {
      tibble::tibble(
        Group = character(),
        Outliers = integer(),
        ParametersWithOutliers = integer()
      )
    }
    datatable(group_tbl, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  }, server = FALSE)

  output$qcTechOutlierTable <- renderDT({
    lang <- input$app_lang %||% i18n_lang
    params <- qc_tech_param_cols()
    validate(need(length(params) > 0, tr_text("no_data_selection", lang)))

    out_tbl <- qc_summarize_outliers(qc_tech_outlier_detected())
    if ("Subgroup" %in% names(out_tbl)) {
      out_tbl <- out_tbl %>% dplyr::rename(BiologicalReplicate = Subgroup)
    } else {
      out_tbl <- tibble::tibble(
        Parameter = character(),
        Group = character(),
        BiologicalReplicate = character(),
        Outliers = integer(),
        MinOutlier = numeric(),
        MaxOutlier = numeric()
      )
    }
    datatable(out_tbl, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  }, server = FALSE)

  observeEvent(input$qc_apply_outlier_exclusion, {
    lang <- input$app_lang %||% i18n_lang
    params <- qc_param_cols()
    if (!length(params)) {
      showNotification(tr_text("qc_outlier_apply_error", lang), type = "error", duration = 6)
      return()
    }

    out_reps <- tryCatch(
      qc_outlier_replicates(
        df = qc_scope_df(),
        params = params,
        group_col = qc_group_var(),
        rep_col = "BiologicalReplicate",
        iqr_mult = input$qc_outlier_iqr_mult %||% 1.5
      ),
      error = function(e) NULL
    )

    if (is.null(out_reps)) {
      showNotification(tr_text("qc_outlier_apply_error", lang), type = "error", duration = 6)
      return()
    }
    if (!nrow(out_reps)) {
      showNotification(tr_text("qc_outlier_no_matches", lang), type = "error", duration = 6)
      return()
    }

    selector_state <- qc_replicate_selectors(qc_selector_base_df())
    if (!length(selector_state)) {
      showNotification(tr_text("qc_outlier_not_enough_data", lang), type = "error", duration = 6)
      return()
    }

    selected_map <- list()
    changed <- 0L
    for (g in names(selector_state)) {
      current <- selector_state[[g]]$selected
      flagged <- as.character(out_reps$Replicate[out_reps$Group == g])
      next_sel <- setdiff(current, flagged)
      selected_map[[g]] <- next_sel
      changed <- changed + length(setdiff(current, next_sel))
    }

    if (changed <= 0L) {
      showNotification(tr_text("qc_outlier_no_matches", lang), type = "error", duration = 6)
      return()
    }

    qc_apply_replicate_selection(selector_state, selected_map)

    per_group <- out_reps %>%
      dplyr::count(Group, name = "n") %>%
      dplyr::arrange(Group)
    group_msg <- paste(sprintf("%s=%s", per_group$Group, per_group$n), collapse = "; ")
    showNotification(
      sprintf(tr_text("qc_outlier_apply_done", lang), changed, group_msg),
      type = "message",
      duration = 8
    )
  }, ignoreInit = TRUE)

  observeEvent(input$qc_apply_top_n, {
    lang <- input$app_lang %||% i18n_lang
    keep_n <- suppressWarnings(as.integer(input$qc_keep_top_n)[1])
    if (!is.finite(keep_n) || keep_n <= 0) {
      showNotification(tr_text("qc_outlier_topn_invalid", lang), type = "error", duration = 6)
      return()
    }

    selector_df <- qc_selector_base_df()
    selector_state <- qc_replicate_selectors(selector_df)
    if (!length(selector_state)) {
      showNotification(tr_text("qc_outlier_not_enough_data", lang), type = "error", duration = 6)
      return()
    }

    params <- qc_param_cols()
    score_tbl <- tryCatch(
      qc_rank_replicates(
        df = selector_df,
        params = params,
        group_col = qc_group_var(),
        rep_col = "BiologicalReplicate"
      ),
      error = function(e) NULL
    )
    picked <- qc_pick_top_replicates(score_tbl, keep_n = keep_n)
    if (is.null(score_tbl) || !is.data.frame(score_tbl) || !nrow(score_tbl) || !nrow(picked)) {
      showNotification(tr_text("qc_outlier_topn_error", lang), type = "error", duration = 6)
      return()
    }

    if (!all(names(selector_state) %in% unique(as.character(picked$Group)))) {
      showNotification(tr_text("qc_outlier_topn_error", lang), type = "error", duration = 6)
      return()
    }

    selected_map <- setNames(vector("list", length(selector_state)), names(selector_state))
    changed_groups <- 0L
    for (g in names(selector_state)) {
      next_sel <- unique(as.character(picked$Replicate[picked$Group == g]))
      if (!length(next_sel)) {
        showNotification(tr_text("qc_outlier_topn_error", lang), type = "error", duration = 6)
        return()
      }
      selected_map[[g]] <- next_sel
      current <- sort(selector_state[[g]]$selected)
      if (!identical(current, sort(next_sel))) {
        changed_groups <- changed_groups + 1L
      }
    }

    if (changed_groups <= 0L) {
      showNotification(tr_text("qc_outlier_topn_no_change", lang), type = "warning", duration = 6)
      return()
    }

    qc_apply_replicate_selection(selector_state, selected_map)

    kept_counts <- vapply(selected_map, length, integer(1))
    kept_total <- sum(kept_counts)
    group_msg <- paste(sprintf("%s=%s", names(kept_counts), kept_counts), collapse = "; ")
    showNotification(
      sprintf(tr_text("qc_outlier_topn_done", lang), kept_total, group_msg),
      type = "message",
      duration = 8
    )
  }, ignoreInit = TRUE)

  observeEvent(input$qc_apply_tech_outlier_exclusion, {
    lang <- input$app_lang %||% i18n_lang
    params <- qc_tech_param_cols()
    if (!isTRUE(qc_tech_available()) || !length(params)) {
      showNotification(tr_text("qc_tech_apply_error", lang), type = "error", duration = 6)
      return()
    }

    selector_state <- qc_tech_selector_state()
    if (!length(selector_state)) {
      showNotification(tr_text("qc_tech_not_enough_data", lang), type = "error", duration = 6)
      return()
    }

    tech_df <- qc_apply_tech_selection_df(qc_tech_source_df())
    out_reps <- tryCatch(
      qc_outlier_replicates(
        df = tech_df,
        params = params,
        group_col = qc_group_var(),
        rep_col = "TechnicalReplicate",
        subgroup_col = "BiologicalReplicate",
        iqr_mult = input$qc_tech_outlier_iqr_mult %||% 1.5
      ),
      error = function(e) NULL
    )

    if (is.null(out_reps)) {
      showNotification(tr_text("qc_tech_apply_error", lang), type = "error", duration = 6)
      return()
    }
    if (!nrow(out_reps)) {
      showNotification(tr_text("qc_tech_no_matches", lang), type = "error", duration = 6)
      return()
    }

    selected_map <- list()
    changed <- 0L
    for (key in names(selector_state)) {
      info <- selector_state[[key]]
      flagged <- as.character(out_reps$Replicate[
        out_reps$Group == info$group &
          out_reps$Subgroup == info$biorep
      ])
      next_sel <- setdiff(info$selected, flagged)
      selected_map[[key]] <- next_sel
      changed <- changed + length(setdiff(info$selected, next_sel))
    }

    if (changed <= 0L) {
      showNotification(tr_text("qc_tech_no_matches", lang), type = "error", duration = 6)
      return()
    }

    qc_apply_tech_selection(selector_state, selected_map)

    per_group <- out_reps %>%
      dplyr::count(Group, Subgroup, name = "n") %>%
      dplyr::arrange(Group, Subgroup)
    group_msg <- paste(
      sprintf("%s [%s]=%s", per_group$Group, per_group$Subgroup, per_group$n),
      collapse = "; "
    )
    showNotification(
      sprintf(tr_text("qc_tech_apply_done", lang), changed, group_msg),
      type = "message",
      duration = 8
    )
  }, ignoreInit = TRUE)

  observeEvent(input$qc_tech_select_all, {
    if (!isTRUE(qc_tech_available())) return()
    selector_state <- qc_tech_selector_state()
    if (!length(selector_state)) return()

    selected_map <- setNames(vector("list", length(selector_state)), names(selector_state))
    for (key in names(selector_state)) {
      selected_map[[key]] <- selector_state[[key]]$choices
    }
    qc_apply_tech_selection(selector_state, selected_map)
  }, ignoreInit = TRUE)

  observeEvent(input$qc_tech_deselect_all, {
    if (!isTRUE(qc_tech_available())) return()
    selector_state <- qc_tech_selector_state()
    if (!length(selector_state)) return()

    selected_map <- setNames(vector("list", length(selector_state)), names(selector_state))
    for (key in names(selector_state)) {
      selected_map[[key]] <- character(0)
    }
    qc_apply_tech_selection(selector_state, selected_map)
  }, ignoreInit = TRUE)

  observeEvent(input$qc_apply_tech_top_n, {
    lang <- input$app_lang %||% i18n_lang
    keep_n <- suppressWarnings(as.integer(input$qc_tech_keep_top_n)[1])
    if (!is.finite(keep_n) || keep_n <= 0) {
      showNotification(tr_text("qc_tech_topn_invalid", lang), type = "error", duration = 6)
      return()
    }
    if (!isTRUE(qc_tech_available())) {
      showNotification(tr_text("qc_tech_not_enough_data", lang), type = "error", duration = 6)
      return()
    }

    selector_state <- qc_tech_selector_state()
    if (!length(selector_state)) {
      showNotification(tr_text("qc_tech_not_enough_data", lang), type = "error", duration = 6)
      return()
    }

    tech_df <- qc_apply_tech_selection_df(qc_tech_source_df())
    tech_params <- qc_tech_param_cols()
    if (!length(tech_params)) {
      showNotification(tr_text("qc_tech_topn_error", lang), type = "error", duration = 6)
      return()
    }
    score_tbl <- tryCatch(
      qc_rank_replicates(
        df = tech_df,
        params = tech_params,
        group_col = qc_group_var(),
        rep_col = "TechnicalReplicate",
        subgroup_col = "BiologicalReplicate"
      ),
      error = function(e) NULL
    )
    picked <- qc_pick_top_replicates(score_tbl, keep_n = keep_n)
    if (is.null(score_tbl) || !is.data.frame(score_tbl) || !nrow(score_tbl) || !nrow(picked)) {
      showNotification(tr_text("qc_tech_topn_error", lang), type = "error", duration = 6)
      return()
    }

    selected_map <- setNames(vector("list", length(selector_state)), names(selector_state))
    changed_groups <- 0L
    for (key in names(selector_state)) {
      info <- selector_state[[key]]
      next_sel <- unique(as.character(picked$Replicate[
        picked$Group == info$group &
          picked$Subgroup == info$biorep
      ]))
      if (!length(next_sel)) {
        showNotification(tr_text("qc_tech_topn_error", lang), type = "error", duration = 6)
        return()
      }
      selected_map[[key]] <- normalize_rep_selection(next_sel)
      current <- sort(info$selected)
      if (!identical(current, sort(selected_map[[key]]))) {
        changed_groups <- changed_groups + 1L
      }
    }

    if (changed_groups <= 0L) {
      showNotification(tr_text("qc_tech_topn_no_change", lang), type = "warning", duration = 6)
      return()
    }

    qc_apply_tech_selection(selector_state, selected_map)

    kept_counts <- vapply(selected_map, length, integer(1))
    kept_total <- sum(kept_counts)
    detail_labels <- vapply(selector_state, function(info) {
      sprintf("%s [%s]", info$group, info$biorep)
    }, character(1))
    group_msg <- paste(sprintf("%s=%s", detail_labels, kept_counts), collapse = "; ")
    showNotification(
      sprintf(tr_text("qc_tech_topn_done", lang), kept_total, group_msg),
      type = "message",
      duration = 8
    )
  }, ignoreInit = TRUE)

  outputOptions(output, "qcTechTabAvailable", suspendWhenHidden = FALSE)

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

  adv_pal_group_overrides <- reactiveVal(setNames(character(0), character(0)))

  palette_group_levels <- reactive({
    tipo  <- input$tipo %||% ""
    scope <- input$scope %||% "Por Cepa"
    if (!tipo %in% c("Boxplot", "Barras", "Violin", "Curvas", "Apiladas", "Heatmap")) {
      return(character(0))
    }

    if (identical(tipo, "Apiladas")) {
      params <- input$stackParams %||% character(0)
      if (!length(params)) return(character(0))
      order_input <- trimws(strsplit(input$orderStack %||% "", ",")[[1]])
      order_levels <- intersect(order_input[nzchar(order_input)], params)
      stack_levels <- if (length(order_levels)) order_levels else params
      return(as.character(stack_levels))
    }


    if (identical(tipo, "Curvas")) {
      if (is.null(curve_data()) || is.null(datos_combinados())) return(character(0))
      df_cur <- curve_long_df()
      if (is.null(df_cur) || !nrow(df_cur)) return(character(0))
      if (scope == "Por Cepa") {
        if (is.null(input$strain)) return(character(0))
        df_cur <- df_cur %>%
          filter(Strain == input$strain) %>%
          order_filter_strain() %>%
          filter_reps_strain()
        if (!nrow(df_cur)) return(character(0))
        labels <- df_cur %>%
          distinct(Media, Orden) %>%
          arrange(Orden) %>%
          pull(Media)
      } else {
        df_cur <- df_cur %>%
          order_filter_group() %>%
          filter_reps_group()
        if (!nrow(df_cur)) return(character(0))
        if (isTRUE(input$labelMode)) {
          df_base <- datos_agrupados()
          if (is.null(df_base) || !nrow(df_base)) return(character(0))
          available <- unique(as.character(df_cur$Strain))
          strain_order <- df_base %>%
            group_by(Strain) %>%
            summarise(minO = min(Orden), .groups = "drop") %>%
            arrange(minO) %>%
            pull(Strain)
          return(as.character(intersect(strain_order, available)))
        }
        labels <- df_cur$Label
      }
      if (is.factor(labels)) {
        return(levels(labels))
      }
      return(unique(as.character(labels)))
    }

    if (scope == "Por Cepa" && is.null(input$strain)) return(character(0))
    df <- scoped_plot_df()
    if (is.null(df) || !nrow(df)) return(character(0))
    labels <- if (scope == "Por Cepa") df$Media else df$Label
    if (isTRUE(input$x_wrap)) {
      labels <- wrap_label(labels, lines = input$x_wrap_lines)
    }
    if (is.factor(labels)) {
      return(levels(labels))
    }
    unique(as.character(labels))
  })

  output$adv_pal_group_ui <- renderUI({
    input$app_lang
    groups <- palette_group_levels()
    if (!length(groups)) {
      return(helpText(tr("palette_group_empty")))
    }
    selected <- intersect(input$adv_pal_group_sel %||% character(0), groups)
    tagList(
      selectizeInput(
        "adv_pal_group_sel",
        tr("palette_group_select"),
        choices = groups,
        selected = selected,
        multiple = TRUE,
        options = list(
          placeholder = tr("palette_group_placeholder"),
          plugins = list("remove_button")
        )
      ),
      fluidRow(
        column(
          6,
          div(
            class = "form-group shiny-input-container",
            tags$label(`for` = "adv_pal_group_color", tr("palette_group_color")),
            tags$input(
              id = "adv_pal_group_color",
              type = "color",
              value = input$adv_pal_group_color %||% "#E15759",
              class = "form-control form-control-color",
              style = "width: 100%; height: 38px;",
              oninput = "Shiny.setInputValue('adv_pal_group_color', this.value, {priority: 'event'});"
            )
          )
        ),
        column(
          6,
          div(
            style = "padding-top: 25px;",
            actionButton(
              "adv_pal_group_apply",
              tr("palette_group_apply"),
              class = "btn btn-primary btn-sm"
            )
          )
        )
      )
    )
  })

  observeEvent(input$adv_pal_group_apply, {
    if (!isTRUE(input$adv_pal_group_enable)) return()
    groups <- input$adv_pal_group_sel %||% character(0)
    if (!length(groups)) return()
    color <- input$adv_pal_group_color %||% "#E15759"
    if (!nzchar(color)) return()
    current <- adv_pal_group_overrides()
    current[groups] <- color
    adv_pal_group_overrides(current)
  })

  adv_extra_info <- data.frame(
    name = c(
      "Viridis", "Plasma", "Magma", "Inferno", "Cividis",
      "Aqua", "Rose", "Amber", "Slate", "Forest", "Ocean", "BlueWhiteRed", "SkyWhiteRed",
      "BlueRed", "PurpleOrange", "GreenBrown",
      "BlueOrange", "TealRed", "PurpleGreen", "CyanMagenta", "BrownTeal",
      "Hue", "OkabeIto", "Tableau", "Kelly", "TolBright", "TolMuted", "TolLight",
      "D3Category10", "D3Category20"
    ),
    category = c(
      "seq", "seq", "seq", "seq", "seq",
      "seq", "seq", "seq", "seq", "seq", "seq", "seq", "seq",
      "div", "div", "div",
      "div", "div", "div", "div", "div",
      "qual", "qual", "qual", "qual", "qual", "qual", "qual",
      "qual", "qual"
    ),
    maxcolors = c(
      Inf, Inf, Inf, Inf, Inf,
      Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf,
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
             "BlueWhiteRed" = grDevices::colorRampPalette(
               c("blue", "white", "red")
             )(n),
             "SkyWhiteRed"  = grDevices::colorRampPalette(
               c("#55BDEB", "#FFFFFF", "#E64B4B")
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
  
  apply_palette_overrides <- function(pal, levels_vec = NULL) {
    if (!isTRUE(input$adv_pal_group_enable)) return(pal)
    overrides <- adv_pal_group_overrides()
    if (is.null(overrides) || !length(overrides)) return(pal)
    if (!length(pal)) return(pal)
    if (is.null(names(pal)) && !is.null(levels_vec)) {
      names(pal) <- levels_vec
    }
    if (is.null(names(pal)) || !length(names(pal))) return(pal)
    hits <- intersect(names(pal), names(overrides))
    if (length(hits)) {
      pal[hits] <- overrides[hits]
    }
    pal
  }

  palette_for_levels <- function(levels_vec) {
    levels_vec <- as.character(levels_vec)
    if (!length(levels_vec)) return(character(0))
    pal <- get_palette(length(levels_vec))
    names(pal) <- levels_vec
    apply_palette_overrides(pal, levels_vec)
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
    if (!length(levels_vec)) return(character(0))
    if (isTRUE(input$repeat_colors_combined)) {  
      col_map <- palette_by_strain(df_lab)  
      if (!is.null(col_map) && length(col_map)) {  
        pal <- col_map[match(levels_vec, names(col_map))]  
        names(pal) <- levels_vec  
        return(apply_palette_overrides(pal, levels_vec))  
      }  
    }  
    pal <- get_palette(length(levels_vec))  
    names(pal) <- levels_vec  
    apply_palette_overrides(pal, levels_vec)  
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

  get_extra_margin_inputs <- function() {
    extra_right  <- as.numeric(input$margin_right_adj  %||% 0)
    extra_bottom <- as.numeric(input$margin_bottom_adj %||% 0)
    if (!is.finite(extra_right) || extra_right < 0) extra_right <- 0
    if (!is.finite(extra_bottom) || extra_bottom < 0) extra_bottom <- 0
    list(right = extra_right, bottom = extra_bottom)
  }

  effective_plot_width <- function(width = input$plot_w) {
    base <- as.numeric(width %||% input$plot_w %||% 1000)
    if (!is.finite(base) || base <= 0) base <- 1000
    extra <- get_extra_margin_inputs()$right
    base + extra
  }

  effective_plot_height <- function(height = input$plot_h) {
    base <- as.numeric(height %||% input$plot_h %||% 700)
    if (!is.finite(base) || base <= 0) base <- 700
    extra <- get_extra_margin_inputs()$bottom
    base + extra
  }

  apply_plotly_autofit <- function(plt,
                                   min_right = 0,
                                   min_bottom = 0,
                                   min_left = 0,
                                   min_top = 0,
                                   force_legend_right = FALSE) {
    plotly_autofit_widget(
      plt,
      min_right = min_right,
      min_bottom = min_bottom,
      min_left = min_left,
      min_top = min_top,
      force_legend_right = force_legend_right
    )
  }

  estimate_legend_right_margin <- function(plt) {
    if (is.null(plt) || is.null(plt$x$data) || !length(plt$x$data)) return(0)
    traces <- plt$x$data
    names_vec <- vapply(traces, function(tr) {
      show_leg <- tr$showlegend
      if (!is.null(show_leg) && isFALSE(show_leg)) return("")
      nm <- tr$name %||% tr$legendgroup %||% ""
      nm <- sanitize_curve_label(nm)
      if (length(nm) > 1) nm <- nm[[1]]
      nm <- as.character(nm %||% "")
      if (!nzchar(nm)) return("")
      nm
    }, character(1))
    names_vec <- unique(names_vec[nzchar(names_vec)])
    if (!length(names_vec)) return(0)
    fs <- suppressWarnings(as.numeric(input$fs_legend %||% 12))
    if (!is.finite(fs) || fs <= 0) fs <- 12
    max_chars <- max(nchar(names_vec, type = "width"), na.rm = TRUE)
    char_px <- max(6.5, fs * 0.58)
    key_px <- max(34, fs * 2.8)
    pad_px <- 28
    est <- key_px + (max_chars * char_px) + pad_px
    as.numeric(min(520, max(140, round(est))))
  }

  apply_margin_inputs_to_plotly <- function(plt, legend_in_margin = FALSE, expand_canvas = TRUE) {
    if (is.null(plt)) return(NULL)
    extra <- get_extra_margin_inputs()
    legend_margin_x <- 1.02
    legend_min_right <- if (isTRUE(legend_in_margin)) estimate_legend_right_margin(plt) else 0
    if (extra$right == 0 && extra$bottom == 0) {
      layout_obj <- plt$x$layout %||% list()
      cur_margin <- layout_obj$margin %||% list(t = 0, r = 0, b = 0, l = 0)
      if (isTRUE(legend_in_margin)) {
        cur_legend <- layout_obj$legend %||% list()
        cur_legend$xref <- "paper"
        cur_legend$xanchor <- "left"
        cur_legend$x <- legend_margin_x
        layout_obj$legend <- cur_legend
        plt$x$layout <- layout_obj
      }
      return(apply_plotly_autofit(
        plt,
        min_right = max(cur_margin$r %||% 0, legend_min_right),
        min_bottom = cur_margin$b %||% 0,
        min_left = cur_margin$l %||% 0,
        min_top = cur_margin$t %||% 0,
        force_legend_right = isTRUE(legend_in_margin)
      ))
    }
    base_w <- as.numeric(input$plot_w %||% 1000)
    base_h <- as.numeric(input$plot_h %||% 700)
    if (!is.finite(base_w) || base_w <= 0) base_w <- 1000
    if (!is.finite(base_h) || base_h <= 0) base_h <- 700
    layout_obj <- plt$x$layout %||% list()
    cur_margin <- layout_obj$margin %||% list(t = 0, r = 0, b = 0, l = 0)
    cur_width <- as.numeric(layout_obj$width %||% base_w)
    cur_height <- as.numeric(layout_obj$height %||% base_h)
    if (!is.finite(cur_width) || cur_width <= 0) cur_width <- base_w
    if (!is.finite(cur_height) || cur_height <= 0) cur_height <- base_h
    if (isTRUE(expand_canvas)) {
      new_w <- cur_width + extra$right
      new_h <- cur_height + extra$bottom
    } else {
      new_w <- cur_width
      new_h <- cur_height
    }
    cur_margin$r <- (cur_margin$r %||% 0) + extra$right
    cur_margin$b <- (cur_margin$b %||% 0) + extra$bottom
    if (isTRUE(legend_in_margin) && is.finite(legend_min_right)) {
      cur_margin$r <- max(cur_margin$r %||% 0, legend_min_right + extra$right)
    }
    layout_obj$margin <- cur_margin
    layout_obj$width <- new_w
    layout_obj$height <- new_h
    layout_obj$autosize <- FALSE

    if (isTRUE(legend_in_margin) || extra$right > 0) {
      cur_legend <- layout_obj$legend %||% list()
      if (isTRUE(legend_in_margin)) {
        cur_legend$xref <- "paper"
        cur_legend$xanchor <- "left"
        cur_legend$x <- legend_margin_x
      } else {
        cur_legend$xref <- "paper"
        cur_legend$xanchor <- "right"
        cur_legend$x <- 1
      }
      layout_obj$legend <- cur_legend
    }

    plt$x$layout <- layout_obj
    apply_plotly_autofit(
      plt,
      min_right = cur_margin$r %||% 0,
      min_bottom = cur_margin$b %||% 0,
      min_left = cur_margin$l %||% 0,
      min_top = cur_margin$t %||% 0,
      force_legend_right = isTRUE(legend_in_margin)
    )
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
                                  delay = 0.5,
                                  zoom  = 1,
                                  background = "white") {
    bg <- as.character(background %||% "white")
    if (!nzchar(bg)) bg <- "white"
    p <- p %>% layout(
      paper_bgcolor = bg,
      plot_bgcolor  = bg
    )

    tmp_html <- tempfile(fileext = ".html")
    tmp_lib <- tempfile("plotly_lib_")
    dir.create(tmp_lib, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(c(tmp_html, tmp_lib), recursive = TRUE, force = TRUE), add = TRUE)

    # Faster than self-contained HTML and more stable under heavy load.
    htmlwidgets::saveWidget(
      p,
      tmp_html,
      selfcontained = FALSE,
      libdir = tmp_lib
    )

    attempts <- list(
      list(delay = delay, zoom = zoom),
      list(delay = max(delay, 1), zoom = max(1, zoom - 1))
    )
    last_err <- NULL
    for (att in attempts) {
      ok <- tryCatch({
        webshot2::webshot(
          url = tmp_html,
          file = file,
          vwidth = width,
          vheight = height,
          delay = att$delay,
          zoom = att$zoom,
          max_concurrent = 1,
          quiet = TRUE
        )
        file.exists(file) && is.finite(file.info(file)$size) && file.info(file)$size > 0
      }, error = function(e) {
        last_err <<- conditionMessage(e)
        FALSE
      })
      if (isTRUE(ok)) return(invisible(TRUE))
    }
    stop(sprintf("Plot export failed (Chromote/webshot timeout): %s", last_err %||% "unknown error"))
  }

  resolve_prefixed_param_col <- function(df, prefix, param_name) {
    if (is.null(df) || !is.data.frame(df) || is.null(param_name)) return(NULL)
    base <- trimws(as.character(sub("_Norm$", "", param_name)))
    if (!nzchar(base)) return(NULL)
    direct <- paste0(prefix, base)
    if (direct %in% names(df)) return(direct)

    norm_key <- function(x) {
      x <- iconv(as.character(x), from = "", to = "ASCII//TRANSLIT")
      x <- gsub("[^[:alnum:]]+", "", x)
      tolower(trimws(x))
    }

    nms <- names(df)
    pref_idx <- startsWith(nms, prefix)
    if (!any(pref_idx)) return(NULL)
    candidates <- nms[pref_idx]
    stripped <- sub(paste0("^", prefix), "", candidates)
    tgt <- norm_key(base)
    keys <- vapply(stripped, norm_key, character(1))
    hit <- which(keys == tgt)
    if (length(hit)) candidates[[hit[[1]]]] else NULL
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
    if ("Strain" %in% names(df_f)) df_f$Strain <- sanitize_curve_label_preserve_levels(df_f$Strain)
    if ("Media" %in% names(df_f))  df_f$Media  <- sanitize_curve_label_preserve_levels(df_f$Media)
    if ("Label" %in% names(df_f))  df_f$Label  <- sanitize_curve_label_preserve_levels(df_f$Label)
    
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
        mutate(
          Parametro = factor(Parametro, levels = stack_levels),
          !!eje_x := factor(.data[[eje_x]], levels = eje_levels)
        ) %>%
        arrange(.data[[eje_x]], Parametro)
    } else {
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
    }
    
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
    pal <- palette_for_levels(stack_levels)
    
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
            array      = ifelse(is.finite(sub$SD), sub$SD, 0), # +SD hacia arriba
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
          showgrid  = FALSE,
          automargin = TRUE
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




  build_heatmap_snapshot <- function(scope, strain = NULL) {
    df_h <- get_scope_df(scope, strain)
    if ("Strain" %in% names(df_h)) df_h$Strain <- sanitize_curve_label_preserve_levels(df_h$Strain)
    if ("Media" %in% names(df_h))  df_h$Media  <- sanitize_curve_label_preserve_levels(df_h$Media)
    if ("Label" %in% names(df_h))  df_h$Label  <- sanitize_curve_label_preserve_levels(df_h$Label)

    all_params <- as.character(plot_settings()$Parameter %||% character(0))
    all_params <- all_params[!is.na(all_params) & nzchar(all_params)]
    scale_mode <- input$heat_scale_mode %||% if (isTRUE(input$heat_norm_z)) "row" else "none"

    list(
      df_h = df_h,
      scope = scope,
      strain = as.character(strain %||% ""),
      label_mode = isTRUE(input$labelMode),
      all_params = all_params,
      params_raw = as.character(input$heat_params %||% character(0)),
      high_dim = is_large_param_set(all_params),
      do_norm = isTRUE(input$doNorm),
      has_ctrl_selected = has_ctrl_selected(),
      show_param_labels = isTRUE(input$heat_show_param_labels %||% TRUE),
      orientation = as.character(input$heat_orientation %||% "params_rows"),
      scale_mode = scale_mode,
      cluster_rows = isTRUE(input$heat_cluster_rows),
      cluster_cols = isTRUE(input$heat_cluster_cols),
      hclust_method = input$heat_hclust_method %||% "ward.D2",
      k_rows = input$heat_k_rows %||% 2L,
      k_cols = input$heat_k_cols %||% 2L,
      scope_groups = as.character(input$showGroups %||% character(0)),
      reps_strain = reps_strain_selected(),
      reps_group = reps_group_selected(),
      rm_reps = as.character(input$rm_reps_all %||% character(0))
    )
  }

  heatmap_snapshot_key <- function(snapshot) {
    paste(
      "heatmap",
      input_file_stamp(),
      as.character(snapshot$scope %||% ""),
      as.character(snapshot$strain %||% ""),
      as.character(snapshot$label_mode),
      stable_key_value(snapshot$params_raw),
      as.character(snapshot$high_dim),
      as.character(snapshot$do_norm),
      as.character(snapshot$has_ctrl_selected),
      as.character(snapshot$show_param_labels),
      as.character(snapshot$orientation),
      as.character(snapshot$scale_mode),
      as.character(snapshot$cluster_rows),
      as.character(snapshot$cluster_cols),
      as.character(snapshot$hclust_method),
      as.character(snapshot$k_rows),
      as.character(snapshot$k_cols),
      stable_key_value(snapshot$scope_groups),
      stable_key_value(snapshot$reps_strain),
      stable_key_value(snapshot$reps_group),
      stable_key_value(snapshot$rm_reps),
      sep = "||"
    )
  }

  compute_heatmap_payload_from_snapshot <- function(snapshot) {
    prepare_heatmap_payload(
      df_h = snapshot$df_h,
      scope = snapshot$scope,
      label_mode = isTRUE(snapshot$label_mode),
      all_params = snapshot$all_params,
      params_raw = snapshot$params_raw,
      high_dim = isTRUE(snapshot$high_dim),
      do_norm = isTRUE(snapshot$do_norm),
      has_ctrl_selected = isTRUE(snapshot$has_ctrl_selected),
      show_param_labels = isTRUE(snapshot$show_param_labels),
      orientation = snapshot$orientation,
      scale_mode = snapshot$scale_mode,
      cluster_rows = isTRUE(snapshot$cluster_rows),
      cluster_cols = isTRUE(snapshot$cluster_cols),
      hclust_method = snapshot$hclust_method,
      k_rows = snapshot$k_rows,
      k_cols = snapshot$k_cols,
      should_abort = is_session_closing
    )
  }

  get_heatmap_payload_cached <- function(snapshot) {
    key <- heatmap_snapshot_key(snapshot)
    cached <- plot_payload_cache_get("heatmap", key)
    if (!is.null(cached)) return(cached)
    payload <- compute_heatmap_payload_from_snapshot(snapshot)
    plot_payload_cache_set("heatmap", key, payload, max_entries = 8L)
    payload
  }

  get_corrm_payload_cached <- function(snapshot) {
    key <- corrm_snapshot_key(snapshot)
    cached <- plot_payload_cache_get("corrm", key)
    if (!is.null(cached)) return(cached)
    payload <- compute_corrm_payload_from_snapshot(snapshot)
    plot_payload_cache_set("corrm", key, payload, max_entries = 8L)
    payload
  }

  compute_heatmap_payload <- function(scope, strain = NULL) {
    snapshot <- build_heatmap_snapshot(scope, strain)
    get_heatmap_payload_cached(snapshot)
  }

  heatmap_payload_current <- reactive({
    req(plot_settings())
    scope_sel <- if (identical(input$scope, "Combinado")) "Combinado" else "Por Cepa"
    strain_sel <- if (identical(scope_sel, "Por Cepa")) input$strain else NULL
    compute_heatmap_payload(scope_sel, strain_sel)
  })

  build_corrm_snapshot <- function(scope, strain = NULL) {
    df_m <- get_scope_df(scope, strain)
    all_params <- as.character(plot_settings()$Parameter %||% character(0))
    all_params <- all_params[!is.na(all_params) & nzchar(all_params)]

    list(
      df_m = df_m,
      scope = scope,
      strain = as.character(strain %||% ""),
      all_params = all_params,
      params_raw = as.character(input$corrm_params %||% character(0)),
      high_dim = is_large_param_set(all_params),
      do_norm = isTRUE(input$doNorm),
      has_ctrl_selected = has_ctrl_selected(),
      corr_method = input$corrm_method %||% "spearman",
      adjust_method = input$corrm_adjust %||% "none",
      order_profile = isTRUE(input$corrm_order_profile),
      show_sig_only = isTRUE(input$corrm_show_sig),
      scope_groups = as.character(input$showGroups %||% character(0)),
      reps_strain = reps_strain_selected(),
      reps_group = reps_group_selected(),
      rm_reps = as.character(input$rm_reps_all %||% character(0))
    )
  }

  corrm_snapshot_key <- function(snapshot) {
    paste(
      "corrm",
      input_file_stamp(),
      as.character(snapshot$scope %||% ""),
      as.character(snapshot$strain %||% ""),
      stable_key_value(snapshot$params_raw),
      as.character(snapshot$high_dim),
      as.character(snapshot$do_norm),
      as.character(snapshot$has_ctrl_selected),
      as.character(snapshot$corr_method),
      as.character(snapshot$adjust_method),
      as.character(snapshot$order_profile),
      as.character(snapshot$show_sig_only),
      stable_key_value(snapshot$scope_groups),
      stable_key_value(snapshot$reps_strain),
      stable_key_value(snapshot$reps_group),
      stable_key_value(snapshot$rm_reps),
      sep = "||"
    )
  }

  compute_corrm_payload_from_snapshot <- function(snapshot) {
    prepare_corr_matrix_payload(
      df_m = snapshot$df_m,
      all_params = snapshot$all_params,
      params_raw = snapshot$params_raw,
      high_dim = isTRUE(snapshot$high_dim),
      do_norm = isTRUE(snapshot$do_norm),
      has_ctrl_selected = isTRUE(snapshot$has_ctrl_selected),
      corr_method = snapshot$corr_method,
      adjust_method = snapshot$adjust_method,
      order_profile = isTRUE(snapshot$order_profile),
      show_sig_only = isTRUE(snapshot$show_sig_only),
      should_abort = is_session_closing
    )
  }

  # Heatmap/Correlation Matrix prefetch disabled; rendering is synchronous.

  # --------------------------------------------------------------------  
  # FunciÃƒÂ³n auxiliar para exportar PNG sin tocar <input> (para ZIP)  
  #         Ã¢â€“Âº MISMO LOOK que plot_base()  
  # --------------------------------------------------------------------  

  build_plot <- function(scope, strain = NULL, tipo, for_interactive = FALSE) {  
    lang <- input$app_lang %||% i18n_lang
    req(plot_settings())
    if (!identical(tipo, "Heatmap")) req(input$param)
    
    # Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬ Nuevo bloque para normalizaciÃƒÂ³n Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬  
    rawParam <- enc2utf8(input$param %||% "")  
    is_norm  <- isTRUE(input$doNorm)  
    msg_no_data_sel <- tr_text("no_data_selection", lang)
    add_whisker_caps <- draw_whisker_caps
    add_black_t_errorbar <- function(p, summary_df, x_col, cap_width = 0.18, lw = 0.8) {
      if (!is.data.frame(summary_df) || !nrow(summary_df)) return(p)
      if (!all(c(x_col, "Mean", "SD") %in% names(summary_df))) return(p)
      lw_num <- suppressWarnings(as.numeric(lw))
      if (!is.finite(lw_num) || lw_num <= 0) lw_num <- 0.8
      cap_num <- suppressWarnings(as.numeric(cap_width))
      if (!is.finite(cap_num) || cap_num <= 0) cap_num <- 0.18
      err_df <- summary_df %>%
        mutate(
          x_val = .data[[x_col]],
          ystart = suppressWarnings(as.numeric(Mean)),
          yend = ystart + ifelse(is.finite(SD), pmax(SD, 0), 0)
        ) %>%
        filter(!is.na(x_val), is.finite(ystart), is.finite(yend), yend > ystart)
      if (!nrow(err_df)) return(p)
      p +
        geom_linerange(
          data = err_df,
          inherit.aes = FALSE,
          aes(x = x_val, ymin = ystart, ymax = yend),
          linewidth = lw_num,
          colour = "black",
          show.legend = FALSE
        ) +
        geom_errorbar(
          data = err_df,
          inherit.aes = FALSE,
          aes(x = x_val, ymin = yend, ymax = yend),
          width = cap_num,
          linewidth = lw_num,
          colour = "black",
          show.legend = FALSE
        )
    }
    downsample_points_by_group <- function(df, group_col, cap_total = 7000L, min_per_group = 80L) {
      if (!is.data.frame(df) || !nrow(df) || !group_col %in% names(df)) return(df)
      cap_total <- suppressWarnings(as.integer(cap_total))
      min_per_group <- suppressWarnings(as.integer(min_per_group))
      if (!is.finite(cap_total) || cap_total <= 0) return(df)
      if (!is.finite(min_per_group) || min_per_group <= 0) min_per_group <- 40L
      if (nrow(df) <= cap_total) return(df)

      n_groups <- dplyr::n_distinct(df[[group_col]])
      if (!is.finite(n_groups) || n_groups <= 0) n_groups <- 1L
      per_group <- max(min_per_group, floor(cap_total / n_groups))
      if (!is.finite(per_group) || per_group <= 0) per_group <- min_per_group

      df %>%
        dplyr::group_by(.data[[group_col]]) %>%
        dplyr::mutate(
          .idx = dplyr::row_number(),
          .step = pmax(1L, ceiling(dplyr::n() / per_group))
        ) %>%
        dplyr::filter((.idx - 1L) %% .step == 0L) %>%
        dplyr::ungroup() %>%
        dplyr::select(-.idx, -.step)
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
    if ("Strain" %in% names(scope_df)) scope_df$Strain <- sanitize_curve_label_preserve_levels(scope_df$Strain)
    if ("Media" %in% names(scope_df))  scope_df$Media  <- sanitize_curve_label_preserve_levels(scope_df$Media)
    if ("Label" %in% names(scope_df))  scope_df$Label  <- sanitize_curve_label_preserve_levels(scope_df$Label)
    box_stats <- NULL

    # lÃƒÂ­mites segÃƒÂºn param_sel
    lims    <- get_ylim(param_sel)
    ymax    <- lims$ymax
    ybreak  <- lims$ybreak
    if (!is.finite(ymax)  || ymax  <= 0) ymax  <- 1
    if (!is.finite(ybreak) || ybreak <= 0) ybreak <- ymax / 5
    
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
    
    # --- 3.x) Stacked bars -----------------------------------------------
    if (tipo == "Apiladas") {
      return(
        build_apiladas_plot_impl(list(
          scope = scope,
          scope_df = scope_df,
          input = input,
          lang = lang,
          ps = ps,
          fs_title = fs_title,
          fs_axis = fs_axis,
          fs_legend = fs_legend,
          axis_size = axis_size,
          tr_text = tr_text,
          is_summary_mode = is_summary_mode,
          resolve_prefixed_param_col = resolve_prefixed_param_col,
          wrap_label = wrap_label,
          get_x_angle = get_x_angle,
          get_bottom_margin = get_bottom_margin,
          palette_for_levels = palette_for_levels,
          margin_adj = margin_adj,
          apply_sig_layers = apply_sig_layers
        ))
      )
    }

    # --- 3.x) Heatmap -----------------------------------------------------
    if (tipo == "Heatmap") {
      req_scope <- as.character(scope %||% "")
      snapshot <- tryCatch(
        build_heatmap_snapshot(req_scope, if (identical(req_scope, "Por Cepa")) strain else NULL),
        error = function(e) NULL
      )
      validate(need(!is.null(snapshot), msg_no_data_sel))
      payload <- tryCatch(
        get_heatmap_payload_cached(snapshot),
        error = function(e) NULL
      )
      validate(need(!is.null(payload), msg_no_data_sel))

      plot_df <- payload$plot_df
      scale_mode <- payload$scale_mode
      n_rows <- payload$n_rows
      n_cols <- payload$n_cols
      row_breaks <- suppressWarnings(as.numeric(payload$row_breaks %||% seq_len(n_rows)))
      col_breaks <- suppressWarnings(as.numeric(payload$col_breaks %||% seq_len(n_cols)))
      if (length(row_breaks) != length(payload$row_labels %||% character(0))) row_breaks <- seq_len(n_rows)
      if (length(col_breaks) != length(payload$col_labels %||% character(0))) col_breaks <- seq_len(n_cols)

      show_top_dendro <- isTRUE(input$heat_show_top_dend %||% FALSE) &&
        isTRUE(payload$can_show_top_dendro %||% FALSE)
      show_side_dendro <- isTRUE(input$heat_show_side_dend %||% FALSE) &&
        isTRUE(payload$can_show_side_dendro %||% FALSE)

      x_base_min <- suppressWarnings(as.numeric(payload$x_base_min %||% payload$x_min %||% 0.5))
      x_base_max <- suppressWarnings(as.numeric(payload$x_base_max %||% payload$x_max %||% (n_cols + 0.5)))
      y_base_max <- suppressWarnings(as.numeric(payload$y_base_max %||% payload$y_max %||% (n_rows + 0.5)))
      side_dend_gap <- suppressWarnings(as.numeric(payload$side_dend_gap %||% payload$dend_gap %||% 0.06))
      top_dend_gap <- suppressWarnings(as.numeric(payload$top_dend_gap %||% payload$dend_gap %||% 0.35))
      top_space <- suppressWarnings(as.numeric(payload$top_space %||% 0))
      side_space <- suppressWarnings(as.numeric(payload$side_space %||% 0))

      if (!is.finite(x_base_min)) x_base_min <- 0.5
      if (!is.finite(x_base_max)) x_base_max <- n_cols + 0.5
      if (!is.finite(y_base_max)) y_base_max <- n_rows + 0.5
      if (!is.finite(side_dend_gap) || side_dend_gap < 0) side_dend_gap <- 0.06
      if (!is.finite(top_dend_gap) || top_dend_gap < 0) top_dend_gap <- 0.35
      if (!is.finite(top_space) || top_space < 0) top_space <- 0
      if (!is.finite(side_space) || side_space < 0) side_space <- 0

      x_min <- if (show_side_dendro) x_base_min - side_dend_gap - side_space else x_base_min
      x_max <- x_base_max
      y_max <- if (show_top_dendro) y_base_max + top_dend_gap + top_space else y_base_max
      heat_special_palette <- NULL
      heat_breaks <- NULL
      if (isTRUE(input$adv_pal_enable)) {
        adv_name <- input$adv_pal_name %||% ""
        if (identical(adv_name, "BlueWhiteRed")) {
          heat_special_palette <- grDevices::colorRampPalette(c("blue", "white", "red"))(50)
          heat_breaks <- seq(-2, 2, length.out = 51)
        } else if (identical(adv_name, "SkyWhiteRed")) {
          heat_special_palette <- grDevices::colorRampPalette(c("#55BDEB", "#FFFFFF", "#E64B4B"))(50)
          heat_breaks <- seq(-2, 2, length.out = 51)
        }
      }

      pal9 <- get_palette(9)
      if (length(pal9) < 9 || any(!nzchar(as.character(pal9)))) {
        pal9 <- colorRampPalette(c("blue", "white", "red"))(9)
      }
      low_col <- pal9[1]
      mid_col <- pal9[5]
      high_col <- pal9[9]

      p <- ggplot(plot_df, aes(x = x, y = y, fill = Valor)) +
        geom_tile(color = NA, width = 1, height = 1)
      if (isTRUE(input$heat_show_values)) {
        p <- p + geom_text(aes(label = sprintf("%.2f", Valor)), size = 3, na.rm = TRUE)
      }
      if (isTRUE(show_top_dendro) && !is.null(payload$top_segs) && nrow(payload$top_segs) > 0) {
        p <- p + geom_segment(
          data = payload$top_segs,
          aes(x = x, y = y, xend = xend, yend = yend),
          inherit.aes = FALSE,
          linewidth = 0.35,
          colour = "black"
        )
      }
      if (isTRUE(show_side_dendro) && !is.null(payload$side_segs) && nrow(payload$side_segs) > 0) {
        p <- p + geom_segment(
          data = payload$side_segs,
          aes(x = x, y = y, xend = xend, yend = yend),
          inherit.aes = FALSE,
          linewidth = 0.35,
          colour = "black"
        )
      }
      if (!identical(scale_mode, "none")) {
        if (!is.null(heat_special_palette)) {
          p <- p +
            scale_fill_gradientn(
              colours = heat_special_palette,
              limits = c(-2, 2),
              breaks = pretty(heat_breaks, n = 5),
              oob = scales::squish
            )
        } else {
          p <- p +
            scale_fill_gradient2(
              low = low_col,
              mid = mid_col,
              high = high_col,
              midpoint = 0,
              limits = c(-2, 2),
              oob = scales::squish
            )
        }
      } else {
        val_mid <- suppressWarnings(stats::median(plot_df$Valor, na.rm = TRUE))
        if (!is.finite(val_mid)) val_mid <- 0
        p <- p +
          scale_fill_gradient2(
            low = low_col,
            mid = mid_col,
            high = high_col,
            midpoint = val_mid,
            oob = scales::squish
          )
      }
      p <- p +
        scale_x_continuous(
          breaks = col_breaks,
          labels = payload$col_labels,
          limits = c(x_min, x_max),
          expand = c(0, 0)
        ) +
        scale_y_continuous(
          breaks = row_breaks,
          labels = payload$row_labels,
          limits = c(0.5, y_max),
          expand = c(0, 0)
        ) +
        coord_cartesian(clip = "off") +
        labs(
          title = input$plotTitle,
          x = NULL,
          y = NULL
        ) +
        theme_minimal(base_size = input$base_size, base_family = "Helvetica") +
        theme(
          plot.title = element_text(size = fs_title, face = "bold"),
          axis.text.x = element_text(size = fs_axis, angle = 45, hjust = 1, colour = "black"),
          axis.text.y = element_text(size = fs_axis, colour = "black"),
          panel.grid = element_blank()
        )
      return(p)
    }

    # --- 3.x) Correlation matrix ------------------------------------------
    if (tipo == "MatrizCorrelacion") {
      snapshot <- tryCatch(
        build_corrm_snapshot(scope, strain),
        error = function(e) NULL
      )
      validate(need(!is.null(snapshot), tr_text("corr_select_two_params", lang)))

      corr_payload <- tryCatch(
        get_corrm_payload_cached(snapshot),
        error = function(e) NULL
      )
      validate(need(!is.null(corr_payload), tr_text("corr_select_two_params", lang)))
      mat_df <- corr_payload$mat_df
      params_plot <- corr_payload$params_plot

      pal9 <- get_palette(9)
      if (length(pal9) < 9 || any(!nzchar(as.character(pal9)))) {
        pal9 <- colorRampPalette(c("blue", "white", "red"))(9)
      }
      low_col <- pal9[1]
      mid_col <- pal9[5]
      high_col <- pal9[9]
      corr_special_palette <- NULL
      if (isTRUE(input$adv_pal_enable)) {
        adv_name <- input$adv_pal_name %||% ""
        if (identical(adv_name, "BlueWhiteRed")) {
          corr_special_palette <- grDevices::colorRampPalette(c("blue", "white", "red"))(50)
        } else if (identical(adv_name, "SkyWhiteRed")) {
          corr_special_palette <- grDevices::colorRampPalette(c("#55BDEB", "#FFFFFF", "#E64B4B"))(50)
        }
      }

      p <- ggplot(mat_df, aes(x = param_x, y = param_y, fill = r)) +
        geom_tile(color = "white", linewidth = 0.3) +
        geom_text(aes(label = label_txt), size = 3)
      if (!is.null(corr_special_palette)) {
        p <- p +
          scale_fill_gradientn(
            colours = corr_special_palette,
            limits = c(-1, 1),
            breaks = seq(-1, 1, by = 0.5),
            oob = scales::squish
          )
      } else {
        p <- p +
          scale_fill_gradient2(
            low = low_col,
            mid = mid_col,
            high = high_col,
            midpoint = 0,
            limits = c(-1, 1),
            oob = scales::squish
          )
      }
      p <- p +
        labs(
          title = input$plotTitle,
          x = NULL,
          y = NULL
        ) +
        theme_minimal(base_size = input$base_size, base_family = "Helvetica") +
        theme(
          plot.title = element_text(size = fs_title, face = "bold"),
          axis.text.x = element_text(size = fs_axis, angle = 45, hjust = 1, colour = "black"),
          axis.text.y = element_text(size = fs_axis, colour = "black"),
          panel.grid = element_blank()
        )
      return(p)
    }

    # --- 3.x) Correlacion -------------------------------------------------
    if (tipo == "Correlacion") {
      return(
        build_correlation_plot_impl(
          scope = scope,
          scope_df = scope_df,
          input = input,
          lang = lang,
          has_ctrl_selected = has_ctrl_selected,
          corr_adv_last_pair = corr_adv_last_pair,
          tr_text = tr_text,
          margin_adj = margin_adj
        )
      )
    }

    
    
    # --- 3.x) Curves (Por Cepa and Combinado) ---
    if (tipo == "Curvas") {
      return(
        build_curvas_plot_impl(list(
          scope = scope,
          strain = strain,
          input = input,
          lang = lang,
          curve_data = curve_data,
          curve_settings = curve_settings,
          curve_long_df = curve_long_df,
          curve_summary_mode = curve_summary_mode,
          order_filter_strain = order_filter_strain,
          filter_reps_strain = filter_reps_strain,
          order_filter_group = order_filter_group,
          filter_reps_group = filter_reps_group,
          datos_agrupados = datos_agrupados,
          sanitize_curve_label = sanitize_curve_label,
          get_bottom_margin = get_bottom_margin,
          palette_for_labels = palette_for_labels,
          palette_for_levels = palette_for_levels,
          margin_adj = margin_adj,
          tr_text = tr_text
        ))
      )
    }

    if (tipo == "Boxplot") {
      return(
        build_boxplot_plot_impl(list(
          scope = scope,
          scope_df = scope_df,
          param_sel = param_sel,
          input = input,
          msg_no_data_sel = msg_no_data_sel,
          ylab = ylab,
          ymax = ymax,
          ybreak = ybreak,
          fs_title = fs_title,
          fs_axis = fs_axis,
          axis_size = axis_size,
          colourMode = colourMode,
          box_coef = box_coef,
          for_interactive = for_interactive,
          wrap_label = wrap_label,
          palette_for_labels = palette_for_labels,
          palette_for_levels = palette_for_levels,
          get_x_angle = get_x_angle,
          get_bottom_margin = get_bottom_margin,
          margin_adj = margin_adj,
          apply_sig_layers = apply_sig_layers,
          apply_square_legend_right = apply_square_legend_right,
          legend_right_enabled = legend_right_enabled,
          add_whisker_caps = add_whisker_caps,
          downsample_points_by_group = downsample_points_by_group
        ))
      )
    }

    if (tipo == "Violin") {
      return(
        build_violin_plot_impl(list(
          scope = scope,
          scope_df = scope_df,
          param_sel = param_sel,
          input = input,
          msg_no_data_sel = msg_no_data_sel,
          ylab = ylab,
          ymax = ymax,
          ybreak = ybreak,
          fs_title = fs_title,
          fs_axis = fs_axis,
          axis_size = axis_size,
          colourMode = colourMode,
          for_interactive = for_interactive,
          wrap_label = wrap_label,
          palette_for_labels = palette_for_labels,
          palette_for_levels = palette_for_levels,
          get_x_angle = get_x_angle,
          get_bottom_margin = get_bottom_margin,
          margin_adj = margin_adj,
          apply_sig_layers = apply_sig_layers,
          apply_square_legend_right = apply_square_legend_right,
          legend_right_enabled = legend_right_enabled,
          downsample_points_by_group = downsample_points_by_group,
          box_stats = box_stats
        ))
      )
    }

    if (tipo == "Barras") {
      return(
        build_barras_plot_impl(list(
          scope = scope,
          scope_df = scope_df,
          param_sel = param_sel,
          input = input,
          msg_no_data_sel = msg_no_data_sel,
          ylab = ylab,
          ymax = ymax,
          ybreak = ybreak,
          fs_title = fs_title,
          fs_axis = fs_axis,
          axis_size = axis_size,
          colourMode = colourMode,
          wrap_label = wrap_label,
          get_x_angle = get_x_angle,
          get_bottom_margin = get_bottom_margin,
          is_summary_mode = is_summary_mode,
          resolve_prefixed_param_col = resolve_prefixed_param_col,
          palette_for_labels = palette_for_labels,
          palette_for_levels = palette_for_levels,
          legend_right_enabled = legend_right_enabled,
          add_black_t_errorbar = add_black_t_errorbar,
          margin_adj = margin_adj,
          apply_sig_layers = apply_sig_layers,
          apply_square_legend_right = apply_square_legend_right
        ))
      )
    }

    # Ã¢â‚¬â€Ã¢â‚¬â€Ã¢â‚¬â€ fallback para nunca retornar NULL Ã¢â‚¬â€Ã¢â‚¬â€Ã¢â‚¬â€  
    return(
      ggplot() +
        theme_void() +
        annotate("text", 0, 0, label = msg_no_data_sel)
    )  
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

  sanitize_plotly_display_labels <- function(plt) {
    if (is.null(plt) || is.null(plt$x$data) || !length(plt$x$data)) return(plt)

    seen_names <- character(0)
    for (i in seq_along(plt$x$data)) {
      tr <- plt$x$data[[i]]
      tr_type <- tolower(as.character(tr$type %||% ""))
      tr_mode <- tolower(as.character(tr$mode %||% ""))

      raw_name <- tr$name %||% ""
      if (!nzchar(as.character(raw_name)) && !is.null(tr$legendgroup)) {
        raw_name <- tr$legendgroup
      }
      clean_name <- sanitize_curve_label(raw_name)
      if (length(clean_name) > 1) clean_name <- clean_name[[1]]
      clean_name <- ifelse(is.na(clean_name), "", as.character(clean_name))

      if (nzchar(clean_name)) {
        tr$name <- clean_name
        if (!is.null(tr$legendgroup) && nzchar(as.character(tr$legendgroup %||% ""))) {
          tr$legendgroup <- clean_name
        }
      }

      marker_opacity <- suppressWarnings(
        as.numeric(if (is.list(tr$marker)) tr$marker$opacity else NA_real_)
      )
      is_err_helper <- tr_type %in% c("scatter", "scattergl") &&
        tr_mode == "markers" &&
        !is.null(tr$error_y) &&
        is.finite(marker_opacity) &&
        marker_opacity == 0
      if (is_err_helper) {
        tr$showlegend <- FALSE
      } else {
        show_leg <- tr$showlegend
        if (is.null(show_leg)) show_leg <- TRUE
        if (isTRUE(show_leg) && nzchar(clean_name)) {
          if (clean_name %in% seen_names) {
            tr$showlegend <- FALSE
          } else {
            seen_names <- c(seen_names, clean_name)
          }
        }
      }

      plt$x$data[[i]] <- tr
    }

    if (!is.null(plt$x$layout$xaxis$ticktext)) {
      ticks <- unlist(plt$x$layout$xaxis$ticktext, use.names = FALSE)
      if (length(ticks)) {
        plt$x$layout$xaxis$ticktext <- sanitize_curve_label(ticks)
      }
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
      hovermode = "closest",
      xaxis     = list(automargin = TRUE),
      yaxis     = list(automargin = TRUE)
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
      clip_types <- c("scatter", "scattergl", "bar", "histogram")
      plt$x$data <- lapply(plt$x$data, function(tr) {
        tr_type <- tolower(tr$type %||% "")
        if (!tr_type %in% clip_types) return(tr)
        tr$cliponaxis <- FALSE
        tr
      })
    }
    
    # 4. Devuelve SIEMPRE el objeto plotly resultante
    return(sanitize_plotly_display_labels(plt))
  }
  
  
  # ---- plot_base: la versiÃƒÂ³n Ã¢â‚¬Å“reactiveÃ¢â‚¬Â que usa la interfaz actual ----  
  plot_base <- reactive({  
    scope_sel  <- if (input$scope == "Combinado") "Combinado" else "Por Cepa"  
    strain_sel <- if (scope_sel == "Por Cepa") input$strain else NULL
    lang <- input$app_lang %||% i18n_lang
    msg_no_data <- tr_text("no_data_plot", lang)

    tryCatch(
      {
        if (input$tipo == "Apiladas") {
          # Devuelve un objeto *plotly* listo
          build_plotly_stack(
            scope_sel,
            strain_sel,
            width  = input$plot_w,
            height = input$plot_h
          )
        } else {
          # Todo lo demÃƒÂ¡s sigue con tu funciÃƒÂ³n ggplot2
          build_plot(scope_sel, strain_sel, input$tipo, for_interactive = TRUE)
        }
      },
      error = function(e) {
        msg <- conditionMessage(e)
        is_expected_transient <- inherits(e, "shiny.silent.error") ||
          grepl("need at least 3 points|no data to plot", msg, ignore.case = TRUE)
        if (!is_expected_transient) {
          showNotification(
            paste(msg_no_data, "-", msg),
            type = "error",
            duration = 6
          )
        }
        ggplot() +
          theme_void() +
          annotate("text", 0, 0, label = if (is_expected_transient && nzchar(msg)) msg else msg_no_data)
      }
    )
  })  
  # --- Salidas ---  
  output$plotInteractivoUI <- renderUI({
    base_w <- as.numeric(input$plot_w %||% 1000)
    base_h <- as.numeric(input$plot_h %||% 700)
    if (!is.finite(base_w) || base_w <= 0) base_w <- 1000
    if (!is.finite(base_h) || base_h <= 0) base_h <- 700
    use_effective <- identical(input$tipo, "Apiladas")
    plot_w <- if (use_effective) effective_plot_width(base_w) else base_w
    plot_h <- if (use_effective) effective_plot_height(base_h) else base_h
    lang <- input$app_lang %||% i18n_lang
    div(
      id = "plot-loading-wrap",
      class = "plot-loading-wrap",
      style = "overflow-x:auto; overflow-y:visible;",
      div(
        class = "plot-loading-indicator",
        tags$i(class = "fa fa-circle-o-notch fa-spin", `aria-hidden` = "true"),
        tags$span(tr_text("loading_plot_data", lang))
      ),
      div(
        style = sprintf("width:%spx; max-width:none;", plot_w),
        plotlyOutput(
          "plotInteractivo",
          width  = "100%",
          height = paste0(plot_h, "px")
        )
      )
    )
  })

  outputOptions(output, "rmRepsGlobalUI", suspendWhenHidden = FALSE)
  outputOptions(output, "repsStrainUI", suspendWhenHidden = FALSE)
  outputOptions(output, "repsGrpUI", suspendWhenHidden = FALSE)

  observe({
    use_param <- !is.null(input$tipo) && input$tipo %in% c("Boxplot", "Barras", "Violin")
    df <- if (use_param) {
      param_rep_df()
    } else {
      if (isTRUE(input$doNorm)) datos_agrupados_norm() else datos_agrupados()
    }
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return()
    if (is.null(input$strain) || !length(input$strain)) return()

    df <- df %>% dplyr::filter(Strain == input$strain)
    if (!is.null(input$showMedios)) {
      df <- df %>% dplyr::filter(Media %in% input$showMedios)
    }

    medias <- unique(as.character(df$Media))
    if (!length(medias)) return()
    drop_all <- as.character(input$rm_reps_all %||% character(0))

    available_map <- setNames(vector("list", length(medias)), medias)
    input_map <- setNames(vector("list", length(medias)), medias)
    for (m in medias) {
      reps <- normalize_rep_selection(df$BiologicalReplicate[df$Media == m])
      input_id <- paste0("reps_", make.names(m))
      available_map[[m]] <- reps
      input_map[[m]] <- input[[input_id]]
    }
    strain_sel <- current_strain_value()
    if (!nzchar(strain_sel)) return()
    full_map <- isolate(reps_strain_selected())
    sync_res <- sync_replicate_selection_map(
      current_map = get_strain_selection_map(full_map, strain_sel),
      groups = medias,
      available_map = available_map,
      input_map = input_map,
      drop_all = drop_all
    )
    updated_strain_map <- set_strain_selection_map(full_map, strain_sel, sync_res$map)
    if (!identical(updated_strain_map, full_map)) {
      reps_strain_selected(updated_strain_map)
    }

    map_group <- isolate(reps_group_selected())
    map_group_new <- map_group
    for (m in medias) {
      map_group_new <- set_group_media_selection(
        map_group_new,
        strain = strain_sel,
        media = m,
        selected = sync_res$map[[m]] %||% character(0)
      )
    }
    if (!identical(map_group_new, map_group)) {
      reps_group_selected(map_group_new)
    }
  })

  observe({
    grps <- input$showGroups %||% character(0)
    if (!length(grps)) return()

    use_param <- !is.null(input$tipo) && input$tipo %in% c("Boxplot", "Barras", "Violin")
    df <- if (use_param) {
      param_rep_df()
    } else {
      if (isTRUE(input$doNorm)) datos_agrupados_norm() else datos_agrupados()
    }
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return()

    df <- df %>% dplyr::filter(paste(Strain, Media, sep = "-") %in% grps)
    drop_all <- as.character(input$rm_reps_all %||% character(0))

    available_map <- setNames(vector("list", length(grps)), grps)
    input_map <- setNames(vector("list", length(grps)), grps)
    grp_id <- paste(df$Strain, df$Media, sep = "-")
    for (g in grps) {
      reps <- normalize_rep_selection(df$BiologicalReplicate[grp_id == g])
      input_id <- paste0("reps_grp_", make.names(g))
      available_map[[g]] <- reps
      input_map[[g]] <- input[[input_id]]
    }
    sync_res <- sync_replicate_selection_map(
      current_map = isolate(reps_group_selected()),
      groups = grps,
      available_map = available_map,
      input_map = input_map,
      drop_all = drop_all
    )
    map_group_new <- sync_res$map
    if (!identical(map_group_new, isolate(reps_group_selected()))) {
      reps_group_selected(map_group_new)
    }

    map_strain <- isolate(reps_strain_selected())
    map_strain_new <- map_strain
    for (g in grps) {
      idx <- grp_id == g
      if (!any(idx)) next
      strain_g <- as.character(df$Strain[idx][[1]])
      media_g <- as.character(df$Media[idx][[1]])
      map_strain_new <- set_strain_media_selection(
        map_strain_new,
        strain = strain_g,
        media = media_g,
        selected = map_group_new[[as.character(g)]] %||% character(0)
      )
    }
    if (!identical(map_strain_new, map_strain)) {
      reps_strain_selected(map_strain_new)
    }
  })

  observeEvent(input$repsGrpSelectAll, {
    use_param <- !is.null(input$tipo) && input$tipo %in% c("Boxplot", "Barras", "Violin")
    df <- if (use_param) {
      param_rep_df()
    } else {
      if (isTRUE(input$doNorm)) datos_agrupados_norm() else datos_agrupados()
    }
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return()

    all_groups <- df %>%
      dplyr::transmute(
        Strain = as.character(Strain),
        Media = as.character(Media)
      ) %>%
      dplyr::filter(
        !is.na(Strain), nzchar(trimws(Strain)),
        !is.na(Media), nzchar(trimws(Media))
      ) %>%
      dplyr::transmute(Group = paste(Strain, Media, sep = "-")) %>%
      dplyr::distinct(Group) %>%
      dplyr::pull(Group)
    if (!length(all_groups)) return()

    ordered_groups <- datos_agrupados() %>%
      dplyr::distinct(Strain, Media, Orden) %>%
      dplyr::mutate(Group = paste(Strain, Media, sep = "-")) %>%
      dplyr::arrange(Orden) %>%
      dplyr::pull(Group)
    if (length(ordered_groups)) {
      all_groups <- intersect(ordered_groups, all_groups)
    } else {
      all_groups <- sort(unique(all_groups))
    }

    updateCheckboxGroupInput(
      session,
      inputId = "showGroups",
      choices = all_groups,
      selected = all_groups
    )
    updateCheckboxInput(session, "toggleGroups", value = TRUE)
    updateTextInput(session, "orderGroups", value = paste(all_groups, collapse = ","))

    drop_all <- as.character(input$rm_reps_all %||% character(0))
    grp_id <- paste(df$Strain, df$Media, sep = "-")

    map_group <- reps_group_selected()
    map_strain <- reps_strain_selected()
    for (g in all_groups) {
      idx <- grp_id == g
      reps <- normalize_rep_selection(df$BiologicalReplicate[idx])
      selected <- normalize_rep_selection(setdiff(reps, drop_all))
      strain_g <- if (any(idx)) as.character(df$Strain[idx][[1]]) else ""
      media_g <- if (any(idx)) as.character(df$Media[idx][[1]]) else ""
      sync_maps <- set_synced_media_selection(
        strain_map = map_strain,
        group_map = map_group,
        strain = strain_g,
        media = media_g,
        selected = selected
      )
      map_strain <- sync_maps$reps_strain_map
      map_group <- sync_maps$reps_group_map
      updateCheckboxGroupInput(
        session,
        inputId = paste0("reps_grp_", make.names(g)),
        choices = reps,
        selected = selected
      )
    }
    reps_group_selected(map_group)
    reps_strain_selected(map_strain)
  }, ignoreInit = TRUE)

  observeEvent(input$repsStrainSelectAll, {
    use_param <- !is.null(input$tipo) && input$tipo %in% c("Boxplot", "Barras", "Violin")
    df <- if (use_param) {
      param_rep_df()
    } else {
      if (isTRUE(input$doNorm)) datos_agrupados_norm() else datos_agrupados()
    }
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return()
    if (is.null(input$strain) || !length(input$strain)) return()
    df <- df %>% dplyr::filter(Strain == input$strain)

    medias <- df %>%
      dplyr::transmute(Media = as.character(Media), Orden = Orden) %>%
      dplyr::filter(!is.na(Media), nzchar(Media)) %>%
      dplyr::distinct(Media, Orden) %>%
      dplyr::arrange(Orden) %>%
      dplyr::pull(Media)
    if (!length(medias)) return()

    updateCheckboxGroupInput(
      session,
      inputId = "showMedios",
      choices = medias,
      selected = medias
    )
    updateCheckboxInput(session, "toggleMedios", value = TRUE)
    updateTextInput(session, "orderMedios", value = paste(medias, collapse = ","))

    drop_all <- as.character(input$rm_reps_all %||% character(0))
    map_strain <- reps_strain_selected()
    map_group <- reps_group_selected()
    for (m in medias) {
      reps <- normalize_rep_selection(df$BiologicalReplicate[df$Media == m])
      selected <- normalize_rep_selection(setdiff(reps, drop_all))
      sync_maps <- set_synced_media_selection(
        strain_map = map_strain,
        group_map = map_group,
        strain = input$strain,
        media = m,
        selected = selected
      )
      map_strain <- sync_maps$reps_strain_map
      map_group <- sync_maps$reps_group_map
      updateCheckboxGroupInput(
        session,
        inputId = paste0("reps_", make.names(m)),
        choices = reps,
        selected = selected
      )
    }
    reps_strain_selected(map_strain)
    reps_group_selected(map_group)
  }, ignoreInit = TRUE)

  plotly_tooltip_fields <- function(tipo) {
    # Keep tooltips compact for large datasets to avoid huge JSON payloads.
    if (identical(tipo, "Correlacion")) return(c("x", "y", "text", "name"))
    if (identical(tipo, "Curvas")) return(c("x", "y", "name", "text"))
    if (identical(tipo, "MatrizCorrelacion")) return(c("x", "y", "fill", "text"))
    if (identical(tipo, "Heatmap")) return(c("x", "y", "fill", "text"))
    c("x", "y", "name", "text")
  }

  output$plotInteractivo <- renderPlotly({
    input$mobile_plot_refresh
    req(input$dataFile)
    if (input$tipo == "Curvas") {
      req(cur_data_box(), cur_cfg_box())
    }
    
    lang <- input$app_lang %||% i18n_lang
    if (isTRUE(dataset_loading())) {
      validate(need(FALSE, tr_text("loading_plot_data", lang)))
    }
    msg_no_data <- tr_text("no_data_plot", lang)
    p <- tryCatch(plot_base(), error = function(e) NULL)
    validate(need(!is.null(p), msg_no_data))
    if (inherits(p, "ggplot")) {
      plotly_width <- input$plot_w
      plotly_height <- input$plot_h
      tooltip_fields <- plotly_tooltip_fields(input$tipo %||% "")
      plt <- tryCatch(
        suppressWarnings(
          safe_ggplotly(
            p,
            tooltip      = tooltip_fields,
            width        = plotly_width,
            height       = plotly_height,
            originalData = FALSE
          )
        ),
        error = function(e) NULL
      )
      validate(need(!is.null(plt), msg_no_data))
      plt <- plt %>% config(responsive = FALSE)
      plt <- apply_margin_inputs_to_plotly(plt, legend_in_margin = TRUE, expand_canvas = FALSE)
      plt
    } else {
      # Ya p es un plotly puro generado por build_plotly_stack()
      p <- apply_margin_inputs_to_plotly(p)
      p <- p %>% config(responsive = FALSE)
      p
    }
  })

  # Keep plot outputs active on small-screen one-pane mode.
  outputOptions(output, "plotInteractivoUI", suspendWhenHidden = FALSE)
  outputOptions(output, "plotInteractivo", suspendWhenHidden = FALSE)
  
  
  # --- Descarga individual PNG -----------------------------------------------
  output$downloadPlot_png <- downloadHandler(
    filename = function(){
      paste0(
        if (input$scope == "Combinado") "Combinado" else sanitize(input$strain),
        "_", input$tipo, ".png"
      )
    },
    content = function(file){
      width <- input$plot_w %||% 900
      height <- input$plot_h %||% 700
      eff_width <- effective_plot_width(width)
      eff_height <- effective_plot_height(height)
      key <- plot_export_key("png", eff_width, eff_height)
      raw <- cache_capture_raw(
        slot = "plot_png",
        key = key,
        ext = ".png",
        writer = function(tmp) write_current_plot_png(tmp, width = width, height = height),
        max_entries = 8L
      )
      writeBin(raw, file)
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
      width <- input$plot_w %||% 900
      height <- input$plot_h %||% 700
      eff_width <- effective_plot_width(width)
      eff_height <- effective_plot_height(height)
      key <- plot_export_key("pdf", eff_width, eff_height)
      raw <- cache_capture_raw(
        slot = "plot_pdf",
        key = key,
        ext = ".pdf",
        writer = function(tmp) write_current_plot_pdf(tmp, width = width, height = height),
        max_entries = 8L
      )
      writeBin(raw, file)
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
    lang <- input$app_lang %||% i18n_lang
    mode <- input$plot_copy_success$message %||% "copied"
    msg <- if (mode %in% c("downloaded", "opened")) {
      tr_text("notify_copy_fallback_success", lang)
    } else {
      tr_text("notify_copy_success", lang)
    }
    showNotification(msg, type = "message", duration = 4)
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
      width    = effective_plot_width(),
      height   = effective_plot_height(),
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
      width    = effective_plot_width(),
      height   = effective_plot_height(),
      format   = "pdf"
    ))
  })

  output$downloadHeatClusters <- downloadHandler(
    filename = function() {
      paste0("heatmap_clusters_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".xlsx")
    },
    content = function(file) {
      lang <- input$app_lang %||% i18n_lang
      if (!identical(input$tipo %||% "", "Heatmap")) {
        stop(tr_text("no_data_selection", lang), call. = FALSE)
      }
      if (!isTRUE(input$heat_cluster_rows)) {
        stop(tr_text("heatmap_enable_row_clusters", lang), call. = FALSE)
      }
      payload <- tryCatch(
        heatmap_payload_current(),
        error = function(e) {
          msg <- sprintf(
            tr_text("global_error_template", lang),
            tr_text("heatmap_download_clusters", lang),
            conditionMessage(e)
          )
          showNotification(msg, type = "error", duration = 8)
          stop(msg, call. = FALSE)
        }
      )
      write_heatmap_cluster_workbook(payload, file)
    }
  )
  
  
  download_param_content <- function(file) {
    datos  <- datos_combinados()
    params <- plot_settings()$Parameter
    params <- intersect(as.character(params), names(datos))
    wb_sum <- generate_summary_wb(datos, params)
    
    cur_wide <- curve_data()
    if (!is.null(cur_wide)) {
      wb_sum <- add_curves_by_group_sheet(
        wb         = wb_sum,
        curve_wide = cur_wide,
        meta_df    = datos
      )
    }

    scope_sel <- isolate(input$scope %||% "Por Cepa")
    active_strain <- if (identical(scope_sel, "Por Cepa")) isolate(input$strain) else NULL
    strain_map_export <- list()
    group_map_export <- list()
    if (identical(scope_sel, "Por Cepa")) {
      # Export should respect saved selections for every strain/media, not only the open strain.
      strain_map_export <- isolate(reps_strain_selected())
      active_strain <- NULL
    } else {
      group_map_export <- isolate(reps_group_selected())
    }
    export_filter <- filter_export_replicates_for_download(
      df = datos,
      reps_strain_map = strain_map_export,
      reps_group_map = group_map_export,
      drop_all = as.character(isolate(input$rm_reps_all %||% character(0))),
      active_strain = active_strain,
      tech_selection_map = isolate(qc_tech_selected())
    )

    if (isTRUE(export_filter$has_changes) &&
        is.data.frame(export_filter$df) &&
        nrow(export_filter$df) > 0) {
      filtered_params <- detect_filtered_params_for_download(
        raw_df = datos,
        filtered_df = export_filter$df,
        params = params
      )

      if (length(filtered_params)) {
        datos_filtered <- renumber_replicates_for_export(export_filter$df)
        wb_sum <- generate_summary_wb(
          datos = datos_filtered,
          params = filtered_params,
          wb = wb_sum,
          sheet_suffix = "_filt"
        )

        if (!is.null(cur_wide)) {
          wb_sum <- add_curves_by_group_sheet(
            wb = wb_sum,
            curve_wide = filter_curve_wide_for_export(cur_wide, datos_filtered),
            meta_df = datos_filtered,
            sheet_name = "Curvas por grupo_filt"
          )
        }
      }
    }
    openxlsx::saveWorkbook(wb_sum, file, overwrite = TRUE)
  }

  output$downloadExcel <- downloadHandler(  
    filename    = function() "Parametros_por_grupo.xlsx",  
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",  
    content     = function(file){
      lang <- input$app_lang %||% i18n_lang
      key <- params_export_key()
      raw <- tryCatch(
        cache_capture_raw(
          slot = "params",
          key = key,
          ext = ".xlsx",
          writer = download_param_content,
          max_entries = 6L
        ),
        error = function(e) {
          msg <- sprintf(
            tr_text("global_error_template", lang),
            tr_text("download_data", lang),
            conditionMessage(e)
          )
          showNotification(msg, type = "error", duration = 8)
          stop(msg, call. = FALSE)
        }
      )
      if (!is.raw(raw) || !length(raw)) {
        msg <- sprintf(
          tr_text("global_error_template", lang),
          tr_text("download_data", lang),
          "Empty workbook generated."
        )
        showNotification(msg, type = "error", duration = 8)
        stop(msg, call. = FALSE)
      }
      writeBin(raw, file)
    }
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

  stable_key_value <- function(x) {
    if (is.null(x)) return("NULL")
    if (is.data.frame(x)) x <- as.list(x)
    if (is.atomic(x)) {
      vals <- as.character(x)
      vals[is.na(vals)] <- "NA"
      return(paste(vals, collapse = ","))
    }
    if (is.list(x)) {
      nms <- names(x)
      if (is.null(nms)) nms <- as.character(seq_along(x))
      nms[is.na(nms) | !nzchar(nms)] <- as.character(seq_along(x))[is.na(nms) | !nzchar(nms)]
      ord <- order(nms)
      parts <- vapply(ord, function(i) {
        paste0(nms[[i]], "=[", stable_key_value(x[[i]]), "]")
      }, character(1))
      return(paste(parts, collapse = ";"))
    }
    paste(capture.output(str(x, give.attr = FALSE)), collapse = "")
  }

  input_file_stamp <- function() {
    f <- isolate(input$dataFile)
    if (is.null(f) || !nrow(f)) return("NO_FILE")
    path <- as.character(f$datapath[[1]] %||% "")
    name <- as.character(f$name[[1]] %||% "")
    if (!nzchar(path) || !file.exists(path)) return(paste(name, path, sep = "|"))
    info <- file.info(path)
    paste(
      name,
      path,
      as.character(info$size %||% ""),
      as.character(as.numeric(info$mtime) %||% ""),
      sep = "|"
    )
  }

  cache_get_raw <- function(slot, key) {
    bucket <- isolate(export_cache[[slot]])
    if (!is.list(bucket)) return(NULL)
    val <- bucket[[key]]
    if (!is.raw(val) || !length(val)) return(NULL)
    val
  }

  cache_set_raw <- function(slot, key, raw, max_entries = 6L) {
    if (!is.raw(raw) || !length(raw)) return(invisible(NULL))
    bucket <- isolate(export_cache[[slot]])
    if (!is.list(bucket)) bucket <- list()
    if (!is.null(bucket[[key]])) bucket[[key]] <- NULL
    bucket[[key]] <- raw
    keys <- names(bucket)
    if (length(keys) > max_entries) {
      keep <- tail(keys, max_entries)
      bucket <- bucket[keep]
    }
    isolate(export_cache[[slot]] <- bucket)
    invisible(NULL)
  }

  cache_capture_raw <- function(slot, key, ext, writer, max_entries = 6L) {
    cached <- cache_get_raw(slot, key)
    if (!is.null(cached)) return(cached)
    tmp <- tempfile(fileext = ext)
    on.exit(unlink(tmp), add = TRUE)
    writer(tmp)
    size <- file.info(tmp)$size
    if (is.na(size) || size <= 0) return(raw(0))
    out <- readBin(tmp, "raw", n = size)
    cache_set_raw(slot, key, out, max_entries = max_entries)
    out
  }

  plot_export_key <- function(kind, width, height) {
    meta <- collect_metadata_tbl()
    export_renderer_version <- "preview_match_v2"
    paste(
      kind,
      export_renderer_version,
      input_file_stamp(),
      as.character(input$scope %||% ""),
      as.character(input$strain %||% ""),
      as.character(input$tipo %||% ""),
      as.character(width),
      as.character(height),
      stable_key_value(reps_strain_selected()),
      stable_key_value(reps_group_selected()),
      stable_key_value(as.character(input$rm_reps_all %||% character(0))),
      paste(meta$Campo, meta$Valor, sep = "=", collapse = ";"),
      sep = "||"
    )
  }

  params_export_key <- function() {
    params <- as.character(plot_settings()$Parameter %||% character(0))
    paste(
      "params",
      input_file_stamp(),
      as.character(input$scope %||% ""),
      as.character(input$strain %||% ""),
      stable_key_value(params),
      stable_key_value(reps_strain_selected()),
      stable_key_value(reps_group_selected()),
      stable_key_value(as.character(input$rm_reps_all %||% character(0))),
      stable_key_value(qc_tech_selected()),
      as.character(isTRUE(input$doNorm)),
      as.character(input$ctrlMedium %||% ""),
      sep = "||"
    )
  }

  stats_export_key <- function() {
    params <- as.character(plot_settings()$Parameter %||% character(0))
    paste(
      "stats",
      input_file_stamp(),
      as.character(input$scope %||% ""),
      as.character(input$strain %||% ""),
      stable_key_value(params),
      as.character(input$sigTest %||% ""),
      as.character(input$postHoc %||% ""),
      as.character(input$compMode %||% ""),
      as.character(input$controlGroup %||% ""),
      as.character(input$group1 %||% ""),
      as.character(input$group2 %||% ""),
      as.character(input$multitest_method %||% "none"),
      stable_key_value(reps_strain_selected()),
      stable_key_value(reps_group_selected()),
      stable_key_value(as.character(input$rm_reps_all %||% character(0))),
      sep = "||"
    )
  }

  metadata_export_key <- function() {
    meta <- collect_metadata_tbl()
    paste(
      "metadata",
      input_file_stamp(),
      as.character(input$tipo %||% ""),
      paste(meta$Campo, meta$Valor, sep = "=", collapse = ";"),
      sep = "||"
    )
  }

  bundle_export_key <- function() {
    versions <- bundle_store$versions
    if (!length(versions)) return("bundle-empty")
    version_sig <- vapply(versions, function(v) {
      paste(
        as.character(v$id %||% ""),
        as.character(v$dataset %||% ""),
        as.character(v$tipo %||% ""),
        as.character(v$dir_label %||% ""),
        as.character(v$stats_hash %||% ""),
        as.character(length(v$plot_raw %||% raw(0))),
        as.character(length(v$plot_pdf_raw %||% raw(0))),
        as.character(length(v$metadata_raw %||% raw(0))),
        as.character(v$created %||% ""),
        sep = "~"
      )
    }, character(1))
    paste("bundle", paste(version_sig, collapse = "||"), sep = "::")
  }

  ensure_dataset_record <- function(dataset_key, dataset_name,
                                    datos_comb, datos_agru,
                                    comb_raw = NULL, agru_raw = NULL,
                                    stats_hash = NULL, stats_raw = NULL,
                                    params_raw = NULL){
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
      raw_params <- params_raw %||% cache_capture_raw(
        slot = "params",
        key = params_export_key(),
        ext = ".xlsx",
        writer = download_param_content,
        max_entries = 6L
      )
      record$files$parametros <- list(
        name = "Parametros_por_grupo.xlsx",
        raw  = raw_params,
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
      tipo_sel <- isolate(input$tipo) %||% ""
      is_curve_stats <- identical(tipo_sel, "Curvas")
      curve_method_labels <- c(
        S1 = tr_text("curves_stats_s1", lang),
        S2 = tr_text("curves_stats_s2", lang),
        S3 = tr_text("curves_stats_s3", lang),
        S4 = tr_text("curves_stats_s4", lang)
      )
      curve_methods_sel <- as.character(isolate(input$curve_stats_methods) %||% character(0))
      mapped_curve_methods <- unname(curve_method_labels[curve_methods_sel])
      mapped_curve_methods <- mapped_curve_methods[!is.na(mapped_curve_methods) & nzchar(mapped_curve_methods)]
      curve_methods_text <- if (length(mapped_curve_methods)) {
        paste(unique(mapped_curve_methods), collapse = "; ")
      } else {
        "none selected"
      }
      
      scope_sel    <- isolate(input$scope)          %||% "Por Cepa"
      strain_sel   <- isolate(input$strain)
      sigTest_sel  <- isolate(input$sigTest)        %||% "ANOVA"
      postHoc_sel  <- isolate(input$postHoc)        %||% "Tukey"
      compMode_sel <- isolate(input$compMode)       %||% "all"
      multitest_sel <- isolate(input$multitest_method) %||% "none"
      if (!multitest_sel %in% c("holm", "fdr", "bonferroni", "none")) multitest_sel <- "none"
      controlGroup_sel <- isolate(input$controlGroup) %||% ""
      group1_sel   <- isolate(input$group1)         %||% ""
      group2_sel   <- isolate(input$group2)         %||% ""

      base_df <- if (scope_sel == "Por Cepa") {
        datos_agrupados() |>
          dplyr::filter(Strain == strain_sel) |>
          order_filter_strain() |>
          filter_reps_strain()
      } else {
        datos_agrupados() |>
          order_filter_group()
      }
      if (is.null(base_df) || !is.data.frame(base_df) || nrow(base_df) == 0) {
        stop("No rows available for statistical export after applying the current filters.")
      }
      
      wb_tests <- createWorkbook()      # <- libro que vamos a rellenar
      
      ## helper interno (idÃƒÂ©ntico al usado arriba)
      split_comparison <- function(x) stringr::str_split_fixed(x, "-", 2)
      
      sheet_names <- make.unique(vapply(params, safe_sheet, character(1)), sep = "_")
      for (idx in seq_along(params)){
        param <- params[[idx]]
        sheet <- sheet_names[[idx]]
        if (!param %in% names(base_df)) next
        addWorksheet(wb_tests, sheet)
        
        # ---------- reutiliza el dataframe ya filtrado para evitar recomputo ----
        df_param <- base_df |>
          dplyr::transmute(
            Label = if (scope_sel == "Por Cepa") Media else Label,
            Valor = .data[[param]]
          ) |>
          dplyr::filter(!is.na(Label), is.finite(Valor))
        
        # Ã¢â€â‚¬Ã¢â€â‚¬ si no hay datos suficientes se elimina la hoja y se pasa al sig.
        if (nrow(df_param) < 3 || dplyr::n_distinct(df_param$Label) < 2){
          removeWorksheet(wb_tests, sheet)
          next
        }
        
        norm_split <- split(df_param$Valor, df_param$Label)
        norm_tbl <- tibble::tibble(
          Label = names(norm_split),
          Shapiro.stat = vapply(norm_split, function(v) safe_shapiro_test(v)$stat, numeric(1)),
          Shapiro.p    = vapply(norm_split, function(v) safe_shapiro_test(v)$p, numeric(1))
        ) %>%
          dplyr::mutate(
            Normal = dplyr::if_else(
              !is.na(Shapiro.p) & Shapiro.p > 0.05,
              tr_text("yes_label", lang),
              tr_text("no_label", lang)
            )
          )
        
        section_start_row <- 1L
        normality_title <- "Normalidad"
        significance_title <- "Significancia"

        if (isTRUE(is_curve_stats)) {
          curve_context_tbl <- tibble::tibble(
            Field = c("Source", "Curve metric", "Curve methods"),
            Value = c("Curves", as.character(param), curve_methods_text)
          )
          writeData(
            wb_tests, sheet, "Curve statistics context",
            startRow = section_start_row, startCol = 1,
            headerStyle = createStyle(textDecoration = "bold")
          )
          writeData(
            wb_tests, sheet, curve_context_tbl,
            startRow = section_start_row + 1, startCol = 1,
            headerStyle = createStyle(textDecoration = "bold")
          )
          section_start_row <- section_start_row + nrow(curve_context_tbl) + 3L
          normality_title <- paste0("Normalidad (Curvas - metrica: ", as.character(param), ")")
          significance_title <- paste0("Significancia (Curvas - metrica: ", as.character(param), ")")
        }

        writeData(wb_tests, sheet, normality_title,
                  startRow = section_start_row, startCol = 1,
                  headerStyle = createStyle(textDecoration = "bold"))
        writeData(wb_tests, sheet, norm_tbl,
                  startRow = section_start_row + 1, startCol = 1,
                  headerStyle = createStyle(textDecoration = "bold"))
        
        ## Ã¢â€“Âº helpers Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬
        do_anova <- function(df){
          aovm <- aov(Valor ~ Label, data = df)
          switch(postHoc_sel,
                 "Tukey"      = rstatix::tukey_hsd(df, Valor ~ Label),
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
                              safe_pairwise_t(df_param, multitest_sel)
                            } else if (compMode_sel == "control"){
                              rstatix::t_test(df_param, Valor ~ Label, ref.group = controlGroup_sel)
                            } else {
                              grupos <- c(group1_sel, group2_sel)
                              sub <- df_param |> dplyr::filter(Label %in% grupos)
                              if (is.factor(sub$Label)) {
                                sub$Label <- droplevels(sub$Label)
                              } else {
                                sub$Label <- factor(as.character(sub$Label), levels = unique(as.character(sub$Label)))
                              }
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
                              safe_pairwise_wilcox(df_param, multitest_sel)
                            } else if (compMode_sel == "control"){
                              rstatix::wilcox_test(df_param, Valor ~ Label, ref.group = controlGroup_sel)
                            } else {
                              grupos <- c(group1_sel, group2_sel)
                              sub <- df_param |> dplyr::filter(Label %in% grupos)
                              if (is.factor(sub$Label)) {
                                sub$Label <- droplevels(sub$Label)
                              } else {
                                sub$Label <- factor(as.character(sub$Label), levels = unique(as.character(sub$Label)))
                              }
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
        if ("comparison" %in% names(sig_raw) || "contrast" %in% names(sig_raw)){
          cmp_src <- if ("comparison" %in% names(sig_raw)) sig_raw$comparison else sig_raw$contrast
          cmp <- split_comparison(cmp_src)
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
        if (!length(p_candidates)) {
          writeData(
            wb_tests, sheet,
            tr_text("no_sig_results", lang),
            startRow = nrow(norm_tbl) + 6, startCol = 1
          )
          next
        }
        pcol <- p_candidates[1]
        
        sig_tbl <- sig_raw |>
          dplyr::mutate(
            P_valor       = .data[[pcol]],
            Significativo = dplyr::if_else(
              P_valor < 0.05,
              tr_text("yes_label", input$app_lang %||% i18n_lang),
              tr_text("no_label",  input$app_lang %||% i18n_lang)
            ),
            Estrellas     = dplyr::case_when(
              P_valor < 0.001 ~ "***",
              P_valor < 0.01  ~ "**",
              P_valor < 0.05  ~ "*",
              TRUE            ~ ""
            )
          )
        
        # ------------------- ESCRITURA EN LA HOJA ------------------------------
        start <- section_start_row + nrow(norm_tbl) + 3
        
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
        
        writeData(wb_tests, sheet, significance_title,
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
    content  = function(file){
      lang <- input$app_lang %||% i18n_lang
      key <- stats_export_key()
      raw <- tryCatch(
        cache_capture_raw(
          slot = "stats",
          key = key,
          ext = ".xlsx",
          writer = download_stats_content,
          max_entries = 6L
        ),
        error = function(e) {
          msg <- sprintf(
            tr_text("global_error_template", lang),
            tr_text("download_stats", lang),
            conditionMessage(e)
          )
          showNotification(msg, type = "error", duration = 8)
          stop(msg, call. = FALSE)
        }
      )
      if (!is.raw(raw) || !length(raw)) {
        msg <- sprintf(
          tr_text("global_error_template", lang),
          tr_text("download_stats", lang),
          "Empty workbook generated."
        )
        showNotification(msg, type = "error", duration = 8)
        stop(msg, call. = FALSE)
      }
      writeBin(raw, file)
    }
  )

  observeEvent(input$save_bundle_version, {
    req(input$dataFile)
    req(datos_combinados(), datos_agrupados())
    lang <- input$app_lang %||% i18n_lang
    width <- input$plot_w %||% 900
    height <- input$plot_h %||% 700
    eff_width <- effective_plot_width(width)
    eff_height <- effective_plot_height(height)

    plot_raw <- tryCatch(
      cache_capture_raw(
        slot = "plot_png",
        key = plot_export_key("png", eff_width, eff_height),
        ext = ".png",
        writer = function(tmp) write_current_plot_png(tmp, width = width, height = height),
        max_entries = 8L
      ),
      error = function(e) {
        msg <- sprintf(
          tr_text("global_error_template", lang),
          tr_text("download_plot", lang),
          conditionMessage(e)
        )
        showNotification(msg, type = "error", duration = 8)
        raw(0)
      }
    )
    if (length(plot_raw) == 0) {
      showNotification(tr_text("plot_capture_failed", lang), type = "error", duration = 5)
      return()
    }
    plot_pdf_raw <- tryCatch(
      cache_capture_raw(
        slot = "plot_pdf",
        key = plot_export_key("pdf", eff_width, eff_height),
        ext = ".pdf",
        writer = function(tmp) write_current_plot_pdf(tmp, width = width, height = height),
        max_entries = 8L
      ),
      error = function(e) {
        msg <- sprintf(
          tr_text("global_error_template", lang),
          tr_text("download_plot", lang),
          conditionMessage(e)
        )
        showNotification(msg, type = "error", duration = 8)
        raw(0)
      }
    )
    if (length(plot_pdf_raw) == 0) {
      showNotification(tr_text("plot_pdf_failed", lang), type = "error", duration = 5)
      return()
    }
    metadata_raw <- tryCatch(
      cache_capture_raw(
        slot = "metadata",
        key = metadata_export_key(),
        ext = ".xlsx",
        writer = write_metadata_xlsx,
        max_entries = 10L
      ),
      error = function(e) {
        msg <- sprintf(
          tr_text("global_error_template", lang),
          tr_text("download_metadata", lang),
          conditionMessage(e)
        )
        showNotification(msg, type = "error", duration = 8)
        raw(0)
      }
    )
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

    params_try <- try(
      cache_capture_raw(
        slot = "params",
        key = params_export_key(),
        ext = ".xlsx",
        writer = download_param_content,
        max_entries = 6L
      ),
      silent = TRUE
    )
    params_raw <- if (!inherits(params_try, "try-error")) params_try else NULL

    stats_try  <- try(
      cache_capture_raw(
        slot = "stats",
        key = stats_export_key(),
        ext = ".xlsx",
        writer = download_stats_content,
        max_entries = 6L
      ),
      silent = TRUE
    )
    stats_raw  <- if (!inherits(stats_try, "try-error")) stats_try else NULL
    stats_hash <- md5_raw(stats_raw)

    ensure_dataset_record(
      dataset_key   = dataset_key,
      dataset_name  = dataset_name,
      datos_comb    = comb_df,
      datos_agru    = agru_df,
      comb_raw      = comb_raw,
      stats_hash    = stats_hash,
      stats_raw     = stats_raw,
      params_raw    = params_raw
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

  write_bundle_zip <- function(file) {
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

  output$downloadBundleZip <- downloadHandler(
    filename = function(){
      paste0("BIOSZEN_paquete_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".zip")
    },
    content = function(file){
      lang <- input$app_lang %||% i18n_lang
      key <- bundle_export_key()
      raw <- tryCatch(
        cache_capture_raw(
          slot = "bundle",
          key = key,
          ext = ".zip",
          writer = write_bundle_zip,
          max_entries = 4L
        ),
        error = function(e) {
          msg <- sprintf(
            tr_text("global_error_template", lang),
            tr_text("download_bundle", lang),
            conditionMessage(e)
          )
          showNotification(msg, type = "error", duration = 8)
          stop(msg, call. = FALSE)
        }
      )
      if (!is.raw(raw) || !length(raw)) {
        msg <- sprintf(
          tr_text("global_error_template", lang),
          tr_text("download_bundle", lang),
          "Empty ZIP generated."
        )
        showNotification(msg, type = "error", duration = 8)
        stop(msg, call. = FALSE)
      }
      writeBin(raw, file)
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
    blocked_data_keys <- metadata_data_keys()
    get_val <- function(campo){
      if (campo %in% blocked_data_keys) return(NULL)
      v <- meta$Valor[meta$Campo == campo]
      if (!length(v)) return(NULL)
      v <- as.character(v)
      v <- v[!is.na(v)]
      if (!length(v)) return(NULL)
      v[[1]]
    }
    parse_csv_values <- function(x) {
      x <- as.character(x %||% "")
      if (!length(x) || is.na(x[1]) || !nzchar(trimws(x[1]))) return(character(0))
      vals <- trimws(strsplit(x[1], ",", fixed = TRUE)[[1]])
      vals[nzchar(vals)]
    }
    parse_bool <- function(x) {
      vals <- as.character(x %||% "")
      if (!length(vals)) return(FALSE)
      tolower(trimws(vals[[1]])) %in% c("true", "1", "yes", "y")
    }

    if (!is.null(v <- get_val("scope")))      updateRadioButtons(session, "scope",      selected = v)
    if (!is.null(v <- get_val("strain")))     updateSelectizeInput(session, "strain", selected = v, server = TRUE)
    if (!is.null(v <- get_val("colorMode")))  updateSelectInput(session,  "colorMode",  selected = v)
    if (!is.null(v <- get_val("repeat_colors_combined"))) updateCheckboxInput(session, "repeat_colors_combined", value = parse_bool(v))
    if (!is.null(v <- get_val("adv_pal_enable"))) updateCheckboxInput(session, "adv_pal_enable", value = parse_bool(v))
    if (!is.null(v <- get_val("adv_pal_type"))) updateRadioButtons(session, "adv_pal_type", selected = v)
    if (!is.null(v <- get_val("adv_pal_reverse"))) updateCheckboxInput(session, "adv_pal_reverse", value = parse_bool(v))
    if (!is.null(v <- get_val("adv_pal_filters"))) updateCheckboxGroupInput(session, "adv_pal_filters", selected = parse_csv_values(v))
    if (!is.null(v <- get_val("adv_pal_name"))) updateSelectInput(session, "adv_pal_name", selected = v)
    if (!is.null(v <- get_val("adv_pal_group_enable"))) updateCheckboxInput(session, "adv_pal_group_enable", value = parse_bool(v))
    if (!is.null(v <- get_val("adv_pal_group_sel"))) updateSelectizeInput(session, "adv_pal_group_sel", selected = parse_csv_values(v), server = TRUE)
    if (!is.null(v <- get_val("adv_pal_group_color"))) {
      safe_col <- gsub("'", "", as.character(v))
      shinyjs::runjs(sprintf(
        "var el=document.getElementById('adv_pal_group_color'); if(el){el.value='%s'; Shiny.setInputValue('adv_pal_group_color','%s',{priority:'event'});}",
        safe_col, safe_col
      ))
    }
    if (!is.null(v <- get_val("adv_pal_group_overrides"))) {
      adv_pal_group_overrides(decode_named_metadata(v))
    }
    if (!is.null(v <- get_val("plot_w")))     updateNumericInput(session, "plot_w",     value = as.numeric(v))
    if (!is.null(v <- get_val("plot_h")))     updateNumericInput(session, "plot_h",     value = as.numeric(v))
    if (!is.null(v <- get_val("base_size")))   updateNumericInput(session, "base_size",   value = as.numeric(v))
    if (!is.null(v <- get_val("fs_title")))   updateNumericInput(session, "fs_title",   value = as.numeric(v))
    if (!is.null(v <- get_val("fs_axis")))    updateNumericInput(session, "fs_axis",    value = as.numeric(v))
    if (!is.null(v <- get_val("fs_legend")))  updateNumericInput(session, "fs_legend",  value = as.numeric(v))
    if (!is.null(v <- get_val("axis_line_size"))) updateNumericInput(session, "axis_line_size", value = as.numeric(v))
    if (!is.null(v <- get_val("yLab")))       updateTextInput(session, "yLab", value = v)
    if (!is.null(v <- get_val("plotTitle")))  updateTextInput(session, "plotTitle", value = v)
    if (!is.null(v <- get_val("labelMode")))  updateCheckboxInput(session, "labelMode", value = parse_bool(v))
    if (!is.null(v <- get_val("margin_top_adj"))) updateNumericInput(session, "margin_top_adj", value = as.numeric(v))
    if (!is.null(v <- get_val("margin_right_adj"))) updateNumericInput(session, "margin_right_adj", value = as.numeric(v))
    if (!is.null(v <- get_val("margin_bottom_adj"))) updateNumericInput(session, "margin_bottom_adj", value = as.numeric(v))
    if (!is.null(v <- get_val("margin_left_adj"))) updateNumericInput(session, "margin_left_adj", value = as.numeric(v))
    if (!is.null(v <- get_val("pt_size")))     updateNumericInput(session, "pt_size",     value = as.numeric(v))
    if (!is.null(v <- get_val("x_angle")))     updateNumericInput(session, "x_angle",     value = as.numeric(v))
    if (!is.null(v <- get_val("x_wrap")))      updateCheckboxInput(session, "x_wrap", value = tolower(v) == "true")
    if (!is.null(v <- get_val("x_wrap_lines")))updateNumericInput(session, "x_wrap_lines", value = as.numeric(v))
    if (!is.null(v <- get_val("legend_right"))) updateCheckboxInput(session, "legend_right", value = tolower(v) == "true")
    if (!is.null(v <- get_val("pt_jit")))      updateNumericInput(session, "pt_jit",      value = as.numeric(v))
    if (!is.null(v <- get_val("box_w")))      updateNumericInput(session, "box_w",      value = as.numeric(v))
    if (!is.null(v <- get_val("violin_width")))     updateNumericInput(session, "violin_width",     value = as.numeric(v))
    if (!is.null(v <- get_val("violin_linewidth"))) updateNumericInput(session, "violin_linewidth", value = as.numeric(v))
    if (!is.null(v <- get_val("curve_lwd")))   updateNumericInput(session, "curve_lwd",   value = as.numeric(v))
    if (!is.null(v <- get_val("curve_geom"))) updateRadioButtons(session, "curve_geom", selected = v)
    if (!is.null(v <- get_val("curve_color_mode"))) updateRadioButtons(session, "curve_color_mode", selected = v)
    if (!is.null(v <- get_val("curve_single_color"))) {
      safe_col <- gsub("'", "", as.character(v))
      shinyjs::runjs(sprintf(
        "var el=document.getElementById('curve_single_color'); if(el){el.value='%s'; Shiny.setInputValue('curve_single_color','%s',{priority:'event'});}",
        safe_col, safe_col
      ))
    }
    if (!is.null(v <- get_val("cur_ci_style"))) updateRadioButtons(session, "cur_ci_style", selected = v)
    if (!is.null(v <- get_val("sig_linewidth")))updateNumericInput(session, "sig_linewidth", value = as.numeric(v))
    if (!is.null(v <- get_val("sig_textsize"))) updateNumericInput(session, "sig_textsize", value = as.numeric(v))
    if (!is.null(v <- get_val("sig_sep")))      updateNumericInput(session, "sig_sep",      value = as.numeric(v))
    if (!is.null(v <- get_val("sig_textpad")))  updateNumericInput(session, "sig_textpad",  value = as.numeric(v))
    if (!is.null(v <- get_val("sig_mode")))     updateRadioButtons(session, "sig_mode", selected = v)
    if (!is.null(v <- get_val("sig_label_param_color"))) updateCheckboxInput(session, "sig_label_param_color", value = tolower(v) == "true")
    if (!is.null(v <- get_val("multitest_method"))) updateRadioButtons(session, "multitest_method", selected = v)
    if (!is.null(v <- get_val("param")))      updateSelectizeInput(session, "param", selected = v, server = TRUE)
    if (!is.null(v <- get_val("doNorm")))     updateCheckboxInput(session, "doNorm",    value = tolower(v) == "true")
    if (!is.null(v <- get_val("ctrlMedium"))) updateSelectInput(session, "ctrlMedium", selected = if (v == "NULL") character(0) else v)
    if (!is.null(v <- get_val("errbar_size")))updateNumericInput(session, "errbar_size", value = as.numeric(v))
    if (!is.null(v <- get_val("ymax")))       updateNumericInput(session, "ymax",       value = as.numeric(v))
    if (!is.null(v <- get_val("ybreak")))     updateNumericInput(session, "ybreak",     value = as.numeric(v))
    if (!is.null(v <- get_val("stackParams"))){
      stack_meta <- strsplit(v, ",", fixed = TRUE)[[1]]
      stack_meta <- trimws(stack_meta)
      stack_meta <- stack_meta[nzchar(stack_meta)]
      params_all <- as.character(plot_settings()$Parameter %||% character(0))
      update_stack_params_input(params_all, selected = stack_meta)
    }
    if (!is.null(v <- get_val("orderStack"))) updateTextInput(session, "orderStack", value = v)
    if (!is.null(v <- get_val("showErrBars")))updateCheckboxInput(session, "showErrBars", value = tolower(v) == "true")
    if (!is.null(v <- get_val("stack_outline_only"))) updateCheckboxInput(session, "stack_outline_only", value = tolower(v) == "true")
    if (!is.null(v <- get_val("errbar_param_color"))) updateCheckboxInput(session, "errbar_param_color", value = tolower(v) == "true")
    if (!is.null(v <- get_val("xmax_cur")))   updateNumericInput(session, "xmax_cur",   value = as.numeric(v))
    if (!is.null(v <- get_val("xbreak_cur"))) updateNumericInput(session, "xbreak_cur", value = as.numeric(v))
    if (!is.null(v <- get_val("ymax_cur")))   updateNumericInput(session, "ymax_cur",   value = as.numeric(v))
    if (!is.null(v <- get_val("ybreak_cur"))) updateNumericInput(session, "ybreak_cur", value = as.numeric(v))
    if (!is.null(v <- get_val("cur_xlab"))) updateTextInput(session, "cur_xlab", value = v)
    if (!is.null(v <- get_val("cur_ylab"))) updateTextInput(session, "cur_ylab", value = v)
    if (!is.null(v <- get_val("cur_show_ci"))) updateCheckboxInput(session, "cur_show_ci", value = tolower(v) == "true")
    if (!is.null(v <- get_val("cur_show_reps"))) updateCheckboxInput(session, "cur_show_reps", value = tolower(v) == "true")
    if (!is.null(v <- get_val("cur_rep_alpha"))) updateSliderInput(session, "cur_rep_alpha", value = as.numeric(v))
    if (!is.null(v <- get_val("curve_stats_methods"))) updateCheckboxGroupInput(session, "curve_stats_methods", selected = parse_csv_values(v))
    if (!is.null(v <- get_val("corr_param_x"))) updateSelectizeInput(session, "corr_param_x", selected = v, server = TRUE)
    if (!is.null(v <- get_val("corr_param_y"))) updateSelectizeInput(session, "corr_param_y", selected = v, server = TRUE)
    if (!is.null(v <- get_val("corr_method")))  updateRadioButtons(session, "corr_method", selected = v)
    if (!is.null(v <- get_val("corr_norm_target"))) updateRadioButtons(session, "corr_norm_target", selected = v)
    if (!is.null(v <- get_val("corr_show_line")))   updateCheckboxInput(session, "corr_show_line",   value = tolower(v) == "true")
    if (!is.null(v <- get_val("corr_show_labels"))) updateCheckboxInput(session, "corr_show_labels", value = tolower(v) == "true")
    if (!is.null(v <- get_val("corr_show_r")))      updateCheckboxInput(session, "corr_show_r",      value = tolower(v) == "true")
    if (!is.null(v <- get_val("corr_show_p")))      updateCheckboxInput(session, "corr_show_p",      value = tolower(v) == "true")
    if (!is.null(v <- get_val("corr_show_r2")))     updateCheckboxInput(session, "corr_show_r2",     value = tolower(v) == "true")
    if (!is.null(v <- get_val("corr_show_eq")))     updateCheckboxInput(session, "corr_show_eq",     value = tolower(v) == "true")
    if (!is.null(v <- get_val("corr_show_ci")))     updateCheckboxInput(session, "corr_show_ci",     value = tolower(v) == "true")
    if (!is.null(v <- get_val("corr_ci_style")))    updateRadioButtons(session, "corr_ci_style", selected = v)
    if (!is.null(v <- get_val("corr_ci_level")))    updateNumericInput(session, "corr_ci_level", value = as.numeric(v))
    if (!is.null(v <- get_val("corr_xlab")))        updateTextInput(session, "corr_xlab", value = v)
    if (!is.null(v <- get_val("corr_ylab")))        updateTextInput(session, "corr_ylab", value = v)
    if (!is.null(v <- get_val("xmin_corr")))        updateNumericInput(session, "xmin_corr", value = as.numeric(v))
    if (!is.null(v <- get_val("xmax_corr")))        updateNumericInput(session, "xmax_corr", value = as.numeric(v))
    if (!is.null(v <- get_val("xbreak_corr")))      updateNumericInput(session, "xbreak_corr", value = as.numeric(v))
    if (!is.null(v <- get_val("ymin_corr")))        updateNumericInput(session, "ymin_corr", value = as.numeric(v))
    if (!is.null(v <- get_val("ymax_corr")))        updateNumericInput(session, "ymax_corr", value = as.numeric(v))
    if (!is.null(v <- get_val("ybreak_corr")))      updateNumericInput(session, "ybreak_corr", value = as.numeric(v))
    if (!is.null(v <- get_val("corr_label_size")))  updateNumericInput(session, "corr_label_size", value = as.numeric(v))
    if (!is.null(v <- get_val("corr_adv_anchor")))  updateSelectizeInput(session, "corr_adv_anchor", selected = v, server = TRUE)
    if (!is.null(v <- get_val("corr_adv_method")))  updateRadioButtons(session, "corr_adv_method", selected = v)
    if (!is.null(v <- get_val("corr_adv_data_mode"))) updateRadioButtons(session, "corr_adv_data_mode", selected = v)
    if (!is.null(v <- get_val("corr_adv_sig_only"))) updateCheckboxInput(session, "corr_adv_sig_only", value = tolower(v) == "true")
    if (!is.null(v <- get_val("corr_adv_pvalue_max"))) updateNumericInput(session, "corr_adv_pvalue_max", value = as.numeric(v))
    if (!is.null(v <- get_val("corr_adv_direction"))) updateSelectInput(session, "corr_adv_direction", selected = v)
    r_min_meta <- suppressWarnings(as.numeric(get_val("corr_adv_r_min")))
    r_max_meta <- suppressWarnings(as.numeric(get_val("corr_adv_r_max")))
    if (length(r_min_meta) >= 1 && length(r_max_meta) >= 1 &&
        is.finite(r_min_meta[[1]]) && is.finite(r_max_meta[[1]])) {
      lo <- min(r_min_meta[[1]], r_max_meta[[1]])
      hi <- max(r_min_meta[[1]], r_max_meta[[1]])
      updateSliderInput(session, "corr_adv_r_filter", value = c(lo, hi))
    }

    if (!is.null(v <- get_val("corrm_params"))) {
      updateSelectizeInput(session, "corrm_params", selected = parse_csv_values(v), server = TRUE)
    }
    if (!is.null(v <- get_val("corrm_method"))) updateRadioButtons(session, "corrm_method", selected = v)
    if (!is.null(v <- get_val("corrm_adjust"))) updateRadioButtons(session, "corrm_adjust", selected = v)
    if (!is.null(v <- get_val("corrm_show_sig"))) updateCheckboxInput(session, "corrm_show_sig", value = tolower(v) == "true")

    if (!is.null(v <- get_val("heat_params"))) {
      updateSelectizeInput(session, "heat_params", selected = parse_csv_values(v), server = TRUE)
    }
    if (!is.null(v <- get_val("heat_scale_mode"))) {
      updateRadioButtons(session, "heat_scale_mode", selected = v)
    } else if (!is.null(v <- get_val("heat_norm_z"))) {
      updateRadioButtons(session, "heat_scale_mode", selected = if (tolower(v) == "true") "row" else "none")
    }
    if (!is.null(v <- get_val("heat_hclust_method"))) updateSelectInput(session, "heat_hclust_method", selected = v)
    if (!is.null(v <- get_val("heat_cluster_rows"))) updateCheckboxInput(session, "heat_cluster_rows", value = tolower(v) == "true")
    if (!is.null(v <- get_val("heat_cluster_cols"))) updateCheckboxInput(session, "heat_cluster_cols", value = tolower(v) == "true")
    if (!is.null(v <- get_val("heat_k_rows"))) updateNumericInput(session, "heat_k_rows", value = as.numeric(v))
    if (!is.null(v <- get_val("heat_k_cols"))) updateNumericInput(session, "heat_k_cols", value = as.numeric(v))
    if (!is.null(v <- get_val("heat_show_side_dend"))) updateCheckboxInput(session, "heat_show_side_dend", value = tolower(v) == "true")
    if (!is.null(v <- get_val("heat_show_top_dend"))) updateCheckboxInput(session, "heat_show_top_dend", value = tolower(v) == "true")
    if (!is.null(v <- get_val("heat_show_param_labels"))) updateCheckboxInput(session, "heat_show_param_labels", value = tolower(v) == "true")
    if (!is.null(v <- get_val("heat_orientation"))) updateRadioButtons(session, "heat_orientation", selected = v)
    if (!is.null(v <- get_val("heat_show_values"))) updateCheckboxInput(session, "heat_show_values", value = tolower(v) == "true")
  }

  # --- Corregir descarga de Metadata ---
  output$downloadMetadata <- downloadHandler(
    filename = function(){
      paste0("metadata_", input$tipo, ".xlsx")
    },
    content  = function(file){
      lang <- input$app_lang %||% i18n_lang
      key <- metadata_export_key()
      raw <- tryCatch(
        cache_capture_raw(
          slot = "metadata",
          key = key,
          ext = ".xlsx",
          writer = write_metadata_xlsx,
          max_entries = 10L
        ),
        error = function(e) {
          msg <- sprintf(
            tr_text("global_error_template", lang),
            tr_text("download_metadata", lang),
            conditionMessage(e)
          )
          showNotification(msg, type = "error", duration = 8)
          stop(msg, call. = FALSE)
        }
      )
      if (!is.raw(raw) || !length(raw)) {
        msg <- sprintf(
          tr_text("global_error_template", lang),
          tr_text("download_metadata", lang),
          "Empty workbook generated."
        )
        showNotification(msg, type = "error", duration = 8)
        stop(msg, call. = FALSE)
      }
      writeBin(raw, file)
    }
  )

  # --- Carga de metadata de diseno -----------------------------------
  observeEvent(input$metaFiles, {
    req(input$metaFiles)
    valid_types <- c("Boxplot", "Barras", "Violin", "Curvas", "Apiladas", "Correlacion", "MatrizCorrelacion", "Heatmap")
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
    if (!is_valid_reactive_key(input$tipo)) return()
    meta <- meta_store[[as.character(input$tipo[[1]])]]
    if (!is.null(meta)) apply_metadata(meta)
  })

  combo_export_dims <- function() {
    w_px <- suppressWarnings(as.numeric(input$combo_width))
    h_px <- suppressWarnings(as.numeric(input$combo_height))
    if (!is.finite(w_px) || w_px <= 0) w_px <- 1000
    if (!is.finite(h_px) || h_px <= 0) h_px <- 700
    preview_ppi <- 96
    list(
      width_px = w_px,
      height_px = h_px,
      preview_ppi = preview_ppi,
      width_in = w_px / preview_ppi,
      height_in = h_px / preview_ppi
    )
  }

  # 5.1 PNG
  output$dl_combo_png <- downloadHandler(
    filename = function() "combo.png",
    content  = function(file){
      dims <- combo_export_dims()
      ggplot2::ggsave(
        filename = file,
        plot = combo_plot(),
        width = dims$width_in,
        height = dims$height_in,
        units = "in",
        dpi = dims$preview_ppi,
        limitsize = FALSE,
        bg = "white"
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
      dims <- combo_export_dims()
      ggplot2::ggsave(
        filename = file,
        plot = combo_plot(),
        width = dims$width_in,
        height = dims$height_in,
        units = "in",
        limitsize = FALSE,
        device = cairo_pdf,
        bg = "white"
      )
    }
  )

  observeEvent(input$copy_combo_clipboard, {
    session$sendCustomMessage(
      "copyStaticPlotToClipboard",
      list(
        elementId = "comboPreview",
        success = "combo_copy_success",
        fail = "combo_copy_error"
      )
    )
  })

  observeEvent(input$combo_copy_success, {
    lang <- input$app_lang %||% i18n_lang
    mode <- input$combo_copy_success$message %||% "copied"
    msg <- if (mode %in% c("downloaded", "opened")) {
      tr_text("notify_copy_fallback_success", lang)
    } else {
      tr_text("notify_copy_success", lang)
    }
    showNotification(msg, type = "message", duration = 4)
  }, ignoreNULL = TRUE)

  observeEvent(input$combo_copy_error, {
    lang <- input$app_lang %||% i18n_lang
    msg <- input$combo_copy_error$message %||% "Unknown error."
    showNotification(
      paste(tr_text("notify_copy_error_prefix", lang), msg),
      type = "error",
      duration = 6
    )
  }, ignoreNULL = TRUE)
  
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
    curvas_f <- list.files(growth_out_dir, pattern = "^(Curvas|Curves)_.*\\.xlsx$", full.names = TRUE)
    params_f <- list.files(growth_out_dir, pattern = "^(Parametros|Parameters)_.*\\.xlsx$", full.names = TRUE)
    
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
      update_selectize_adaptive(
        "param",
        choices = new_params,
        selected = if (length(new_params)) new_params[1] else character(0)
      )
    }
    
    # 5) Refrescar todos los inputs que dependen de plot_cfg_box()
    update_stack_params_input(plot_cfg_box()$Parameter)
    update_selectize_adaptive(
      "param",
      choices = plot_cfg_box()$Parameter,
      selected = if (length(plot_cfg_box()$Parameter)) plot_cfg_box()$Parameter[1] else character(0)
    )
    update_selectize_adaptive(
      "corr_param_x",
      choices = plot_cfg_box()$Parameter,
      selected = if (length(plot_cfg_box()$Parameter)) plot_cfg_box()$Parameter[1] else character(0)
    )
    update_selectize_adaptive(
      "corr_param_y",
      choices = plot_cfg_box()$Parameter,
      selected = plot_cfg_box()$Parameter[min(2, length(plot_cfg_box()$Parameter))]
    )
    
    # 6) Volvemos a la pestaÃƒÂ±a de resultados
    updateTabsetPanel(session, "mainTabs", selected = "tab_plots")
  })
  
}  



