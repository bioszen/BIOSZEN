# app.R - Shiny interactivo con carga dinámica de archivos, gráfico por cepa o combinado,  
# descarga de PNG y Excel (.xlsx)  

library(shiny)  
library(shiny.i18n)
library(plotly)  
library(ggplot2)  
library(readxl)  
library(dplyr)  
library(tidyr)  
library(scales)  
library(openxlsx)
library(rstatix)
library(DT)  
library(shinyBS)  
library(nortest)  
library(DescTools)    # para DunnettTest()  
library(multcomp)     # alternativa Dunnett y glht()  
library(PMCMRplus)    # Scheffé, Conover, Nemenyi, DSCF  
library(dunn.test)    # otra implementación de Dunn  
library(broom)  
library(stringr)  
library(forcats)  
library(tibble)  
library(RColorBrewer)  
library(viridis)
library(htmlwidgets)   # para saveWidget()
library(webshot2)      # PNG desde html/plotly   (trae Chrome headless)
library(ggrepel)        # etiquetas que no se sobre-ponen
library(shinyjs)
library(bslib)
library(gcplyr)
library(rlang)
library(patchwork)
library(officer)
library(rvg)

find_i18n_dir <- function(pkg = "BIOSZEN") {
  if (dir.exists("i18n")) return(normalizePath("i18n", winslash = "/", mustWork = TRUE))
  dev_dir <- file.path("inst", "app", "i18n")
  if (dir.exists(dev_dir)) return(normalizePath(dev_dir, winslash = "/", mustWork = TRUE))
  dir <- system.file("app/i18n", package = pkg)
  if (nzchar(dir) && dir.exists(dir)) return(dir)
  ""
}

i18n_dir <- find_i18n_dir()
if (!nzchar(i18n_dir)) {
  stop("No se encontro la carpeta de traducciones 'i18n'.")
}

i18n_lang <- "en"
i18n_config <- file.path(i18n_dir, "config.yml")
if (!file.exists(i18n_config)) {
  i18n_config <- NULL
}
i18n <- shiny.i18n::Translator$new(
  translation_csvs_path = i18n_dir,
  translation_csv_config = i18n_config
)
i18n$set_translation_language(i18n_lang)
i18n$use_js()

i18n_valid_lang <- function(lang) {
  if (is.null(lang) || !nzchar(lang)) return(FALSE)
  langs <- setdiff(i18n$get_languages(), i18n$get_key_translation())
  lang %in% langs
}

load_translation_tables <- function(dir) {
  files <- list.files(dir, pattern = "^translation_.*\\.csv$", full.names = TRUE)
  out <- list()
  for (file in files) {
    dat <- tryCatch(
      read.csv(file, stringsAsFactors = FALSE, fileEncoding = "UTF-8"),
      error = function(e) NULL
    )
    if (is.null(dat) || ncol(dat) < 2) next
    key_col <- names(dat)[1]
    lang_col <- names(dat)[2]
    keys <- as.character(dat[[key_col]])
    vals <- as.character(dat[[lang_col]])
    ok <- !is.na(keys) & nzchar(keys)
    out[[lang_col]] <- stats::setNames(vals[ok], keys[ok])
  }
  out
}

i18n_translations <- load_translation_tables(i18n_dir)

tr_text <- function(key, lang = NULL) {
  if (is.null(key)) return("")
  key <- as.character(key)
  if (!length(key)) return(character(0))
  if (is.null(lang) || !nzchar(lang)) lang <- i18n_lang
  lookup <- i18n_translations[[lang]]
  fallback <- i18n_translations[["en"]]
  translate_one <- function(k) {
    if (!nzchar(k)) return(k)
    if (!is.null(lookup)) {
      val <- lookup[[k]]
      if (!is.null(val) && !is.na(val) && nzchar(val)) return(enc2utf8(val))
    }
    if (!is.null(fallback)) {
      val <- fallback[[k]]
      if (!is.null(val) && !is.na(val) && nzchar(val)) return(enc2utf8(val))
    }
    k
  }
  out <- vapply(key, translate_one, character(1))
  unname(out)
}

extract_i18n_key <- function(x) {
  if (inherits(x, "shiny.tag")) {
    key <- x$attribs[["data-key"]]
    if (is.null(key) || !nzchar(key)) {
      key <- x$attribs[["data-i18n"]]
    }
    if (!is.null(key) && nzchar(key)) return(key)
  }
  if (inherits(x, "shiny.tag.list")) {
    for (item in x) {
      key <- extract_i18n_key(item)
      if (!is.null(key)) return(key)
    }
  }
  NULL
}

label_to_text <- function(label) {
  if (is.null(label)) return("")
  if (is.character(label)) return(label[1])
  key <- extract_i18n_key(label)
  if (!is.null(key)) return(tr_text(key))
  if (is.list(label)) {
    nested <- vapply(label, label_to_text, character(1))
    nested <- trimws(nested)
    nested <- nested[nzchar(nested) & !tolower(nested) %in% c("span", "i18n")]
    if (length(nested)) return(nested[[1]])
  }
  out <- tryCatch(as.character(label), error = function(e) character(0))
  if (length(out)) {
    out <- gsub("<[^>]+>", "", out)
    out <- trimws(out)
    out <- out[nzchar(out) & !tolower(out) %in% c("span", "i18n")]
    if (length(out)) return(out[[1]])
  }
  ""
}

tr <- function(key) {
  out <- i18n$t(key)
  if (is.null(out)) return(key)
  if (is.character(out)) {
    if (!length(out)) return(key)
    missing <- is.na(out) | !nzchar(out)
    if (any(missing)) out[missing] <- key[missing]
    out <- enc2utf8(out)
  } else if (!length(out)) {
    return(key)
  }
  out
}

named_choices <- function(values, labels) {
  values <- as.character(values)
  if (missing(labels) || is.null(labels)) labels <- values
  label_list <- if (inherits(labels, "shiny.tag")) {
    list(labels)
  } else if (inherits(labels, "shiny.tag.list") || is.list(labels)) {
    labels
  } else {
    as.list(labels)
  }
  if (length(label_list) != length(values)) {
    label_list <- rep_len(label_list, length(values))
  }
  labels_txt <- vapply(label_list, label_to_text, character(1))
  labels_txt <- enc2utf8(labels_txt)
  out <- as.list(values)
  names(out) <- labels_txt
  out
}

accordion_panel_safe <- function(title, ..., value = NULL) {
  value_chr <- NULL
  if (!is.null(value)) {
    val <- tryCatch(as.character(value), error = function(e) character(0))
    if (length(val) && !is.na(val[1]) && nzchar(val[1])) value_chr <- val[1]
  }
  if (is.null(value_chr)) {
    key <- extract_i18n_key(title)
    if (!is.null(key)) {
      value_chr <- key
    } else if (is.character(title) && length(title) && !is.na(title[1]) && nzchar(title[1])) {
      value_chr <- title[1]
    } else {
      value_chr <- "panel"
    }
  }
  bslib::accordion_panel(title = title, ..., value = value_chr)
}


# Reporta errores indicando el archivo de origen (soporta handlers sin argumento)
options(shiny.error = function(e = NULL) {
  calls <- sys.calls()
  src <- NULL
  for (i in rev(seq_along(calls))) {
    sr <- attr(calls[[i]], 'srcref')
    if (!is.null(sr)) {
      srcfile <- attr(sr, "srcfile")
      if (!is.null(srcfile) && !is.null(srcfile$filename)) {
        src <- basename(srcfile$filename)
        break
      }
    }
  }
  if (is.null(src)) src <- 'fuente_desconocida'
  msg <- if (!is.null(e)) conditionMessage(e) else geterrmessage()
  shiny::showNotification(
    sprintf(tr_text("global_error_template", i18n_lang), src, msg),
    type = 'error', duration = NULL
  )
})

# Avoid noisy jsonlite deprecation warnings from named vectors in Shiny internals.
options(jsonlite.warn_keep_vec_names = FALSE)

## Fuente y color por defecto para TODO ggplot2
theme_update(
  text = element_text(colour = "black", family = "Helvetica")
)

#──────── helpers genéricos para objetos sin método broom::tidy() ────────
# Las funciones matrix_to_tibble() y pmcmr_to_tibble() ahora residen en
# helpers.R para evitar duplicaciones.

# Canonical statistical/file helper implementations live in helpers.R.
# Keep this file free of duplicate helper definitions to avoid silent overrides.

theme_light <- bs_theme(version = 5)
theme_dark  <- bs_theme(version = 5, bootswatch = "cyborg")

tab_compos <- tabPanel(
  title = tr("tab_composition"),
  value = "tab_composition",
  sidebarLayout(
    sidebarPanel(style="overflow-y:auto;max-height:95vh;position:sticky;top:0;",
      bslib::accordion(
        id = "comboAccordion",
        open = c("combo_section_selection", "combo_section_layout", "combo_section_actions"),
        multiple = TRUE,
        accordion_panel_safe(
          tr("combo_section_selection"),
          uiOutput("plotPicker"),
          value = "combo_section_selection"
        ),
        accordion_panel_safe(
          tr("combo_section_layout"),
          numericInput("nrow_combo", tr("combo_rows"), 1, min = 1, max = 4),
          numericInput("ncol_combo", tr("combo_cols"), 1, min = 1, max = 4),
          textAreaInput(
            "combo_layout_grid",
            tr("combo_layout_grid"),
            value = "",
            rows = 4,
            placeholder = "1 1 2\n3 4 4"
          ),
          textInput("combo_col_widths", tr("combo_col_widths"), ""),
          textInput("combo_row_heights", tr("combo_row_heights"), ""),
          helpText(tr("combo_layout_hint")),
          textOutput("comboLayoutMap"),
          numericInput("combo_width",  tr("combo_width"), 1000, min = 400),
          numericInput("combo_height", tr("combo_height"), 700, min = 400),
          value = "combo_section_layout"
        ),
        accordion_panel_safe(
          tr("combo_section_style"),
          helpText(tr("combo_style_live_hint")),
          checkboxInput("show_legend_combo", tr("combo_show_legend"), value = FALSE),
          textInput("combo_title", tr("combo_title"), ""),
          numericInput("combo_title_size", tr("combo_comp_title_size"), 24, min = 8),
          selectInput(
            "combo_legend_scope",
            tr("combo_legend_scope"),
            choices = named_choices(
              c("all_plots", "by_type", "collect"),
              tr_text(
                c("combo_legend_scope_all", "combo_legend_scope_type", "combo_legend_scope_collect"),
                i18n_lang
              )
            ),
            selected = "by_type"
          ),
          radioButtons(
            "combo_legend_side",
            tr("combo_legend_side"),
            choices = named_choices(
              c("right", "left"),
              tr_text(c("combo_legend_side_right", "combo_legend_side_left"), i18n_lang)
            ),
            selected = "right",
            inline = TRUE
          ),
          selectInput(
            "combo_font_family",
            tr("combo_font_family"),
            choices = c("Helvetica", "Arial", "Calibri", "Times New Roman", "Courier New"),
            selected = "Helvetica"
          ),
          checkboxInput("combo_apply_paper_theme", tr("combo_apply_paper_theme"), value = FALSE),
          numericInput("fs_title_all",      tr("combo_plot_title_size"), 20, min = 6),
          numericInput("fs_axis_title_all", tr("combo_axis_titles"), 16, min = 6),
          numericInput("fs_axis_text_all",  tr("combo_axis_ticks"),  14, min = 6),
          numericInput("combo_axis_line_size", tr("combo_axis_line_size"), 1, min = 0.1, step = 0.1),
          numericInput("fs_legend_all",     tr("combo_legend_text"), 16, min = 6),
          selectInput(
            "combo_pal",
            tr("combo_palette_label"),
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
                i18n_lang
              )
            ),
            selected = "Original"
          ),
          checkboxInput("combo_adv_pal_enable", tr("palette_advanced_enable"), value = FALSE),
          conditionalPanel(
            condition = "input.combo_adv_pal_enable == true",
            radioButtons(
              "combo_adv_pal_type",
              tr("palette_type_label"),
              choices = named_choices(
                c("seq", "div", "qual"),
                tr_text(c("palette_type_seq", "palette_type_div", "palette_type_qual"), i18n_lang)
              ),
              selected = "seq",
              inline = TRUE
            ),
            checkboxInput("combo_adv_pal_reverse", tr("palette_reverse"), value = FALSE),
            checkboxGroupInput(
              "combo_adv_pal_filters",
              tr("palette_filters_label"),
              choices = named_choices(
                c("colorblind", "print", "photocopy"),
                tr_text(
                  c("palette_filter_colorblind", "palette_filter_print", "palette_filter_photocopy"),
                  i18n_lang
                )
              ),
              selected = character(0),
              inline = TRUE
            ),
            selectInput(
              "combo_adv_pal_name",
              tr("palette_scheme_label"),
              choices = c("Viridis", "Plasma", "Magma", "Inferno", "Cividis"),
              selected = "Viridis"
            )
          ),
          value = "combo_section_style"
        ),
        accordion_panel_safe(
          tr("combo_section_richtext"),
          checkboxInput("combo_rich_enable", tr("combo_rich_enable"), value = FALSE),
          textAreaInput("combo_rich_text", tr("combo_rich_text"), value = "", rows = 3),
          numericInput("combo_rich_x", tr("combo_rich_x"), 0.50, min = 0, max = 1, step = 0.01),
          numericInput("combo_rich_y", tr("combo_rich_y"), 0.92, min = 0, max = 1, step = 0.01),
          numericInput("combo_rich_box_w", tr("combo_rich_box_w"), 0.40, min = 0.05, max = 1, step = 0.01),
          numericInput("combo_rich_box_h", tr("combo_rich_box_h"), 0.16, min = 0.05, max = 1, step = 0.01),
          numericInput("combo_rich_size", tr("combo_rich_size"), 4.5, min = 1, max = 20, step = 0.25),
          textInput("combo_rich_color", tr("combo_rich_color"), "black"),
          textInput("combo_rich_fill", tr("combo_rich_fill"), "#FFFFFFCC"),
          value = "combo_section_richtext"
        ),
        accordion_panel_safe(
          tr("combo_section_overrides"),
          helpText(tr("combo_override_apply_hint")),
          uiOutput("plotOverrideUI"),
          value = "combo_section_overrides"
        ),
        accordion_panel_safe(
          tr("combo_section_actions"),
          helpText(tr("combo_actions_hint")),
          actionButton("makeCombo", tr("combo_preview_optional"), class = "btn btn-primary"),
          br(), br(),
          downloadButton("dl_combo_png",  tr("combo_download_png")),
          downloadButton("dl_combo_pptx", tr("combo_download_pptx")),
          downloadButton("dl_combo_pdf",  tr("combo_download_pdf")),
          br(), br(),
          downloadButton("dl_combo_meta", tr("combo_download_meta")),
          fileInput("combo_meta", tr("combo_upload_meta"), accept = ".xlsx"),
          value = "combo_section_actions"
        )
      )
    ),
    mainPanel(
      plotOutput("comboPreview", height = "auto"),
      br(),
      actionButton(
        "copy_combo_clipboard",
        label = tagList(icon("copy"), tr("copy_plot")),
        class = "btn btn-secondary"
      )
    )
  )
)



# ──────────────────────────────────────────────────────────────────────────────  
# Helper robusto: lee una hoja Excel aunque el nombre temporal NO tenga extensión  
read_excel_tmp <- function(path, sheet = NULL) {  
  sig <- readBin(path, "raw", n = 8)  
  
  # 0xD0CF11E0 = formato OLE (xls 97-2003)  
  is_xls  <- identical(sig[1:4], as.raw(c(0xD0,0xCF,0x11,0xE0)))  
  # 0x504B0304 = inicio de archivo ZIP (xlsx, xlsm, ods…)  
  is_zip  <- identical(sig[1:4], as.raw(c(0x50,0x4B,0x03,0x04)))  
  
  if (is_xls)       return(readxl::read_xls (path, sheet = sheet))  
  if (is_zip)       return(readxl::read_xlsx(path, sheet = sheet))  
  
  stop("El archivo subido no es un Excel válido (.xls o .xlsx)")  
}  

excel_sheets_tmp <- function(path) {
  sig <- readBin(path, "raw", n = 8)

  is_xls <- identical(sig[1:4], as.raw(c(0xD0, 0xCF, 0x11, 0xE0)))
  is_zip <- identical(sig[1:4], as.raw(c(0x50, 0x4B, 0x03, 0x04)))

  if (!isTRUE(is_xls) && !isTRUE(is_zip)) {
    stop("El archivo subido no es un Excel válido (.xls o .xlsx)")
  }

  ext <- if (isTRUE(is_xls)) ".xls" else ".xlsx"
  tmp <- tempfile(fileext = ext)
  on.exit(unlink(tmp), add = TRUE)

  copied <- file.copy(path, tmp, overwrite = TRUE)
  if (!isTRUE(copied)) {
    stop("No se pudo preparar el archivo temporal para leer las hojas de Excel.")
  }

  readxl::excel_sheets(tmp)
}

is_csv_filename <- function(name) {
  nm <- as.character(name %||% "")
  if (!length(nm) || is.na(nm[[1]]) || !nzchar(nm[[1]])) return(FALSE)
  identical(tolower(tools::file_ext(nm[[1]])), "csv")
}

detect_csv_delimiter <- function(path, candidates = c(",", ";", "\t", "|"), n_max = 20L) {
  lines <- tryCatch(
    readLines(path, n = n_max, warn = FALSE, encoding = "UTF-8"),
    error = function(e) character(0)
  )
  if (!length(lines)) return(",")
  lines[[1]] <- sub("^\ufeff", "", lines[[1]])
  lines <- lines[!is.na(lines) & nzchar(trimws(lines))]
  if (!length(lines)) return(",")

  # Prefer header structure first to avoid confusing decimal commas as delimiters.
  header <- lines[[1]]
  header_counts <- vapply(candidates, function(delim) {
    length(strsplit(header, delim, fixed = TRUE)[[1]])
  }, integer(1))
  header_best <- which.max(header_counts)
  if (is.finite(header_counts[[header_best]]) && header_counts[[header_best]] > 1) {
    return(candidates[[header_best]])
  }

  score_delim <- function(delim) {
    pieces <- strsplit(lines, delim, fixed = TRUE)
    counts <- vapply(pieces, length, integer(1))
    if (!length(counts)) return(-Inf)
    stats::median(counts, na.rm = TRUE)
  }

  scores <- vapply(candidates, score_delim, numeric(1))
  best_idx <- which.max(scores)
  best <- candidates[[best_idx]]
  if (!is.finite(scores[[best_idx]]) || scores[[best_idx]] <= 1) "," else best
}

read_csv_tmp <- function(path, delim = NULL) {
  delim_chr <- as.character(delim %||% detect_csv_delimiter(path))
  delim_chr <- if (length(delim_chr)) delim_chr[[1]] else ","
  if (!nzchar(delim_chr)) delim_chr <- ","

  out <- tryCatch(
    readr::read_delim(
      file = path,
      delim = delim_chr,
      col_names = TRUE,
      locale = readr::locale(encoding = "UTF-8"),
      trim_ws = TRUE,
      show_col_types = FALSE,
      progress = FALSE,
      name_repair = "minimal",
      na = c("", "NA", "N/A")
    ),
    error = function(e) NULL
  )

  if (is.null(out)) {
    out <- tryCatch(
      {
        if (identical(delim_chr, "\t")) {
          utils::read.delim(
            path,
            stringsAsFactors = FALSE,
            check.names = FALSE,
            fileEncoding = "UTF-8-BOM"
          )
        } else {
          utils::read.table(
            path,
            sep = delim_chr,
            header = TRUE,
            quote = "\"",
            comment.char = "",
            stringsAsFactors = FALSE,
            check.names = FALSE,
            fileEncoding = "UTF-8-BOM"
          )
        }
      },
      error = function(e) {
        stop(sprintf("Could not read CSV file: %s", conditionMessage(e)))
      }
    )
  }

  as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
}
# ──────────────────────────────────────────────────────────────────────────────  

# Natural sort for replicate-like IDs (1,2,3,...,10) while keeping non-numeric IDs stable.
order_export_replicates <- function(
  df,
  strain_col = NULL,
  rep_col = "BiologicalReplicate",
  tech_col = NULL,
  time_col = NULL
) {
  if (is.null(df) || !is.data.frame(df) || !nrow(df)) return(df)

  key_chr_num <- function(x) {
    chr <- as.character(x)
    num <- suppressWarnings(as.numeric(chr))
    non_num <- !is.finite(num)
    num[non_num] <- Inf
    list(non_num = non_num, num = num, chr = chr)
  }

  n <- nrow(df)
  strain_chr <- if (!is.null(strain_col) && strain_col %in% names(df)) as.character(df[[strain_col]]) else rep("", n)

  rep_key <- if (rep_col %in% names(df)) key_chr_num(df[[rep_col]]) else list(non_num = rep(FALSE, n), num = rep(Inf, n), chr = rep("", n))
  tech_key <- if (!is.null(tech_col) && tech_col %in% names(df)) key_chr_num(df[[tech_col]]) else list(non_num = rep(FALSE, n), num = rep(Inf, n), chr = rep("", n))
  time_key <- if (!is.null(time_col) && time_col %in% names(df)) key_chr_num(df[[time_col]]) else list(non_num = rep(FALSE, n), num = rep(Inf, n), chr = rep("", n))

  ord <- order(
    strain_chr,
    rep_key$non_num, rep_key$num, rep_key$chr,
    tech_key$non_num, tech_key$num, tech_key$chr,
    time_key$non_num, time_key$num, time_key$chr,
    na.last = TRUE
  )
  df[ord, , drop = FALSE]
}

# ── Función para generar el Excel de resumen por parámetro ────────────  
generate_summary_wb <- function(datos, params, wb = NULL, sheet_suffix = "") {  
  if (is.null(wb)) wb <- createWorkbook()
  suffix <- as.character(sheet_suffix %||% "")
  for (param in params) {
    sheet <- safe_sheet(paste0(param, suffix))
    if (sheet %in% openxlsx::sheets(wb)) {
      removeWorksheet(wb, sheet = sheet)
    }
    addWorksheet(wb, sheet)

    # 1) Detalle técnico
    det <- datos %>%
      dplyr::filter(!is.na(Strain), !is.na(Media)) %>%
      dplyr::select(
        Strain, Media, Orden,
        BiologicalReplicate, TechnicalReplicate,
        Valor = !!rlang::sym(param)
      ) %>%
      dplyr::group_by(Strain, Media, Orden,
                       BiologicalReplicate, TechnicalReplicate) %>%
      dplyr::summarise(Valor = mean(Valor, na.rm = TRUE),
                       .groups = "drop")
    det <- order_export_replicates(
      det,
      strain_col = "Strain",
      rep_col = "BiologicalReplicate",
      tech_col = "TechnicalReplicate"
    )
    
    # 2) Orden de medios según 'Orden'  
    medias_order <- det %>%  
      dplyr::distinct(Media, Orden) %>%  
      dplyr::arrange(Orden) %>%  
      dplyr::pull(Media)  
    
    # 3) Escribir por cepa  
    fila <- 1  
    for (s in unique(det$Strain)) {  
      writeData(wb, sheet, paste("Strain:", s),  
                startRow = fila, startCol = 1)  
      fila <- fila + 1  
      
      tab_cepa <- det %>%
        dplyr::filter(Strain == s) %>%
        dplyr::select(BiologicalReplicate, TechnicalReplicate, Media, Valor) %>%
        tidyr::pivot_wider(
          id_cols    = c(BiologicalReplicate, TechnicalReplicate),
          names_from = Media,
          values_from = Valor,
          values_fill = NA,
          values_fn  = list(Valor = ~ mean(.x, na.rm = TRUE))
        )
      tab_cepa <- order_export_replicates(
        tab_cepa,
        rep_col = "BiologicalReplicate",
        tech_col = "TechnicalReplicate"
      ) %>%
        dplyr::select(BiologicalReplicate,
                      dplyr::any_of(medias_order)) %>%
        dplyr::rename(RepBiol = BiologicalReplicate)
      
      writeData(wb, sheet, tab_cepa,  
                startRow    = fila,  
                startCol    = 1,  
                headerStyle = createStyle(textDecoration = "bold"))  
      fila <- fila + nrow(tab_cepa) + 2  
    }  
    
    # 4) Resumen por réplica biológica  
    writeData(wb, sheet, "Resumen por réplica biológica",  
              startRow    = fila,  
              startCol    = 1,  
              headerStyle = createStyle(fontSize = 12, textDecoration = "bold"))  
    fila <- fila + 1  
    
    resumen <- det %>%
      dplyr::group_by(Strain, BiologicalReplicate, Media) %>%
      dplyr::summarise(Promedio = mean(Valor, na.rm = TRUE),
                       .groups = "drop") %>%
      tidyr::pivot_wider(
        id_cols    = c(Strain, BiologicalReplicate),
        names_from = Media,
        values_from = Promedio,
        values_fill = NA,
        values_fn  = list(Promedio = ~ mean(.x, na.rm = TRUE))
      )
    resumen <- order_export_replicates(
      resumen,
      strain_col = "Strain",
      rep_col = "BiologicalReplicate"
    ) %>%
      dplyr::rename(RepBiol = BiologicalReplicate)
    
    writeData(wb, sheet, resumen,  
              startRow    = fila,  
              startCol    = 1,  
              headerStyle = createStyle(textDecoration = "bold"))  
  }  
  wb
}

add_curves_by_group_sheet <- function(wb, curve_wide, meta_df, sheet_name = "Curvas por grupo") {  
  if (is.null(wb) || is.null(curve_wide) || is.null(meta_df)) return(wb)  
  
  required_cols <- c("Well", "Strain", "Media", "Orden", "BiologicalReplicate")  
  if (!all(required_cols %in% names(meta_df))) return(wb)  
  if (!"Time" %in% names(curve_wide)) return(wb)  
  
  wells <- intersect(setdiff(names(curve_wide), "Time"), meta_df$Well)  
  if (!length(wells)) return(wb)  
  
  meta_curves <- meta_df %>%  
    dplyr::select(dplyr::all_of(required_cols)) %>%  
    dplyr::distinct() %>%  
    dplyr::filter(.data$Well %in% wells)  
  if (!nrow(meta_curves)) return(wb)  
  
  curves_long <- curve_wide %>%  
    dplyr::mutate(Time = suppressWarnings(as.numeric(.data$Time))) %>%  
    dplyr::mutate(dplyr::across(dplyr::all_of(wells), ~ suppressWarnings(as.numeric(.x)))) %>%  
    tidyr::pivot_longer(  
      cols      = dplyr::all_of(wells),  
      names_to  = "Well",  
      values_to = "Valor"  
    ) %>%  
    dplyr::left_join(meta_curves, by = "Well") %>%  
    dplyr::filter(!is.na(.data$Strain), !is.na(.data$Media), !is.na(.data$BiologicalReplicate))  
  if (!nrow(curves_long)) return(wb)  
  
  resumen_curvas <- curves_long %>%  
    dplyr::group_by(Strain, Media, Orden, BiologicalReplicate, Time) %>%  
    dplyr::summarise(Valor = mean(Valor, na.rm = TRUE), .groups = "drop")  
  if (!nrow(resumen_curvas)) return(wb)  
  
  sheet_name <- as.character(sheet_name %||% "Curvas por grupo")
  if (!length(sheet_name) || is.na(sheet_name[[1]]) || !nzchar(sheet_name[[1]])) {
    sheet_name <- "Curvas por grupo"
  } else {
    sheet_name <- sheet_name[[1]]
  }
  sheet_name <- gsub("[\\[\\]:*?/\\\\]", "_", sheet_name)
  sheet_name <- substr(sheet_name, 1, 31)
  existing_sheets <- tryCatch(openxlsx::sheets(wb),  
                              error = function(e) tryCatch(names(wb), error = function(...) character()))  
  if (sheet_name %in% existing_sheets) {  
    removeWorksheet(wb, sheet = sheet_name)  
  }  
  addWorksheet(wb, sheetName = sheet_name)  
  header_style <- createStyle(textDecoration = "bold")  
  
  fila <- 1  
  for (s in unique(resumen_curvas$Strain)) {  
    sub <- dplyr::filter(resumen_curvas, Strain == s)  
    if (!nrow(sub)) next  
    
    media_order <- sub %>%  
      dplyr::distinct(Media, Orden) %>%  
      dplyr::arrange(Orden) %>%  
      dplyr::pull(Media)  
    
    tab_cur <- sub %>%  
      tidyr::pivot_wider(  
        id_cols    = c(Time, BiologicalReplicate),  
        names_from = Media,  
        values_from = Valor,  
        values_fill = NA,  
        values_fn  = list(Valor = ~ mean(.x, na.rm = TRUE))  
      )
    tab_cur <- order_export_replicates(
      tab_cur,
      rep_col = "BiologicalReplicate",
      time_col = "Time"
    ) %>%  
      dplyr::select(Time, BiologicalReplicate, dplyr::any_of(media_order)) %>%  
      dplyr::rename(RepBiol = BiologicalReplicate)  
    
    writeData(wb, sheet_name, paste("Strain:", s),  
              startRow = fila, startCol = 1)  
    fila <- fila + 1  
    
    writeData(wb, sheet_name, tab_cur,  
              startRow    = fila,  
              startCol    = 1,  
              headerStyle = header_style)  
    fila <- fila + nrow(tab_cur) + 2  
  }  
  
  wb  
}  

# ────────────────────────────────────────────────────────────────
# Conversión de "Resumen por réplica biológica" a formato platemap
# ────────────────────────────────────────────────────────────────

round_up_sig <- function(x, digits = 2) {
  if (is.na(x)) return(NA_real_)
  if (x <= 0) return(0)
  pow   <- floor(log10(abs(x)))
  scale <- 10^(pow - (digits - 1))
  ceiling(x / scale) * scale
}

to_numeric <- function(v) {
  if (is.numeric(v)) return(as.numeric(v))
  v_chr <- as.character(v)
  y <- suppressWarnings(readr::parse_number(v_chr, locale = readr::locale(decimal_mark = ".")))
  if (any(is.na(y) & grepl(",", v_chr, fixed = TRUE))) {
    idx <- is.na(y) & grepl(",", v_chr, fixed = TRUE)
    y[idx] <- suppressWarnings(readr::parse_number(v_chr[idx], locale = readr::locale(decimal_mark = ",")))
  }
  y
}

gen_wells <- function(n) {
  base_letters <- rep(LETTERS[1:8], each = 12)
  base_numbers <- rep(1:12, times = 8)
  base <- paste0(base_letters, base_numbers)
  if (n <= 96) base[seq_len(n)] else c(base, paste0("H", 13:(12 + (n - 96))))
}

normalize_header_key <- function(x) {
  if (is.null(x)) return(character(0))
  out <- as.character(x)
  out <- gsub("\u00A0", " ", out, fixed = TRUE)
  out <- trimws(out)
  out[is.na(out)] <- ""
  out <- iconv(out, from = "", to = "ASCII//TRANSLIT")
  out[is.na(out)] <- ""
  out <- toupper(out)
  gsub("[^A-Z0-9]", "", out)
}

column_aliases <- list(
  Strain = c("Strain", "Cepa", "Muestra", "Sample"),
  Media  = c("Media", "Condicion", "Condition", "Tratamiento", "Treatment")
)

match_alias_column <- function(headers, aliases) {
  if (is.null(headers) || !length(headers)) return(NA_integer_)
  hdr_norm <- normalize_header_key(headers)
  alias_norm <- normalize_header_key(aliases)
  idx <- match(alias_norm, hdr_norm)
  idx <- idx[!is.na(idx)]
  if (length(idx)) idx[1] else NA_integer_
}

apply_column_aliases <- function(df, allow_media_alias = TRUE) {
  labels <- list(Strain = NULL, Media = NULL)
  if (is.null(df) || !is.data.frame(df) || is.null(names(df))) {
    return(list(datos = df, labels = labels))
  }
  headers <- names(df)
  strain_idx <- match_alias_column(headers, column_aliases$Strain)
  if (!is.na(strain_idx)) {
    labels$Strain <- sub(":$", "", trimws(headers[[strain_idx]]))
    names(df)[strain_idx] <- "Strain"
  }
  if (isTRUE(allow_media_alias)) {
    media_idx <- match_alias_column(headers, column_aliases$Media)
    if (!is.na(media_idx)) {
      labels$Media <- sub(":$", "", trimws(headers[[media_idx]]))
      names(df)[media_idx] <- "Media"
    }
  }
  list(datos = df, labels = labels)
}

platemap_from_summary_wb <- function(file) {
  sheets <- excel_sheets_tmp(file)
  res <- lapply(sheets, function(sh) {
    full <- readxl::read_excel(file, sheet = sh, col_names = FALSE)

    idx <- which(
      normalize_header_key(full[[1]]) %in% normalize_header_key(column_aliases$Strain) &
        normalize_header_key(full[[2]]) == normalize_header_key("RepBiol")
    )
    if (length(idx) == 0) return(NULL)
    idx <- idx[1]

    raw_hdr <- as.character(unlist(full[idx, ], use.names = FALSE))
    raw_hdr <- trimws(raw_hdr)
    is_blank <- is.na(raw_hdr) | raw_hdr == ""
    raw_hdr[is_blank] <- paste0("V", seq_len(sum(is_blank)))
    hdr <- make.unique(raw_hdr, sep = "_")
    hdr_norm <- normalize_header_key(hdr)
    hdr[hdr_norm %in% normalize_header_key(column_aliases$Strain)] <- "Strain"
    hdr[hdr_norm == normalize_header_key("RepBiol")] <- "RepBiol"

    if (nrow(full) <= idx) return(NULL)
    ncols <- length(hdr)

    dat <- full[(idx + 1):nrow(full), seq_len(ncols)]
    dat <- as.data.frame(dat, stringsAsFactors = FALSE, check.names = FALSE)
    names(dat) <- hdr
    dat <- tibble::as_tibble(dat)

    if (!all(c("Strain", "RepBiol") %in% names(dat))) return(NULL)
    strain_label <- raw_hdr[match(TRUE, hdr_norm %in% normalize_header_key(column_aliases$Strain))]
    strain_label <- sub(":$", "", trimws(as.character(strain_label)))
    if (is.na(strain_label) || !nzchar(strain_label)) strain_label <- NULL
    dat <- dplyr::filter(dat, !is.na(.data$Strain), !is.na(.data$RepBiol))
    if (!nrow(dat)) return(NULL)

    media_cols <- setdiff(names(dat), c("Strain", "RepBiol"))

    out <- dat |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(media_cols),
        names_to = "Media",
        values_to = "Valor"
      ) |>
      dplyr::mutate(
        BiologicalReplicate = .data$RepBiol,
        TechnicalReplicate = 1,
        Parameter = sh
      ) |>
      dplyr::select(dplyr::all_of(
        c(
          "Strain",
          "Media",
          "BiologicalReplicate",
          "TechnicalReplicate",
          "Parameter",
          "Valor"
        )
      ))
    attr(out, "col_labels") <- list(Strain = strain_label, Media = NULL)
    out
  })

  res <- Filter(Negate(is.null), res)
  if (!length(res)) return(NULL)
  labels <- attr(res[[1]], "col_labels")
  out <- dplyr::bind_rows(res)
  attr(out, "col_labels") <- labels
  out
}

build_platemap_from_summary <- function(file) {
  long_df <- platemap_from_summary_wb(file)
  if (is.null(long_df) || !nrow(long_df)) return(NULL)
  col_labels <- attr(long_df, "col_labels")

  order_map <- long_df |>
    dplyr::distinct(Strain, Media) |>
    dplyr::mutate(Orden = dplyr::row_number())

  param_order <- unique(long_df$Parameter)
  first_or_na <- function(x) { x <- x[!is.na(x)]; if (length(x)) x[1] else NA }

  wide_df <- long_df |>
    tidyr::pivot_wider(
      id_cols    = c(Strain, Media, BiologicalReplicate, TechnicalReplicate),
      names_from = Parameter,
      values_from = Valor,
      values_fn   = first_or_na,
      values_fill = NA
    ) |>
    dplyr::left_join(order_map, by = c("Strain", "Media")) |>
    dplyr::relocate(Orden, .after = Media) |>
    dplyr::relocate(dplyr::any_of(param_order), .after = TechnicalReplicate) |>
    dplyr::arrange(Orden, BiologicalReplicate, TechnicalReplicate)

  wells <- gen_wells(nrow(wide_df))
  wide_df <- wide_df |>
    dplyr::mutate(Well = wells) |>
    dplyr::relocate(Well, .before = Strain)

  id_cols <- c("Well", "Strain", "Media", "Orden", "BiologicalReplicate", "TechnicalReplicate")
  param_cols <- setdiff(names(wide_df), id_cols)

  wide_df <- wide_df |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(param_cols), ~ to_numeric(.x)),
      BiologicalReplicate = as.integer(to_numeric(BiologicalReplicate)),
      TechnicalReplicate  = as.integer(to_numeric(TechnicalReplicate)),
      Orden               = as.integer(to_numeric(Orden))
    )

  col_max <- function(v) if (all(is.na(v))) NA_real_ else max(v, na.rm = TRUE)
  raw_max <- vapply(param_cols, function(nm) col_max(wide_df[[nm]]), numeric(1))
  y_max   <- vapply(param_cols, function(nm) round_up_sig(raw_max[[nm]], digits = 2), numeric(1))
  interval <- ifelse(is.na(y_max) | y_max == 0, NA_real_, y_max / 5)

  plot_settings <- tibble::tibble(
    Parameter = param_cols,
    Y_Max     = y_max,
    Interval  = interval,
    Y_Title   = param_cols
  )

  list(Datos = wide_df, PlotSettings = plot_settings, Labels = col_labels)
}

platemap_merge_key <- function(strain, media) {
  strain_chr <- ifelse(is.na(strain), "<NA>", as.character(strain))
  media_chr <- ifelse(is.na(media), "<NA>", as.character(media))
  paste0(strain_chr, "||", media_chr)
}

merge_clean_char <- function(x) {
  if (is.character(x)) trimws(x) else x
}

merge_read_sheet_text <- function(path, sheet = NULL) {
  sig <- readBin(path, "raw", n = 8)
  is_xls <- identical(sig[1:4], as.raw(c(208, 207, 17, 224)))
  is_zip <- identical(sig[1:4], as.raw(c(80, 75, 3, 4)))

  # Force text import for merge workflow so literal labels like "NA"
  # are preserved as text and not silently coerced to missing values.
  if (is_xls) {
    return(readxl::read_xls(path, sheet = sheet, col_types = "text", na = ""))
  }
  if (is_zip) {
    return(readxl::read_xlsx(path, sheet = sheet, col_types = "text", na = ""))
  }
  stop("The uploaded file is not a valid Excel workbook (.xls or .xlsx).")
}

merge_add_missing_cols <- function(df, target_cols) {
  missing_cols <- setdiff(target_cols, names(df))
  if (length(missing_cols)) {
    for (cc in missing_cols) df[[cc]] <- NA
  }
  df[, target_cols, drop = FALSE]
}

merge_excel_letters <- function(n) {
  out <- character(n)
  for (i in seq_len(n)) {
    x <- i
    s <- ""
    while (x > 0) {
      r <- (x - 1) %% 26
      s <- paste0(LETTERS[r + 1], s)
      x <- (x - 1) %/% 26
    }
    out[[i]] <- s
  }
  out
}

merge_make_plate_wells <- function(n, ncol = 12) {
  row_idx <- ((seq_len(n) - 1) %/% ncol) + 1
  col_idx <- ((seq_len(n) - 1) %% ncol) + 1
  paste0(merge_excel_letters(max(row_idx))[row_idx], col_idx)
}

merge_pad_group_rows <- function(df, n_target) {
  if (nrow(df) >= n_target) return(df)

  if (!nrow(df)) {
    out <- as.data.frame(
      matrix(NA, nrow = n_target, ncol = ncol(df)),
      stringsAsFactors = FALSE
    )
    names(out) <- names(df)
    return(out)
  }

  extra_n <- n_target - nrow(df)
  extra <- df[rep(1, extra_n), , drop = FALSE]
  extra[,] <- NA

  if ("Strain" %in% names(df)) extra$Strain <- df$Strain[[1]]
  if ("Media" %in% names(df)) extra$Media <- df$Media[[1]]
  if ("Orden" %in% names(df)) extra$Orden <- df$Orden[[1]]

  dplyr::bind_rows(df, extra)
}

merge_prepare_group_replicates <- function(df) {
  if (is.null(df) || !is.data.frame(df) || !nrow(df)) return(df)

  if (!"BiologicalReplicate" %in% names(df)) df$BiologicalReplicate <- NA_integer_
  if (!"Replicate" %in% names(df)) df$Replicate <- NA_integer_
  if (!"TechnicalReplicate" %in% names(df)) df$TechnicalReplicate <- NA_character_

  bio <- suppressWarnings(as.integer(to_numeric(df$BiologicalReplicate)))
  rep <- suppressWarnings(as.integer(to_numeric(df$Replicate)))

  bio_from_rep <- is.na(bio) & !is.na(rep)
  bio[bio_from_rep] <- rep[bio_from_rep]

  rep_from_bio <- is.na(rep) & !is.na(bio)
  rep[rep_from_bio] <- bio[rep_from_bio]

  tech <- as.character(df$TechnicalReplicate)
  tech_blank <- is.na(tech) | !nzchar(trimws(tech))
  tech[tech_blank] <- "A"

  df$BiologicalReplicate <- bio
  df$Replicate <- rep
  df$TechnicalReplicate <- tech
  df
}

merge_compact_biorep <- function(df) {
  if (is.null(df) || !is.data.frame(df) || !nrow(df)) return(integer(0))
  tmp <- merge_prepare_group_replicates(df)
  bio <- suppressWarnings(as.integer(to_numeric(tmp$BiologicalReplicate)))
  rep <- suppressWarnings(as.integer(to_numeric(tmp$Replicate)))
  bio_from_rep <- is.na(bio) & !is.na(rep)
  bio[bio_from_rep] <- rep[bio_from_rep]
  if (all(is.na(bio))) bio <- rep(1L, length(bio))
  bio[is.na(bio)] <- 1L
  uniq <- unique(bio)
  mapping <- stats::setNames(seq_along(uniq), as.character(uniq))
  as.integer(unname(mapping[as.character(bio)]))
}

merge_offset_biorep <- function(old_group, inc_group) {
  if (is.null(inc_group) || !is.data.frame(inc_group) || !nrow(inc_group)) return(inc_group)
  old_group <- merge_prepare_group_replicates(old_group)
  inc_group <- merge_prepare_group_replicates(inc_group)

  old_bio <- suppressWarnings(as.integer(to_numeric(old_group$BiologicalReplicate)))
  old_max <- suppressWarnings(max(old_bio, na.rm = TRUE))
  if (!is.finite(old_max)) old_max <- 0L

  inc_bio <- merge_compact_biorep(inc_group)
  inc_group$BiologicalReplicate <- as.integer(inc_bio + as.integer(old_max))
  inc_group$Replicate <- inc_group$BiologicalReplicate
  inc_group
}

merge_finalize_group_replicates <- function(df) {
  if (is.null(df) || !is.data.frame(df) || !nrow(df)) return(df)
  out <- merge_prepare_group_replicates(df)
  if (!".GroupKey" %in% names(out)) {
    out$.GroupKey <- platemap_merge_key(out$Strain, out$Media)
  }

  keys <- as.character(out$.GroupKey)
  key_levels <- unique(keys)

  for (kk in key_levels) {
    idx <- which(keys == kk)
    if (!length(idx)) next

    bio <- suppressWarnings(as.integer(to_numeric(out$BiologicalReplicate[idx])))
    rep <- suppressWarnings(as.integer(to_numeric(out$Replicate[idx])))

    bio_from_rep <- is.na(bio) & !is.na(rep)
    bio[bio_from_rep] <- rep[bio_from_rep]

    if (all(is.na(bio))) {
      bio <- rep(1L, length(idx))
    } else if (any(is.na(bio))) {
      next_val <- max(bio, na.rm = TRUE) + 1L
      bio[is.na(bio)] <- seq.int(next_val, length.out = sum(is.na(bio)))
    }

    uniq <- unique(bio)
    mapping <- stats::setNames(seq_along(uniq), as.character(uniq))
    bio <- as.integer(unname(mapping[as.character(bio)]))

    out$BiologicalReplicate[idx] <- bio
    out$Replicate[idx] <- bio
  }

  out
}

auto_map_cfg_parameters <- function(cfg, data_params) {
  if (is.null(cfg) || !is.data.frame(cfg) || !nrow(cfg) || !"Parameter" %in% names(cfg)) {
    return(cfg)
  }
  data_params <- as.character(data_params %||% character(0))
  data_params <- data_params[!is.na(data_params) & nzchar(data_params)]
  if (!length(data_params)) return(cfg)

  cfg$Parameter <- trimws(as.character(cfg$Parameter))
  cfg_only <- setdiff(cfg$Parameter[!is.na(cfg$Parameter) & nzchar(cfg$Parameter)], data_params)
  data_only <- setdiff(data_params, cfg$Parameter)

  # Conservative auto-fix for one-to-one mismatch (for example CTP1 vs YBR291C).
  if (length(cfg_only) == 1L && length(data_only) == 1L) {
    cfg$Parameter[cfg$Parameter == cfg_only[[1]]] <- data_only[[1]]
  }

  cfg
}

read_platemap_for_merge <- function(file, file_label = basename(file), media_alias = NULL, file_index = 1L) {
  datos_raw <- merge_read_sheet_text(file, sheet = "Datos")
  mapped <- apply_column_aliases(datos_raw, allow_media_alias = TRUE)
  datos <- as.data.frame(mapped$datos, stringsAsFactors = FALSE, check.names = FALSE)

  names(datos) <- trimws(names(datos))
  datos[] <- lapply(datos, merge_clean_char)

  if (!is.null(media_alias) && "Media" %in% names(datos)) {
    datos$Media <- dplyr::recode(datos$Media, !!!as.list(media_alias))
  }

  plot <- tryCatch(
    merge_read_sheet_text(file, sheet = "PlotSettings"),
    error = function(e) data.frame()
  )
  plot <- as.data.frame(plot, stringsAsFactors = FALSE, check.names = FALSE)
  if (ncol(plot) > 0) {
    names(plot) <- trimws(names(plot))
    plot[] <- lapply(plot, merge_clean_char)
  }

  meta_cols <- c(
    "Well", "Strain", "Media", "Orden",
    "Replicate", "BiologicalReplicate", "TechnicalReplicate"
  )

  for (cc in setdiff(meta_cols, names(datos))) {
    datos[[cc]] <- NA
  }

  datos$Strain <- as.character(datos$Strain)
  datos$Media <- as.character(datos$Media)
  datos$Well <- as.character(datos$Well)
  datos$TechnicalReplicate <- as.character(datos$TechnicalReplicate)
  datos$.SourceLabel <- as.character(file_label)
  datos$.SourceWell <- as.character(datos$Well)
  datos$.SourceRow <- seq_len(nrow(datos))
  datos$.SourceIndex <- as.integer(file_index)

  datos$Orden <- suppressWarnings(as.integer(to_numeric(datos$Orden)))
  if (all(is.na(datos$Orden))) {
    grp0 <- unique(platemap_merge_key(datos$Strain, datos$Media))
    ord0 <- seq_along(grp0)
    names(ord0) <- grp0
    datos$Orden <- ord0[platemap_merge_key(datos$Strain, datos$Media)]
  }

  datos$Replicate <- suppressWarnings(as.integer(to_numeric(datos$Replicate)))
  datos$BiologicalReplicate <- suppressWarnings(as.integer(to_numeric(datos$BiologicalReplicate)))
  if (all(is.na(datos$BiologicalReplicate)) && any(!is.na(datos$Replicate))) {
    datos$BiologicalReplicate <- datos$Replicate
  }
  if (all(is.na(datos$Replicate)) && any(!is.na(datos$BiologicalReplicate))) {
    datos$Replicate <- datos$BiologicalReplicate
  }

  technical_blank <- is.na(datos$TechnicalReplicate) | !nzchar(trimws(datos$TechnicalReplicate))
  datos$TechnicalReplicate[technical_blank] <- "A"

  non_param_cols <- c(meta_cols, ".SourceLabel", ".SourceWell", ".SourceRow", ".SourceIndex", ".merge_key")
  data_params <- setdiff(names(datos), non_param_cols)
  data_params <- data_params[!is.na(data_params) & nzchar(data_params)]
  if (length(data_params)) {
    datos[data_params] <- lapply(datos[data_params], to_numeric)
  }

  plot <- auto_map_cfg_parameters(plot, data_params)
  if (nrow(plot) > 0 && "Parameter" %in% names(plot)) {
    plot$Parameter <- trimws(as.character(plot$Parameter))
    plot <- plot[!is.na(plot$Parameter) & nzchar(plot$Parameter), , drop = FALSE]
  }

  datos$.merge_key <- platemap_merge_key(datos$Strain, datos$Media)

  list(
    datos = datos,
    plot = plot,
    labels = mapped$labels,
    source = tools::file_path_sans_ext(file_label),
    params = data_params,
    file_label = as.character(file_label),
    file_index = as.integer(file_index)
  )
}

merge_platemap_workbooks <- function(
  base_file,
  additional_files = character(0),
  additional_labels = NULL,
  media_alias = NULL,
  plate_ncol = 12
) {
  additional_files <- as.character(additional_files %||% character(0))
  additional_files <- additional_files[!is.na(additional_files) & nzchar(additional_files)]
  if (!length(additional_files)) {
    stop("No additional platemap files were provided for merge.")
  }

  if (is.null(base_file) || !nzchar(base_file) || !file.exists(base_file)) {
    stop("Base platemap file does not exist.")
  }

  initial_labels <- if (is.null(additional_labels)) {
    basename(additional_files)
  } else {
    additional_labels <- as.character(additional_labels)
    if (length(additional_labels) == length(additional_files)) {
      additional_labels
    } else {
      basename(additional_files)
    }
  }

  base_norm <- normalizePath(base_file, winslash = "/", mustWork = FALSE)
  add_norm <- normalizePath(additional_files, winslash = "/", mustWork = FALSE)
  keep <- add_norm != base_norm
  additional_files <- additional_files[keep]
  add_norm <- add_norm[keep]
  initial_labels <- initial_labels[keep]

  # Avoid duplicate files when users upload the same file multiple times.
  if (length(additional_files)) {
    unique_idx <- !duplicated(add_norm)
    additional_files <- additional_files[unique_idx]
    add_norm <- add_norm[unique_idx]
    initial_labels <- initial_labels[unique_idx]
  }
  if (!length(additional_files)) {
    stop("No additional platemap files were provided for merge.")
  }

  additional_labels <- initial_labels

  files <- c(base_file, additional_files)
  labels <- c(basename(base_file), additional_labels)

  plate_list <- lapply(
    seq_along(files),
    function(i) {
      read_platemap_for_merge(
        file = files[[i]],
        file_label = labels[[i]],
        media_alias = media_alias,
        file_index = i
      )
    }
  )

  meta_cols <- c(
    "Well", "Strain", "Media", "Orden",
    "Replicate", "BiologicalReplicate", "TechnicalReplicate"
  )
  track_cols <- c(".SourceLabel", ".SourceWell", ".SourceRow", ".SourceIndex")

  base_df <- plate_list[[1]]$datos
  names(base_df) <- trimws(names(base_df))
  base_df[] <- lapply(base_df, merge_clean_char)

  for (cc in setdiff(meta_cols, names(base_df))) {
    base_df[[cc]] <- NA
  }
  for (cc in setdiff(track_cols, names(base_df))) {
    base_df[[cc]] <- NA
  }
  if (!".merge_key" %in% names(base_df)) {
    base_df$.merge_key <- platemap_merge_key(base_df$Strain, base_df$Media)
  }

  if (all(is.na(suppressWarnings(as.integer(to_numeric(base_df$Orden)))))) {
    grp0 <- unique(base_df$.merge_key)
    ord0 <- seq_along(grp0)
    names(ord0) <- grp0
    base_df$Orden <- ord0[base_df$.merge_key]
  }
  base_df$Orden <- suppressWarnings(as.integer(to_numeric(base_df$Orden)))

  base_params <- setdiff(names(base_df), c(meta_cols, ".merge_key", track_cols))
  base_params <- base_params[!is.na(base_params) & nzchar(base_params)]
  all_param_cols <- base_params
  all_cols <- c(meta_cols, all_param_cols, ".merge_key", track_cols)

  base_df <- merge_add_missing_cols(base_df, all_cols)

  base_keys_df <- base_df %>%
    dplyr::distinct(.merge_key, Strain, Media, Orden) %>%
    dplyr::arrange(Orden, Strain, Media)

  if (!nrow(base_keys_df)) {
    stop("Base platemap does not contain groups to merge.")
  }

  group_keys <- base_keys_df$.merge_key
  group_order <- as.list(stats::setNames(base_keys_df$Orden, group_keys))

  group_list <- list()
  group_params <- list()
  group_sequence <- character(0)
  source_map_records <- list()
  append_source_map <- function(df, group_key, target_rows) {
    if (is.null(df) || !nrow(df) || !length(target_rows)) return(invisible(NULL))
    rec <- data.frame(
      .GroupKey = as.character(group_key),
      .RowWithinGroup = as.integer(target_rows),
      SourceLabel = as.character(df$.SourceLabel %||% NA_character_),
      SourceWell = as.character(df$.SourceWell %||% NA_character_),
      SourceRow = suppressWarnings(as.integer(df$.SourceRow %||% NA_integer_)),
      SourceIndex = suppressWarnings(as.integer(df$.SourceIndex %||% NA_integer_)),
      stringsAsFactors = FALSE
    )
    source_map_records[[length(source_map_records) + 1L]] <<- rec
    invisible(NULL)
  }

  for (kk in group_keys) {
    grp <- base_df[base_df$.merge_key == kk, , drop = FALSE]
    group_list[[kk]] <- grp
    group_params[[kk]] <- base_params
    group_sequence <- c(group_sequence, kk)
    append_source_map(grp, kk, seq_len(nrow(grp)))
  }

  next_order <- suppressWarnings(max(base_df$Orden, na.rm = TRUE))
  if (!is.finite(next_order)) next_order <- 0

  for (i in 2:length(plate_list)) {
    inc_df <- plate_list[[i]]$datos
    names(inc_df) <- trimws(names(inc_df))
    inc_df[] <- lapply(inc_df, merge_clean_char)

    for (cc in setdiff(meta_cols, names(inc_df))) {
      inc_df[[cc]] <- NA
    }
    for (cc in setdiff(track_cols, names(inc_df))) {
      inc_df[[cc]] <- NA
    }
    if (!".merge_key" %in% names(inc_df)) {
      inc_df$.merge_key <- platemap_merge_key(inc_df$Strain, inc_df$Media)
    }

    inc_params <- setdiff(names(inc_df), c(meta_cols, ".merge_key", track_cols))
    inc_params <- inc_params[!is.na(inc_params) & nzchar(inc_params)]

    new_param_cols <- setdiff(inc_params, all_param_cols)
    if (length(new_param_cols) > 0) {
      all_param_cols <- c(all_param_cols, new_param_cols)
    }

    all_cols <- c(meta_cols, all_param_cols, ".merge_key", track_cols)

    group_list <- lapply(group_list, merge_add_missing_cols, target_cols = all_cols)
    inc_df <- merge_add_missing_cols(inc_df, all_cols)

    inc_keys <- unique(inc_df$.merge_key)
    inc_keys <- inc_keys[!is.na(inc_keys) & nzchar(inc_keys)]

    for (kk in inc_keys) {
      inc_group <- inc_df[inc_df$.merge_key == kk, , drop = FALSE]
      if (!nrow(inc_group)) next

      if (!(kk %in% names(group_list))) {
        next_order <- next_order + 1
        inc_group$Orden <- next_order
        inc_group <- merge_prepare_group_replicates(inc_group)
        group_list[[kk]] <- merge_add_missing_cols(inc_group, all_cols)
        group_order[[kk]] <- next_order
        group_params[[kk]] <- inc_params
        group_sequence <- c(group_sequence, kk)
        append_source_map(inc_group, kk, seq_len(nrow(inc_group)))
        next
      }

      old_group <- merge_add_missing_cols(group_list[[kk]], all_cols)
      already_present_params <- group_params[[kk]] %||% character(0)
      repeated_params <- intersect(inc_params, already_present_params)

      if (length(repeated_params) > 0) {
        old_n <- nrow(old_group)
        inc_group$Orden <- group_order[[kk]]
        old_group <- merge_prepare_group_replicates(old_group)
        inc_group <- merge_offset_biorep(old_group, inc_group)
        inc_group <- merge_add_missing_cols(inc_group, all_cols)
        group_list[[kk]] <- dplyr::bind_rows(old_group, inc_group)
        group_params[[kk]] <- union(already_present_params, inc_params)
        append_source_map(inc_group, kk, old_n + seq_len(nrow(inc_group)))
      } else {
        old_n_raw <- nrow(old_group)
        n_target <- max(nrow(old_group), nrow(inc_group))
        old_group <- merge_pad_group_rows(old_group, n_target)
        inc_group <- merge_pad_group_rows(inc_group, n_target)
        old_group <- merge_prepare_group_replicates(old_group)
        inc_group <- merge_prepare_group_replicates(inc_group)

        if (length(inc_params)) {
          old_group[seq_len(nrow(inc_group)), inc_params] <- inc_group[, inc_params, drop = FALSE]
        }
        row_idx <- seq_len(nrow(inc_group))
        old_bio <- old_group$BiologicalReplicate[row_idx]
        old_rep <- old_group$Replicate[row_idx]
        old_tech <- as.character(old_group$TechnicalReplicate[row_idx])
        inc_bio <- inc_group$BiologicalReplicate[row_idx]
        inc_rep <- inc_group$Replicate[row_idx]
        inc_tech <- as.character(inc_group$TechnicalReplicate[row_idx])

        fill_bio <- is.na(old_bio) & !is.na(inc_bio)
        fill_rep <- is.na(old_rep) & !is.na(inc_rep)
        fill_tech <- ((row_idx > old_n_raw) | (is.na(old_tech) | !nzchar(trimws(old_tech)))) &
          (!is.na(inc_tech) & nzchar(trimws(inc_tech)))

        if (any(fill_bio)) {
          tmp_bio <- old_group$BiologicalReplicate[row_idx]
          tmp_bio[fill_bio] <- inc_bio[fill_bio]
          old_group$BiologicalReplicate[row_idx] <- tmp_bio
        }
        if (any(fill_rep)) {
          tmp_rep <- old_group$Replicate[row_idx]
          tmp_rep[fill_rep] <- inc_rep[fill_rep]
          old_group$Replicate[row_idx] <- tmp_rep
        }
        if (any(fill_tech)) {
          tmp_tech <- as.character(old_group$TechnicalReplicate[row_idx])
          tmp_tech[fill_tech] <- inc_tech[fill_tech]
          old_group$TechnicalReplicate[row_idx] <- tmp_tech
        }
        # Preserve source mapping for rows introduced by the incoming platemap
        # so curve concatenation can remap wells consistently.
        if (".SourceLabel" %in% names(old_group) && ".SourceLabel" %in% names(inc_group)) {
          old_src_label <- as.character(old_group$.SourceLabel[row_idx])
          inc_src_label <- as.character(inc_group$.SourceLabel[row_idx])
          fill_src_label <- ((row_idx > old_n_raw) | is.na(old_src_label) | !nzchar(trimws(old_src_label))) &
            (!is.na(inc_src_label) & nzchar(trimws(inc_src_label)))
          if (any(fill_src_label)) {
            tmp_src_label <- as.character(old_group$.SourceLabel[row_idx])
            tmp_src_label[fill_src_label] <- inc_src_label[fill_src_label]
            old_group$.SourceLabel[row_idx] <- tmp_src_label
          }
        }
        if (".SourceWell" %in% names(old_group) && ".SourceWell" %in% names(inc_group)) {
          old_src_well <- as.character(old_group$.SourceWell[row_idx])
          inc_src_well <- as.character(inc_group$.SourceWell[row_idx])
          fill_src_well <- ((row_idx > old_n_raw) | is.na(old_src_well) | !nzchar(trimws(old_src_well))) &
            (!is.na(inc_src_well) & nzchar(trimws(inc_src_well)))
          if (any(fill_src_well)) {
            tmp_src_well <- as.character(old_group$.SourceWell[row_idx])
            tmp_src_well[fill_src_well] <- inc_src_well[fill_src_well]
            old_group$.SourceWell[row_idx] <- tmp_src_well
          }
        }
        if (".SourceRow" %in% names(old_group) && ".SourceRow" %in% names(inc_group)) {
          old_src_row <- suppressWarnings(as.integer(old_group$.SourceRow[row_idx]))
          inc_src_row <- suppressWarnings(as.integer(inc_group$.SourceRow[row_idx]))
          fill_src_row <- ((row_idx > old_n_raw) | is.na(old_src_row)) & !is.na(inc_src_row)
          if (any(fill_src_row)) {
            tmp_src_row <- suppressWarnings(as.integer(old_group$.SourceRow[row_idx]))
            tmp_src_row[fill_src_row] <- inc_src_row[fill_src_row]
            old_group$.SourceRow[row_idx] <- tmp_src_row
          }
        }
        if (".SourceIndex" %in% names(old_group) && ".SourceIndex" %in% names(inc_group)) {
          old_src_idx <- suppressWarnings(as.integer(old_group$.SourceIndex[row_idx]))
          inc_src_idx <- suppressWarnings(as.integer(inc_group$.SourceIndex[row_idx]))
          fill_src_idx <- ((row_idx > old_n_raw) | is.na(old_src_idx)) & !is.na(inc_src_idx)
          if (any(fill_src_idx)) {
            tmp_src_idx <- suppressWarnings(as.integer(old_group$.SourceIndex[row_idx]))
            tmp_src_idx[fill_src_idx] <- inc_src_idx[fill_src_idx]
            old_group$.SourceIndex[row_idx] <- tmp_src_idx
          }
        }
        old_group$Orden <- group_order[[kk]]

        group_list[[kk]] <- old_group
        group_params[[kk]] <- union(already_present_params, inc_params)
        append_source_map(inc_group, kk, seq_len(nrow(inc_group)))
      }
    }
  }

  group_sequence <- unique(group_sequence)
  merged_df <- dplyr::bind_rows(group_list[group_sequence], .id = ".GroupKey")

  if (!nrow(merged_df)) {
    stop("Merged platemap has no rows.")
  }

  merged_df <- merged_df %>%
    dplyr::group_by(.GroupKey) %>%
    dplyr::mutate(.RowWithinGroup = dplyr::row_number()) %>%
    dplyr::ungroup()

  merged_df$.GroupKey <- factor(merged_df$.GroupKey, levels = group_sequence)
  merged_df <- merged_df %>%
    dplyr::arrange(.GroupKey, .RowWithinGroup)

  merged_df <- merge_finalize_group_replicates(merged_df)

  if (!"TechnicalReplicate" %in% names(merged_df)) {
    merged_df$TechnicalReplicate <- "A"
  } else {
    tech_blank <- is.na(merged_df$TechnicalReplicate) | !nzchar(trimws(as.character(merged_df$TechnicalReplicate)))
    merged_df$TechnicalReplicate[tech_blank] <- "A"
  }

  if (!length(all_param_cols)) {
    all_param_cols <- "Parametro_dummy"
    merged_df[[all_param_cols]] <- NA_real_
  }

  merged_df$Well <- merge_make_plate_wells(nrow(merged_df), ncol = plate_ncol)
  well_map <- data.frame(
    SourceIndex = integer(0),
    SourceLabel = character(0),
    SourceWell = character(0),
    SourceRow = integer(0),
    TargetWell = character(0),
    TargetRow = integer(0),
    stringsAsFactors = FALSE
  )
  well_map_from_merged <- merged_df %>%
    dplyr::mutate(
      SourceIndex = suppressWarnings(as.integer(.SourceIndex)),
      SourceLabel = as.character(.SourceLabel),
      SourceWell = as.character(.SourceWell),
      SourceRow = suppressWarnings(as.integer(.SourceRow)),
      TargetWell = as.character(Well),
      TargetRow = dplyr::row_number()
    ) %>%
    dplyr::select(SourceIndex, SourceLabel, SourceWell, SourceRow, TargetWell, TargetRow) %>%
    dplyr::filter(
      !is.na(SourceIndex),
      !is.na(SourceLabel), nzchar(SourceLabel),
      !is.na(SourceWell), nzchar(SourceWell),
      !is.na(TargetWell), nzchar(TargetWell)
    ) %>%
    dplyr::arrange(SourceIndex, SourceRow, TargetRow) %>%
    dplyr::distinct(SourceIndex, SourceLabel, SourceWell, SourceRow, .keep_all = TRUE)
  if (length(source_map_records)) {
    map_raw <- dplyr::bind_rows(source_map_records)
    map_lookup <- merged_df %>%
      dplyr::mutate(
        .GroupKey = as.character(.GroupKey),
        .RowWithinGroup = as.integer(.RowWithinGroup),
        TargetWell = as.character(Well),
        TargetRow = dplyr::row_number()
      ) %>%
      dplyr::select(.GroupKey, .RowWithinGroup, TargetWell, TargetRow)

    well_map_from_records <- map_raw %>%
      dplyr::mutate(
        .GroupKey = as.character(.GroupKey),
        .RowWithinGroup = as.integer(.RowWithinGroup),
        SourceLabel = as.character(SourceLabel),
        SourceWell = as.character(SourceWell),
        SourceRow = suppressWarnings(as.integer(SourceRow)),
        SourceIndex = suppressWarnings(as.integer(SourceIndex))
      ) %>%
      dplyr::left_join(map_lookup, by = c(".GroupKey", ".RowWithinGroup")) %>%
      dplyr::filter(
        !is.na(SourceIndex),
        !is.na(SourceLabel), nzchar(SourceLabel),
        !is.na(SourceWell), nzchar(SourceWell),
        !is.na(TargetWell), nzchar(TargetWell)
      ) %>%
      dplyr::arrange(SourceIndex, SourceRow, TargetRow) %>%
      dplyr::distinct(SourceIndex, SourceLabel, SourceWell, SourceRow, .keep_all = TRUE) %>%
      dplyr::select(SourceIndex, SourceLabel, SourceWell, SourceRow, TargetWell, TargetRow)
    if (nrow(well_map_from_records)) {
      well_map <- well_map_from_records
    } else {
      well_map <- well_map_from_merged
    }
  } else {
    well_map <- well_map_from_merged
  }
  merged_df <- merged_df[, c(meta_cols, all_param_cols), drop = FALSE]

  merged_df$Orden <- suppressWarnings(as.integer(to_numeric(merged_df$Orden)))
  merged_df$Replicate <- suppressWarnings(as.integer(to_numeric(merged_df$Replicate)))
  merged_df$BiologicalReplicate <- suppressWarnings(as.integer(to_numeric(merged_df$BiologicalReplicate)))
  merged_df$TechnicalReplicate <- as.character(merged_df$TechnicalReplicate)
  for (p in all_param_cols) {
    merged_df[[p]] <- to_numeric(merged_df[[p]])
  }

  plot_list <- lapply(plate_list, function(x) x$plot)
  plot_list <- plot_list[sapply(plot_list, function(x) !is.null(x) && nrow(x) > 0)]

  if (length(plot_list) > 0) {
    plot_merged <- dplyr::bind_rows(plot_list)
    if ("Parameter" %in% names(plot_merged)) {
      plot_merged$Parameter <- trimws(as.character(plot_merged$Parameter))
      plot_merged <- plot_merged[!is.na(plot_merged$Parameter) & nzchar(plot_merged$Parameter), , drop = FALSE]
      if (nrow(plot_merged)) {
        plot_merged <- dplyr::distinct(plot_merged, Parameter, .keep_all = TRUE)
      }
    } else {
      plot_merged <- dplyr::distinct(plot_merged)
      plot_merged$Parameter <- character(nrow(plot_merged))
    }
  } else {
    plot_merged <- data.frame(Parameter = character(0), stringsAsFactors = FALSE)
  }

  if (!"Parameter" %in% names(plot_merged)) {
    plot_merged$Parameter <- character(nrow(plot_merged))
  }

  missing_plot_params <- setdiff(all_param_cols, plot_merged$Parameter)
  if (length(missing_plot_params)) {
    add_rows <- as.data.frame(
      matrix(NA, nrow = length(missing_plot_params), ncol = ncol(plot_merged)),
      stringsAsFactors = FALSE
    )
    names(add_rows) <- names(plot_merged)
    add_rows$Parameter <- missing_plot_params
    plot_merged <- dplyr::bind_rows(plot_merged, add_rows)
  }

  if (!"Y_Max" %in% names(plot_merged)) plot_merged$Y_Max <- NA_real_
  if (!"Interval" %in% names(plot_merged)) plot_merged$Interval <- NA_real_
  if (!"Y_Title" %in% names(plot_merged)) plot_merged$Y_Title <- NA_character_

  plot_merged$Y_Max <- to_numeric(plot_merged$Y_Max)
  plot_merged$Interval <- to_numeric(plot_merged$Interval)
  plot_merged$Y_Title <- as.character(plot_merged$Y_Title)

  for (param_name in all_param_cols) {
    idx <- which(plot_merged$Parameter == param_name)
    if (!length(idx)) next
    y_max <- suppressWarnings(as.numeric(plot_merged$Y_Max[idx[1]]))
    if (!is.finite(y_max)) {
      vals <- suppressWarnings(as.numeric(merged_df[[param_name]]))
      vals <- vals[is.finite(vals)]
      if (length(vals)) {
        y_max <- round_up_sig(max(vals), digits = 2)
        plot_merged$Y_Max[idx[1]] <- y_max
      }
    }
    interval <- suppressWarnings(as.numeric(plot_merged$Interval[idx[1]]))
    if (!is.finite(interval) || interval <= 0) {
      plot_merged$Interval[idx[1]] <- if (is.finite(y_max) && y_max != 0) y_max / 5 else NA_real_
    }
    y_title <- plot_merged$Y_Title[idx[1]]
    if (is.na(y_title) || !nzchar(trimws(y_title))) {
      plot_merged$Y_Title[idx[1]] <- param_name
    }
  }

  rank_vec <- match(plot_merged$Parameter, all_param_cols)
  rank_vec[is.na(rank_vec)] <- length(all_param_cols) + seq_len(sum(is.na(rank_vec)))
  plot_merged <- plot_merged[order(rank_vec), , drop = FALSE]

  base_keys <- unique(plate_list[[1]]$datos$.merge_key)
  add_keys <- unique(unlist(lapply(plate_list[-1], function(item) unique(item$datos$.merge_key))))
  overlap_groups <- length(intersect(base_keys, add_keys))

  list(
    Datos = merged_df,
    PlotSettings = plot_merged,
    Labels = plate_list[[1]]$labels,
    WellMap = well_map,
    Info = list(
      rows_total = nrow(merged_df),
      groups_total = length(unique(platemap_merge_key(merged_df$Strain, merged_df$Media))),
      parameters_total = length(all_param_cols),
      overlap_groups = overlap_groups,
      base_file = basename(base_file),
      extra_files = basename(additional_files),
      source_labels = labels
    )
  )
}

find_sheet_alias <- function(sheets, aliases) {
  if (is.null(sheets) || !length(sheets)) return(NULL)
  sh_norm <- normalize_header_key(sheets)
  al_norm <- normalize_header_key(aliases)
  idx <- match(al_norm, sh_norm)
  idx <- idx[!is.na(idx)]
  if (!length(idx)) return(NULL)
  sheets[[idx[1]]]
}

rename_columns_by_alias <- function(df, aliases_map) {
  if (is.null(df) || !is.data.frame(df) || is.null(names(df))) return(df)
  hdr_norm <- normalize_header_key(names(df))
  for (target in names(aliases_map)) {
    if (target %in% names(df)) next
    ali_norm <- normalize_header_key(aliases_map[[target]])
    idx <- match(ali_norm, hdr_norm)
    idx <- idx[!is.na(idx)]
    if (length(idx)) names(df)[idx[1]] <- target
  }
  df
}

first_finite <- function(x, fallback = NA_real_) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (length(x)) x[1] else fallback
}

fill_missing_order <- function(df_groups, order_col = "Orden") {
  out <- df_groups
  ord <- suppressWarnings(as.integer(to_numeric(out[[order_col]])))
  out[[order_col]] <- ord
  miss <- which(is.na(out[[order_col]]) | out[[order_col]] <= 0)
  if (length(miss)) {
    used <- out[[order_col]][is.finite(out[[order_col]]) & out[[order_col]] > 0]
    next_start <- if (length(used)) max(used) + 1L else 1L
    out[[order_col]][miss] <- seq.int(next_start, length.out = length(miss))
  }
  out
}

build_platemap_from_mean_sd_data <- function(
  raw,
  col_labels = list(Strain = NULL, Media = NULL),
  default_profile = c("excel", "csv")
) {
  default_profile <- match.arg(default_profile)
  csv_profile <- identical(default_profile, "csv")

  mapped <- apply_column_aliases(raw, allow_media_alias = TRUE)
  df <- mapped$datos
  if (is.null(col_labels$Strain)) col_labels$Strain <- mapped$labels$Strain
  if (is.null(col_labels$Media)) col_labels$Media <- mapped$labels$Media

  df <- rename_columns_by_alias(
    df,
    list(
      Parameter = c("Parameter", "Parametro", "Parámetro"),
      Mean = c("Mean", "Promedio", "Average"),
      SD = c("SD", "StdDev", "Std_Dev", "Desviacion", "Desviación"),
      N = c("N", "n", "Count", "Replicates"),
      Orden = c("Orden", "Order")
    )
  )

  required_cols <- c("Strain", "Media", "Parameter", "Mean")
  if (!all(required_cols %in% names(df))) return(NULL)

  df <- df %>%
    dplyr::mutate(
      Strain = trimws(as.character(Strain)),
      Media = trimws(as.character(Media)),
      Parameter = trimws(as.character(Parameter)),
      Mean = to_numeric(Mean),
      SD = if ("SD" %in% names(.)) to_numeric(SD) else NA_real_,
      N = if ("N" %in% names(.)) to_numeric(N) else NA_real_,
      Orden = if ("Orden" %in% names(.)) suppressWarnings(as.integer(to_numeric(Orden))) else NA_integer_
    ) %>%
    dplyr::filter(
      !is.na(Strain), nzchar(Strain),
      !is.na(Media), nzchar(Media),
      !is.na(Parameter), nzchar(Parameter)
    )
  if (!nrow(df)) return(NULL)

  param_order <- unique(df$Parameter)
  first_or_na <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    x <- x[is.finite(x)]
    if (length(x)) x[1] else NA_real_
  }

  order_map <- df %>%
    dplyr::distinct(Strain, Media, Orden) %>%
    fill_missing_order("Orden") %>%
    dplyr::arrange(Orden)

  mean_wide <- df %>%
    dplyr::group_by(Strain, Media, Parameter) %>%
    dplyr::summarise(Mean = mean(Mean, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(
      id_cols = c(Strain, Media),
      names_from = Parameter,
      values_from = Mean,
      values_fill = NA_real_,
      values_fn = first_or_na
    )

  sd_wide <- df %>%
    dplyr::group_by(Strain, Media, Parameter) %>%
    dplyr::summarise(SD = mean(SD, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(
      id_cols = c(Strain, Media),
      names_from = Parameter,
      values_from = SD,
      names_prefix = "SD_",
      values_fill = NA_real_,
      values_fn = first_or_na
    )

  n_wide <- df %>%
    dplyr::group_by(Strain, Media, Parameter) %>%
    dplyr::summarise(N = mean(N, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(
      id_cols = c(Strain, Media),
      names_from = Parameter,
      values_from = N,
      names_prefix = "N_",
      values_fill = NA_real_,
      values_fn = first_or_na
    )

  wide_df <- order_map %>%
    dplyr::left_join(mean_wide, by = c("Strain", "Media")) %>%
    dplyr::left_join(sd_wide, by = c("Strain", "Media")) %>%
    dplyr::left_join(n_wide, by = c("Strain", "Media")) %>%
    dplyr::mutate(
      BiologicalReplicate = 1L,
      TechnicalReplicate = 1L
    )

  wells <- gen_wells(nrow(wide_df))
  wide_df <- wide_df %>%
    dplyr::mutate(Well = wells) %>%
    dplyr::relocate(Well, .before = Strain)

  id_cols <- c("Well", "Strain", "Media", "Orden", "BiologicalReplicate", "TechnicalReplicate")
  param_cols <- intersect(param_order, names(wide_df))
  sd_cols <- intersect(paste0("SD_", param_order), names(wide_df))
  n_cols <- intersect(paste0("N_", param_order), names(wide_df))

  if (length(param_cols)) {
    wide_df[param_cols] <- lapply(wide_df[param_cols], to_numeric)
  }
  if (length(sd_cols)) {
    wide_df[sd_cols] <- lapply(wide_df[sd_cols], to_numeric)
  }
  if (length(n_cols)) {
    wide_df[n_cols] <- lapply(wide_df[n_cols], to_numeric)
  }
  wide_df <- wide_df %>%
    dplyr::mutate(
      BiologicalReplicate = as.integer(to_numeric(BiologicalReplicate)),
      TechnicalReplicate = as.integer(to_numeric(TechnicalReplicate)),
      Orden = as.integer(to_numeric(Orden))
    ) %>%
    dplyr::relocate(dplyr::any_of(param_cols), .after = TechnicalReplicate)

  y_max_raw <- vapply(param_cols, function(pm) {
    vals <- suppressWarnings(as.numeric(wide_df[[pm]]))
    sd_col <- paste0("SD_", pm)
    sds <- if (sd_col %in% names(wide_df)) suppressWarnings(as.numeric(wide_df[[sd_col]])) else rep(0, length(vals))
    mx <- suppressWarnings(max(vals + ifelse(is.finite(sds), sds, 0), na.rm = TRUE))
    if (!is.finite(mx)) mx <- suppressWarnings(max(vals, na.rm = TRUE))
    if (!is.finite(mx)) mx <- NA_real_
    mx
  }, numeric(1))

  if (isTRUE(csv_profile)) {
    y_max <- y_max_raw
    interval <- vapply(y_max, function(mx) {
      if (!is.finite(mx) || is.na(mx) || mx <= 0) return(NA_real_)
      max(1, round(mx / 5))
    }, numeric(1))
    y_titles <- rep("", length(param_cols))
  } else {
    y_max <- vapply(y_max_raw, round_up_sig, numeric(1), digits = 2)
    interval <- ifelse(is.na(y_max) | y_max == 0, NA_real_, y_max / 5)
    y_titles <- param_cols
  }

  plot_settings <- tibble::tibble(
    Parameter = param_cols,
    Y_Max = y_max,
    Interval = interval,
    Y_Title = y_titles
  )

  list(
    Datos = wide_df,
    PlotSettings = plot_settings,
    Labels = col_labels,
    SummaryMode = TRUE
  )
}

build_platemap_from_mean_sd <- function(file) {
  sheets <- excel_sheets_tmp(file)
  sh <- find_sheet_alias(
    sheets,
    c("Parameters_Summary", "Parametros_Summary", "Summary_Parameters", "Resumen_Parametros")
  )
  if (is.null(sh)) return(NULL)

  raw <- read_excel_tmp(file, sheet = sh)
  build_platemap_from_mean_sd_data(
    raw = raw,
    default_profile = "excel"
  )
}

build_curve_from_mean_sd_data <- function(raw, defaults_profile = c("excel", "csv")) {
  defaults_profile <- match.arg(defaults_profile)
  csv_profile <- identical(defaults_profile, "csv")
  mapped <- apply_column_aliases(raw, allow_media_alias = TRUE)
  df <- mapped$datos
  df <- rename_columns_by_alias(
    df,
    list(
      Time = c("Time", "Tiempo", "Minutes", "Minutos"),
      Mean = c("Mean", "Promedio", "Average"),
      SD = c("SD", "StdDev", "Std_Dev", "Desviacion", "Desviación"),
      N = c("N", "n", "Count", "Replicates"),
      Orden = c("Orden", "Order")
    )
  )

  required_cols <- c("Time", "Strain", "Media", "Mean")
  if (!all(required_cols %in% names(df))) return(NULL)

  df <- df %>%
    dplyr::mutate(
      Time = to_numeric(Time),
      Strain = trimws(as.character(Strain)),
      Media = trimws(as.character(Media)),
      Mean = to_numeric(Mean),
      SD = if ("SD" %in% names(.)) to_numeric(SD) else NA_real_,
      N = if ("N" %in% names(.)) to_numeric(N) else NA_real_,
      Orden = if ("Orden" %in% names(.)) suppressWarnings(as.integer(to_numeric(Orden))) else NA_integer_
    ) %>%
    dplyr::filter(
      is.finite(Time),
      !is.na(Strain), nzchar(Strain),
      !is.na(Media), nzchar(Media)
    )
  if (!nrow(df)) return(NULL)

  group_map <- df %>%
    dplyr::distinct(Strain, Media, Orden) %>%
    fill_missing_order("Orden") %>%
    dplyr::arrange(Orden) %>%
    dplyr::mutate(Well = paste0("SUM_", dplyr::row_number()))

  long <- df %>%
    dplyr::select(-dplyr::any_of("Orden")) %>%
    dplyr::left_join(
      group_map %>% dplyr::select(Strain, Media, Orden, Well),
      by = c("Strain", "Media")
    ) %>%
    dplyr::transmute(
      Time = Time,
      Well = Well,
      Strain = Strain,
      Media = Media,
      Orden = Orden,
      Mean = Mean,
      SD = SD,
      N = N
    )

  sheet1 <- long %>%
    dplyr::group_by(Time, Well) %>%
    dplyr::summarise(Value = mean(Mean, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(
      id_cols = Time,
      names_from = Well,
      values_from = Value,
      values_fill = NA_real_
    ) %>%
    dplyr::arrange(Time)

  x_max <- suppressWarnings(max(long$Time, na.rm = TRUE))
  y_max <- suppressWarnings(max(long$Mean + ifelse(is.finite(long$SD), long$SD, 0), na.rm = TRUE))
  if (!is.finite(x_max) || x_max <= 0) x_max <- 1
  if (!is.finite(y_max) || y_max <= 0) y_max <- suppressWarnings(max(long$Mean, na.rm = TRUE))
  if (!is.finite(y_max) || y_max <= 0) y_max <- 1
  x_break <- x_max / if (isTRUE(csv_profile)) 4 else 3
  y_break <- y_max / if (isTRUE(csv_profile)) 4 else 3
  if (!is.finite(x_break) || x_break <= 0) x_break <- 1
  if (!is.finite(y_break) || y_break <= 0) y_break <- 1
  x_title_default <- if (isTRUE(csv_profile)) "" else "Time"
  y_title_default <- if (isTRUE(csv_profile)) "" else "Signal"

  sheet2 <- group_map %>%
    dplyr::transmute(
      X_Max = x_max,
      Interval_X = x_break,
      Y_Max = y_max,
      Interval_Y = y_break,
      X_Title = x_title_default,
      Y_Title = y_title_default,
      Well = Well,
      BiologicalReplicate = 1L
    )

  meta <- group_map %>%
    dplyr::transmute(
      Well = Well,
      Strain = Strain,
      Media = Media,
      Orden = as.integer(Orden),
      BiologicalReplicate = 1L,
      TechnicalReplicate = 1L
    )

  curve_summary <- long %>%
    dplyr::transmute(
      Time = Time,
      Well = Well,
      Strain = Strain,
      Media = Media,
      Orden = as.integer(Orden),
      Mean = Mean,
      SD = SD,
      N = N
    )

  list(
    Sheet1 = sheet1,
    Sheet2 = sheet2,
    Meta = meta,
    Summary = curve_summary,
    SummaryMode = TRUE
  )
}

build_curve_from_mean_sd <- function(file) {
  sheets <- excel_sheets_tmp(file)
  sh <- find_sheet_alias(
    sheets,
    c("Curves_Summary", "Curvas_Summary", "Summary_Curves", "Resumen_Curvas")
  )
  if (is.null(sh)) return(NULL)

  raw <- read_excel_tmp(file, sheet = sh)
  build_curve_from_mean_sd_data(raw, defaults_profile = "excel")
}

load_curve_workbook <- function(file, file_name = NULL) {
  file_chr <- as.character(file %||% "")
  if (!length(file_chr) || is.na(file_chr[[1]]) || !nzchar(file_chr[[1]])) file_chr <- ""
  name_chr <- as.character(file_name %||% "")
  if (!length(name_chr) || is.na(name_chr[[1]]) || !nzchar(name_chr[[1]])) {
    name_chr <- basename(file_chr)
  }
  name_chr <- if (length(name_chr)) name_chr[[1]] else ""
  ext_file <- tolower(tools::file_ext(file_chr))
  is_csv_input <- is_csv_filename(name_chr) || identical(ext_file, "csv")

  curve_fail <- function(reason = "invalid", message = NULL) {
    list(
      ok = FALSE,
      reason = reason,
      message = message,
      Sheet1 = NULL,
      Sheet2 = NULL,
      Meta = NULL,
      Summary = NULL,
      SummaryMode = FALSE
    )
  }

  d <- NULL
  s <- NULL
  meta <- NULL
  summary_tbl <- NULL
  from_summary <- FALSE
  parse_message <- NULL

  if (isTRUE(is_csv_input)) {
    d <- tryCatch(
      read_csv_tmp(file_chr),
      error = function(e) {
        parse_message <<- conditionMessage(e)
        NULL
      }
    )
    if (is.null(d) || !is.data.frame(d) || !nrow(d)) {
      if (!is.character(parse_message) || !nzchar(parse_message)) {
        parse_message <- "Curve CSV format not recognized."
      }
      return(curve_fail(reason = "invalid", message = parse_message))
    }

    parsed <- tryCatch(
      build_curve_from_mean_sd_data(d, defaults_profile = "csv"),
      error = function(e) {
        parse_message <<- conditionMessage(e)
        NULL
      }
    )
    if (!is.null(parsed)) {
      d <- parsed$Sheet1
      s <- parsed$Sheet2
      meta <- parsed$Meta
      summary_tbl <- parsed$Summary
      from_summary <- TRUE
    } else {
      d <- rename_columns_by_alias(
        d,
        list(Time = c("Time", "Tiempo", "Minutes", "Minutos"))
      )
      if (!"Time" %in% names(d)) {
        if (!is.character(parse_message) || !nzchar(parse_message)) {
          parse_message <- "Curve CSV format not recognized."
        }
        return(curve_fail(reason = "invalid", message = parse_message))
      }
    }
  } else {
    sheets <- tryCatch(excel_sheets_tmp(file_chr), error = function(e) character(0))
    summary_aliases <- c(
      "Curves_Summary", "Curvas_Summary", "Summary_Curves", "Resumen_Curvas"
    )
    has_sheet1 <- "Sheet1" %in% sheets
    has_summary_sheet <- !is.null(find_sheet_alias(sheets, summary_aliases))

    d <- tryCatch(read_excel_tmp(file_chr, sheet = "Sheet1"), error = function(e) NULL)
    if (is.null(d) || !"Time" %in% names(d)) {
      parsed <- tryCatch(
        build_curve_from_mean_sd(file_chr),
        error = function(e) {
          parse_message <<- conditionMessage(e)
          NULL
        }
      )
      if (is.null(parsed)) {
        reason <- if (isTRUE(has_sheet1) || isTRUE(has_summary_sheet)) "invalid" else "not_found"
        if (identical(reason, "invalid") && (!is.character(parse_message) || !nzchar(parse_message))) {
          parse_message <- "Curve workbook format not recognized."
        }
        return(curve_fail(reason = reason, message = parse_message))
      }
      d <- parsed$Sheet1
      s <- parsed$Sheet2
      meta <- parsed$Meta
      summary_tbl <- parsed$Summary
      from_summary <- TRUE
    } else {
      s <- tryCatch(read_excel_tmp(file_chr, sheet = "Sheet2"), error = function(e) NULL)
    }
  }

  d <- as.data.frame(d %||% data.frame(), stringsAsFactors = FALSE, check.names = FALSE)
  d <- rename_columns_by_alias(
    d,
    list(Time = c("Time", "Tiempo", "Minutes", "Minutos"))
  )
  if (!"Time" %in% names(d)) {
    msg <- if (isTRUE(is_csv_input)) "Curve CSV format not recognized." else "Curve workbook format not recognized."
    return(curve_fail(reason = "invalid", message = msg))
  }

  d$Time <- suppressWarnings(as.numeric(to_numeric(d$Time)))
  d <- d[is.finite(d$Time), , drop = FALSE]
  curve_cols <- setdiff(names(d), "Time")
  if (!length(curve_cols)) {
    msg <- if (isTRUE(is_csv_input)) "Curve CSV format not recognized." else "Curve workbook format not recognized."
    return(curve_fail(reason = "invalid", message = msg))
  }
  for (nm in curve_cols) {
    d[[nm]] <- suppressWarnings(as.numeric(to_numeric(d[[nm]])))
  }
  keep_curve_cols <- curve_cols[vapply(curve_cols, function(nm) {
    any(is.finite(d[[nm]]))
  }, logical(1))]
  if (!length(keep_curve_cols)) {
    msg <- if (isTRUE(is_csv_input)) "Curve CSV format not recognized." else "Curve workbook format not recognized."
    return(curve_fail(reason = "invalid", message = msg))
  }
  d <- d[, c("Time", keep_curve_cols), drop = FALSE]

  d_time_max <- suppressWarnings(max(as.numeric(d$Time), na.rm = TRUE))
  if (!is.finite(d_time_max)) d_time_max <- 1

  d_vals <- d[, setdiff(names(d), "Time"), drop = FALSE]
  if (ncol(d_vals) == 0) {
    d_val_max <- NA_real_
  } else {
    d_val_max <- suppressWarnings(
      max(unlist(lapply(d_vals, as.numeric), use.names = FALSE), na.rm = TRUE)
    )
  }
  if (!is.finite(d_val_max)) d_val_max <- 1

  default_divisor <- if (isTRUE(is_csv_input)) 4 else 3
  default_x_break <- if (is.finite(d_time_max) && d_time_max > 0) d_time_max / default_divisor else 1
  default_y_break <- if (is.finite(d_val_max) && d_val_max > 0) d_val_max / default_divisor else 1

  required_cols <- c("X_Max", "Interval_X", "Y_Max", "Interval_Y", "X_Title", "Y_Title")
  if (is.null(s) || !all(required_cols %in% names(s))) {
    s <- tibble::tibble(
      X_Max = d_time_max,
      Interval_X = default_x_break,
      Y_Max = d_val_max,
      Interval_Y = default_y_break,
      X_Title = if (isTRUE(is_csv_input)) "" else "Tiempo (min)",
      Y_Title = if (isTRUE(is_csv_input)) "" else "OD (620 nm)",
      Well = NA,
      BiologicalReplicate = NA
    )
  } else {
    if (!"Well" %in% names(s)) s$Well <- NA
    if (!"BiologicalReplicate" %in% names(s)) s$BiologicalReplicate <- NA
  }

  s <- s %>%
    dplyr::mutate(
      X_Max = suppressWarnings(as.numeric(X_Max)),
      Interval_X = suppressWarnings(as.numeric(Interval_X)),
      Y_Max = suppressWarnings(as.numeric(Y_Max)),
      Interval_Y = suppressWarnings(as.numeric(Interval_Y))
    )

  s$X_Max[!is.finite(s$X_Max)] <- d_time_max
  s$Y_Max[!is.finite(s$Y_Max)] <- d_val_max
  s$Interval_X[!is.finite(s$Interval_X) | s$Interval_X <= 0] <- default_x_break
  s$Interval_Y[!is.finite(s$Interval_Y) | s$Interval_Y <= 0] <- default_y_break

  list(
    ok = TRUE,
    reason = "ok",
    message = NULL,
    Sheet1 = d,
    Sheet2 = s,
    Meta = meta,
    Summary = summary_tbl,
    SummaryMode = from_summary
  )
}

merge_curve_workbooks_by_well_map <- function(
  base_file,
  additional_files = character(0),
  well_map,
  base_name = NULL,
  additional_names = character(0)
) {
  additional_files <- as.character(additional_files %||% character(0))
  additional_files <- additional_files[!is.na(additional_files) & nzchar(additional_files)]
  curve_files <- c(base_file, additional_files)
  if (!length(curve_files)) {
    stop("No curve files were provided for merge.")
  }
  curve_labels <- c(base_name, additional_names)
  curve_labels <- as.character(curve_labels %||% character(0))
  if (length(curve_labels) < length(curve_files)) {
    curve_labels <- c(curve_labels, rep("", length(curve_files) - length(curve_labels)))
  } else if (length(curve_labels) > length(curve_files)) {
    curve_labels <- curve_labels[seq_len(length(curve_files))]
  }
  fallback_names <- basename(curve_files)
  missing_label <- is.na(curve_labels) | !nzchar(trimws(curve_labels))
  curve_labels[missing_label] <- fallback_names[missing_label]

  map_tbl <- as.data.frame(well_map %||% data.frame(), stringsAsFactors = FALSE)
  required_map_cols <- c("SourceIndex", "SourceLabel", "SourceWell", "SourceRow", "TargetWell", "TargetRow")
  if (!all(required_map_cols %in% names(map_tbl))) {
    stop("Platemap merge map is not available for curve remapping.")
  }
  map_tbl <- map_tbl %>%
    dplyr::mutate(
      SourceIndex = suppressWarnings(as.integer(SourceIndex)),
      SourceLabel = as.character(SourceLabel),
      SourceWell = as.character(SourceWell),
      SourceRow = suppressWarnings(as.integer(SourceRow)),
      TargetWell = as.character(TargetWell),
      TargetRow = suppressWarnings(as.integer(TargetRow))
    ) %>%
    dplyr::filter(
      !is.na(SourceIndex),
      !is.na(SourceWell), nzchar(SourceWell),
      !is.na(TargetWell), nzchar(TargetWell)
    ) %>%
    dplyr::arrange(SourceIndex, SourceRow, TargetRow)

  if (!nrow(map_tbl)) {
    stop("No well remapping rows were found for curve merge.")
  }

  curve_list <- lapply(seq_along(curve_files), function(i) {
    parsed <- load_curve_workbook(curve_files[[i]], file_name = curve_labels[[i]])
    if (!isTRUE(parsed$ok) || is.null(parsed$Sheet1) || !"Time" %in% names(parsed$Sheet1)) {
      msg <- parsed$message
      if (!is.character(msg) || !nzchar(msg)) {
        msg <- sprintf("Curve file '%s' format not recognized.", curve_labels[[i]])
      }
      stop(msg)
    }
    parsed
  })

  all_times <- sort(unique(unlist(lapply(curve_list, function(item) {
    suppressWarnings(as.numeric(item$Sheet1$Time))
  }), use.names = FALSE)))
  all_times <- all_times[is.finite(all_times)]
  if (!length(all_times)) {
    stop("Curve files do not contain valid numeric Time values.")
  }

  merged_sheet1 <- data.frame(Time = all_times, stringsAsFactors = FALSE, check.names = FALSE)

  map_tbl$CurveFile <- ifelse(
    map_tbl$SourceIndex >= 1 & map_tbl$SourceIndex <= length(curve_files),
    curve_labels[pmax(1L, pmin(length(curve_files), map_tbl$SourceIndex))],
    NA_character_
  )
  map_tbl$ColumnFound <- FALSE

  for (k in seq_len(nrow(map_tbl))) {
    src_idx <- map_tbl$SourceIndex[[k]]
    if (!is.finite(src_idx) || src_idx < 1 || src_idx > length(curve_list)) next

    src_sheet <- curve_list[[src_idx]]$Sheet1
    src_well <- map_tbl$SourceWell[[k]]
    tgt_well <- map_tbl$TargetWell[[k]]
    if (!nzchar(src_well) || !nzchar(tgt_well)) next
    if (!(src_well %in% names(src_sheet))) next

    map_tbl$ColumnFound[[k]] <- TRUE
    src_time <- suppressWarnings(as.numeric(src_sheet$Time))
    src_vals <- suppressWarnings(as.numeric(src_sheet[[src_well]]))
    aligned <- rep(NA_real_, length(all_times))
    idx <- match(all_times, src_time)
    hit <- is.finite(idx)
    aligned[hit] <- src_vals[idx[hit]]

    if (!tgt_well %in% names(merged_sheet1)) {
      merged_sheet1[[tgt_well]] <- NA_real_
    }
    fill_idx <- is.na(merged_sheet1[[tgt_well]])
    merged_sheet1[[tgt_well]][fill_idx] <- aligned[fill_idx]
  }

  target_well_order <- map_tbl %>%
    dplyr::filter(!is.na(TargetWell) & nzchar(TargetWell)) %>%
    dplyr::group_by(TargetWell) %>%
    dplyr::summarise(TargetRow = suppressWarnings(min(TargetRow, na.rm = TRUE)), .groups = "drop") %>%
    dplyr::arrange(TargetRow)

  target_wells <- as.character(target_well_order$TargetWell)
  target_wells <- target_wells[!is.na(target_wells) & nzchar(target_wells)]
  for (ww in target_wells) {
    if (!ww %in% names(merged_sheet1)) merged_sheet1[[ww]] <- NA_real_
  }
  merged_sheet1 <- merged_sheet1[, c("Time", target_wells), drop = FALSE]

  cfg_base <- curve_list[[1]]$Sheet2
  if (is.null(cfg_base) || !is.data.frame(cfg_base) || !nrow(cfg_base)) {
    cfg_base <- data.frame(
      X_Max = NA_real_,
      Interval_X = NA_real_,
      Y_Max = NA_real_,
      Interval_Y = NA_real_,
      X_Title = "Time",
      Y_Title = "Signal",
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }

  first_finite_num <- function(v, fallback) {
    num <- suppressWarnings(as.numeric(v))
    num <- num[is.finite(num)]
    if (length(num)) num[[1]] else fallback
  }
  first_text <- function(v, fallback) {
    chr <- as.character(v)
    chr <- chr[!is.na(chr) & nzchar(trimws(chr))]
    if (length(chr)) chr[[1]] else fallback
  }

  x_max <- first_finite_num(cfg_base$X_Max, fallback = suppressWarnings(max(all_times, na.rm = TRUE)))
  if (!is.finite(x_max) || x_max <= 0) x_max <- 1
  x_break <- first_finite_num(cfg_base$Interval_X, fallback = x_max / 3)
  if (!is.finite(x_break) || x_break <= 0) x_break <- x_max / 3
  if (!is.finite(x_break) || x_break <= 0) x_break <- 1

  curve_vals <- unlist(merged_sheet1[, setdiff(names(merged_sheet1), "Time"), drop = FALSE], use.names = FALSE)
  curve_vals <- suppressWarnings(as.numeric(curve_vals))
  curve_vals <- curve_vals[is.finite(curve_vals)]
  y_max_data <- if (length(curve_vals)) max(curve_vals, na.rm = TRUE) else NA_real_
  y_max <- first_finite_num(cfg_base$Y_Max, fallback = y_max_data)
  if (!is.finite(y_max) || y_max <= 0) y_max <- if (is.finite(y_max_data) && y_max_data > 0) y_max_data else 1
  y_break <- first_finite_num(cfg_base$Interval_Y, fallback = y_max / 3)
  if (!is.finite(y_break) || y_break <= 0) y_break <- y_max / 3
  if (!is.finite(y_break) || y_break <= 0) y_break <- 1

  x_title <- first_text(cfg_base$X_Title, fallback = "Time")
  y_title <- first_text(cfg_base$Y_Title, fallback = "Signal")

  if (!length(target_wells)) target_wells <- NA_character_
  sheet2 <- data.frame(
    X_Max = x_max,
    Interval_X = x_break,
    Y_Max = y_max,
    Interval_Y = y_break,
    X_Title = x_title,
    Y_Title = y_title,
    Well = target_wells,
    BiologicalReplicate = seq_along(target_wells),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  source_map <- map_tbl %>%
    dplyr::select(
      SourceIndex, SourceLabel, CurveFile, SourceWell, SourceRow,
      TargetWell, TargetRow, ColumnFound
    )

  list(
    Sheet1 = merged_sheet1,
    Sheet2 = sheet2,
    SourceMap = source_map
  )
}

# ────────────────────────────────────────────────────────────────
# Funciones Growth Rates – robustas y permisivas
# (colócalas en el ámbito global, antes de server)
# ────────────────────────────────────────────────────────────────

# 1. Fase exponencial - ROBUSTA
identify_exponential_phase_robust <- function(df, time_col, measure_col,
                                              umax_lower_bound = 0.05,
                                              umax_upper_bound = 0.25,
                                              max_iterations = 10,
                                              initial_r_squared_threshold = 0.95) {
  
  best_model <- NULL
  best_r2    <- -Inf
  best_start <- best_end <- NULL
  
  # limpia NAs y filtrado del 5-95 %
  df <- df[!is.na(df[[time_col]]) & !is.na(df[[measure_col]]), ]
  df <- dplyr::filter(df,
                      dplyr::between(df[[measure_col]],
                                     stats::quantile(df[[measure_col]], 0.05),
                                     stats::quantile(df[[measure_col]], 0.95)))
  
  min_pts            <- 10
  r2_threshold       <- initial_r_squared_threshold
  
  for (i in seq_len(max_iterations)) {
    for (start in seq_len(nrow(df) - min_pts)) {
      for (end in seq(start + min_pts, nrow(df))) {
        
        if ((end - start + 1) < min_pts) next
        
        model <- tryCatch(
          lm(log(df[[measure_col]][start:end]) ~
               df[[time_col]][start:end]),
          error = function(e) NULL)
        
        if (is.null(model)) next
        
        r2   <- summary(model)$r.squared
        umax <- coef(model)[2]
        
        if (!is.na(r2) &&
            umax > umax_lower_bound &&
            umax < umax_upper_bound &&
            r2   > r2_threshold &&
            r2   > best_r2) {
          
          best_r2    <- r2
          best_model <- model
          best_start <- start
          best_end   <- end
        }
      }
    }
    
    # heurística de ajuste de límites
    if (!is.null(best_model)) {
      umax <- coef(best_model)[2]
      if (umax < umax_lower_bound) {
        min_pts          <- max(min_pts - 1, 5)
        umax_lower_bound <- umax_lower_bound - 0.01
        r2_threshold     <- max(r2_threshold - 0.01, 0.90)
      } else if (umax > umax_upper_bound) {
        min_pts          <- min(min_pts + 1, nrow(df) - 5)
        umax_upper_bound <- umax_upper_bound + 0.01
        r2_threshold     <- min(r2_threshold + 0.01, 0.99)
      } else {
        break
      }
    }
  }
  
  list(start = best_start, end = best_end, model = best_model)
}

# 2. Fase exponencial - PERMISIVA
identify_exponential_phase_permissive <- function(df, time_col, measure_col,
                                                  umax_lower_bound = 0.01,
                                                  umax_upper_bound = 0.50,
                                                  max_iterations   = 10) {
  
  best_model <- NULL
  best_r2    <- -Inf
  best_start <- best_end <- NULL
  
  df <- df[!is.na(df[[time_col]]) & !is.na(df[[measure_col]]), ]
  df <- dplyr::filter(df,
                      dplyr::between(df[[measure_col]],
                                     stats::quantile(df[[measure_col]], 0.05),
                                     stats::quantile(df[[measure_col]], 0.95)))
  
  min_pts <- 10
  
  for (i in seq_len(max_iterations)) {
    for (start in seq_len(nrow(df) - min_pts)) {
      for (end in seq(start + min_pts, nrow(df))) {
        
        if ((end - start + 1) < min_pts) next
        
        model <- tryCatch(
          lm(log(df[[measure_col]][start:end]) ~
               df[[time_col]][start:end]),
          error = function(e) NULL)
        
        if (is.null(model)) next
        
        r2 <- summary(model)$r.squared
        if (!is.na(r2) && r2 > best_r2) {
          best_r2    <- r2
          best_model <- model
          best_start <- start
          best_end   <- end
        }
      }
    }
    break        # en permisiva basta la 1.ª iteración
  }
  
  list(start = best_start, end = best_end, model = best_model)
}

# 3. Calcular parámetros – ROBUSTO
calculate_growth_rates_robust <- function(df) {
  
  df %>%
    dplyr::group_by(Well) %>%
    dplyr::do({
      
      phase <- identify_exponential_phase_robust(.,
                                                 time_col = "Time",
                                                 measure_col = "Measurements")
      
      model <- phase$model
      start <- phase$start
      end   <- phase$end
      
      if (!is.null(model)) {
        
        lag_time <- .$Time[which.max(.$Time[1:start])]
        
        dplyr::tibble(
          µMax            = coef(model)[2],
          max_percap_time = mean(.$Time[start:end]),
          doub_time       = log(2) / coef(model)[2],
          lag_time        = lag_time,
          ODmax           = max(.$Measurements),
          max_time        = .$Time[which.max(.$Measurements)],
          AUC             = gcplyr::auc(x = .$Time, y = .$Measurements)
        )
      } else {
        dplyr::tibble(
          µMax            = NA_real_,
          max_percap_time = NA_real_,
          doub_time       = NA_real_,
          lag_time        = NA_real_,
          ODmax           = max(.$Measurements),
          max_time        = NA_real_,
          AUC             = NA_real_
        )
      }
    })
}

# 4. Calcular parámetros – PERMISIVO
calculate_growth_rates_permissive <- function(df) {
  
  df %>%
    dplyr::group_by(Well) %>%
    dplyr::do({
      
      phase <- identify_exponential_phase_permissive(.,
                                                     time_col = "Time",
                                                     measure_col = "Measurements")
      
      model <- phase$model
      start <- phase$start
      end   <- phase$end
      
      if (!is.null(model)) {
        
        lag_time <- .$Time[which.max(.$Time[1:start])]
        
        dplyr::tibble(
          Well           = .$Well[1],
          µMax            = coef(model)[2],
          max_percap_time = mean(.$Time[start:end]),
          doub_time       = log(2) / coef(model)[2],
          lag_time        = lag_time,
          ODmax           = max(.$Measurements),
          max_time        = .$Time[which.max(.$Measurements)],
          AUC             = gcplyr::auc(x = .$Time, y = .$Measurements)
        )
      } else {
        dplyr::tibble(
          Well           = .$Well[1],
          µMax            = NA_real_,
          max_percap_time = NA_real_,
          doub_time       = NA_real_,
          lag_time        = NA_real_,
          ODmax           = max(.$Measurements),
          max_time        = NA_real_,
          AUC             = NA_real_
        )
      }
    })
}

# 5. Detectar valores vacíos
is_empty_value <- function(x) {
  is.na(x) | x == "" | is.nan(x)
}
# ────────────────────────────────────────────────────────────────

#───────────────────────────────────────────────────────────────────────────────
# Helper: asegura que existan PlotSettings y columnas de parámetros
#───────────────────────────────────────────────────────────────────────────────
prepare_platemap <- function(df_datos, cfg, defaults_profile = c("excel", "csv")) {
  defaults_profile <- match.arg(defaults_profile)
  csv_profile <- identical(defaults_profile, "csv")
  normalize_text <- function(x) {
    if (is.null(x)) return(x)
    out <- as.character(x)
    out <- gsub("\u00A0", " ", out, fixed = TRUE)
    out <- trimws(out)
    out[out == ""] <- NA_character_
    out
  }
  is_numeric_like <- function(x) {
    x <- normalize_text(x)
    x <- x[!is.na(x)]
    if (!length(x)) return(FALSE)
    all(grepl("^[-+]?[0-9]+([\\.,][0-9]+)?$", x))
  }
  safe_num <- function(x, keep_text_when_all_na = TRUE, strict_numeric = FALSE) {
    if (is.numeric(x)) return(as.numeric(x))
    txt <- normalize_text(x)
    if (isTRUE(strict_numeric) && !is_numeric_like(txt)) return(txt)
    num <- to_numeric(txt)
    if (all(is.na(num))) {
      if (isTRUE(keep_text_when_all_na)) return(txt)
      return(num)
    }
    num
  }
  build_default_cfg <- function(df, params) {
    params <- unique(as.character(params %||% character(0)))
    params <- params[!is.na(params) & nzchar(params)]
    if (!length(params)) {
      return(tibble::tibble(
        Parameter = character(0),
        Y_Max = numeric(0),
        Interval = numeric(0),
        Y_Title = character(0)
      ))
    }
    col_max <- function(v) if (all(is.na(v))) NA_real_ else max(v, na.rm = TRUE)
    raw_max <- vapply(params, function(nm) col_max(df[[nm]]), numeric(1))
    if (isTRUE(csv_profile)) {
      y_max <- raw_max
      interval <- vapply(y_max, function(mx) {
        if (!is.finite(mx) || is.na(mx) || mx <= 0) return(NA_real_)
        max(1, round(mx / 5))
      }, numeric(1))
      y_title <- rep("", length(params))
    } else {
      y_max <- vapply(params, function(nm) round_up_sig(raw_max[[nm]], digits = 2), numeric(1))
      interval <- ifelse(is.na(y_max) | y_max == 0, NA_real_, y_max / 5)
      y_title <- rep("", length(params))
    }
    tibble::tibble(
      Parameter = params,
      Y_Max = y_max,
      Interval = interval,
      Y_Title = y_title
    )
  }
  id_cols <- c("Strain", "Media", "Orden", "BiologicalReplicate", "TechnicalReplicate", "Well", "Replicate")

  if (!is.null(df_datos) && !is.null(names(df_datos))) {
    names(df_datos) <- normalize_text(names(df_datos))
    for (col in c("Strain", "Media", "Well", "TechnicalReplicate", "Replicate")) {
      if (col %in% names(df_datos)) {
        df_datos[[col]] <- normalize_text(df_datos[[col]])
      }
    }
    if ("BiologicalReplicate" %in% names(df_datos)) {
      df_datos[["BiologicalReplicate"]] <- safe_num(
        df_datos[["BiologicalReplicate"]],
        keep_text_when_all_na = TRUE,
        strict_numeric = TRUE
      )
    }
    if ("Orden" %in% names(df_datos)) {
      df_datos[["Orden"]] <- safe_num(
        df_datos[["Orden"]],
        keep_text_when_all_na = FALSE,
        strict_numeric = FALSE
      )
    }
  }
  if (!is.null(cfg) && !is.null(names(cfg))) {
    names(cfg) <- normalize_text(names(cfg))
    if ("Parameter" %in% names(cfg)) {
      cfg$Parameter <- normalize_text(cfg$Parameter)
    }
    if ("Y_Max" %in% names(cfg)) {
      cfg$Y_Max <- to_numeric(cfg$Y_Max)
    }
    if ("Interval" %in% names(cfg)) {
      cfg$Interval <- to_numeric(cfg$Interval)
    }
    if ("Y_Title" %in% names(cfg)) {
      cfg$Y_Title <- normalize_text(cfg$Y_Title)
    }
  }
  data_param_cols <- setdiff(names(df_datos), id_cols)
  data_param_cols <- data_param_cols[!is.na(data_param_cols) & nzchar(data_param_cols)]
  if (length(data_param_cols)) {
    df_datos[data_param_cols] <- lapply(df_datos[data_param_cols], to_numeric)
  }

  # ─ 1. PlotSettings inexistente o vacío ────────────────────────────────────
  if (is.null(cfg) || nrow(cfg) == 0 || !"Parameter" %in% names(cfg)) {
    # Identificamos columnas de parámetros desde Datos.
    if (length(data_param_cols) == 0) {
      data_param_cols <- "Parametro_dummy"
      df_datos[[data_param_cols]] <- NA_real_
    }
    cfg <- build_default_cfg(df_datos, data_param_cols)
  }
  
  # ─ 2. Limpiar parametros vacios y quedarnos solo con los que tienen datos ─
  if ("Parameter" %in% names(cfg)) {
    cfg$Parameter <- trimws(as.character(cfg$Parameter))
    cfg <- cfg[!is.na(cfg$Parameter) & nzchar(cfg$Parameter), , drop = FALSE]
  }
  
  if (nrow(cfg) > 0) {
    if (!"Y_Max" %in% names(cfg)) cfg$Y_Max <- NA_real_
    if (!"Interval" %in% names(cfg)) cfg$Interval <- NA_real_
    if (!"Y_Title" %in% names(cfg)) {
      cfg$Y_Title <- rep("", nrow(cfg))
    }

    # Keep compatibility by materializing declared parameters even when missing
    # in Datos, but do not force them into UI if they have no real data column.
    missing_declared <- setdiff(cfg$Parameter, names(df_datos))
    if (length(missing_declared)) {
      df_datos[missing_declared] <- NA_real_
    }

    cfg_present <- cfg[cfg$Parameter %in% data_param_cols, , drop = FALSE]
    extra_data_params <- setdiff(data_param_cols, cfg_present$Parameter)
    if (length(extra_data_params)) {
      cfg_extra <- build_default_cfg(df_datos, extra_data_params)
      cfg_present <- dplyr::bind_rows(cfg_present, cfg_extra)
    }

    cfg <- cfg_present
    if (!nrow(cfg) && length(data_param_cols)) {
      cfg <- build_default_cfg(df_datos, data_param_cols)
    }
  } else if (length(data_param_cols)) {
    cfg <- build_default_cfg(df_datos, data_param_cols)
  } else {
    cfg <- tibble::tibble(
      Parameter = "Parametro_dummy",
      Y_Max = NA_real_,
      Interval = NA_real_,
      Y_Title = ""
    )
    if (!"Parametro_dummy" %in% names(df_datos)) {
      df_datos[["Parametro_dummy"]] <- NA_real_
    }
  }

  if (nrow(cfg)) {
    cfg$Y_Max <- to_numeric(cfg$Y_Max)
    cfg$Interval <- to_numeric(cfg$Interval)
    cfg$Y_Title <- normalize_text(cfg$Y_Title)
    idx_title_empty <- is.na(cfg$Y_Title) | !nzchar(cfg$Y_Title)
    cfg$Y_Title[idx_title_empty] <- ""
  }
  
  list(datos = df_datos, cfg = cfg)
}

