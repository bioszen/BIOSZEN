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
library(webshot)
library(ggrepel)        # etiquetas que no se sobre-ponen
library(shinyjs)
library(bslib)
library(gcplyr)
library(rlang)
library(patchwork)
library(officer)
library(rvg)
library(future)
library(future.apply)
library(parallelly)

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
  vapply(key, translate_one, character(1))
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
  out <- tryCatch(as.character(label), error = function(e) character(0))
  if (length(out) && nzchar(out[1])) {
    out[1] <- gsub("<[^>]+>", "", out[1])
    return(out[1])
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
  stats::setNames(values, labels_txt)
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

## Fuente y color por defecto para TODO ggplot2
theme_update(
  text = element_text(colour = "black", family = "Helvetica")
)

#──────── helpers genéricos para objetos sin método broom::tidy() ────────
# Las funciones matrix_to_tibble() y pmcmr_to_tibble() ahora residen en
# helpers.R para evitar duplicaciones.

#──────────────────────────────────────────────────────────────────────────  
# Helpers estadísticos ─ NUEVOS  
split_comparison <- function(x) {  
  stringr::str_split_fixed(x, "-", 2)  
}  

dunnett_to_tibble <- function(obj) {  
  # obj es la lista que devuelve DescTools::DunnettTest()  
  mat <- obj[[1]][ , 4, drop = FALSE]        # 4ª columna = p ajustado  
  cmp <- split_comparison(rownames(mat))  
  tibble(  
    grupo1 = cmp[, 1],  
    grupo2 = cmp[, 2],  
    p.adj  = mat[, 1]  
  )  
}  

set_control <- function(df, control_lbl) {  
  if (!is.null(control_lbl) && control_lbl %in% df$Label)  
    df$Label <- forcats::fct_relevel(df$Label, control_lbl)  
  df  
}  

# Si ya existían matrix_to_tibble() y pmcmr_to_tibble() NO los dupliques.  
#──────────────────────────────────────────────────────────────────────────  
safe_pairwise_t <- function(df, method = "sidak") {
  df <- filter_min_obs(df) |> droplevels()
  grupos <- levels(df$Label)
  if (length(grupos) < 2) return(tibble())
  combinaciones <- utils::combn(grupos, 2, simplify = FALSE)
  resultados <- lapply(combinaciones, function(g) {
    sub <- dplyr::filter(df, Label %in% g)
    tryCatch(
      rstatix::t_test(sub, Valor ~ Label, paired = can_paired(sub)),
      error = function(e) NULL
    )
  })
  resultados <- resultados[!vapply(resultados, is.null, logical(1))]
  if (length(resultados) == 0) return(tibble())
  dplyr::bind_rows(resultados) %>%
    rstatix::adjust_pvalue(method = method) %>%
    rstatix::add_significance("p.adj")
}

safe_pairwise_wilcox <- function(df, method = "holm") {
  df <- filter_min_obs(df) |> droplevels()
  grupos <- levels(df$Label)
  if (length(grupos) < 2) return(tibble())
  combinaciones <- utils::combn(grupos, 2, simplify = FALSE)
  resultados <- lapply(combinaciones, function(g) {
    sub <- dplyr::filter(df, Label %in% g)
    tryCatch(
      rstatix::wilcox_test(sub, Valor ~ Label, paired = can_paired(sub)),
      error = function(e) NULL
    )
  })
  resultados <- resultados[!vapply(resultados, is.null, logical(1))]
  if (length(resultados) == 0) return(tibble())
  dplyr::bind_rows(resultados) %>%
    rstatix::adjust_pvalue(method = method) %>%
    rstatix::add_significance("p.adj")
}


# ── Nombre seguro para archivos (mantiene la extensión) ────────────  
safe_file <- function(x) {  
  # separa nombre y extensión  
  ext  <- tools::file_ext(x)           # "png"  
  name <- tools::file_path_sans_ext(x) # "NAD+_Boxplot"  
  
  # sanea solo el nombre (letras, números, _ y -)  
  name <- gsub("[^A-Za-z0-9_\\-]", "_", name)  
  
  paste0(name, ".", ext)               # vuelve a unir con .  
}  

# ── Nombre seguro para hojas de Excel ──────────────────────────────  
safe_sheet <- function(x) {  
  ## solo letras, números o "_" (lo demás → "_")  
  gsub("[^A-Za-z0-9_]", "_", x)  
}  
# ── Nombre seguro para etiquetas simples ────────────────────  
sanitize <- function(x) {  
  gsub("[/\\\\:*?\"<>|]", "_", x)  
}  

theme_light <- bs_theme(version = 5)
theme_dark  <- bs_theme(version = 5, bootswatch = "cyborg")

tab_compos <- tabPanel(
  title = tr("tab_composition"),
  value = "tab_composition",
  sidebarLayout(
    sidebarPanel(style="overflow-y:auto;max-height:95vh;position:sticky;top:0;",
      uiOutput("plotPicker"),
      checkboxInput("show_legend_combo", tr("combo_show_legend"), value = FALSE),
      hr(),
      numericInput("nrow_combo", tr("combo_rows"), 1, min = 1, max = 4),
      numericInput("ncol_combo", tr("combo_cols"), 1, min = 1, max = 4),
      numericInput("combo_width",  tr("combo_width"), 1920, min = 400),
      numericInput("combo_height", tr("combo_height"), 1080, min = 400),
      numericInput("base_size_combo", tr("combo_base_size"), 18, min = 8),
      numericInput("fs_title_all",      tr("combo_title_size"), 20, min = 6),
      numericInput("fs_axis_title_all", tr("combo_axis_titles"), 16, min = 6),
      numericInput("fs_axis_text_all",  tr("combo_axis_ticks"),  14, min = 6),
      numericInput("fs_legend_all",     tr("combo_legend_text"), 16, min = 6),
      selectInput(
        "combo_pal",
        tr("combo_palette_label"),
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
        selected = "Original"
      ),
      uiOutput("plotOverrideUI"),
      actionButton("makeCombo", tr("combo_preview"),
                   class = "btn btn-primary"),
      br(), br(),
      downloadButton("dl_combo_png",  tr("combo_download_png")),
      downloadButton("dl_combo_pptx", tr("combo_download_pptx")),
      downloadButton("dl_combo_pdf",  tr("combo_download_pdf")),
      br(), br(),
      downloadButton("dl_combo_meta", tr("combo_download_meta")),
      fileInput("combo_meta", tr("combo_upload_meta"),
                accept = ".xlsx")
    ),
    mainPanel(
      plotOutput("comboPreview", height = "auto")
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
# ──────────────────────────────────────────────────────────────────────────────  

# ── Función para generar el Excel de resumen por parámetro ────────────  
generate_summary_wb <- function(datos, params) {  
  wb <- createWorkbook()
  for (param in params) {
    sheet <- safe_sheet(param)
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
                       .groups = "drop") %>%
      dplyr::arrange(Strain, BiologicalReplicate, TechnicalReplicate)
    
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
      ) %>%
      dplyr::arrange(Strain, BiologicalReplicate) %>%
      dplyr::rename(RepBiol = BiologicalReplicate)
    
    writeData(wb, sheet, resumen,  
              startRow    = fila,  
              startCol    = 1,  
              headerStyle = createStyle(textDecoration = "bold"))  
  }  
  wb
}

add_curves_by_group_sheet <- function(wb, curve_wide, meta_df) {  
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
  
  sheet_name <- "Curvas por grupo"  
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
      ) %>%  
      dplyr::arrange(BiologicalReplicate, Time) %>%  
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

platemap_from_summary_wb <- function(file) {
  sheets <- readxl::excel_sheets(file)
  res <- lapply(sheets, function(sh) {
    full <- readxl::read_excel(file, sheet = sh, col_names = FALSE)

    idx <- which(
      toupper(trimws(full[[1]])) == "STRAIN" &
        toupper(trimws(full[[2]])) == "REPBIOL"
    )
    if (length(idx) == 0) return(NULL)
    idx <- idx[1]

    raw_hdr <- as.character(unlist(full[idx, ], use.names = FALSE))
    raw_hdr <- trimws(raw_hdr)
    is_blank <- is.na(raw_hdr) | raw_hdr == ""
    raw_hdr[is_blank] <- paste0("V", seq_len(sum(is_blank)))
    hdr <- make.unique(raw_hdr, sep = "_")
    hdr[toupper(hdr) == "STRAIN"]  <- "Strain"
    hdr[toupper(hdr) == "REPBIOL"] <- "RepBiol"

    if (nrow(full) <= idx) return(NULL)
    ncols <- length(hdr)

    dat <- full[(idx + 1):nrow(full), seq_len(ncols)]
    dat <- as.data.frame(dat, stringsAsFactors = FALSE, check.names = FALSE)
    names(dat) <- hdr
    dat <- tibble::as_tibble(dat)

    if (!all(c("Strain", "RepBiol") %in% names(dat))) return(NULL)
    dat <- dplyr::filter(dat, !is.na(.data$Strain), !is.na(.data$RepBiol))
    if (!nrow(dat)) return(NULL)

    media_cols <- setdiff(names(dat), c("Strain", "RepBiol"))

    dat |>
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
      dplyr::select(
        .data$Strain, .data$Media, .data$BiologicalReplicate,
        .data$TechnicalReplicate, .data$Parameter, .data$Valor
      )
  })

  res <- Filter(Negate(is.null), res)
  if (!length(res)) return(NULL)
  dplyr::bind_rows(res)
}

build_platemap_from_summary <- function(file) {
  long_df <- platemap_from_summary_wb(file)
  if (is.null(long_df) || !nrow(long_df)) return(NULL)

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

  list(Datos = wide_df, PlotSettings = plot_settings)
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
prepare_platemap <- function(df_datos, cfg) {
  normalize_text <- function(x) {
    if (is.null(x)) return(x)
    out <- as.character(x)
    out <- gsub("\u00A0", " ", out, fixed = TRUE)
    out <- trimws(out)
    out[out == ""] <- NA_character_
    out
  }
  safe_num <- function(x) {
    if (is.numeric(x)) return(x)
    num <- to_numeric(x)
    if (all(is.na(num))) return(x)
    num
  }

  if (!is.null(df_datos) && !is.null(names(df_datos))) {
    names(df_datos) <- normalize_text(names(df_datos))
    for (col in c("Strain", "Media", "Well", "TechnicalReplicate", "Replicate")) {
      if (col %in% names(df_datos)) {
        df_datos[[col]] <- normalize_text(df_datos[[col]])
      }
    }
    for (col in c("Orden", "BiologicalReplicate")) {
      if (col %in% names(df_datos)) {
        df_datos[[col]] <- safe_num(df_datos[[col]])
      }
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

  # ─ 1. PlotSettings inexistente o vacío ────────────────────────────────────
  if (is.null(cfg) || nrow(cfg) == 0 || !"Parameter" %in% names(cfg)) {
    message("⚠️  PlotSettings no encontrado: se genera configuración mínima")
    # Identificamos columnas numéricas >> posibles parámetros
    param_cols <- setdiff(names(df_datos),
                          c("Strain","Media","Orden",
                            "BiologicalReplicate","TechnicalReplicate","Well"))
    if (length(param_cols) == 0) {
      param_cols <- "Parametro_dummy"
      df_datos[[param_cols]] <- NA_real_
    }
    cfg <- tibble::tibble(
      Parameter = param_cols,
      Y_Max     = 1,
      Interval  = 1,
      Y_Title   = param_cols
    )
  }
  
  # ─ 2. Limpiar parametros vacios y quedarnos solo con los que tienen datos ─
  if ("Parameter" %in% names(cfg)) {
    cfg$Parameter <- trimws(as.character(cfg$Parameter))
    cfg <- cfg[!is.na(cfg$Parameter) & nzchar(cfg$Parameter), , drop = FALSE]
  }
  
  if (nrow(cfg) > 0) {
    faltantes <- setdiff(cfg$Parameter, names(df_datos))
    if (length(faltantes)) {
      df_datos[ faltantes ] <- NA_real_
    }
    
    present <- intersect(cfg$Parameter, names(df_datos))
    if (length(present)) {
      df_datos[present] <- lapply(df_datos[present], to_numeric)
      has_data <- vapply(present, function(p) {
        col <- df_datos[[p]]
        if (is.numeric(col)) {
          any(is.finite(col))
        } else if (is.character(col) || is.factor(col)) {
          vals <- trimws(as.character(col))
          any(nzchar(vals))
        } else {
          any(!is.na(col))
        }
      }, logical(1))
      cfg <- cfg[cfg$Parameter %in% present[has_data], , drop = FALSE]
    } else {
      cfg <- cfg[0, , drop = FALSE]
    }
  }
  
  list(datos = df_datos, cfg = cfg)
}

