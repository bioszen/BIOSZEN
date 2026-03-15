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
safe_pairwise_t <- function(df, method = "none") {
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

safe_pairwise_wilcox <- function(df, method = "none") {
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
  sheets <- readxl::excel_sheets(file)
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
      dplyr::select(
        .data$Strain, .data$Media, .data$BiologicalReplicate,
        .data$TechnicalReplicate, .data$Parameter, .data$Valor
      )
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

build_platemap_from_mean_sd <- function(file) {
  sheets <- readxl::excel_sheets(file)
  sh <- find_sheet_alias(
    sheets,
    c("Parameters_Summary", "Parametros_Summary", "Summary_Parameters", "Resumen_Parametros")
  )
  if (is.null(sh)) return(NULL)

  raw <- read_excel_tmp(file, sheet = sh)
  mapped <- apply_column_aliases(raw, allow_media_alias = TRUE)
  df <- mapped$datos
  col_labels <- mapped$labels

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

  y_max <- vapply(param_cols, function(pm) {
    vals <- suppressWarnings(as.numeric(wide_df[[pm]]))
    sd_col <- paste0("SD_", pm)
    sds <- if (sd_col %in% names(wide_df)) suppressWarnings(as.numeric(wide_df[[sd_col]])) else rep(0, length(vals))
    mx <- suppressWarnings(max(vals + ifelse(is.finite(sds), sds, 0), na.rm = TRUE))
    if (!is.finite(mx)) mx <- suppressWarnings(max(vals, na.rm = TRUE))
    if (!is.finite(mx)) mx <- NA_real_
    round_up_sig(mx, digits = 2)
  }, numeric(1))
  interval <- ifelse(is.na(y_max) | y_max == 0, NA_real_, y_max / 5)

  plot_settings <- tibble::tibble(
    Parameter = param_cols,
    Y_Max = y_max,
    Interval = interval,
    Y_Title = param_cols
  )

  list(
    Datos = wide_df,
    PlotSettings = plot_settings,
    Labels = col_labels,
    SummaryMode = TRUE
  )
}

build_curve_from_mean_sd <- function(file) {
  sheets <- readxl::excel_sheets(file)
  sh <- find_sheet_alias(
    sheets,
    c("Curves_Summary", "Curvas_Summary", "Summary_Curves", "Resumen_Curvas")
  )
  if (is.null(sh)) return(NULL)

  raw <- read_excel_tmp(file, sheet = sh)
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
  x_break <- x_max / 3
  y_break <- y_max / 3
  if (!is.finite(x_break) || x_break <= 0) x_break <- 1
  if (!is.finite(y_break) || y_break <= 0) y_break <- 1

  sheet2 <- group_map %>%
    dplyr::transmute(
      X_Max = x_max,
      Interval_X = x_break,
      Y_Max = y_max,
      Interval_Y = y_break,
      X_Title = "Time",
      Y_Title = "Signal",
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

load_curve_workbook <- function(file) {
  sheets <- tryCatch(readxl::excel_sheets(file), error = function(e) character(0))
  summary_aliases <- c(
    "Curves_Summary", "Curvas_Summary", "Summary_Curves", "Resumen_Curvas"
  )
  has_sheet1 <- "Sheet1" %in% sheets
  has_summary_sheet <- !is.null(find_sheet_alias(sheets, summary_aliases))

  d <- tryCatch(read_excel_tmp(file, sheet = "Sheet1"), error = function(e) NULL)
  s <- NULL
  meta <- NULL
  summary_tbl <- NULL
  from_summary <- FALSE
  parse_message <- NULL

  if (is.null(d) || !"Time" %in% names(d)) {
    parsed <- tryCatch(
      build_curve_from_mean_sd(file),
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
      return(list(
        ok = FALSE,
        reason = reason,
        message = parse_message,
        Sheet1 = NULL,
        Sheet2 = NULL,
        Meta = NULL,
        Summary = NULL,
        SummaryMode = FALSE
      ))
    }
    d <- parsed$Sheet1
    s <- parsed$Sheet2
    meta <- parsed$Meta
    summary_tbl <- parsed$Summary
    from_summary <- TRUE
  } else {
    s <- tryCatch(read_excel_tmp(file, sheet = "Sheet2"), error = function(e) NULL)
  }

  if (is.null(d) || !"Time" %in% names(d)) {
    return(list(
      ok = FALSE,
      reason = "invalid",
      message = "Curve workbook format not recognized.",
      Sheet1 = NULL,
      Sheet2 = NULL,
      Meta = NULL,
      Summary = NULL,
      SummaryMode = FALSE
    ))
  }

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

  default_x_break <- if (is.finite(d_time_max) && d_time_max > 0) d_time_max / 3 else 1
  default_y_break <- if (is.finite(d_val_max) && d_val_max > 0) d_val_max / 3 else 1

  required_cols <- c("X_Max", "Interval_X", "Y_Max", "Interval_Y", "X_Title", "Y_Title")
  if (is.null(s) || !all(required_cols %in% names(s))) {
    s <- tibble::tibble(
      X_Max = d_time_max,
      Interval_X = default_x_break,
      Y_Max = d_val_max,
      Interval_Y = default_y_break,
      X_Title = "Tiempo (min)",
      Y_Title = "OD (620 nm)",
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

