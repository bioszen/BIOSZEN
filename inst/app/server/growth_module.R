# Growth rate processing module
setup_growth_module <- function(input, output, session) {
  growth_out_dir <- file.path(tempdir(), 'growth_results')

  # Use translated text when available; fall back to defaults for standalone tests
  current_lang <- function() {
    lang <- NULL
    if (!is.null(input$app_lang)) lang <- input$app_lang
    if (is.null(lang) || !length(lang) || is.na(lang[1]) || !nzchar(lang[1])) {
      lang <- get0("i18n_lang", ifnotfound = "en", inherits = TRUE)
    }
    if (is.null(lang) || !length(lang) || is.na(lang[1]) || !nzchar(lang[1])) {
      return("en")
    }
    as.character(lang[[1]])
  }

  growth_tr <- function(key, default, lang = current_lang()) {
    translator <- get0("tr_text", mode = "function")
    if (is.function(translator)) {
      val <- tryCatch(translator(key, lang = lang), error = function(e) NULL)
      if (is.character(val) && length(val) && !is.na(val[1]) && nzchar(val[1]) && val[1] != key) {
        return(val[1])
      }
    }
    default
  }

  observeEvent(input$runGrowth, {
    req(input$growthFiles)
    files <- input$growthFiles$datapath
    names <- input$growthFiles$name
    if (dir.exists(growth_out_dir)) unlink(growth_out_dir, recursive = TRUE)
    dir.create(growth_out_dir)
    lang <- current_lang()
    curve_prefix <- if (identical(lang, "es")) "Curvas_" else "Curves_"
    param_prefix <- if (identical(lang, "es")) "Parametros_" else "Parameters_"
    withProgress(message = growth_tr("growth_progress_files", "Processing files...", lang), value = 0, {
      n_files <- length(files)
      for (i in seq_along(files)) {
        f  <- files[i]
        nm <- tools::file_path_sans_ext(names[i])
        raw <- readxl::read_excel(f, skip = 2)
        Time <- seq(0, input$maxTime, by = input$timeInterval)
        raw <- raw[seq_len(min(length(Time), nrow(raw))), ]
        new_data <- data.frame(Time = Time[seq_len(nrow(raw))], raw[, -c(1,2)])
        fixed_params <- data.frame(
          X_Max      = 50,
          Interval_X = 10,
          Y_Max      = 1.5,
          Interval_Y = 0.5,
          X_Title    = 'Tiempo (h)',
          Y_Title    = 'OD620',
          stringsAsFactors = FALSE
        )
        curvas_file <- file.path(growth_out_dir, paste0(curve_prefix, nm, '.xlsx'))
        writexl::write_xlsx(list(Sheet1 = new_data, Sheet2 = fixed_params), path = curvas_file)
        raw_wide <- gcplyr::read_wides(curvas_file, sheet = 'Sheet1', startrow = 1, startcol = 1)
        tidy_df  <- gcplyr::trans_wide_to_tidy(raw_wide[, -1], id_cols = 'Time')
        wells    <- unique(tidy_df$Well)
        n_wells  <- length(wells)
        total_steps <- n_wells * 2
        withProgress(message = sprintf(growth_tr("growth_progress_curves", "Processing curves for %s", lang), nm), value = 0, {
          step <- 0
          all_results <- vector('list', n_wells)
          for (k in seq_along(wells)) {
            w  <- wells[k]
            df_w <- tidy_df %>%
              filter(Well == w) %>%
              mutate(Well = factor(Well, levels = wells), Time = as.numeric(Time))
            rob <- calculate_growth_rates_robust(df_w)
            step <- step + 1
            incProgress(1/total_steps, detail = sprintf('R %d/%d: %s', step, total_steps, w))
            perm <- calculate_growth_rates_permissive(df_w)
            step <- step + 1
            incProgress(1/total_steps, detail = sprintf('P %d/%d: %s', step, total_steps, w))
            combined <- combine_growth_results(rob, perm)
            combined$Well <- w
            all_results[[k]] <- combined
          }
        })
        final_df <- bind_rows(all_results) %>%
          mutate(Well = factor(Well, levels = wells)) %>%
          arrange(Well) %>%
          dplyr::select(Well, µMax, ODmax, AUC, lag_time, max_percap_time, doub_time, max_time)
        param_file <- file.path(growth_out_dir, paste0(param_prefix, nm, '.xlsx'))
        openxlsx::write.xlsx(final_df, param_file, sheetName = 'Resultados Combinados',
                             colNames = TRUE, rowNames = FALSE)
        incProgress(1 / n_files, detail = sprintf(growth_tr("growth_progress_file_done", "File %s completed", lang), nm))
      }
    })
    # Escapar el punto para que el parseo no falle
    files <- list.files(growth_out_dir, pattern = '^(Parametros|Parameters)_.*\\.xlsx$', full.names = TRUE)
    dfs <- lapply(files, readxl::read_excel)
    names(dfs) <- basename(files)
    combined <- bind_rows(dfs, .id = 'Archivo')
    output$growthTable <- DT::renderDT(combined, options = list(pageLength = 10))
  })

  output$downloadGrowthZip <- downloadHandler(
    filename = function() "growth_results.zip",
    content = function(file) {
      old_wd <- getwd()
      setwd(growth_out_dir)
      on.exit(setwd(old_wd), add = TRUE)
      files_to_zip <- list.files(pattern = "\\.xlsx$")
      zip::zip(zipfile = file, files = files_to_zip)
    },
    contentType = "application/zip"
  )

  list(growth_dir = growth_out_dir)
}
