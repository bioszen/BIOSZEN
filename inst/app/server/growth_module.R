# Growth rate processing module
compute_growth_results_batch <- function(tidy_df, should_abort = NULL, progress_callback = NULL) {
  .bioszen_abort_if_requested(should_abort)
  wells <- unique(tidy_df$Well)
  tidy_df <- tidy_df %>%
    dplyr::mutate(
      Well = factor(Well, levels = wells),
      Time = as.numeric(Time)
    )

  robust <- calculate_growth_rates_robust(
    tidy_df,
    should_abort = should_abort,
    progress_callback = function(done, total, well) {
      if (is.function(progress_callback)) {
        progress_callback(stage = "robust", done = done, total = total, well = well)
      }
    }
  )
  .bioszen_abort_if_requested(should_abort)

  fill_cols <- setdiff(names(robust), "Well")
  permissive <- robust
  for (col in fill_cols) {
    permissive[[col]] <- rep(NA_real_, nrow(permissive))
  }

  need_permissive <- rep(FALSE, nrow(robust))
  if (length(fill_cols)) {
    for (col in fill_cols) {
      need_permissive <- need_permissive | is_empty_value(robust[[col]])
    }
  }

  if (any(need_permissive)) {
    wells_needed <- as.character(robust$Well[need_permissive])
    subset_df <- tidy_df[as.character(tidy_df$Well) %in% wells_needed, , drop = FALSE]
    subset_df$Well <- factor(subset_df$Well, levels = wells)
    permissive_subset <- calculate_growth_rates_permissive(
      subset_df,
      should_abort = should_abort,
      progress_callback = function(done, total, well) {
        if (is.function(progress_callback)) {
          progress_callback(stage = "permissive", done = done, total = total, well = well)
        }
      }
    )
    idx <- match(as.character(permissive$Well), as.character(permissive_subset$Well))
    matched <- !is.na(idx)
    for (col in fill_cols) {
      permissive[[col]][matched] <- permissive_subset[[col]][idx[matched]]
    }
    if (is.function(progress_callback)) {
      progress_callback(
        stage = "permissive_done",
        done = sum(need_permissive),
        total = sum(need_permissive),
        well = NA_character_
      )
    }
  } else if (is.function(progress_callback)) {
    progress_callback(stage = "permissive_skipped", done = 0L, total = 0L, well = NA_character_)
  }

  .bioszen_abort_if_requested(should_abort)

  combine_growth_results(robust, permissive) %>%
    dplyr::mutate(Well = factor(Well, levels = wells)) %>%
    dplyr::arrange(Well) %>%
    dplyr::select(Well, µMax, ODmax, AUC, lag_time, max_percap_time, doub_time, max_time)
}

.bioszen_parse_numeric <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  y <- trimws(as.character(x))
  y[y == ""] <- NA_character_
  y <- gsub(",", ".", y, fixed = TRUE)
  suppressWarnings(as.numeric(y))
}

.bioszen_is_index_like_series <- function(x) {
  vals <- .bioszen_parse_numeric(x)
  vals <- vals[is.finite(vals)]
  if (length(vals) < 3L) return(FALSE)

  diffs <- diff(vals)
  if (!length(diffs)) return(FALSE)

  tol <- sqrt(.Machine$double.eps)
  frac_int <- mean(abs(vals - round(vals)) <= tol)
  frac_step1 <- mean(abs(diffs - 1) <= tol)
  monotonic <- mean(diffs >= -tol)
  frac_unique <- length(unique(vals)) / length(vals)

  isTRUE(
    is.finite(frac_int) &&
      is.finite(frac_step1) &&
      is.finite(monotonic) &&
      is.finite(frac_unique) &&
      frac_int >= 0.95 &&
      monotonic >= 0.95 &&
      (frac_step1 >= 0.8 || frac_unique >= 0.95)
  )
}

.bioszen_is_processed_curves_table <- function(df) {
  if (is.null(df) || !ncol(df) || ncol(df) < 2) return(FALSE)
  df <- as.data.frame(df, check.names = FALSE, stringsAsFactors = FALSE)
  keep_cols <- vapply(df, function(col) !all(is.na(col)), logical(1))
  df <- df[, keep_cols, drop = FALSE]
  if (!ncol(df) || ncol(df) < 2) return(FALSE)

  time_num <- .bioszen_parse_numeric(df[[1]])
  time_finite <- is.finite(time_num)
  frac_time <- mean(time_finite)
  if (!is.finite(frac_time) || frac_time < 0.8) return(FALSE)

  time_vals <- time_num[time_finite]
  nondecreasing <- if (length(time_vals) > 1L) {
    mean(diff(time_vals) >= 0)
  } else {
    1
  }
  if (!is.finite(nondecreasing) || nondecreasing < 0.8) return(FALSE)

  if (ncol(df) >= 2) {
    # Raw reader exports often begin with two index-like columns that should
    # be dropped; avoid treating those files as already processed.
    first_index_like <- .bioszen_is_index_like_series(df[[1]])
    second_index_like <- .bioszen_is_index_like_series(df[[2]])
    if (isTRUE(first_index_like) && isTRUE(second_index_like)) return(FALSE)
  }

  col_numeric_frac <- vapply(df[-1], function(col) {
    vals <- .bioszen_parse_numeric(col)
    mean(is.finite(vals))
  }, numeric(1))

  any(is.finite(col_numeric_frac) & col_numeric_frac >= 0.6)
}

.bioszen_normalize_processed_curves <- function(df) {
  df <- as.data.frame(df, check.names = FALSE, stringsAsFactors = FALSE)
  keep_cols <- vapply(df, function(col) !all(is.na(col)), logical(1))
  df <- df[, keep_cols, drop = FALSE]
  names(df)[1] <- "Time"
  df$Time <- .bioszen_parse_numeric(df$Time)
  for (nm in names(df)[-1]) {
    df[[nm]] <- .bioszen_parse_numeric(df[[nm]])
  }
  df <- df[is.finite(df$Time), , drop = FALSE]
  if (!nrow(df) || ncol(df) < 2) {
    stop("Processed curves file does not contain a valid Time + wells table.")
  }
  df
}

.bioszen_build_curves_sheet <- function(file_path, max_time, time_interval) {
  first_sheet <- readxl::read_excel(file_path, sheet = 1, .name_repair = "minimal")
  fixed_params <- data.frame(
    X_Max      = 50,
    Interval_X = 10,
    Y_Max      = 1.5,
    Interval_Y = 0.5,
    X_Title    = "Tiempo (h)",
    Y_Title    = "OD620",
    stringsAsFactors = FALSE
  )

  if (.bioszen_is_processed_curves_table(first_sheet)) {
    new_data <- .bioszen_normalize_processed_curves(first_sheet)
    return(list(new_data = new_data, fixed_params = fixed_params, format = "processed"))
  }

  raw <- readxl::read_excel(file_path, skip = 2, .name_repair = "minimal")
  Time <- seq(0, max_time, by = time_interval)
  raw <- raw[seq_len(min(length(Time), nrow(raw))), , drop = FALSE]
  if (ncol(raw) < 3) {
    stop("Raw curves file does not contain expected columns.")
  }
  meas <- as.data.frame(raw[, -c(1, 2), drop = FALSE], check.names = FALSE, stringsAsFactors = FALSE)
  for (nm in names(meas)) {
    meas[[nm]] <- .bioszen_parse_numeric(meas[[nm]])
  }
  new_data <- data.frame(Time = Time[seq_len(nrow(raw))], meas, check.names = FALSE, stringsAsFactors = FALSE)
  list(new_data = new_data, fixed_params = fixed_params, format = "raw")
}

setup_growth_module <- function(input, output, session) {
  growth_out_dir <- file.path(tempdir(), 'growth_results')
  cancel_requested <- shiny::reactiveVal(FALSE)
  growth_running <- shiny::reactiveVal(FALSE)
  status_text <- shiny::reactiveVal("")
  growth_state <- new.env(parent = emptyenv())
  growth_state$cancel_requested <- FALSE
  growth_state$running <- FALSE

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

  growth_tr <- function(key, default, lang = NULL) {
    if (is.null(lang) || !length(lang) || is.na(lang[1]) || !nzchar(lang[1])) {
      lang <- get0("i18n_lang", ifnotfound = "en", inherits = TRUE)
    }
    if (is.null(lang) || !length(lang) || is.na(lang[1]) || !nzchar(lang[1])) {
      lang <- "en"
    }
    translator <- get0("tr_text", mode = "function")
    if (is.function(translator)) {
      val <- tryCatch(translator(key, lang = lang), error = function(e) NULL)
      if (is.character(val) && length(val) && !is.na(val[1]) && nzchar(val[1]) && val[1] != key) {
        return(val[1])
      }
    }
    default
  }

  should_abort <- local({
    counter <- 0L
    function() {
      counter <<- counter + 1L
      if ((counter %% 25L) != 0L) {
        return(FALSE)
      }
      isTRUE(growth_state$cancel_requested)
    }
  })

  set_growth_buttons <- function(running) {
    if (!requireNamespace("shinyjs", quietly = TRUE)) return(invisible(NULL))
    try({
      if (isTRUE(running)) {
        shinyjs::disable("runGrowth")
        shinyjs::enable("stopGrowth")
      } else {
        shinyjs::enable("runGrowth")
        shinyjs::disable("stopGrowth")
      }
    }, silent = TRUE)
    invisible(NULL)
  }

  set_growth_running_flag <- function(running) {
    try(
      session$sendCustomMessage(
        "bioszen-growth-running",
        list(running = isTRUE(running), ts = as.numeric(Sys.time()))
      ),
      silent = TRUE
    )
    invisible(NULL)
  }

  output$growthStatus <- renderText({
    status_text()
  })

  clear_growth_upload_ui <- function() {
    if (requireNamespace("shinyjs", quietly = TRUE)) {
      try(shinyjs::reset("growthFiles"), silent = TRUE)
    }
    try(session$sendCustomMessage("bioszen-clear-growth-files", list(ts = as.numeric(Sys.time()))), silent = TRUE)
    invisible(NULL)
  }

  run_growth_job <- function(files, names, max_time, time_interval, lang) {
    on.exit({
      growth_state$running <- FALSE
      growth_running(FALSE)
      set_growth_buttons(FALSE)
      set_growth_running_flag(FALSE)
    }, add = TRUE)

    if (dir.exists(growth_out_dir)) unlink(growth_out_dir, recursive = TRUE)
    dir.create(growth_out_dir)
    curve_prefix <- if (identical(lang, "es")) "Curvas_" else "Curves_"
    param_prefix <- if (identical(lang, "es")) "Parametros_" else "Parameters_"
    was_cancelled <- FALSE
    run_error <- NULL

    tryCatch({
      withProgress(message = growth_tr("growth_progress_files", "Processing files...", lang), value = 0, {
        n_files <- length(files)
        for (i in seq_along(files)) {
          .bioszen_abort_if_requested(should_abort)
          f  <- files[i]
          nm <- tools::file_path_sans_ext(names[i])
          prepared <- .bioszen_build_curves_sheet(f, max_time = max_time, time_interval = time_interval)
          new_data <- prepared$new_data
          fixed_params <- prepared$fixed_params
          status_text(sprintf("Processing file %d/%d: %s (%s format)", i, n_files, nm, prepared$format))
          curvas_file <- file.path(growth_out_dir, paste0(curve_prefix, nm, '.xlsx'))
          writexl::write_xlsx(list(Sheet1 = new_data, Sheet2 = fixed_params), path = curvas_file)
          .bioszen_abort_if_requested(should_abort)
          tidy_df  <- gcplyr::trans_wide_to_tidy(new_data, id_cols = 'Time')
          total_wells <- length(unique(tidy_df$Well))
          last_progress <- 0
          final_df <- NULL
          withProgress(message = sprintf(growth_tr("growth_progress_curves", "Processing curves for %s", lang), nm), value = 0, {
            final_df <- compute_growth_results_batch(
              tidy_df,
              should_abort = should_abort,
              progress_callback = function(stage, done, total, well) {
                target <- last_progress
                if (identical(stage, "robust") && total > 0) {
                  target <- 0.5 * (done / total)
                } else if (identical(stage, "permissive") && total > 0) {
                  target <- 0.5 + 0.5 * (done / total)
                } else if (identical(stage, "permissive_skipped") || identical(stage, "permissive_done")) {
                  target <- 1
                }
                delta <- target - last_progress
                if (delta > 0) {
                  detail_label <- if (!is.null(well) && !is.na(well) && nzchar(as.character(well))) {
                    as.character(well)
                  } else {
                    nm
                  }
                  phase_label <- if (identical(stage, "robust")) "Strict" else "Permissive"
                  if (identical(stage, "permissive_skipped")) phase_label <- "Permissive skipped"
                  status_text(sprintf(
                    "File %d/%d (%s): %s [%d/%d]",
                    i, n_files, nm, phase_label, min(done, total_wells), total_wells
                  ))
                  incProgress(
                    delta,
                    detail = sprintf("%s - %s [%d/%d]", phase_label, detail_label, min(done, total_wells), total_wells)
                  )
                  last_progress <<- target
                }
              }
            )
            if (last_progress < 1) incProgress(1 - last_progress, detail = nm)
          })
          param_file <- file.path(growth_out_dir, paste0(param_prefix, nm, '.xlsx'))
          openxlsx::write.xlsx(final_df, param_file, sheetName = 'Resultados Combinados',
                               colNames = TRUE, rowNames = FALSE)
          incProgress(1 / n_files, detail = sprintf(growth_tr("growth_progress_file_done", "File %s completed", lang), nm))
        }
      })
    }, bioszen_growth_cancelled = function(e) {
      was_cancelled <<- TRUE
    }, error = function(e) {
      run_error <<- conditionMessage(e)
    })

    if (!is.null(run_error)) {
      status_text(sprintf("Error: %s", run_error))
      showNotification(
        sprintf(growth_tr("global_error_template", "Error in %s: %s", lang), "growth", run_error),
        type = "error",
        duration = 8
      )
    } else if (was_cancelled) {
      status_text("Process stopped by user.")
      showNotification(
        growth_tr("growth_stopped", "Growth parameter extraction stopped."),
        type = "warning",
        duration = 5
      )
    } else {
      status_text("Completed.")
    }

    files_done <- list.files(growth_out_dir, pattern = '^(Parametros|Parameters)_.*\\.xlsx$', full.names = TRUE)
    if (!length(files_done)) {
      output$growthTable <- DT::renderDT(data.frame(), options = list(pageLength = 10))
    } else {
      dfs <- lapply(files_done, readxl::read_excel, .name_repair = "minimal")
      names(dfs) <- basename(files_done)
      combined <- dplyr::bind_rows(dfs, .id = 'Archivo')
      output$growthTable <- DT::renderDT(combined, options = list(pageLength = 10))
    }
  }

  observeEvent(input$stopGrowth, {
    growth_state$cancel_requested <- TRUE
    cancel_requested(TRUE)
    clear_growth_upload_ui()
    status_text("Stop requested. Waiting for safe cancellation point...")
    if (isTRUE(growth_running())) {
      showNotification(
        growth_tr("growth_stop_requested", "Stop requested. Cancelling current processing..."),
        type = "warning",
        duration = 4
      )
    }
  }, ignoreInit = TRUE)

  observeEvent(input$growth_progress_closed, {
    growth_state$cancel_requested <- TRUE
    cancel_requested(TRUE)
    clear_growth_upload_ui()
    status_text("Processing window closed. Stopping at next safe point...")
    if (isTRUE(growth_running())) {
      showNotification(
        growth_tr("growth_stop_requested", "Stop requested. Cancelling current processing..."),
        type = "warning",
        duration = 4
      )
    }
  }, ignoreInit = TRUE)

  session$onSessionEnded(function() {
    growth_state$cancel_requested <- TRUE
    cancel_requested(TRUE)
  })

  observeEvent(input$runGrowth, {
    req(input$growthFiles)
    if (isTRUE(growth_running())) return()
    growth_state$cancel_requested <- FALSE
    growth_state$running <- TRUE
    cancel_requested(FALSE)
    growth_running(TRUE)
    set_growth_buttons(TRUE)
    set_growth_running_flag(TRUE)
    status_text("Starting...")
    files <- isolate(input$growthFiles$datapath)
    names <- isolate(input$growthFiles$name)
    max_time <- isolate(input$maxTime)
    time_interval <- isolate(input$timeInterval)
    lang <- isolate(current_lang())

    run_growth_job(files, names, max_time, time_interval, lang)
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
