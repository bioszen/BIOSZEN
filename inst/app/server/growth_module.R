# Growth rate processing module
.bioszen_growth_runtime <- new.env(parent = emptyenv())
.bioszen_growth_runtime$active_jobs <- 0L

.bioszen_growth_job_started <- function() {
  .bioszen_growth_runtime$active_jobs <- .bioszen_growth_runtime$active_jobs + 1L
  invisible(.bioszen_growth_runtime$active_jobs)
}

.bioszen_growth_job_finished <- function() {
  .bioszen_growth_runtime$active_jobs <- max(0L, .bioszen_growth_runtime$active_jobs - 1L)
  invisible(.bioszen_growth_runtime$active_jobs)
}

.bioszen_growth_has_active_jobs <- function() {
  isTRUE(.bioszen_growth_runtime$active_jobs > 0L)
}

.bioszen_maybe_stop_app_when_growth_idle <- function() {
  active <- get0("active_sessions", ifnotfound = NULL, inherits = TRUE)
  if (is.null(active) || !length(active) || is.na(active[1]) || active[1] > 0) {
    return(invisible(FALSE))
  }
  if (.bioszen_growth_has_active_jobs()) return(invisible(FALSE))
  stop_on_last <- get0("should_stop_on_last_session", mode = "function", inherits = TRUE, ifnotfound = NULL)
  if (is.function(stop_on_last) && !isTRUE(stop_on_last())) return(invisible(FALSE))
  if (!is.function(stop_on_last)) {
    value <- tolower(trimws(Sys.getenv("BIOSZEN_STOP_ON_LAST_SESSION", unset = "")))
    if (nzchar(value) && !(value %in% c("1", "true", "yes", "y", "on"))) {
      return(invisible(FALSE))
    }
  }
  try(shiny::stopApp(), silent = TRUE)
  invisible(TRUE)
}

.bioszen_safe_growth_name <- function(x, fallback) {
  if (is.null(x) || !length(x) || is.na(x[1])) x <- ""
  x <- tools::file_path_sans_ext(basename(as.character(x[[1]])))
  x <- gsub("[^A-Za-z0-9._-]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  if (!nzchar(x)) fallback else x
}

.bioszen_unique_growth_stems <- function(names) {
  stems <- vapply(seq_along(names), function(i) {
    .bioszen_safe_growth_name(names[[i]], sprintf("growth_file_%d", i))
  }, character(1))
  make.unique(stems, sep = "_")
}

.bioszen_growth_progress_view <- function(stage,
                                          done,
                                          total,
                                          well = NA_character_,
                                          last_done = 0L,
                                          last_progress = 0) {
  as_count <- function(x, default = 0L) {
    if (is.null(x) || !length(x)) return(default)
    out <- suppressWarnings(as.integer(x[[1]]))
    if (!length(out) || is.na(out) || !is.finite(out)) default else max(0L, out)
  }
  as_fraction <- function(x, default = 0) {
    if (is.null(x) || !length(x)) return(default)
    out <- suppressWarnings(as.numeric(x[[1]]))
    if (!length(out) || is.na(out) || !is.finite(out)) default else max(0, min(1, out))
  }

  stage <- if (is.null(stage) || !length(stage) || is.na(stage[[1]])) "" else as.character(stage[[1]])
  total <- as_count(total)
  done <- as_count(done)
  last_done <- as_count(last_done)
  last_progress <- as_fraction(last_progress)

  final_stage <- stage %in% c("permissive_done", "permissive_skipped")
  advances_wells <- stage %in% c("checkpoint_loaded", "robust")
  visible_done <- last_done
  if (advances_wells) {
    visible_done <- max(last_done, done)
  }
  if (final_stage) {
    visible_done <- total
  }
  visible_done <- max(0L, min(visible_done, total))

  target <- last_progress
  if (total > 0L) {
    target <- if (final_stage) 1 else min(0.95, visible_done / total)
  }
  target <- max(last_progress, max(0, min(1, target)))

  well_label <- if (!is.null(well) && length(well) && !is.na(well[[1]]) && nzchar(as.character(well[[1]]))) {
    as.character(well[[1]])
  } else {
    ""
  }
  detail <- if (total > 0L) {
    if (nzchar(well_label)) {
      sprintf("%s [%d/%d]", well_label, visible_done, total)
    } else {
      sprintf("[%d/%d]", visible_done, total)
    }
  } else {
    well_label
  }

  list(done = visible_done, progress = target, detail = detail)
}

.bioszen_growth_result_columns <- c("Well", "µMax", "ODmax", "AUC", "lag_time", "max_percap_time", "doub_time", "max_time", "OD0")

.bioszen_growth_file_hash <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  hash <- tryCatch(unname(tools::md5sum(path)), error = function(e) NA_character_)
  as.character(hash[[1]])
}

.bioszen_resolve_growth_output_dir <- function(path) {
  if (is.null(path) || !length(path) || is.na(path[[1]])) return(NULL)
  path <- trimws(as.character(path[[1]]))
  if (!nzchar(path)) return(NULL)
  path <- path.expand(path)
  if (file.exists(path) && !dir.exists(path)) {
    stop(sprintf("The growth output path exists but is not a directory: %s", path), call. = FALSE)
  }
  if (!dir.exists(path)) {
    stop(
      sprintf(
        "The growth output folder does not exist. Choose an existing folder or leave it blank to download results at the end: %s",
        path
      ),
      call. = FALSE
    )
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

.bioszen_choose_growth_output_dir <- function(initial = "", caption = "Select growth output folder") {
  initial <- trimws(as.character(initial %||% ""))
  if (nzchar(initial)) {
    initial <- path.expand(initial)
  }
  if (!nzchar(initial) || !dir.exists(initial)) {
    initial <- path.expand("~")
  }

  selected <- NULL
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      isTRUE(tryCatch(rstudioapi::isAvailable(), error = function(e) FALSE))) {
    selected <- tryCatch(
      rstudioapi::selectDirectory(caption = caption, path = initial),
      error = function(e) NULL
    )
  }

  if ((is.null(selected) || !nzchar(selected)) && identical(.Platform$OS.type, "windows")) {
    selected <- tryCatch(
      utils::choose.dir(default = initial, caption = caption),
      error = function(e) NULL
    )
  }

  if ((is.null(selected) || !nzchar(selected)) && requireNamespace("tcltk", quietly = TRUE)) {
    selected <- tryCatch(
      tcltk::tk_choose.dir(default = initial, caption = caption),
      error = function(e) NULL
    )
  }

  if (is.null(selected) || !nzchar(selected) || is.na(selected[[1]])) return(NULL)
  normalizePath(path.expand(selected[[1]]), winslash = "/", mustWork = FALSE)
}

.bioszen_copy_growth_output_file <- function(file, output_dir) {
  if (is.null(output_dir) || !nzchar(output_dir)) return(invisible(FALSE))
  target <- file.path(output_dir, basename(file))
  copied <- file.copy(file, target, overwrite = TRUE)
  if (!isTRUE(copied)) {
    stop(sprintf("Could not save growth output file to: %s", target), call. = FALSE)
  }
  invisible(TRUE)
}

.bioszen_growth_checkpoint <- function(output_dir,
                                       stem,
                                       source_name,
                                       source_hash,
                                       max_time,
                                       time_interval,
                                       format) {
  if (is.null(output_dir) || !nzchar(output_dir)) return(NULL)
  metadata <- list(
    source_name = as.character(source_name),
    source_hash = as.character(source_hash),
    max_time = as.numeric(max_time),
    time_interval = as.numeric(time_interval),
    format = as.character(format),
    bioszen_version = tryCatch(as.character(utils::packageVersion("BIOSZEN")), error = function(e) "source")
  )
  key <- digest::digest(metadata, algo = "xxhash64")
  checkpoint_root <- file.path(output_dir, "BIOSZEN_growth_checkpoints")
  checkpoint_dir <- file.path(checkpoint_root, paste0(.bioszen_safe_growth_name(stem, "growth_file"), "_", substr(key, 1, 12)))
  dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(checkpoint_dir)) {
    stop(sprintf("Could not create growth checkpoint directory: %s", checkpoint_dir), call. = FALSE)
  }
  list(
    key = key,
    metadata = metadata,
    root = checkpoint_root,
    dir = checkpoint_dir,
    rds_file = file.path(checkpoint_dir, "checkpoint.rds"),
    partial_file = file.path(checkpoint_dir, paste0("Parametros_", .bioszen_safe_growth_name(stem, "growth_file"), "_partial.xlsx"))
  )
}

.bioszen_empty_growth_results <- function() {
  out <- as.data.frame(stats::setNames(replicate(length(.bioszen_growth_result_columns), logical(0), simplify = FALSE),
                                       .bioszen_growth_result_columns))
  out$Well <- character(0)
  for (col in setdiff(.bioszen_growth_result_columns, "Well")) out[[col]] <- numeric(0)
  out
}

.bioszen_growth_od0_lookup <- function(tidy_df) {
  if (is.null(tidy_df) || !is.data.frame(tidy_df) ||
      !all(c("Well", "Time", "Measurements") %in% names(tidy_df))) {
    return(stats::setNames(numeric(0), character(0)))
  }
  split_df <- split(tidy_df, as.character(tidy_df$Well), drop = TRUE)
  vapply(split_df, function(d) {
    measurements <- suppressWarnings(as.numeric(d$Measurements))
    time <- suppressWarnings(as.numeric(d$Time))
    ord <- order(time, seq_along(time), na.last = TRUE)
    measurements <- measurements[ord]
    measurements <- measurements[is.finite(measurements)]
    if (!length(measurements)) NA_real_ else measurements[[1]]
  }, numeric(1), USE.NAMES = TRUE)
}

.bioszen_fill_restored_od0 <- function(results, tidy_df) {
  if (is.null(results) || !is.data.frame(results) || !nrow(results)) return(results)
  if (!"OD0" %in% names(results)) results$OD0 <- NA_real_
  lookup <- .bioszen_growth_od0_lookup(tidy_df)
  if (!length(lookup)) return(results)
  idx <- match(as.character(results$Well), names(lookup))
  needs <- is.na(results$OD0) & !is.na(idx)
  results$OD0[needs] <- unname(lookup[idx[needs]])
  results
}

.bioszen_restore_growth_checkpoint <- function(checkpoint) {
  if (is.null(checkpoint) || !file.exists(checkpoint$rds_file)) {
    return(.bioszen_empty_growth_results())
  }
  obj <- tryCatch(readRDS(checkpoint$rds_file), error = function(e) NULL)
  if (is.null(obj) || !identical(obj$key, checkpoint$key) || !is.data.frame(obj$results)) {
    return(.bioszen_empty_growth_results())
  }
  results <- obj$results
  missing_cols <- setdiff(.bioszen_growth_result_columns, names(results))
  if (length(missing_cols) && setequal(missing_cols, "OD0")) {
    results$OD0 <- NA_real_
    missing_cols <- character(0)
  }
  if (length(missing_cols)) return(.bioszen_empty_growth_results())
  results <- results[, .bioszen_growth_result_columns, drop = FALSE]
  results$Well <- as.character(results$Well)
  results
}

.bioszen_write_growth_checkpoint <- function(checkpoint, results, completed = FALSE) {
  if (is.null(checkpoint)) return(invisible(FALSE))
  results <- as.data.frame(results)
  if (!nrow(results)) return(invisible(FALSE))
  results$Well <- as.character(results$Well)
  results <- results[, .bioszen_growth_result_columns, drop = FALSE]
  obj <- list(
    key = checkpoint$key,
    metadata = checkpoint$metadata,
    completed = isTRUE(completed),
    updated_at = as.character(Sys.time()),
    results = results
  )
  tmp_rds <- tempfile("checkpoint_", tmpdir = checkpoint$dir, fileext = ".rds")
  tmp_xlsx <- tempfile("partial_", tmpdir = checkpoint$dir, fileext = ".xlsx")
  on.exit(unlink(c(tmp_rds, tmp_xlsx), force = TRUE), add = TRUE)
  saveRDS(obj, tmp_rds)
  openxlsx::write.xlsx(
    results,
    tmp_xlsx,
    sheetName = if (isTRUE(completed)) "Resultados Combinados" else "Resultados Parciales",
    colNames = TRUE,
    rowNames = FALSE,
    overwrite = TRUE
  )
  if (!isTRUE(file.copy(tmp_rds, checkpoint$rds_file, overwrite = TRUE))) {
    stop(sprintf("Could not save growth checkpoint file: %s", checkpoint$rds_file), call. = FALSE)
  }
  if (!isTRUE(file.copy(tmp_xlsx, checkpoint$partial_file, overwrite = TRUE))) {
    stop(sprintf("Could not save growth partial workbook: %s", checkpoint$partial_file), call. = FALSE)
  }
  invisible(TRUE)
}

.bioszen_cleanup_growth_checkpoint <- function(checkpoint) {
  if (is.null(checkpoint) || is.null(checkpoint$dir) || !nzchar(checkpoint$dir)) {
    return(invisible(FALSE))
  }
  checkpoint_dir <- normalizePath(checkpoint$dir, winslash = "/", mustWork = FALSE)
  checkpoint_root <- if (!is.null(checkpoint$root) && nzchar(checkpoint$root)) {
    normalizePath(checkpoint$root, winslash = "/", mustWork = FALSE)
  } else {
    normalizePath(dirname(checkpoint_dir), winslash = "/", mustWork = FALSE)
  }
  if (!identical(basename(checkpoint_root), "BIOSZEN_growth_checkpoints")) {
    return(invisible(FALSE))
  }
  if (dir.exists(checkpoint_dir)) {
    unlink(checkpoint_dir, recursive = TRUE, force = TRUE)
  }
  if (dir.exists(checkpoint_root) && !length(list.files(checkpoint_root, all.files = FALSE, no.. = TRUE))) {
    unlink(checkpoint_root, recursive = TRUE, force = TRUE)
  }
  invisible(TRUE)
}

.bioszen_copy_growth_uploads <- function(files, names, parent_dir = file.path(tempdir(), "growth_upload_cache")) {
  if (is.null(files) || !length(files)) stop("No growth files were provided.")
  if (is.null(names) || length(names) != length(files)) {
    names <- basename(files)
  }

  if (!dir.exists(parent_dir)) {
    dir.create(parent_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(parent_dir)) {
    stop(sprintf("Could not create growth upload cache directory: %s", parent_dir))
  }

  run_dir <- tempfile(pattern = "run_", tmpdir = parent_dir)
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(run_dir)) {
    stop(sprintf("Could not create growth upload working directory: %s", run_dir))
  }

  stems <- .bioszen_unique_growth_stems(names)
  extensions <- gsub("[^A-Za-z0-9]+", "", tools::file_ext(names))
  extensions[!nzchar(extensions)] <- "xlsx"
  target_names <- paste0(stems, ".", extensions)
  targets <- file.path(run_dir, target_names)

  for (i in seq_along(files)) {
    if (!file.exists(files[[i]])) {
      stop(sprintf("Growth input file is no longer available: %s", files[[i]]))
    }
    copied <- file.copy(files[[i]], targets[[i]], overwrite = TRUE)
    if (!isTRUE(copied)) {
      stop(sprintf("Could not prepare growth input file: %s", names[[i]]))
    }
  }

  list(files = targets, names = names, output_stems = stems, cache_dir = run_dir)
}

.bioszen_compute_growth_results_batch_core <- function(tidy_df, should_abort = NULL, progress_callback = NULL) {
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
    dplyr::select(Well, µMax, ODmax, AUC, lag_time, max_percap_time, doub_time, max_time, OD0)
}

.bioszen_compute_growth_results_batch_checkpointed <- function(tidy_df,
                                                               should_abort = NULL,
                                                               progress_callback = NULL,
                                                               checkpoint = NULL) {
  wells <- unique(tidy_df$Well)
  tidy_df <- tidy_df %>%
    dplyr::mutate(
      Well = factor(Well, levels = wells),
      Time = as.numeric(Time)
    )

  restored <- .bioszen_restore_growth_checkpoint(checkpoint)
  restored <- restored[as.character(restored$Well) %in% as.character(wells), , drop = FALSE]
  restored <- .bioszen_fill_restored_od0(restored, tidy_df)
  if (nrow(restored)) {
    restored$Well <- factor(as.character(restored$Well), levels = wells)
    restored <- restored[!duplicated(as.character(restored$Well)), , drop = FALSE]
    restored <- dplyr::arrange(restored, Well)
  }

  done_wells <- as.character(restored$Well)
  missing_wells <- setdiff(as.character(wells), done_wells)
  total_wells <- length(wells)
  if (is.function(progress_callback) && length(done_wells)) {
    progress_callback(stage = "checkpoint_loaded", done = length(done_wells), total = total_wells, well = NA_character_)
  }

  results <- restored
  permissive_done <- 0L
  for (well in missing_wells) {
    .bioszen_abort_if_requested(should_abort)
    well_df <- tidy_df[as.character(tidy_df$Well) == well, , drop = FALSE]
    well_df$Well <- factor(as.character(well_df$Well), levels = wells)

    robust <- calculate_growth_rates_robust(well_df, should_abort = should_abort)
    robust$Well <- as.character(robust$Well)
    if (is.function(progress_callback)) {
      progress_callback(
        stage = "robust",
        done = length(unique(c(done_wells, as.character(results$Well), well))),
        total = total_wells,
        well = well
      )
    }
    .bioszen_abort_if_requested(should_abort)

    fill_cols <- setdiff(names(robust), "Well")
    needs_permissive <- length(fill_cols) && any(vapply(fill_cols, function(col) {
      any(is_empty_value(robust[[col]]))
    }, logical(1)))

    permissive <- robust
    for (col in fill_cols) permissive[[col]] <- rep(NA_real_, nrow(permissive))
    if (isTRUE(needs_permissive)) {
      permissive <- calculate_growth_rates_permissive(well_df, should_abort = should_abort)
      permissive$Well <- as.character(permissive$Well)
      permissive_done <- permissive_done + 1L
      if (is.function(progress_callback)) {
        progress_callback(stage = "permissive", done = permissive_done, total = total_wells, well = well)
      }
    }

    combined <- combine_growth_results(robust, permissive) %>%
      dplyr::mutate(Well = factor(as.character(Well), levels = wells)) %>%
      dplyr::arrange(Well) %>%
      dplyr::select(Well, µMax, ODmax, AUC, lag_time, max_percap_time, doub_time, max_time, OD0)
    results <- dplyr::bind_rows(results, combined)
    results$Well <- factor(as.character(results$Well), levels = wells)
    results <- results[!duplicated(as.character(results$Well)), , drop = FALSE]
    results <- dplyr::arrange(results, Well)
    .bioszen_write_growth_checkpoint(checkpoint, results, completed = FALSE)
  }

  if (is.function(progress_callback)) {
    progress_callback(stage = "permissive_done", done = total_wells, total = total_wells, well = NA_character_)
  }
  .bioszen_abort_if_requested(should_abort)
  results$Well <- factor(as.character(results$Well), levels = wells)
  results <- dplyr::arrange(results, Well)
  results <- .bioszen_fill_restored_od0(results, tidy_df)
  results <- dplyr::select(results, Well, µMax, ODmax, AUC, lag_time, max_percap_time, doub_time, max_time, OD0)
  .bioszen_write_growth_checkpoint(checkpoint, results, completed = TRUE)
  results
}

compute_growth_results_batch <- function(tidy_df,
                                         should_abort = NULL,
                                         progress_callback = NULL,
                                         checkpoint = NULL) {
  if (is.null(checkpoint)) {
    return(.bioszen_compute_growth_results_batch_core(
      tidy_df,
      should_abort = should_abort,
      progress_callback = progress_callback
    ))
  }
  .bioszen_compute_growth_results_batch_checkpointed(
    tidy_df,
    should_abort = should_abort,
    progress_callback = progress_callback,
    checkpoint = checkpoint
  )
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
  empty_growth_selection <- function() {
    data.frame(
      id = character(),
      name = character(),
      path = character(),
      cache_dir = character(),
      stringsAsFactors = FALSE
    )
  }

  cancel_requested <- shiny::reactiveVal(FALSE)
  growth_running <- shiny::reactiveVal(FALSE)
  status_text <- shiny::reactiveVal("")
  growth_files_selected <- shiny::reactiveVal(empty_growth_selection())
  growth_selection_cache_parent <- tempfile("growth_selected_uploads_")
  dir.create(growth_selection_cache_parent, recursive = TRUE, showWarnings = FALSE)
  growth_state <- new.env(parent = emptyenv())
  growth_state$cancel_requested <- FALSE
  growth_state$running <- FALSE
  growth_state$session_closed <- FALSE
  growth_state$last_event_pump <- Sys.time()

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

  pump_growth_events <- function() {
    if (requireNamespace("httpuv", quietly = TRUE)) {
      try(httpuv::service(1), silent = TRUE)
    }
    if (requireNamespace("later", quietly = TRUE)) {
      try(later::run_now(0), silent = TRUE)
    }
    if (requireNamespace("shiny", quietly = TRUE)) {
      flush_fun <- get0("flushReact", envir = asNamespace("shiny"), inherits = FALSE)
      if (is.function(flush_fun)) try(flush_fun(), silent = TRUE)
    }
    growth_state$last_event_pump <- Sys.time()
    invisible(NULL)
  }

  should_abort <- local({
    counter <- 0L
    function() {
      counter <<- counter + 1L
      now <- Sys.time()
      elapsed <- suppressWarnings(as.numeric(difftime(now, growth_state$last_event_pump, units = "secs")))
      if (!is.finite(elapsed) || elapsed >= 0.1) {
        pump_growth_events()
      }
      isTRUE(growth_state$cancel_requested)
    }
  })

  set_growth_buttons <- function(running) {
    if (requireNamespace("shinyjs", quietly = TRUE)) {
      try({
        if (isTRUE(running)) {
          shinyjs::disable("runGrowth")
          shinyjs::enable("stopGrowth")
        } else {
          shinyjs::enable("runGrowth")
          shinyjs::disable("stopGrowth")
        }
      }, silent = TRUE)
    }
    try(set_growth_running_flag(running), silent = TRUE)
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

  is_growth_session_closed <- function() {
    if (isTRUE(growth_state$session_closed)) return(TRUE)
    by_closed <- tryCatch(isTRUE(session$closed), error = function(e) FALSE)
    if (isTRUE(by_closed)) return(TRUE)
    tryCatch(isTRUE(session$isClosed()), error = function(e) FALSE)
  }

  safe_show_growth_notification <- function(ui,
                                            action = NULL,
                                            duration = 5,
                                            closeButton = TRUE,
                                            id = NULL,
                                            type = c("default", "message", "warning", "error")) {
    if (is_growth_session_closed()) return(invisible(NULL))
    notify_session <- tryCatch(session, error = function(e) NULL)
    if (!inherits(notify_session, "ShinySession")) return(invisible(NULL))
    send_notification <- tryCatch(notify_session$sendNotification, error = function(e) NULL)
    if (!is.function(send_notification)) return(invisible(NULL))
    type <- match.arg(type)
    tryCatch(
      shiny::showNotification(
        ui = ui,
        action = action,
        duration = duration,
        closeButton = closeButton,
        id = id,
        type = type,
        session = notify_session
      ),
      error = function(e) invisible(NULL)
    )
  }

  with_growth_progress <- function(message, value = 0, code) {
    code_expr <- substitute(code)
    code_env <- parent.frame()
    run_code <- function() eval(code_expr, envir = code_env)
    progress_session <- tryCatch(session, error = function(e) NULL)
    if (is_growth_session_closed() || !inherits(progress_session, "ShinySession")) {
      return(run_code())
    }
    tryCatch(
      shiny::withProgress(
        message = message,
        value = value,
        session = progress_session,
        {
          run_code()
        }
      ),
      error = function(e) {
        if (grepl("ShinySession", conditionMessage(e), fixed = TRUE) ||
            is_growth_session_closed()) {
          return(run_code())
        }
        stop(e)
      }
    )
  }

  safe_inc_progress <- function(amount, detail = NULL) {
    if (is_growth_session_closed()) return(invisible(NULL))
    progress_session <- tryCatch(session, error = function(e) NULL)
    if (!inherits(progress_session, "ShinySession")) return(invisible(NULL))
    try(shiny::incProgress(amount, detail = detail, session = progress_session), silent = TRUE)
    invisible(NULL)
  }

  render_growth_results_table <- function() {
    if (is_growth_session_closed()) return(invisible(NULL))
    files_done <- list.files(growth_out_dir, pattern = '^(Parametros|Parameters)_.*\\.xlsx$', full.names = TRUE)
    if (!length(files_done)) {
      output$growthTable <- DT::renderDT(data.frame(), options = list(pageLength = 10))
    } else {
      dfs <- lapply(files_done, readxl::read_excel, .name_repair = "minimal")
      names(dfs) <- basename(files_done)
      combined <- dplyr::bind_rows(dfs, .id = 'Archivo')
      output$growthTable <- DT::renderDT(combined, options = list(pageLength = 10))
    }
    invisible(NULL)
  }

  reset_growth_results <- function() {
    if (dir.exists(growth_out_dir)) unlink(growth_out_dir, recursive = TRUE)
    dir.create(growth_out_dir, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(growth_out_dir)) {
      stop(sprintf("Could not create growth results directory: %s", growth_out_dir))
    }
    render_growth_results_table()
    invisible(NULL)
  }

  output$growthStatus <- renderText({
    status_text()
  })

  reset_growth_results()

  clear_growth_upload_ui <- function() {
    if (requireNamespace("shinyjs", quietly = TRUE)) {
      try(shinyjs::reset("growthFiles"), silent = TRUE)
    }
    try(session$sendCustomMessage("bioszen-clear-growth-files", list(ts = as.numeric(Sys.time()))), silent = TRUE)
    invisible(NULL)
  }

  selected_growth_rows <- function(isolate_read = FALSE) {
    rows <- if (isTRUE(isolate_read)) {
      isolate(growth_files_selected())
    } else {
      growth_files_selected()
    }
    if (is.null(rows) || !is.data.frame(rows) || !nrow(rows)) {
      return(empty_growth_selection())
    }
    rows
  }

  selected_growth_ids <- function(isolate_read = FALSE) {
    rows <- selected_growth_rows(isolate_read = isolate_read)
    if (!nrow(rows)) return(character(0))
    ids <- if (isTRUE(isolate_read)) {
      isolate(input$growthFilesKeep %||% rows$id)
    } else {
      input$growthFilesKeep %||% rows$id
    }
    ids <- as.character(ids)
    ids <- ids[!is.na(ids) & nzchar(ids)]
    ids <- intersect(ids, rows$id)
    if (!length(ids) && nrow(rows)) ids <- rows$id
    ids
  }

  selected_growth_uploads <- function(isolate_read = FALSE) {
    rows <- selected_growth_rows(isolate_read = isolate_read)
    if (!nrow(rows)) return(rows)
    ids <- selected_growth_ids(isolate_read = isolate_read)
    if (!length(ids)) return(rows[0, , drop = FALSE])
    rows[match(ids, rows$id), , drop = FALSE]
  }

  output$growthSelectedFilesUI <- renderUI({
    rows <- selected_growth_rows()
    if (!nrow(rows)) {
      return(tags$div(
        class = "help-block",
        growth_tr("growth_no_selected_files", "No growth files selected yet.", current_lang())
      ))
    }
    selected <- input$growthFilesKeep %||% rows$id
    selected <- intersect(as.character(selected), rows$id)
    checkboxGroupInput(
      "growthFilesKeep",
      growth_tr("growth_selected_files", "Selected growth files", current_lang()),
      choices = stats::setNames(rows$id, rows$name),
      selected = selected
    )
  })

  observeEvent(input$browseGrowthOutputDir, {
    selected <- tryCatch(
      .bioszen_choose_growth_output_dir(
        initial = isolate(input$growthOutputDir %||% ""),
        caption = growth_tr("growth_browse_dir_caption", "Select growth output folder", current_lang())
      ),
      error = function(e) {
        msg <- conditionMessage(e)
        status_text(sprintf("Error: %s", msg))
        safe_show_growth_notification(
          sprintf(growth_tr("global_error_template", "Error in %s: %s", current_lang()), "growth", msg),
          type = "error",
          duration = 8
        )
        NULL
      }
    )
    if (!is.null(selected) && nzchar(selected)) {
      updateTextInput(session, "growthOutputDir", value = selected)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$growthFiles, {
    upload <- input$growthFiles
    if (is.null(upload) || !is.data.frame(upload) || !nrow(upload)) return()
    prepared <- tryCatch(
      .bioszen_copy_growth_uploads(
        files = upload$datapath,
        names = upload$name,
        parent_dir = growth_selection_cache_parent
      ),
      error = function(e) {
        msg <- conditionMessage(e)
        status_text(sprintf("Error: %s", msg))
        safe_show_growth_notification(
          sprintf(growth_tr("global_error_template", "Error in %s: %s", current_lang()), "growth", msg),
          type = "error",
          duration = 8
        )
        NULL
      }
    )
    clear_growth_upload_ui()
    if (is.null(prepared)) return()

    existing <- selected_growth_rows()
    previous_selected <- intersect(as.character(input$growthFilesKeep %||% existing$id), existing$id)
    ts <- sprintf("%.0f", as.numeric(Sys.time()) * 1000)
    new_rows <- data.frame(
      id = paste0("growth_file_", ts, "_", seq_along(prepared$files)),
      name = as.character(prepared$names),
      path = as.character(prepared$files),
      cache_dir = as.character(prepared$cache_dir),
      stringsAsFactors = FALSE
    )
    duplicate_rows <- existing$name %in% new_rows$name
    if (any(duplicate_rows)) {
      unlink(existing$path[duplicate_rows], force = TRUE)
      existing <- existing[!duplicate_rows, , drop = FALSE]
      previous_selected <- intersect(previous_selected, existing$id)
    }
    growth_files_selected(rbind(existing, new_rows))
    updateCheckboxGroupInput(
      session,
      "growthFilesKeep",
      choices = stats::setNames(c(existing$id, new_rows$id), c(existing$name, new_rows$name)),
      selected = unique(c(previous_selected, new_rows$id))
    )
    status_text(sprintf(
      growth_tr("growth_files_selected_count", "%d growth file(s) selected.", current_lang()),
      nrow(existing) + nrow(new_rows)
    ))
    if (!isTRUE(growth_running())) {
      growth_state$cancel_requested <- FALSE
      cancel_requested(FALSE)
      set_growth_buttons(FALSE)
    }
  }, ignoreInit = FALSE, ignoreNULL = TRUE, priority = 100)

  observeEvent(input$clearGrowthFiles, {
    rows <- selected_growth_rows(isolate_read = TRUE)
    if (nrow(rows)) {
      unlink(unique(rows$cache_dir), recursive = TRUE, force = TRUE)
    }
    growth_files_selected(empty_growth_selection())
    clear_growth_upload_ui()
    reset_growth_results()
    growth_state$cancel_requested <- FALSE
    cancel_requested(FALSE)
    set_growth_buttons(FALSE)
    status_text(growth_tr("growth_selection_cleared", "Growth file selection cleared.", current_lang()))
  }, ignoreInit = TRUE)

  run_growth_job <- function(files,
                             names,
                             max_time,
                             time_interval,
                             lang,
                             output_stems = NULL,
                             upload_cache_dir = NULL,
                             external_output_dir = NULL) {
    .bioszen_growth_job_started()
    on.exit({
      growth_state$running <- FALSE
      growth_state$cancel_requested <- FALSE
      growth_running(FALSE)
      cancel_requested(FALSE)
      set_growth_buttons(FALSE)
      .bioszen_growth_job_finished()
      if (!is.null(upload_cache_dir) && dir.exists(upload_cache_dir)) {
        unlink(upload_cache_dir, recursive = TRUE)
      }
      .bioszen_maybe_stop_app_when_growth_idle()
    }, add = TRUE)

    reset_growth_results()
    curve_prefix <- if (identical(lang, "es")) "Curvas_" else "Curves_"
    param_prefix <- if (identical(lang, "es")) "Parametros_" else "Parameters_"
    was_cancelled <- FALSE
    run_error <- NULL
    if (is.null(output_stems) || length(output_stems) != length(files)) {
      output_stems <- .bioszen_unique_growth_stems(names)
    }

    tryCatch({
      with_growth_progress(message = growth_tr("growth_progress_files", "Processing files...", lang), value = 0, {
        n_files <- length(files)
        for (i in seq_along(files)) {
          .bioszen_abort_if_requested(should_abort)
          f  <- files[i]
          display_nm <- tools::file_path_sans_ext(basename(names[i]))
          nm <- output_stems[[i]]
          prepared <- .bioszen_build_curves_sheet(f, max_time = max_time, time_interval = time_interval)
          new_data <- prepared$new_data
          fixed_params <- prepared$fixed_params
          status_text(sprintf("Processing file %d/%d: %s (%s format)", i, n_files, display_nm, prepared$format))
          curvas_file <- file.path(growth_out_dir, paste0(curve_prefix, nm, '.xlsx'))
          writexl::write_xlsx(list(Sheet1 = new_data, Sheet2 = fixed_params), path = curvas_file)
          .bioszen_copy_growth_output_file(curvas_file, external_output_dir)
          .bioszen_abort_if_requested(should_abort)
          tidy_df  <- gcplyr::trans_wide_to_tidy(new_data, id_cols = 'Time')
          total_wells <- length(unique(tidy_df$Well))
          checkpoint <- .bioszen_growth_checkpoint(
            external_output_dir,
            stem = nm,
            source_name = names[i],
            source_hash = .bioszen_growth_file_hash(f),
            max_time = max_time,
            time_interval = time_interval,
            format = prepared$format
          )
          last_progress <- 0
          last_visible_done <- 0L
          final_df <- NULL
          with_growth_progress(message = sprintf(growth_tr("growth_progress_curves", "Processing curves for %s", lang), nm), value = 0, {
            final_df <- compute_growth_results_batch(
              tidy_df,
              should_abort = should_abort,
              checkpoint = checkpoint,
              progress_callback = function(stage, done, total, well) {
                progress_view <- .bioszen_growth_progress_view(
                  stage = stage,
                  done = done,
                  total = total_wells,
                  well = well,
                  last_done = last_visible_done,
                  last_progress = last_progress
                )
                delta <- progress_view$progress - last_progress
                if (delta > 0) {
                  status_text(sprintf(
                    "File %d/%d (%s): %s",
                    i, n_files, display_nm, progress_view$detail
                  ))
                  safe_inc_progress(
                    delta,
                    detail = progress_view$detail
                  )
                  last_progress <<- progress_view$progress
                  last_visible_done <<- progress_view$done
                }
              }
            )
            if (last_progress < 1) safe_inc_progress(1 - last_progress, detail = display_nm)
          })
          param_file <- file.path(growth_out_dir, paste0(param_prefix, nm, '.xlsx'))
          openxlsx::write.xlsx(final_df, param_file, sheetName = 'Resultados Combinados',
                               colNames = TRUE, rowNames = FALSE)
          .bioszen_copy_growth_output_file(param_file, external_output_dir)
          .bioszen_cleanup_growth_checkpoint(checkpoint)
          safe_inc_progress(1 / n_files, detail = sprintf(growth_tr("growth_progress_file_done", "File %s completed", lang), display_nm))
        }
      })
    }, bioszen_growth_cancelled = function(e) {
      was_cancelled <<- TRUE
    }, error = function(e) {
      run_error <<- conditionMessage(e)
    })

    if (!is.null(run_error)) {
      status_text(sprintf("Error: %s", run_error))
      safe_show_growth_notification(
        sprintf(growth_tr("global_error_template", "Error in %s: %s", lang), "growth", run_error),
        type = "error",
        duration = 8
      )
    } else if (was_cancelled) {
      status_text("Process stopped by user.")
      safe_show_growth_notification(
        growth_tr("growth_stopped", "Growth parameter extraction stopped."),
        type = "warning",
        duration = 5
      )
    } else {
      status_text("Completed.")
    }

    render_growth_results_table()
  }

  observeEvent(input$stopGrowth, {
    if (!isTRUE(growth_running())) {
      growth_state$cancel_requested <- FALSE
      cancel_requested(FALSE)
      set_growth_buttons(FALSE)
      return()
    }
    growth_state$cancel_requested <- TRUE
    cancel_requested(TRUE)
    status_text("Stop requested. Waiting for safe cancellation point...")
    safe_show_growth_notification(
      growth_tr("growth_stop_requested", "Stop requested. Cancelling current processing..."),
      type = "warning",
      duration = 4
    )
  }, ignoreInit = TRUE)

  session$onSessionEnded(function() {
    growth_state$session_closed <- TRUE
    growth_state$cancel_requested <- TRUE
    cancel_requested(TRUE)
    rows <- selected_growth_rows(isolate_read = TRUE)
    if (nrow(rows)) unlink(unique(rows$cache_dir), recursive = TRUE, force = TRUE)
    if (dir.exists(growth_selection_cache_parent)) {
      unlink(growth_selection_cache_parent, recursive = TRUE, force = TRUE)
    }
  })

  observeEvent(input$runGrowth, {
    if (isTRUE(growth_running())) return()
    selected_rows <- selected_growth_uploads()
    if (!nrow(selected_rows)) {
      try(reset_growth_results(), silent = TRUE)
      no_file_msg <- growth_tr("growth_need_selected_files", "Select at least one growth file to process.", current_lang())
      status_text(sprintf("Error: %s", no_file_msg))
      safe_show_growth_notification(
        no_file_msg,
        type = "error",
        duration = 5
      )
      return()
    }
    external_output_dir <- tryCatch(
      .bioszen_resolve_growth_output_dir(isolate(input$growthOutputDir)),
      error = function(e) {
        msg <- conditionMessage(e)
        status_text(sprintf("Error: %s", msg))
        safe_show_growth_notification(
          sprintf(growth_tr("global_error_template", "Error in %s: %s", current_lang()), "growth", msg),
          type = "error",
          duration = 8
        )
        NA_character_
      }
    )
    if (length(external_output_dir) && is.na(external_output_dir[[1]])) return()
    reset_ok <- tryCatch(
      {
        reset_growth_results()
        status_text("Starting...")
        TRUE
      },
      error = function(e) {
        msg <- conditionMessage(e)
        status_text(sprintf("Error: %s", msg))
        safe_show_growth_notification(
          sprintf(growth_tr("global_error_template", "Error in %s: %s", current_lang()), "growth", msg),
          type = "error",
          duration = 8
        )
        FALSE
      }
    )
    if (!isTRUE(reset_ok)) return()
    prepared_uploads <- tryCatch(
      .bioszen_copy_growth_uploads(selected_rows$path, selected_rows$name),
      error = function(e) {
        msg <- conditionMessage(e)
        status_text(sprintf("Error: %s", msg))
        safe_show_growth_notification(
          sprintf(growth_tr("global_error_template", "Error in %s: %s", current_lang()), "growth", msg),
          type = "error",
          duration = 8
        )
        NULL
      }
    )
    if (is.null(prepared_uploads)) return()

    growth_state$cancel_requested <- FALSE
    growth_state$running <- TRUE
    cancel_requested(FALSE)
    growth_running(TRUE)
    set_growth_buttons(TRUE)
    max_time <- isolate(input$maxTime)
    time_interval <- isolate(input$timeInterval)
    lang <- isolate(current_lang())

    run_now <- function() {
      run_growth_job(
        prepared_uploads$files,
        prepared_uploads$names,
        max_time,
        time_interval,
        lang,
        output_stems = prepared_uploads$output_stems,
        upload_cache_dir = prepared_uploads$cache_dir,
        external_output_dir = external_output_dir
      )
    }

    if (isFALSE(getOption("bioszen_growth_force_sync", FALSE)) &&
        requireNamespace("later", quietly = TRUE)) {
      later::later(run_now, delay = 0)
    } else {
      run_now()
    }
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

  list(
    growth_dir = growth_out_dir,
    selected_files = growth_files_selected,
    selected_count = shiny::reactive(nrow(selected_growth_uploads())),
    running = growth_running,
    cancel_requested = cancel_requested
  )
}
