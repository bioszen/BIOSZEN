# Growth rate processing module
.bioszen_growth_runtime <- new.env(parent = emptyenv())
.bioszen_growth_runtime$active_jobs <- 0L

.bioszen_growth_table_options <- function() {
  list(
    pageLength = 100,
    lengthMenu = c(10, 25, 50, 100)
  )
}

.bioszen_empty_growth_table <- function() {
  columns <- c("Archivo", .bioszen_growth_result_columns)
  values <- lapply(columns, function(column) {
    if (column %in% c("Archivo", "Well")) character() else numeric()
  })
  names(values) <- columns
  structure(values, class = "data.frame", row.names = integer(0))
}

.bioszen_normalize_growth_table <- function(data) {
  columns <- c("Archivo", .bioszen_growth_result_columns)
  if (is.null(data) || !is.data.frame(data) || !nrow(data)) {
    return(.bioszen_empty_growth_table())
  }

  data <- data[, names(data), drop = FALSE]
  for (column in setdiff(columns, names(data))) {
    data[[column]] <- if (column %in% c("Archivo", "Well")) NA_character_ else NA_real_
  }
  data <- data[, columns, drop = FALSE]
  data$Archivo <- as.character(data$Archivo)
  data$Well <- as.character(data$Well)
  data
}

.bioszen_growth_table_browser_rows <- function(data) {
  data <- .bioszen_normalize_growth_table(data)
  if (!nrow(data)) return(list())

  lapply(seq_len(nrow(data)), function(index) {
    row <- lapply(data[index, , drop = FALSE], function(value) {
      value <- value[[1L]]
      if (!length(value) || is.na(value) || (is.numeric(value) && !is.finite(value))) {
        return("")
      }
      unname(value)
    })
    unname(row)
  })
}

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
  schedule_stop <- get0("schedule_stop_if_last_session", mode = "function", inherits = TRUE, ifnotfound = NULL)
  if (is.function(schedule_stop)) {
    schedule_stop()
    return(invisible(TRUE))
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

.bioszen_growth_file_hash <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  hash <- tryCatch(unname(tools::md5sum(path)), error = function(e) NA_character_)
  as.character(hash[[1]])
}

.bioszen_growth_local_config_enabled <- function() {
  if (isFALSE(getOption("bioszen_growth_persist_output_dir", TRUE))) return(FALSE)
  configured <- as.character(getOption("BIOSZEN.launch_mode", ""))
  launch_mode <- if (length(configured) && !is.na(configured[[1]])) {
    trimws(configured[[1]])
  } else {
    ""
  }
  if (identical(launch_mode, "hosted")) return(FALSE)

  hosted_markers <- c(
    "CONNECT_SERVER",
    "CONNECT_CONTENT_GUID",
    "RSCONNECT_SERVER",
    "RSCONNECT_CONTENT_GUID",
    "RSCONNECT_USER",
    "RSCONNECT_NAME",
    "RSCONNECT_APPLICATION",
    "SHINYAPPS_ACCOUNT",
    "SHINYAPPS_APPLICATION",
    "SHINY_SERVER_VERSION"
  )
  if (any(nzchar(trimws(Sys.getenv(hosted_markers, unset = ""))))) return(FALSE)
  active_config <- tolower(trimws(Sys.getenv("R_CONFIG_ACTIVE", unset = "")))
  !grepl("rsconnect|posit.?connect|shinyapps", active_config)
}

.bioszen_growth_output_config_dir <- function() {
  if (!.bioszen_growth_local_config_enabled()) return(NULL)
  path <- tryCatch(tools::R_user_dir("BIOSZEN", "config"), error = function(e) NULL)
  if (is.null(path) || !length(path) || is.na(path[[1]]) || !nzchar(path[[1]])) return(NULL)
  normalizePath(path[[1]], winslash = "/", mustWork = FALSE)
}

.bioszen_growth_output_config_files <- function(config_dir = .bioszen_growth_output_config_dir()) {
  if (is.null(config_dir) || !dir.exists(config_dir)) return(character(0))
  list.files(
    config_dir,
    pattern = "^growth-output-dir-.*\\.rds$",
    full.names = TRUE
  )
}

.bioszen_clear_persisted_growth_output_dir <- function() {
  if (!.bioszen_growth_local_config_enabled()) return(invisible(FALSE))
  files <- .bioszen_growth_output_config_files()
  if (length(files)) unlink(files, force = TRUE)
  invisible(TRUE)
}

.bioszen_load_persisted_growth_output_dir <- function() {
  if (!.bioszen_growth_local_config_enabled()) return(NULL)
  files <- .bioszen_growth_output_config_files()
  if (!length(files)) return(NULL)
  info <- file.info(files)
  files <- files[order(info$mtime, decreasing = TRUE, na.last = TRUE)]

  for (file in files) {
    record <- tryCatch(readRDS(file), error = function(e) NULL)
    path <- if (is.list(record)) record$path else record
    resolved <- tryCatch(.bioszen_resolve_growth_output_dir(path), error = function(e) NULL)
    if (!is.null(resolved)) return(resolved)
  }

  unlink(files, force = TRUE)
  NULL
}

.bioszen_persist_growth_output_dir <- function(path) {
  if (!.bioszen_growth_local_config_enabled()) return(invisible(FALSE))
  if (is.null(path) || !length(path) || is.na(path[[1]]) ||
      !nzchar(trimws(as.character(path[[1]])))) {
    return(.bioszen_clear_persisted_growth_output_dir())
  }
  resolved <- .bioszen_resolve_growth_output_dir(path)
  config_dir <- .bioszen_growth_output_config_dir()
  if (is.null(config_dir)) return(invisible(FALSE))
  if (!dir.exists(config_dir)) dir.create(config_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(config_dir)) return(invisible(FALSE))

  tmp <- tempfile(".growth-output-dir-", tmpdir = config_dir, fileext = ".tmp")
  on.exit(unlink(tmp, force = TRUE), add = TRUE)
  saved <- tryCatch({
    saveRDS(
      list(version = 1L, path = resolved, updated_at = as.character(Sys.time())),
      tmp
    )
    TRUE
  }, error = function(e) FALSE)
  if (!isTRUE(saved)) return(invisible(FALSE))

  token <- gsub("[^A-Za-z0-9]+", "", basename(tmp))
  target <- file.path(
    config_dir,
    paste0("growth-output-dir-", format(Sys.time(), "%Y%m%d%H%M%OS6"), "-", token, ".rds")
  )
  if (!isTRUE(file.rename(tmp, target))) return(invisible(FALSE))

  config_files <- .bioszen_growth_output_config_files(config_dir)
  config_keys <- normalizePath(config_files, winslash = "/", mustWork = FALSE)
  target_key <- normalizePath(target, winslash = "/", mustWork = FALSE)
  old_files <- config_files[config_keys != target_key]
  if (length(old_files)) unlink(old_files, force = TRUE)
  invisible(TRUE)
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

.bioszen_growth_output_roots <- function() {
  home <- normalizePath(path.expand("~"), winslash = "/", mustWork = FALSE)
  volumes <- tryCatch(shinyFiles::getVolumes()(), error = function(e) character(0))
  roots <- c(Home = home, volumes)
  roots <- roots[nzchar(roots) & dir.exists(roots)]
  if (!length(roots)) return(c(Home = home))

  root_names <- names(roots)
  normalized <- normalizePath(roots, winslash = "/", mustWork = FALSE)
  keys <- if (identical(.Platform$OS.type, "windows")) tolower(normalized) else normalized
  keep <- !duplicated(keys)
  roots <- normalized[keep]
  root_names <- root_names[keep]
  unnamed <- is.na(root_names) | !nzchar(root_names)
  root_names[unnamed] <- basename(roots[unnamed])
  names(roots) <- make.unique(root_names)
  roots
}

.bioszen_parse_growth_output_dir <- function(selection, roots = .bioszen_growth_output_roots()) {
  if (is.null(selection) || !is.list(selection) || !length(selection)) return(NULL)
  selected <- shinyFiles::parseDirPath(roots, selection)
  if (!length(selected) || is.na(selected[[1]]) || !nzchar(selected[[1]])) return(NULL)
  .bioszen_resolve_growth_output_dir(selected[[1]])
}

.bioszen_create_growth_output_dir <- function(request, roots = .bioszen_growth_output_roots()) {
  if (is.null(request) || !is.list(request)) {
    stop("Invalid folder creation request.", call. = FALSE)
  }
  name <- trimws(as.character(request$name %||% ""))
  if (!nzchar(name) || name %in% c(".", "..") || grepl("[/\\\\]", name)) {
    stop("Enter a valid folder name without path separators.", call. = FALSE)
  }

  root_value <- as.character(request$root %||% "")
  root <- NULL
  if (root_value %in% names(roots)) root <- unname(roots[[root_value]])
  if (is.null(root)) {
    root_match <- which(as.character(roots) == root_value)
    if (length(root_match)) root <- unname(roots[[root_match[[1]]]])
  }
  if (is.null(root) || !dir.exists(root)) {
    stop("The selected root folder is unavailable.", call. = FALSE)
  }
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)

  parts <- unlist(request$path %||% character(0), use.names = FALSE)
  parts <- trimws(as.character(parts))
  parts <- parts[nzchar(parts)]
  if (any(parts %in% c(".", "..")) || any(grepl("[/\\\\]", parts))) {
    stop("The selected folder path is invalid.", call. = FALSE)
  }
  parent <- if (length(parts)) do.call(file.path, as.list(c(root, parts))) else root
  parent <- normalizePath(parent, winslash = "/", mustWork = TRUE)
  compare <- function(x) if (identical(.Platform$OS.type, "windows")) tolower(x) else x
  root_key <- paste0(sub("/+$", "", compare(root)), "/")
  parent_key <- paste0(sub("/+$", "", compare(parent)), "/")
  if (!startsWith(parent_key, root_key)) {
    stop("The new folder must remain inside the selected root.", call. = FALSE)
  }

  target <- file.path(parent, name)
  if (!dir.exists(target) && !dir.create(target, recursive = FALSE, showWarnings = FALSE)) {
    stop(sprintf("Could not create the folder: %s", target), call. = FALSE)
  }
  normalizePath(target, winslash = "/", mustWork = TRUE)
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
                                       format,
                                       time_mode = c("fixed", "irregular"),
                                       time_column = NULL) {
  if (is.null(output_dir) || !nzchar(output_dir)) return(NULL)
  time_mode <- match.arg(time_mode)
  if (identical(time_mode, "fixed")) {
    # Keep the historical metadata exactly so existing fixed-mode checkpoints
    # continue to resolve to the same key.
    metadata <- list(
      source_name = as.character(source_name),
      source_hash = as.character(source_hash),
      max_time = as.numeric(max_time),
      time_interval = as.numeric(time_interval),
      format = as.character(format),
      bioszen_version = tryCatch(as.character(utils::packageVersion("BIOSZEN")), error = function(e) "source")
    )
  } else {
    metadata <- list(
      source_name = as.character(source_name),
      source_hash = as.character(source_hash),
      time_mode = "irregular",
      time_column = .bioszen_normalize_growth_column_name(time_column %||% ""),
      format = as.character(format),
      bioszen_version = tryCatch(as.character(utils::packageVersion("BIOSZEN")), error = function(e) "source")
    )
  }
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
  results <- results[, names(results), drop = FALSE]
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

.bioszen_compute_growth_results_batch_checkpointed <- function(tidy_df,
                                                               should_abort = NULL,
                                                               progress_callback = NULL,
                                                               result_callback = NULL,
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
    .bioszen_emit_growth_results(result_callback, restored)
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
      dplyr::select(dplyr::all_of(c(
        "Well", "\u00b5Max", "ODmax", "AUC", "lag_time",
        "max_percap_time", "doub_time", "max_time", "OD0"
      )))
    results <- dplyr::bind_rows(results, combined)
    results$Well <- factor(as.character(results$Well), levels = wells)
    results <- results[!duplicated(as.character(results$Well)), , drop = FALSE]
    results <- dplyr::arrange(results, Well)
    .bioszen_write_growth_checkpoint(checkpoint, results, completed = FALSE)
    .bioszen_emit_growth_results(result_callback, results)
  }

  if (is.function(progress_callback)) {
    progress_callback(stage = "permissive_done", done = total_wells, total = total_wells, well = NA_character_)
  }
  .bioszen_abort_if_requested(should_abort)
  results$Well <- factor(as.character(results$Well), levels = wells)
  results <- dplyr::arrange(results, Well)
  results <- .bioszen_fill_restored_od0(results, tidy_df)
  results <- dplyr::select(results, dplyr::all_of(c(
    "Well", "\u00b5Max", "ODmax", "AUC", "lag_time",
    "max_percap_time", "doub_time", "max_time", "OD0"
  )))
  .bioszen_write_growth_checkpoint(checkpoint, results, completed = TRUE)
  results
}

compute_growth_results_batch <- function(tidy_df,
                                         should_abort = NULL,
                                         progress_callback = NULL,
                                         result_callback = NULL,
                                         checkpoint = NULL) {
  .bioszen_compute_growth_results_batch_checkpointed(
    tidy_df,
    should_abort = should_abort,
    progress_callback = progress_callback,
    result_callback = result_callback,
    checkpoint = checkpoint
  )
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
  growth_table_data <- shiny::reactiveVal(.bioszen_empty_growth_table())
  growth_files_selected <- shiny::reactiveVal(empty_growth_selection())
  growth_selection_cache_parent <- tempfile("growth_selected_uploads_")
  dir.create(growth_selection_cache_parent, recursive = TRUE, showWarnings = FALSE)
  growth_state <- new.env(parent = emptyenv())
  growth_state$cancel_requested <- FALSE
  growth_state$running <- FALSE
  growth_state$session_closed <- FALSE
  growth_state$last_event_pump <- Sys.time()
  growth_state$table_parts <- list()
  growth_state$last_table_publish <- as.POSIXct(NA)
  growth_state$last_table_rows <- 0L
  growth_state$output_dir_initialized <- FALSE
  async_growth <- new.env(parent = emptyenv())
  async_growth$active <- FALSE
  async_growth$finished <- TRUE
  async_growth$file_progress <- NULL
  async_growth$curve_progress <- NULL

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

  growth_error_text <- function(error, lang = NULL) {
    if (!inherits(error, "bioszen_growth_time_error")) {
      if (inherits(error, "condition")) return(conditionMessage(error))
      return(as.character(error %||% ""))
    }
    column <- as.character(error$column %||% "Time")
    requested <- as.character(error$requested %||% "")
    available <- as.character(error$available %||% "")
    switch(
      as.character(error$code %||% ""),
      column_not_found = sprintf(
        growth_tr(
          "growth_time_error_column_not_found",
          "The time column '%s' was not found. Available columns: %s",
          lang
        ),
        requested,
        available
      ),
      column_not_detected = sprintf(
        growth_tr(
          "growth_time_error_column_not_detected",
          "No time column was detected. Enter its name explicitly. Available columns: %s",
          lang
        ),
        available
      ),
      ambiguous_column = growth_tr(
        "growth_time_error_ambiguous_column",
        "More than one possible time column was found. Enter the time-column name explicitly.",
        lang
      ),
      missing_values = sprintf(
        growth_tr(
          "growth_time_error_missing_values",
          "Time column '%s' contains missing values.",
          lang
        ),
        column
      ),
      nonnumeric_values = sprintf(
        growth_tr(
          "growth_time_error_nonnumeric_values",
          "Time column '%s' must contain only finite numeric values.",
          lang
        ),
        column
      ),
      duplicated_values = sprintf(
        growth_tr(
          "growth_time_error_duplicated_values",
          "Time column '%s' contains duplicated values.",
          lang
        ),
        column
      ),
      non_increasing_values = sprintf(
        growth_tr(
          "growth_time_error_non_increasing_values",
          "Time column '%s' must be strictly increasing.",
          lang
        ),
        column
      ),
      conditionMessage(error)
    )
  }

  growth_output_roots <- .bioszen_growth_output_roots()
  shinyFiles::shinyDirChoose(
    input,
    "browseGrowthOutputDir",
    roots = growth_output_roots,
    session = session,
    defaultRoot = "Home",
    allowDirCreate = TRUE
  )

  restored_growth_output_dir <- .bioszen_load_persisted_growth_output_dir()
  restore_growth_output_dir <- function() {
    if (!is.null(restored_growth_output_dir)) {
      updateTextInput(session, "growthOutputDir", value = restored_growth_output_dir)
    }
    growth_state$output_dir_initialized <- TRUE
    invisible(NULL)
  }
  restore_registered <- tryCatch({
    session$onFlushed(restore_growth_output_dir, once = TRUE)
    TRUE
  }, error = function(e) FALSE)
  if (!isTRUE(restore_registered)) restore_growth_output_dir()

  pump_growth_events <- function() {
    if (requireNamespace("shiny", quietly = TRUE)) {
      flush_fun <- get0("flushReact", envir = asNamespace("shiny"), inherits = FALSE)
      if (is.function(flush_fun)) try(flush_fun(), silent = TRUE)
    }
    if (!tryCatch(isTRUE(session$isClosed()), error = function(e) TRUE)) {
      try(session$requestFlush(), silent = TRUE)
      try(session$flushOutput(), silent = TRUE)
    }
    if (requireNamespace("later", quietly = TRUE)) {
      try(later::run_now(0), silent = TRUE)
    }
    if (requireNamespace("httpuv", quietly = TRUE)) {
      try(httpuv::service(1), silent = TRUE)
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

  output$growthTable <- DT::renderDT({
    DT::datatable(
      shiny::isolate(growth_table_data()),
      options = .bioszen_growth_table_options(),
      rownames = FALSE,
      callback = DT::JS(
        "if (window.Shiny && typeof Shiny.setInputValue === 'function') {",
        "  Shiny.setInputValue('growthTableReady', { nonce: Date.now() }, { priority: 'event' });",
        "}"
      )
    )
  }, server = FALSE)
  shiny::outputOptions(output, "growthTable", suspendWhenHidden = FALSE)

  publish_growth_table <- function(force = FALSE, pump = TRUE) {
    if (is_growth_session_closed()) return(invisible(FALSE))
    now <- Sys.time()
    elapsed <- suppressWarnings(as.numeric(difftime(
      now,
      growth_state$last_table_publish,
      units = "secs"
    )))
    parts <- growth_state$table_parts
    combined <- if (length(parts)) {
      dplyr::bind_rows(parts, .id = "Archivo")
    } else {
      data.frame()
    }
    combined <- .bioszen_normalize_growth_table(combined)
    row_count <- nrow(combined)
    new_rows <- row_count - growth_state$last_table_rows
    if (!isTRUE(force) && growth_state$last_table_rows > 0L &&
        new_rows < 5L && is.finite(elapsed) && elapsed < 0.75) {
      return(invisible(FALSE))
    }

    growth_table_data(combined)
    try(
      session$sendCustomMessage(
        "bioszen-growth-table-data",
        list(rows = .bioszen_growth_table_browser_rows(combined))
      ),
      silent = TRUE
    )
    try(session$requestFlush(), silent = TRUE)
    growth_state$last_table_publish <- now
    growth_state$last_table_rows <- row_count
    if (isTRUE(pump)) pump_growth_events()
    invisible(TRUE)
  }

  store_growth_table_results <- function(file_name, results, force = FALSE, pump = TRUE) {
    if (is_growth_session_closed() || is.null(results) || !is.data.frame(results) || !nrow(results)) {
      return(invisible(FALSE))
    }
    rows <- as.data.frame(results)
    if ("Well" %in% names(rows)) rows$Well <- as.character(rows$Well)
    growth_state$table_parts[[as.character(file_name)]] <- rows
    publish_growth_table(force = force, pump = pump)
  }

  shiny::observeEvent(input$growthTableReady, {
    publish_growth_table(force = TRUE, pump = FALSE)
  }, ignoreInit = TRUE)

  render_growth_results_table <- function() {
    if (is_growth_session_closed()) return(invisible(NULL))
    files_done <- list.files(growth_out_dir, pattern = '^(Parametros|Parameters)_.*\\.xlsx$', full.names = TRUE)
    if (!length(files_done)) {
      growth_state$table_parts <- list()
    } else {
      dfs <- lapply(files_done, readxl::read_excel, .name_repair = "minimal")
      names(dfs) <- basename(files_done)
      growth_state$table_parts <- lapply(dfs, as.data.frame)
    }
    publish_growth_table(force = TRUE)
    invisible(NULL)
  }

  reset_growth_results <- function() {
    if (dir.exists(growth_out_dir)) unlink(growth_out_dir, recursive = TRUE)
    dir.create(growth_out_dir, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(growth_out_dir)) {
      stop(sprintf("Could not create growth results directory: %s", growth_out_dir))
    }
    growth_state$table_parts <- list()
    growth_state$last_table_publish <- as.POSIXct(NA)
    growth_state$last_table_rows <- 0L
    publish_growth_table(force = TRUE)
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
      .bioszen_parse_growth_output_dir(
        input$browseGrowthOutputDir,
        roots = growth_output_roots
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

  observeEvent(input$growthOutputDir, {
    if (!isTRUE(growth_state$output_dir_initialized)) return()
    path <- trimws(as.character(input$growthOutputDir %||% ""))
    if (!nzchar(path)) {
      .bioszen_clear_persisted_growth_output_dir()
    } else if (dir.exists(path.expand(path))) {
      try(.bioszen_persist_growth_output_dir(path), silent = TRUE)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$clearGrowthOutputDir, {
    .bioszen_clear_persisted_growth_output_dir()
    updateTextInput(session, "growthOutputDir", value = "")
    status_text(growth_tr(
      "growth_output_dir_cleared",
      "The saved growth output folder was cleared.",
      current_lang()
    ))
  }, ignoreInit = TRUE)

  observeEvent(input[["browseGrowthOutputDir-newDir"]], {
    request <- input[["browseGrowthOutputDir-newDir"]]
    created <- tryCatch(
      .bioszen_create_growth_output_dir(request, roots = growth_output_roots),
      error = function(e) {
        msg <- conditionMessage(e)
        status_text(sprintf("Error: %s", msg))
        safe_show_growth_notification(
          sprintf(growth_tr("growth_folder_create_error", "Could not create folder: %s", current_lang()), msg),
          type = "error",
          duration = 8
        )
        NULL
      }
    )
    if (is.null(created)) return()
    updateTextInput(session, "growthOutputDir", value = created)
    safe_show_growth_notification(
      sprintf(growth_tr("growth_folder_created", "Folder created: %s", current_lang()), created),
      type = "message",
      duration = 5
    )
    session$sendCustomMessage("bioszenGrowthFolderCreated", list(path = created))
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
                             time_mode,
                             time_column,
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
          prepared <- .bioszen_build_curves_sheet(
            f,
            max_time = max_time,
            time_interval = time_interval,
            time_mode = time_mode,
            time_column = time_column
          )
          new_data <- prepared$new_data
          fixed_params <- prepared$fixed_params
          status_text(sprintf("Processing file %d/%d: %s (%s format)", i, n_files, display_nm, prepared$format))
          curvas_file <- file.path(growth_out_dir, paste0(curve_prefix, nm, '.xlsx'))
          writexl::write_xlsx(list(Sheet1 = new_data, Sheet2 = fixed_params), path = curvas_file)
          .bioszen_copy_growth_output_file(curvas_file, external_output_dir)
          .bioszen_abort_if_requested(should_abort)
          tidy_df  <- gcplyr::trans_wide_to_tidy(new_data, id_cols = 'Time')
          total_wells <- length(unique(tidy_df$Well))
          param_name <- paste0(param_prefix, nm, '.xlsx')
          checkpoint <- .bioszen_growth_checkpoint(
            external_output_dir,
            stem = nm,
            source_name = names[i],
            source_hash = .bioszen_growth_file_hash(f),
            max_time = max_time,
            time_interval = time_interval,
            format = prepared$format,
            time_mode = time_mode,
            time_column = prepared$time_column %||% time_column
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
              },
              result_callback = function(results) {
                store_growth_table_results(
                  param_name,
                  results,
                  force = nrow(results) >= total_wells
                )
              }
              )
            if (last_progress < 1) safe_inc_progress(1 - last_progress, detail = display_nm)
          })
          store_growth_table_results(param_name, final_df, force = TRUE)
          param_file <- file.path(growth_out_dir, param_name)
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
      run_error <<- growth_error_text(e, lang)
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

    if (is.null(run_error) && !was_cancelled) {
      render_growth_results_table()
    } else {
      publish_growth_table(force = TRUE)
    }
  }

  new_growth_progress <- function(message, value = 0) {
    if (is_growth_session_closed()) return(NULL)
    progress_session <- tryCatch(session, error = function(e) NULL)
    if (!inherits(progress_session, "ShinySession")) return(NULL)
    progress <- tryCatch(
      shiny::Progress$new(session = progress_session, min = 0, max = 1),
      error = function(e) NULL
    )
    if (!is.null(progress)) {
      try(progress$set(message = message, value = value), silent = TRUE)
    }
    progress
  }

  set_growth_progress <- function(progress, value = NULL, detail = NULL) {
    if (is.null(progress) || is_growth_session_closed()) return(invisible(NULL))
    if (!is.null(value)) value <- max(0, min(1, as.numeric(value)))
    try(progress$set(value = value, detail = detail), silent = TRUE)
    invisible(NULL)
  }

  close_growth_progress <- function(progress) {
    if (!is.null(progress)) try(progress$close(), silent = TRUE)
    invisible(NULL)
  }

  finish_async_growth <- function(run_error = NULL, was_cancelled = FALSE, quiet = FALSE) {
    if (isTRUE(async_growth$finished)) return(invisible(NULL))
    async_growth$finished <- TRUE
    async_growth$active <- FALSE
    close_growth_progress(async_growth$curve_progress)
    close_growth_progress(async_growth$file_progress)
    async_growth$curve_progress <- NULL
    async_growth$file_progress <- NULL

    growth_state$running <- FALSE
    growth_state$cancel_requested <- FALSE
    session_closed <- is_growth_session_closed()
    if (!session_closed) {
      growth_running(FALSE)
      cancel_requested(FALSE)
      set_growth_buttons(FALSE)

      if (!is.null(run_error)) {
        run_error <- growth_error_text(run_error, async_growth$lang)
        status_text(sprintf("Error: %s", run_error))
        safe_show_growth_notification(
          sprintf(
            growth_tr("global_error_template", "Error in %s: %s", async_growth$lang),
            "growth",
            run_error
          ),
          type = "error",
          duration = 8
        )
      } else if (isTRUE(was_cancelled)) {
        status_text("Process stopped by user.")
        safe_show_growth_notification(
          growth_tr("growth_stopped", "Growth parameter extraction stopped.", async_growth$lang),
          type = "warning",
          duration = 5
        )
        publish_growth_table(force = TRUE, pump = FALSE)
      } else if (!isTRUE(quiet)) {
        status_text("Completed.")
        render_growth_results_table()
      }
    }

    upload_cache_dir <- async_growth$upload_cache_dir
    async_growth$upload_cache_dir <- NULL
    if (!is.null(upload_cache_dir) && dir.exists(upload_cache_dir)) {
      unlink(upload_cache_dir, recursive = TRUE, force = TRUE)
    }
    .bioszen_growth_job_finished()
    .bioszen_maybe_stop_app_when_growth_idle()
    invisible(NULL)
  }

  schedule_async_growth_step <- function(step, delay = 0.01) {
    later::later(function() {
      if (!isTRUE(async_growth$active) || isTRUE(async_growth$finished)) {
        return(invisible(NULL))
      }
      if (is_growth_session_closed()) {
        finish_async_growth(was_cancelled = TRUE, quiet = TRUE)
        return(invisible(NULL))
      }
      if (isTRUE(growth_state$cancel_requested)) {
        finish_async_growth(was_cancelled = TRUE)
        return(invisible(NULL))
      }
      tryCatch(
        step(),
        bioszen_growth_cancelled = function(e) {
          finish_async_growth(was_cancelled = TRUE)
        },
        error = function(e) {
          finish_async_growth(run_error = e)
        }
      )
      invisible(NULL)
    }, delay = delay)
    invisible(NULL)
  }

  finish_async_growth_file <- function() {
    results <- async_growth$results
    well_levels <- async_growth$well_levels
    tidy_df <- async_growth$tidy_df

    results$Well <- factor(as.character(results$Well), levels = well_levels)
    results <- dplyr::arrange(results, Well)
    results <- .bioszen_fill_restored_od0(results, tidy_df)
    results <- dplyr::select(results, dplyr::all_of(c(
      "Well", "\u00b5Max", "ODmax", "AUC", "lag_time",
      "max_percap_time", "doub_time", "max_time", "OD0"
    )))
    .bioszen_write_growth_checkpoint(async_growth$checkpoint, results, completed = TRUE)
    store_growth_table_results(async_growth$param_name, results, force = TRUE, pump = FALSE)

    openxlsx::write.xlsx(
      results,
      async_growth$param_file,
      sheetName = "Resultados Combinados",
      colNames = TRUE,
      rowNames = FALSE
    )
    .bioszen_copy_growth_output_file(async_growth$param_file, async_growth$external_output_dir)
    .bioszen_cleanup_growth_checkpoint(async_growth$checkpoint)

    close_growth_progress(async_growth$curve_progress)
    async_growth$curve_progress <- NULL
    set_growth_progress(
      async_growth$file_progress,
      value = async_growth$file_index / async_growth$n_files,
      detail = sprintf(
        growth_tr("growth_progress_file_done", "File %s completed", async_growth$lang),
        async_growth$display_nm
      )
    )

    async_growth$file_index <- async_growth$file_index + 1L
    if (async_growth$file_index > async_growth$n_files) {
      finish_async_growth()
    } else {
      schedule_async_growth_step(start_async_growth_file)
    }
    invisible(NULL)
  }

  process_async_growth_well <- function() {
    if (isTRUE(growth_state$cancel_requested)) {
      finish_async_growth(was_cancelled = TRUE)
      return(invisible(NULL))
    }
    if (async_growth$well_index > length(async_growth$missing_wells)) {
      finish_async_growth_file()
      return(invisible(NULL))
    }

    well <- async_growth$missing_wells[[async_growth$well_index]]
    tidy_df <- async_growth$tidy_df
    well_df <- tidy_df[as.character(tidy_df$Well) == well, , drop = FALSE]
    well_df$Well <- factor(as.character(well_df$Well), levels = async_growth$well_levels)

    # Use the public batch path for one well so strict/permissive selection and
    # all numerical results remain identical to synchronous extraction.
    well_result <- compute_growth_results_batch(
      well_df,
      should_abort = should_abort,
      checkpoint = NULL
    )
    results <- dplyr::bind_rows(async_growth$results, well_result)
    results$Well <- factor(as.character(results$Well), levels = async_growth$well_levels)
    results <- results[!duplicated(as.character(results$Well)), , drop = FALSE]
    results <- dplyr::arrange(results, Well)
    async_growth$results <- results

    .bioszen_write_growth_checkpoint(async_growth$checkpoint, results, completed = FALSE)
    store_growth_table_results(async_growth$param_name, results, force = TRUE, pump = FALSE)

    completed <- length(async_growth$completed_wells) + async_growth$well_index
    detail <- sprintf("%s [%d/%d]", well, completed, async_growth$total_wells)
    status_text(sprintf(
      "File %d/%d (%s): %s",
      async_growth$file_index,
      async_growth$n_files,
      async_growth$display_nm,
      detail
    ))
    set_growth_progress(
      async_growth$curve_progress,
      value = completed / async_growth$total_wells,
      detail = detail
    )

    async_growth$well_index <- async_growth$well_index + 1L
    if (async_growth$well_index > length(async_growth$missing_wells)) {
      schedule_async_growth_step(finish_async_growth_file)
    } else {
      # Returning to the Shiny event loop here is what makes finalized rows
      # visible in ordinary browsers while extraction continues.
      schedule_async_growth_step(process_async_growth_well)
    }
    invisible(NULL)
  }

  start_async_growth_file <- function() {
    i <- async_growth$file_index
    f <- async_growth$files[[i]]
    display_nm <- tools::file_path_sans_ext(basename(async_growth$names[[i]]))
    nm <- async_growth$output_stems[[i]]
    prepared <- .bioszen_build_curves_sheet(
      f,
      max_time = async_growth$max_time,
      time_interval = async_growth$time_interval,
      time_mode = async_growth$time_mode,
      time_column = async_growth$time_column
    )
    new_data <- prepared$new_data
    fixed_params <- prepared$fixed_params
    status_text(sprintf(
      "Processing file %d/%d: %s (%s format)",
      i,
      async_growth$n_files,
      display_nm,
      prepared$format
    ))

    curvas_file <- file.path(
      growth_out_dir,
      paste0(async_growth$curve_prefix, nm, ".xlsx")
    )
    writexl::write_xlsx(list(Sheet1 = new_data, Sheet2 = fixed_params), path = curvas_file)
    .bioszen_copy_growth_output_file(curvas_file, async_growth$external_output_dir)

    tidy_df <- gcplyr::trans_wide_to_tidy(new_data, id_cols = "Time")
    well_levels <- as.character(unique(tidy_df$Well))
    if (!length(well_levels)) stop("No growth wells were found in the selected file.")
    tidy_df <- tidy_df %>%
      dplyr::mutate(
        Well = factor(as.character(Well), levels = well_levels),
        Time = as.numeric(Time)
      )

    param_name <- paste0(async_growth$param_prefix, nm, ".xlsx")
    checkpoint <- .bioszen_growth_checkpoint(
      async_growth$external_output_dir,
      stem = nm,
      source_name = async_growth$names[[i]],
      source_hash = .bioszen_growth_file_hash(f),
      max_time = async_growth$max_time,
      time_interval = async_growth$time_interval,
      format = prepared$format,
      time_mode = async_growth$time_mode,
      time_column = prepared$time_column %||% async_growth$time_column
    )
    restored <- .bioszen_restore_growth_checkpoint(checkpoint)
    restored <- restored[
      as.character(restored$Well) %in% well_levels,
      ,
      drop = FALSE
    ]
    restored <- .bioszen_fill_restored_od0(restored, tidy_df)
    if (nrow(restored)) {
      restored$Well <- factor(as.character(restored$Well), levels = well_levels)
      restored <- restored[!duplicated(as.character(restored$Well)), , drop = FALSE]
      restored <- dplyr::arrange(restored, Well)
    }

    completed_wells <- as.character(restored$Well)
    async_growth$display_nm <- display_nm
    async_growth$nm <- nm
    async_growth$tidy_df <- tidy_df
    async_growth$well_levels <- well_levels
    async_growth$total_wells <- length(well_levels)
    async_growth$completed_wells <- completed_wells
    async_growth$missing_wells <- setdiff(well_levels, completed_wells)
    async_growth$well_index <- 1L
    async_growth$results <- restored
    async_growth$checkpoint <- checkpoint
    async_growth$param_name <- param_name
    async_growth$param_file <- file.path(growth_out_dir, param_name)

    async_growth$curve_progress <- new_growth_progress(
      sprintf(
        growth_tr("growth_progress_curves", "Processing curves for %s", async_growth$lang),
        nm
      ),
      value = length(completed_wells) / length(well_levels)
    )
    if (nrow(restored)) {
      store_growth_table_results(param_name, restored, force = TRUE, pump = FALSE)
      set_growth_progress(
        async_growth$curve_progress,
        value = length(completed_wells) / length(well_levels),
        detail = sprintf("[%d/%d]", length(completed_wells), length(well_levels))
      )
    }

    if (!length(async_growth$missing_wells)) {
      schedule_async_growth_step(finish_async_growth_file)
    } else {
      schedule_async_growth_step(process_async_growth_well)
    }
    invisible(NULL)
  }

  run_growth_job_async <- function(files,
                                   names,
                                   max_time,
                                   time_interval,
                                   time_mode,
                                   time_column,
                                   lang,
                                   output_stems = NULL,
                                   upload_cache_dir = NULL,
                                   external_output_dir = NULL) {
    if (is.null(output_stems) || length(output_stems) != length(files)) {
      output_stems <- .bioszen_unique_growth_stems(names)
    }
    .bioszen_growth_job_started()
    async_growth$active <- TRUE
    async_growth$finished <- FALSE
    async_growth$files <- files
    async_growth$names <- names
    async_growth$output_stems <- output_stems
    async_growth$max_time <- max_time
    async_growth$time_interval <- time_interval
    async_growth$time_mode <- time_mode
    async_growth$time_column <- time_column
    async_growth$lang <- lang
    async_growth$upload_cache_dir <- upload_cache_dir
    async_growth$external_output_dir <- external_output_dir
    async_growth$file_index <- 1L
    async_growth$n_files <- length(files)
    async_growth$curve_prefix <- if (identical(lang, "es")) "Curvas_" else "Curves_"
    async_growth$param_prefix <- if (identical(lang, "es")) "Parametros_" else "Parameters_"
    async_growth$file_progress <- new_growth_progress(
      growth_tr("growth_progress_files", "Processing files...", lang),
      value = 0
    )
    schedule_async_growth_step(start_async_growth_file, delay = 0)
    invisible(NULL)
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
    if (isTRUE(async_growth$active) && !isTRUE(async_growth$finished)) {
      finish_async_growth(was_cancelled = TRUE, quiet = TRUE)
    }
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
    if (is.null(external_output_dir)) {
      .bioszen_clear_persisted_growth_output_dir()
    } else {
      try(.bioszen_persist_growth_output_dir(external_output_dir), silent = TRUE)
    }
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
    time_mode <- isolate(as.character(input$growthTimeMode %||% "fixed")[[1]])
    if (!(time_mode %in% c("fixed", "irregular"))) time_mode <- "fixed"
    time_column <- isolate(trimws(as.character(input$growthTimeColumn %||% "")[[1]]))
    if (!nzchar(time_column)) time_column <- NULL
    lang <- isolate(current_lang())

    run_sync <- function() {
      run_growth_job(
        prepared_uploads$files,
        prepared_uploads$names,
        max_time,
        time_interval,
        time_mode,
        time_column,
        lang,
        output_stems = prepared_uploads$output_stems,
        upload_cache_dir = prepared_uploads$cache_dir,
        external_output_dir = external_output_dir
      )
    }

    if (isFALSE(getOption("bioszen_growth_force_sync", FALSE)) &&
        requireNamespace("later", quietly = TRUE)) {
      run_growth_job_async(
        prepared_uploads$files,
        prepared_uploads$names,
        max_time,
        time_interval,
        time_mode,
        time_column,
        lang,
        output_stems = prepared_uploads$output_stems,
        upload_cache_dir = prepared_uploads$cache_dir,
        external_output_dir = external_output_dir
      )
    } else {
      run_sync()
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
    cancel_requested = cancel_requested,
    table_data = growth_table_data
  )
}
