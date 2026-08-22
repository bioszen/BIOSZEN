# Load the package's canonical growth input and calculation implementation into
# the Shiny app environment. Source checkouts use R/ directly; installed
# packages bind the same functions from the BIOSZEN namespace.
.bioszen_load_growth_core <- function() {
  target <- environment(sys.function())
  source_root_value <- get0("source_root", envir = target, inherits = TRUE, ifnotfound = "")
  app_dir_value <- get0("app_dir", envir = target, inherits = TRUE, ifnotfound = "")
  frame_files <- unlist(lapply(sys.frames(), function(frame) {
    path <- frame$ofile
    if (is.character(path) && length(path) && nzchar(path[[1]])) path[[1]] else character()
  }), use.names = FALSE)
  starts <- unique(c(source_root_value, app_dir_value, getwd(), dirname(frame_files)))
  source_dirs <- character()
  for (start in starts[nzchar(starts)]) {
    current <- normalizePath(start, winslash = "/", mustWork = FALSE)
    repeat {
      candidate <- if (identical(basename(current), "R")) current else file.path(current, "R")
      if (all(file.exists(file.path(candidate, c("growth_core.R", "growth_parameters.R"))))) {
        source_dirs <- c(source_dirs, candidate)
        break
      }
      parent <- dirname(current)
      if (identical(parent, current)) break
      current <- parent
    }
  }
  source_dirs <- unique(source_dirs)

  if (length(source_dirs)) {
    for (file in c("growth_core.R", "growth_parameters.R")) {
      path <- file.path(source_dirs[[1]], file)
      if (!file.exists(path)) {
        stop(sprintf("Could not load shared BIOSZEN growth file: %s", file), call. = FALSE)
      }
      sys.source(path, envir = target)
    }
  } else if (requireNamespace("BIOSZEN", quietly = TRUE)) {
    core_names <- c(
      ".bioszen_map_wells",
      ".bioszen_cancel_condition",
      ".bioszen_abort_if_requested",
      ".bioszen_identify_exponential_phase_robust",
      ".bioszen_identify_exponential_phase_permissive",
      ".bioszen_growth_initial_od",
      ".bioszen_calculate_growth_rates_robust",
      ".bioszen_calculate_growth_rates_permissive",
      ".bioszen_is_empty_growth_value",
      ".bioszen_combine_growth_results",
      ".bioszen_growth_result_columns",
      ".bioszen_empty_growth_results",
      ".bioszen_emit_growth_results",
      ".bioszen_growth_od0_lookup",
      ".bioszen_fill_restored_od0",
      ".bioszen_compute_growth_results_batch_core",
      ".bioszen_parse_numeric",
      ".bioszen_growth_time_condition",
      ".bioszen_stop_growth_time",
      ".bioszen_normalize_growth_column_name",
      ".bioszen_growth_time_aliases",
      ".bioszen_find_growth_time_column",
      ".bioszen_validate_irregular_time",
      ".bioszen_trim_growth_table",
      ".bioszen_has_growth_measurements",
      ".bioszen_is_growth_metadata_column_name",
      ".bioszen_is_raw_growth_footer",
      ".bioszen_is_irregular_raw_table",
      ".bioszen_normalize_irregular_processed_curves",
      ".bioszen_normalize_irregular_raw_curves",
      ".bioszen_is_index_like_series",
      ".bioszen_is_processed_curves_table",
      ".bioszen_normalize_processed_curves",
      ".bioszen_growth_fixed_plot_parameters",
      ".bioszen_build_curves_sheet"
    )
    ns <- asNamespace("BIOSZEN")
    for (nm in core_names) assign(nm, get(nm, envir = ns), envir = target)
  } else {
    stop("Could not load the shared BIOSZEN growth calculation core.", call. = FALSE)
  }

  # Preserve the historical internal names used by the Shiny app and tests.
  assign("identify_exponential_phase_robust", get(".bioszen_identify_exponential_phase_robust", target), target)
  assign("identify_exponential_phase_permissive", get(".bioszen_identify_exponential_phase_permissive", target), target)
  assign("calculate_growth_rates_robust", get(".bioszen_calculate_growth_rates_robust", target), target)
  assign("calculate_growth_rates_permissive", get(".bioszen_calculate_growth_rates_permissive", target), target)
  assign("combine_growth_results", get(".bioszen_combine_growth_results", target), target)
  assign("is_empty_value", get(".bioszen_is_empty_growth_value", target), target)
  invisible(TRUE)
}

.bioszen_load_growth_core()
rm(.bioszen_load_growth_core)
