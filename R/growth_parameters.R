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
  nondecreasing <- if (length(time_vals) > 1L) mean(diff(time_vals) >= 0) else 1
  if (!is.finite(nondecreasing) || nondecreasing < 0.8) return(FALSE)

  if (ncol(df) >= 2) {
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
  for (nm in names(df)[-1]) df[[nm]] <- .bioszen_parse_numeric(df[[nm]])
  df <- df[is.finite(df$Time), , drop = FALSE]
  if (!nrow(df) || ncol(df) < 2) {
    stop("Processed curves data must contain a valid Time column and at least one well.", call. = FALSE)
  }
  df
}

.bioszen_growth_fixed_plot_parameters <- function() {
  data.frame(
    X_Max = 50,
    Interval_X = 10,
    Y_Max = 1.5,
    Interval_Y = 0.5,
    X_Title = "Tiempo (h)",
    Y_Title = "OD620",
    stringsAsFactors = FALSE
  )
}

.bioszen_build_curves_sheet <- function(
    file_path,
    max_time,
    time_interval,
    sheet = NULL) {
  selected_sheet <- if (is.null(sheet)) 1 else sheet
  first_sheet <- readxl::read_excel(
    file_path,
    sheet = selected_sheet,
    .name_repair = "minimal"
  )
  fixed_params <- .bioszen_growth_fixed_plot_parameters()

  if (.bioszen_is_processed_curves_table(first_sheet)) {
    new_data <- .bioszen_normalize_processed_curves(first_sheet)
    return(list(new_data = new_data, fixed_params = fixed_params, format = "processed"))
  }

  raw <- readxl::read_excel(
    file_path,
    sheet = selected_sheet,
    skip = 2,
    .name_repair = "minimal"
  )
  Time <- seq(0, max_time, by = time_interval)
  raw <- raw[seq_len(min(length(Time), nrow(raw))), , drop = FALSE]
  if (ncol(raw) < 3) {
    stop("Raw curves data does not contain the expected measurement columns.", call. = FALSE)
  }
  meas <- as.data.frame(
    raw[, -c(1, 2), drop = FALSE],
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  for (nm in names(meas)) meas[[nm]] <- .bioszen_parse_numeric(meas[[nm]])
  new_data <- data.frame(
    Time = Time[seq_len(nrow(raw))],
    meas,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  list(new_data = new_data, fixed_params = fixed_params, format = "raw")
}

.bioszen_growth_tidy_input <- function(input) {
  input <- as.data.frame(input, check.names = FALSE, stringsAsFactors = FALSE)
  if (!nrow(input) || !ncol(input)) {
    stop("Growth input is empty.", call. = FALSE)
  }

  tidy_cols <- c("Well", "Time", "Measurements")
  if (all(tidy_cols %in% names(input))) {
    out <- input[, tidy_cols, drop = FALSE]
    out$Well <- as.character(out$Well)
    out$Time <- .bioszen_parse_numeric(out$Time)
    out$Measurements <- .bioszen_parse_numeric(out$Measurements)
  } else {
    wide <- .bioszen_normalize_processed_curves(input)
    out <- gcplyr::trans_wide_to_tidy(wide, id_cols = "Time")
    out <- as.data.frame(out, check.names = FALSE, stringsAsFactors = FALSE)
    if (!all(tidy_cols %in% names(out))) {
      stop("Could not convert the growth table to Well, Time, and Measurements columns.", call. = FALSE)
    }
    out <- out[, tidy_cols, drop = FALSE]
  }

  out <- out[!is.na(out$Well) & nzchar(trimws(out$Well)), , drop = FALSE]
  if (!nrow(out) || !any(is.finite(out$Time)) || !any(is.finite(out$Measurements))) {
    stop("Growth input does not contain usable numeric measurements.", call. = FALSE)
  }
  out
}

.bioszen_read_growth_input <- function(
    path,
    sheet = NULL,
    max_time = 48,
    time_interval = 0.5) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx", "xls")) {
    prepared <- .bioszen_build_curves_sheet(
      path,
      max_time = max_time,
      time_interval = time_interval,
      sheet = sheet
    )
    return(list(
      tidy = .bioszen_growth_tidy_input(prepared$new_data),
      format = prepared$format
    ))
  }
  if (ext == "csv") {
    if (!is.null(sheet)) {
      warning("The sheet argument is ignored for CSV input.", call. = FALSE)
    }
    sample_lines <- readLines(path, n = 5L, warn = FALSE, encoding = "UTF-8")
    delimiter_counts <- vapply(c(",", ";", "\t"), function(delimiter) {
      sum(lengths(regmatches(sample_lines, gregexpr(delimiter, sample_lines, fixed = TRUE))))
    }, integer(1))
    delimiter <- names(which.max(delimiter_counts))[[1]]
    raw <- readr::read_delim(
      path,
      delim = delimiter,
      show_col_types = FALSE,
      name_repair = "minimal"
    )
    return(list(tidy = .bioszen_growth_tidy_input(raw), format = "csv"))
  }
  stop(
    "Unsupported growth input format. Use a data frame or an .xlsx, .xls, or .csv file.",
    call. = FALSE
  )
}

.bioszen_growth_output_directory <- function(output_dir) {
  if (is.null(output_dir)) return(NULL)
  if (!is.character(output_dir) || length(output_dir) != 1L || is.na(output_dir) ||
      !nzchar(trimws(output_dir))) {
    stop("output_dir must be NULL or one non-empty directory path.", call. = FALSE)
  }
  output_dir <- path.expand(trimws(output_dir))
  if (file.exists(output_dir) && !dir.exists(output_dir)) {
    stop("output_dir points to a file instead of a directory.", call. = FALSE)
  }
  if (!dir.exists(output_dir) && !dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)) {
    stop("Could not create output_dir.", call. = FALSE)
  }
  normalizePath(output_dir, winslash = "/", mustWork = TRUE)
}

.bioszen_growth_safe_stem <- function(path, index = 1L) {
  stem <- if (is.null(path)) paste0("data_", index) else tools::file_path_sans_ext(basename(path))
  stem <- gsub("[^A-Za-z0-9._-]+", "_", stem)
  stem <- gsub("^_+|_+$", "", stem)
  if (!nzchar(stem)) paste0("data_", index) else stem
}

.bioszen_public_growth_results <- function(results) {
  out <- as.data.frame(results, check.names = FALSE, stringsAsFactors = FALSE)
  out <- out[, .bioszen_growth_result_columns, drop = FALSE]
  class(out) <- c("bioszen_growth_parameters", class(out))
  out
}

.bioszen_legacy_growth_results <- function(results) {
  out <- as.data.frame(results, check.names = FALSE, stringsAsFactors = FALSE)
  out[, .bioszen_growth_result_columns, drop = FALSE]
}

.bioszen_write_growth_results <- function(results, file, overwrite) {
  if (file.exists(file) && !isTRUE(overwrite)) {
    stop(sprintf("Output file already exists: %s", basename(file)), call. = FALSE)
  }
  openxlsx::write.xlsx(
    .bioszen_legacy_growth_results(results),
    file,
    sheetName = "Resultados Combinados",
    colNames = TRUE,
    rowNames = FALSE,
    overwrite = TRUE
  )
  normalizePath(file, winslash = "/", mustWork = TRUE)
}

#' Calculate BIOSZEN growth parameters
#'
#' Calculates microbial growth parameters with the same combined method used by
#' the BIOSZEN Shiny application. The robust exponential-phase detector is run
#' first; the permissive detector fills only values that the robust method could
#' not calculate.
#'
#' @param input A data frame, a supported growth file (`.xlsx`, `.xls`, or
#'   `.csv`), a vector of supported files, or a directory containing supported
#'   files. Data frames may be wide (`Time` plus well columns) or tidy (`Well`,
#'   `Time`, and `Measurements`).
#' @param output_dir Optional directory for generated parameter workbooks. No
#'   files are written when this is `NULL`. A missing directory is created.
#' @param sheet Optional Excel sheet name or index. The first sheet is used by
#'   default. Ignored for data frames and CSV files.
#' @param overwrite Whether existing result workbooks may be replaced.
#' @param max_time Maximum time used when converting the raw reader-export
#'   format supported by the Shiny growth module.
#' @param time_interval Time interval used when converting the raw
#'   reader-export format supported by the Shiny growth module.
#'
#' @return For one input, the same parameter table produced by the Shiny growth
#'   workflow, with class
#'   `bioszen_growth_parameters`. For multiple files, a named list with class
#'   `bioszen_growth_parameters_list`. Generated file paths are stored in the
#'   `output_file` attribute of each result.
#'
#' @examples
#' time <- seq(0, 12, by = 0.5)
#' curves <- data.frame(
#'   Time = time,
#'   A1 = 0.05 + 0.9 / (1 + exp(-0.8 * (time - 5)))
#' )
#' result <- growth_parameters(curves)
#'
#' @export
growth_parameters <- function(
    input,
    output_dir = NULL,
    sheet = NULL,
    overwrite = FALSE,
    max_time = 48,
    time_interval = 0.5) {
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    stop("overwrite must be TRUE or FALSE.", call. = FALSE)
  }
  max_time <- suppressWarnings(as.numeric(max_time))
  time_interval <- suppressWarnings(as.numeric(time_interval))
  if (length(max_time) != 1L || !is.finite(max_time) || max_time < 0) {
    stop("max_time must be one finite, non-negative number.", call. = FALSE)
  }
  if (length(time_interval) != 1L || !is.finite(time_interval) || time_interval <= 0) {
    stop("time_interval must be one finite number greater than zero.", call. = FALSE)
  }
  output_dir <- .bioszen_growth_output_directory(output_dir)

  if (is.data.frame(input)) {
    inputs <- list(list(path = NULL, data = input, label = "data_1", format = "data.frame"))
  } else {
    if (!is.character(input) || !length(input) || anyNA(input)) {
      stop("input must be a data frame, file path, vector of file paths, or directory.", call. = FALSE)
    }
    paths <- path.expand(input)
    if (length(paths) == 1L && dir.exists(paths)) {
      paths <- list.files(paths, pattern = "\\.(xlsx|xls|csv)$", full.names = TRUE, ignore.case = TRUE)
      paths <- paths[!grepl("^~\\$", basename(paths))]
      if (!length(paths)) stop("The input directory contains no supported growth files.", call. = FALSE)
    }
    missing <- paths[!file.exists(paths) | dir.exists(paths)]
    if (length(missing)) {
      stop(sprintf("Growth input file does not exist: %s", basename(missing[[1]])), call. = FALSE)
    }
    paths <- normalizePath(paths, winslash = "/", mustWork = TRUE)
    labels <- make.unique(vapply(seq_along(paths), function(i) {
      .bioszen_growth_safe_stem(paths[[i]], i)
    }, character(1)), sep = "_")
    inputs <- lapply(seq_along(paths), function(i) {
      list(path = paths[[i]], data = NULL, label = labels[[i]], format = NULL)
    })
  }

  if (!is.null(output_dir)) {
    for (i in seq_along(inputs)) {
      inputs[[i]]$output_file <- file.path(output_dir, paste0("Parameters_", inputs[[i]]$label, ".xlsx"))
    }
    existing <- vapply(inputs, function(item) file.exists(item$output_file), logical(1))
    if (any(existing) && !isTRUE(overwrite)) {
      stop(sprintf("Output file already exists: %s", basename(inputs[[which(existing)[[1]]]]$output_file)), call. = FALSE)
    }
  }

  calculate_one <- function(item) {
    if (is.null(item$path)) {
      tidy <- .bioszen_growth_tidy_input(item$data)
      input_format <- item$format
    } else {
      prepared <- .bioszen_read_growth_input(
        item$path,
        sheet = sheet,
        max_time = max_time,
        time_interval = time_interval
      )
      tidy <- prepared$tidy
      input_format <- prepared$format
    }

    legacy <- .bioszen_compute_growth_results_batch_core(tidy)
    result <- .bioszen_public_growth_results(legacy)
    attr(result, "method") <- "robust_with_permissive_fallback"
    attr(result, "input_format") <- input_format
    attr(result, "source") <- if (is.null(item$path)) "data.frame" else basename(item$path)

    output_file <- NULL
    if (!is.null(output_dir)) {
      output_file <- item$output_file
      output_file <- .bioszen_write_growth_results(result, output_file, overwrite)
    }
    attr(result, "output_file") <- output_file
    result
  }

  results <- lapply(inputs, calculate_one)
  if (length(results) == 1L) return(results[[1]])
  names(results) <- vapply(inputs, `[[`, character(1), "label")
  class(results) <- c("bioszen_growth_parameters_list", "list")
  results
}
