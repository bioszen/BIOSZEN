.bioszen_parse_numeric <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  y <- trimws(as.character(x))
  y[y == ""] <- NA_character_
  y <- gsub(",", ".", y, fixed = TRUE)
  suppressWarnings(as.numeric(y))
}

.bioszen_growth_time_condition <- function(code, message, ...) {
  structure(
    c(list(message = message, call = NULL, code = code), list(...)),
    class = c("bioszen_growth_time_error", "error", "condition")
  )
}

.bioszen_stop_growth_time <- function(code, message, ...) {
  stop(.bioszen_growth_time_condition(code, message, ...))
}

.bioszen_normalize_growth_column_name <- function(x) {
  x <- as.character(x)
  ascii <- suppressWarnings(iconv(x, from = "", to = "ASCII//TRANSLIT", sub = ""))
  ascii[is.na(ascii)] <- x[is.na(ascii)]
  tolower(gsub("[^[:alnum:]]+", "", ascii))
}

.bioszen_growth_time_aliases <- function() {
  .bioszen_normalize_growth_column_name(c(
    "Time", "Times", "Time h", "Time hr", "Time hours", "Time min",
    "Time minutes", "Time sec", "Time seconds", "Time point",
    "Elapsed time", "Elapsed hours", "Elapsed minutes", "Elapsed seconds",
    "Measurement time", "Measurement hour", "Measurement hours",
    "Tiempo", "Tiempos", "Tiempo h", "Tiempo transcurrido", "Tiempo medicion",
    "Tiempo de medicion", "Hour", "Hours", "Hora", "Horas", "Hora medicion",
    "Horas medicion", "Hora de medicion", "Horas de medicion",
    "Minute", "Minutes", "Minuto", "Minutos", "Second", "Seconds",
    "Segundo", "Segundos"
  ))
}

.bioszen_find_growth_time_column <- function(df, time_column = NULL, required = TRUE) {
  column_names <- names(df)
  normalized_names <- .bioszen_normalize_growth_column_name(column_names)
  requested <- if (is.null(time_column) || !length(time_column) || is.na(time_column[[1]])) {
    ""
  } else {
    trimws(as.character(time_column[[1]]))
  }

  if (nzchar(requested)) {
    exact_matches <- which(column_names == requested)
    if (length(exact_matches) == 1L) return(exact_matches[[1]])
    case_matches <- which(tolower(trimws(column_names)) == tolower(requested))
    if (length(case_matches) == 1L) return(case_matches[[1]])
    requested_key <- .bioszen_normalize_growth_column_name(requested)
    matches <- which(normalized_names == requested_key)
  } else {
    matches <- which(normalized_names %in% .bioszen_growth_time_aliases())
  }

  if (length(matches) == 1L) return(matches[[1]])
  if (!isTRUE(required) && !length(matches)) return(integer(0))

  available <- column_names[!is.na(column_names) & nzchar(trimws(column_names))]
  available_text <- if (length(available)) paste(available, collapse = ", ") else "(none)"
  if (length(matches) > 1L) {
    .bioszen_stop_growth_time(
      "ambiguous_column",
      "More than one possible time column was found. Enter the time-column name explicitly.",
      available = available_text
    )
  }
  if (nzchar(requested)) {
    .bioszen_stop_growth_time(
      "column_not_found",
      sprintf("The time column '%s' was not found. Available columns: %s", requested, available_text),
      requested = requested,
      available = available_text
    )
  }
  .bioszen_stop_growth_time(
    "column_not_detected",
    sprintf(
      "No time column was detected. Enter its name explicitly. Available columns: %s",
      available_text
    ),
    available = available_text
  )
}

.bioszen_validate_irregular_time <- function(x, column_name = "Time", groups = NULL) {
  text <- trimws(as.character(x))
  missing <- is.na(x) | is.na(text) | !nzchar(text)
  if (!length(x) || any(missing)) {
    .bioszen_stop_growth_time(
      "missing_values",
      sprintf("Time column '%s' contains missing values.", column_name),
      column = column_name
    )
  }

  values <- .bioszen_parse_numeric(x)
  if (any(!is.finite(values))) {
    .bioszen_stop_growth_time(
      "nonnumeric_values",
      sprintf("Time column '%s' must contain only finite numeric values.", column_name),
      column = column_name
    )
  }

  validate_sequence <- function(sequence) {
    if (anyDuplicated(sequence)) {
      .bioszen_stop_growth_time(
        "duplicated_values",
        sprintf("Time column '%s' contains duplicated values.", column_name),
        column = column_name
      )
    }
    if (length(sequence) > 1L && any(diff(sequence) <= 0)) {
      .bioszen_stop_growth_time(
        "non_increasing_values",
        sprintf("Time column '%s' must be strictly increasing.", column_name),
        column = column_name
      )
    }
  }

  if (is.null(groups)) {
    validate_sequence(values)
  } else {
    group_values <- as.character(groups)
    for (group in unique(group_values)) {
      validate_sequence(values[group_values == group])
    }
  }
  values
}

.bioszen_trim_growth_table <- function(df) {
  df <- as.data.frame(df, check.names = FALSE, stringsAsFactors = FALSE)
  if (!nrow(df) || !ncol(df)) return(df)
  nonempty <- lapply(df, function(column) {
    text <- trimws(as.character(column))
    !is.na(column) & !is.na(text) & nzchar(text)
  })
  keep_cols <- vapply(nonempty, any, logical(1))
  df <- df[, keep_cols, drop = FALSE]
  if (!ncol(df)) return(df)
  keep_rows <- Reduce(`|`, nonempty[keep_cols])
  df[keep_rows, , drop = FALSE]
}

.bioszen_has_growth_measurements <- function(df, time_index) {
  measurement_indices <- setdiff(seq_len(ncol(df)), time_index)
  if (!length(measurement_indices)) return(FALSE)
  numeric_fraction <- vapply(df[measurement_indices], function(column) {
    mean(is.finite(.bioszen_parse_numeric(column)))
  }, numeric(1))
  any(is.finite(numeric_fraction) & numeric_fraction >= 0.6)
}

.bioszen_is_growth_metadata_column_name <- function(x) {
  key <- .bioszen_normalize_growth_column_name(x)
  grepl(
    "^(temp|temperature|temperatura|cycle|ciclo|index|indice|ignore|row|fila|record|registro|reading|lectura|number|numero|id)",
    key
  )
}

.bioszen_is_raw_growth_footer <- function(x) {
  key <- .bioszen_normalize_growth_column_name(x)
  grepl(
    "^(fechadelamedicion|measurementdate|sunrise|numerodeseriedelinstrumento|instrumentserial|mododemedicion|measurementmode|longituddeonda|wavelength|modolectura|readingmode|numerodeciclos|numberofkineticcycles|intervalocinetico|kineticinterval|rangovalidodetemperaturas|validtemperaturerange|unidad|unit|tiempototal|totalkineticruntime)",
    key
  )
}

.bioszen_is_irregular_raw_table <- function(df, time_index) {
  if (ncol(df) < 3L) return(FALSE)
  leading <- seq_len(min(2L, ncol(df)))
  other_leading <- setdiff(leading, time_index)
  if (!length(other_leading)) return(FALSE)
  if (time_index > 2L) {
    return(all(
      vapply(df[leading], .bioszen_is_index_like_series, logical(1)) |
        .bioszen_is_growth_metadata_column_name(names(df)[leading])
    ))
  }
  any(
    vapply(df[other_leading], .bioszen_is_index_like_series, logical(1)) |
      .bioszen_is_growth_metadata_column_name(names(df)[other_leading])
  )
}

.bioszen_normalize_irregular_processed_curves <- function(df, time_column = NULL) {
  df <- .bioszen_trim_growth_table(df)
  if (!nrow(df) || ncol(df) < 2L) {
    stop("Processed curves data must contain a time column and at least one well.", call. = FALSE)
  }
  time_index <- .bioszen_find_growth_time_column(df, time_column = time_column)
  selected_name <- names(df)[[time_index]]
  time_values <- .bioszen_validate_irregular_time(df[[time_index]], selected_name)
  measurement_indices <- setdiff(seq_len(ncol(df)), time_index)
  measurements <- df[, measurement_indices, drop = FALSE]
  for (nm in names(measurements)) measurements[[nm]] <- .bioszen_parse_numeric(measurements[[nm]])
  new_data <- data.frame(
    Time = time_values,
    measurements,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  list(data = new_data, time_column = selected_name)
}

.bioszen_normalize_irregular_raw_curves <- function(df, time_column = NULL) {
  df <- .bioszen_trim_growth_table(df)
  if (!nrow(df) || ncol(df) < 3L) {
    stop("Raw curves data does not contain the expected time and measurement columns.", call. = FALSE)
  }
  time_index <- .bioszen_find_growth_time_column(df, time_column = time_column)
  selected_name <- names(df)[[time_index]]
  footer_rows <- which(.bioszen_is_raw_growth_footer(df[[time_index]]))
  if (length(footer_rows)) {
    first_footer <- min(footer_rows)
    if (first_footer <= 1L) {
      stop("Raw curves data does not contain numeric time rows before its metadata footer.", call. = FALSE)
    }
    df <- df[seq_len(first_footer - 1L), , drop = FALSE]
  }
  time_values <- .bioszen_validate_irregular_time(df[[time_index]], selected_name)
  measurement_indices <- setdiff(seq_len(ncol(df)), unique(c(1L, 2L, time_index)))
  if (!length(measurement_indices)) {
    stop("Raw curves data does not contain the expected measurement columns.", call. = FALSE)
  }
  measurements <- as.data.frame(
    df[, measurement_indices, drop = FALSE],
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  for (nm in names(measurements)) measurements[[nm]] <- .bioszen_parse_numeric(measurements[[nm]])
  new_data <- data.frame(
    Time = time_values,
    measurements,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  list(data = new_data, time_column = selected_name)
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
    sheet = NULL,
    time_mode = c("fixed", "irregular"),
    time_column = NULL) {
  time_mode <- match.arg(time_mode)
  selected_sheet <- if (is.null(sheet)) 1 else sheet
  first_sheet <- readxl::read_excel(
    file_path,
    sheet = selected_sheet,
    .name_repair = "minimal"
  )
  fixed_params <- .bioszen_growth_fixed_plot_parameters()

  if (identical(time_mode, "fixed")) {
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
    return(list(new_data = new_data, fixed_params = fixed_params, format = "raw"))
  }

  first_sheet <- .bioszen_trim_growth_table(first_sheet)
  time_index <- .bioszen_find_growth_time_column(
    first_sheet,
    time_column = time_column,
    required = FALSE
  )
  first_sheet_is_raw <- length(time_index) == 1L &&
    .bioszen_is_irregular_raw_table(first_sheet, time_index)
  if (isTRUE(first_sheet_is_raw)) {
    prepared <- .bioszen_normalize_irregular_raw_curves(
      first_sheet,
      time_column = names(first_sheet)[[time_index]]
    )
    return(list(
      new_data = prepared$data,
      fixed_params = fixed_params,
      format = "raw",
      time_column = prepared$time_column
    ))
  }
  if (length(time_index) == 1L && !isTRUE(first_sheet_is_raw) &&
      (.bioszen_is_processed_curves_table(first_sheet) ||
       .bioszen_has_growth_measurements(first_sheet, time_index))) {
    prepared <- .bioszen_normalize_irregular_processed_curves(
      first_sheet,
      time_column = names(first_sheet)[[time_index]]
    )
    return(list(
      new_data = prepared$data,
      fixed_params = fixed_params,
      format = "processed",
      time_column = prepared$time_column
    ))
  }

  raw <- readxl::read_excel(
    file_path,
    sheet = selected_sheet,
    skip = 2,
    .name_repair = "minimal"
  )
  prepared <- .bioszen_normalize_irregular_raw_curves(raw, time_column = time_column)
  list(
    new_data = prepared$data,
    fixed_params = fixed_params,
    format = "raw",
    time_column = prepared$time_column
  )
}

.bioszen_growth_tidy_input <- function(input,
                                       time_mode = c("fixed", "irregular"),
                                       time_column = NULL) {
  time_mode <- match.arg(time_mode)
  input <- as.data.frame(input, check.names = FALSE, stringsAsFactors = FALSE)
  if (!nrow(input) || !ncol(input)) {
    stop("Growth input is empty.", call. = FALSE)
  }

  tidy_cols <- c("Well", "Time", "Measurements")
  validation_name <- "Time"
  irregular_tidy <- identical(time_mode, "irregular") &&
    all(c("Well", "Measurements") %in% names(input))
  if (isTRUE(irregular_tidy)) {
    time_index <- .bioszen_find_growth_time_column(input, time_column = time_column)
    selected_name <- names(input)[[time_index]]
    validation_name <- selected_name
    out <- data.frame(
      Well = as.character(input$Well),
      Time = input[[time_index]],
      Measurements = input$Measurements,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    out$Measurements <- .bioszen_parse_numeric(out$Measurements)
  } else if (all(tidy_cols %in% names(input))) {
    out <- input[, tidy_cols, drop = FALSE]
    out$Well <- as.character(out$Well)
    out$Time <- .bioszen_parse_numeric(out$Time)
    out$Measurements <- .bioszen_parse_numeric(out$Measurements)
  } else {
    if (identical(time_mode, "irregular")) {
      prepared <- .bioszen_normalize_irregular_processed_curves(input, time_column = time_column)
      wide <- prepared$data
    } else {
      wide <- .bioszen_normalize_processed_curves(input)
    }
    out <- gcplyr::trans_wide_to_tidy(wide, id_cols = "Time")
    out <- as.data.frame(out, check.names = FALSE, stringsAsFactors = FALSE)
    if (!all(tidy_cols %in% names(out))) {
      stop("Could not convert the growth table to Well, Time, and Measurements columns.", call. = FALSE)
    }
    out <- out[, tidy_cols, drop = FALSE]
  }

  out <- out[!is.na(out$Well) & nzchar(trimws(out$Well)), , drop = FALSE]
  if (identical(time_mode, "irregular")) {
    out$Time <- .bioszen_validate_irregular_time(
      out$Time,
      column_name = validation_name,
      groups = out$Well
    )
  }
  if (!nrow(out) || !any(is.finite(out$Time)) || !any(is.finite(out$Measurements))) {
    stop("Growth input does not contain usable numeric measurements.", call. = FALSE)
  }
  out
}

.bioszen_read_growth_input <- function(
    path,
    sheet = NULL,
    max_time = 48,
    time_interval = 0.5,
    time_mode = c("fixed", "irregular"),
    time_column = NULL) {
  time_mode <- match.arg(time_mode)
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx", "xls")) {
    prepared <- .bioszen_build_curves_sheet(
      path,
      max_time = max_time,
      time_interval = time_interval,
      sheet = sheet,
      time_mode = time_mode,
      time_column = time_column
    )
    return(list(
      tidy = .bioszen_growth_tidy_input(
        prepared$new_data,
        time_mode = time_mode,
        time_column = if (identical(time_mode, "irregular")) "Time" else NULL
      ),
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
    return(list(
      tidy = .bioszen_growth_tidy_input(
        raw,
        time_mode = time_mode,
        time_column = time_column
      ),
      format = "csv"
    ))
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
#' @param time_mode Time handling mode. `"fixed"` preserves the historical
#'   maximum-time and interval conversion. `"irregular"` reads numeric time
#'   values from the input file.
#' @param time_column Optional time-column name for irregular mode. When `NULL`
#'   or blank, common English and Spanish names are detected automatically.
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
    time_interval = 0.5,
    time_mode = c("fixed", "irregular"),
    time_column = NULL) {
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    stop("overwrite must be TRUE or FALSE.", call. = FALSE)
  }
  time_mode <- match.arg(time_mode)
  if (identical(time_mode, "fixed")) {
    max_time <- suppressWarnings(as.numeric(max_time))
    time_interval <- suppressWarnings(as.numeric(time_interval))
    if (length(max_time) != 1L || !is.finite(max_time) || max_time < 0) {
      stop("max_time must be one finite, non-negative number.", call. = FALSE)
    }
    if (length(time_interval) != 1L || !is.finite(time_interval) || time_interval <= 0) {
      stop("time_interval must be one finite number greater than zero.", call. = FALSE)
    }
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
      tidy <- .bioszen_growth_tidy_input(
        item$data,
        time_mode = time_mode,
        time_column = time_column
      )
      input_format <- item$format
    } else {
      prepared <- .bioszen_read_growth_input(
        item$path,
        sheet = sheet,
        max_time = max_time,
        time_interval = time_interval,
        time_mode = time_mode,
        time_column = time_column
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

#' Calculate BIOSZEN growth parameters from irregular time points
#'
#' A convenience variant of [growth_parameters()] for curve tables whose
#' recorded time points are uneven or discontinuous. Numeric time values are
#' read directly from the selected column and must be finite, unique, and
#' strictly increasing within each well.
#'
#' @param input A data frame, a supported growth file (`.xlsx`, `.xls`, or
#'   `.csv`), a vector of supported files, or a directory containing supported
#'   files. Data frames may be wide or tidy.
#' @param output_dir Optional directory for generated parameter workbooks. No
#'   files are written when this is `NULL`.
#' @param sheet Optional Excel sheet name or index. The first sheet is used by
#'   default. Ignored for data frames and CSV files.
#' @param overwrite Whether existing result workbooks may be replaced.
#' @param time_column Optional time-column name. When `NULL` or blank, common
#'   English and Spanish names such as `Time`, `Tiempo`, `Hour`, and `Hora` are
#'   detected automatically.
#'
#' @return The same result type and parameter columns as [growth_parameters()].
#'
#' @examples
#' irregular <- data.frame(
#'   Time = c(0, 0.08, 0.17, 0.5, 1, 2),
#'   A1 = c(0.05, 0.051, 0.053, 0.06, 0.08, 0.16)
#' )
#' result <- growth_parameters_irregular(irregular)
#'
#' @seealso [growth_parameters()]
#' @export
growth_parameters_irregular <- function(
    input,
    output_dir = NULL,
    sheet = NULL,
    overwrite = FALSE,
    time_column = NULL) {
  growth_parameters(
    input = input,
    output_dir = output_dir,
    sheet = sheet,
    overwrite = overwrite,
    time_mode = "irregular",
    time_column = time_column
  )
}
