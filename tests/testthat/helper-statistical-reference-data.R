bioszen_reference_path <- function(filename) {
  path <- testthat::test_path("reference-data", filename)
  if (!file.exists(path)) {
    stop("Missing statistical reference fixture: ", filename, call. = FALSE)
  }
  path
}

bioszen_read_reference <- function(filename) {
  utils::read.csv(
    bioszen_reference_path(filename),
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = c("", "NA")
  )
}

bioszen_reference_test_df <- function(filename,
                                      group_col = "Media",
                                      value_col = "Value") {
  dat <- bioszen_read_reference(filename)
  stopifnot(group_col %in% names(dat), value_col %in% names(dat))
  data.frame(
    Label = factor(as.character(dat[[group_col]]), levels = unique(as.character(dat[[group_col]]))),
    Valor = suppressWarnings(as.numeric(dat[[value_col]])),
    BiologicalReplicate = as.character(dat$BiologicalReplicate),
    stringsAsFactors = FALSE
  )
}

# The normality guards are local to the Shiny server. Extracting their exact
# function expressions lets focused tests validate production behavior without
# starting a browser or copying the formulas into a second implementation.
bioszen_extract_server_function <- function(function_name,
                                            eval_env = new.env(parent = globalenv())) {
  lines <- readLines(app_test_path("server", "server_main.R"), warn = FALSE, encoding = "UTF-8")
  pattern <- paste0("^[[:space:]]*", function_name, "[[:space:]]*<-[[:space:]]*function[[:space:]]*[(]")
  start <- grep(pattern, lines, perl = TRUE)
  if (length(start) != 1L) {
    stop("Unable to uniquely find server function: ", function_name, call. = FALSE)
  }

  for (end in seq.int(start, length(lines))) {
    parsed <- try(parse(text = paste(lines[start:end], collapse = "\n"), keep.source = FALSE), silent = TRUE)
    if (!inherits(parsed, "try-error") && length(parsed) == 1L) {
      assignment <- parsed[[1L]]
      if (is.call(assignment) && identical(as.character(assignment[[1L]]), "<-")) {
        fun <- eval(assignment[[3L]], envir = eval_env)
        if (is.function(fun)) return(fun)
      }
    }
  }
  stop("Unable to parse server function: ", function_name, call. = FALSE)
}

bioszen_extract_curve_method_function <- function(method_code) {
  skip_if_not_installed("dplyr")
  lines <- readLines(app_test_path("server", "server_main.R"), warn = FALSE, encoding = "UTF-8")
  pattern <- paste0('make_pairwise_rows[(]"', method_code, '"')
  start <- grep(pattern, lines, fixed = FALSE)
  if (length(start) != 1L) {
    stop("Unable to uniquely find curve method: ", method_code, call. = FALSE)
  }
  function_start <- regexpr("function[[:space:]]*[(]", lines[[start]], perl = TRUE)
  if (function_start[[1L]] < 1L) {
    stop("Curve method has no inline function: ", method_code, call. = FALSE)
  }

  eval_env <- new.env(parent = globalenv())
  eval_env$`%>%` <- dplyr::`%>%`
  eval_env$filter <- dplyr::filter
  eval_env$slice <- dplyr::slice

  first_line <- substring(lines[[start]], function_start[[1L]])
  for (end in seq.int(start, length(lines))) {
    block_lines <- c(first_line, if (end > start) lines[(start + 1L):end] else character(0))
    candidate <- paste(block_lines, collapse = "\n")
    candidates <- c(candidate, sub("[)][[:space:]]*$", "", candidate, perl = TRUE))
    for (text in unique(candidates)) {
      parsed <- try(parse(text = text, keep.source = FALSE), silent = TRUE)
      if (!inherits(parsed, "try-error") && length(parsed) == 1L) {
        fun <- eval(parsed[[1L]], envir = eval_env)
        if (is.function(fun)) return(fun)
      }
    }
  }
  stop("Unable to parse curve method implementation: ", method_code, call. = FALSE)
}

bioszen_curve_long_reference <- function() {
  wide <- bioszen_read_reference("curves_reference.csv")
  series <- setdiff(names(wide), "Time")
  rows <- lapply(series, function(series_name) {
    parts <- strsplit(series_name, "_", fixed = TRUE)[[1L]]
    data.frame(
      Time = wide$Time,
      Label = parts[[1L]],
      BiologicalReplicate = parts[[2L]],
      Value = as.numeric(wide[[series_name]]),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

bioszen_curve_summary_reference <- function(curve_long = bioszen_curve_long_reference()) {
  split_rows <- split(curve_long, list(curve_long$Label, curve_long$Time), drop = TRUE)
  rows <- lapply(split_rows, function(dat) {
    data.frame(
      Label = dat$Label[[1L]],
      Time = dat$Time[[1L]],
      Avg = mean(dat$Value),
      SD = stats::sd(dat$Value),
      N = length(dat$Value),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out[order(out$Label, out$Time), , drop = FALSE]
}

bioszen_trapezoid_auc <- function(x, y) {
  order_index <- order(x)
  x <- as.numeric(x[order_index])
  y <- as.numeric(y[order_index])
  sum(diff(x) * (y[-length(y)] + y[-1L]) / 2)
}

bioszen_reference_format_p <- function(p, digits = 4L, scientific_below = 1e-4) {
  p <- as.numeric(p)
  if (p == 0) {
    return(paste0("<", formatC(.Machine$double.eps, format = "e", digits = 2L)))
  }
  if (p > 0 && p < scientific_below) {
    return(formatC(p, format = "e", digits = max(1L, digits - 1L)))
  }
  formatC(p, format = "f", digits = digits)
}
