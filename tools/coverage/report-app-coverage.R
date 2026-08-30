#!/usr/bin/env Rscript

# Generate line coverage for the Shiny application source without changing it.
# The script intentionally writes only derived summaries and line locations; it
# does not create an HTML source-code report or send results to an external
# coverage service.

options(
  warn = 1,
  keep.source = TRUE,
  keep.parse.data = TRUE,
  testthat.progress.max_fails = Inf,
  shinytest2.timeout = 240000,
  shinytest2.load_timeout = 240000
)

Sys.setenv(
  NOT_CRAN = "true",
  CI = "true",
  RGL_USE_NULL = "true",
  SHINYTEST2_TIMEOUT = "240000",
  SHINYTEST2_LOAD_TIMEOUT = "240000"
)

script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_argument)) {
  script_file <- sub("^--file=", "", script_argument[[1]])
  script_dir <- dirname(normalizePath(script_file, winslash = "/", mustWork = TRUE))
  project_root <- normalizePath(
    file.path(script_dir, "..", ".."),
    winslash = "/",
    mustWork = TRUE
  )
} else {
  project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

if (!file.exists(file.path(project_root, "DESCRIPTION")) ||
    !dir.exists(file.path(project_root, "inst", "app"))) {
  stop("Run this script from the BIOSZEN checkout or keep it under tools/coverage.", call. = FALSE)
}

setwd(project_root)

arguments <- commandArgs(trailingOnly = TRUE)
output_argument <- if (length(arguments)) arguments[[1]] else "coverage-report"
output_dir <- if (grepl("^(?:[A-Za-z]:)?[/\\\\]", output_argument, perl = TRUE)) {
  output_argument
} else {
  file.path(project_root, output_argument)
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)

required_packages <- c("covr", "testthat")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop(
    "Missing required package(s): ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

app_dir <- file.path("inst", "app")
root_sources <- file.path(app_dir, c("config.R", "global.R", "helpers.R"))
module_directories <- c("params", "stats", "graficos", "server", "ui")
module_sources <- unlist(lapply(module_directories, function(directory) {
  sort(list.files(
    file.path(app_dir, directory),
    pattern = "[.]R$",
    full.names = TRUE
  ))
}), use.names = FALSE)

# app.R is intentionally last: this mirrors the production loader while keeping
# each module's original source reference available to covr.
app_sources <- c(root_sources, module_sources, file.path(app_dir, "app.R"))
app_sources <- gsub("\\\\", "/", app_sources)

missing_sources <- app_sources[!file.exists(app_sources)]
if (length(missing_sources)) {
  stop(
    "Expected application source file(s) were not found: ",
    paste(missing_sources, collapse = ", "),
    call. = FALSE
  )
}

driver_file <- tempfile("bioszen-coverage-tests-", fileext = ".R")
on.exit(unlink(driver_file), add = TRUE)
writeLines(c(
  "options(",
  "  testthat.progress.max_fails = Inf,",
  "  shinytest2.timeout = 240000,",
  "  shinytest2.load_timeout = 240000",
  ")",
  "testthat::test_dir(",
  "  path = file.path(getwd(), 'tests', 'testthat'),",
  "  reporter = 'summary',",
  "  env = environment(),",
  "  stop_on_failure = TRUE,",
  "  stop_on_warning = FALSE",
  ")"
), con = driver_file, useBytes = TRUE)

generated_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
run_information <- c(
  paste0("Generated: ", generated_at),
  paste0("R: ", R.version.string),
  paste0("covr: ", as.character(utils::packageVersion("covr"))),
  paste0("Platform: ", R.version$platform),
  paste0("Application source files: ", length(app_sources)),
  "Coverage scope: inst/app/**/*.R",
  "Coverage service upload: disabled"
)
writeLines(run_information, file.path(output_dir, "coverage-run-info.txt"), useBytes = TRUE)

coverage <- tryCatch(
  covr::file_coverage(
    source_files = app_sources,
    test_files = driver_file,
    parent_env = globalenv()
  ),
  error = function(error) {
    error_message <- conditionMessage(error)
    writeLines(error_message, file.path(output_dir, "coverage-error.txt"), useBytes = TRUE)
    writeLines(c(
      "# BIOSZEN application test coverage",
      "",
      "Coverage could not be calculated because the existing test suite or app loading stopped with an error.",
      "",
      paste0("Error: `", gsub("`", "'", error_message, fixed = TRUE), "`"),
      "",
      "No application source file was changed. Inspect the workflow log for the failing test."
    ), file.path(output_dir, "coverage-summary.md"), useBytes = TRUE)
    stop(error)
  }
)

to_repository_path <- function(path) {
  path <- gsub("\\\\", "/", as.character(path))
  root <- paste0(gsub("\\\\", "/", project_root), "/")
  is_absolute_in_root <- startsWith(tolower(path), tolower(root))
  path[is_absolute_in_root] <- substring(path[is_absolute_in_root], nchar(root) + 1L)
  sub("^[.]/", "", path)
}

section_for_path <- function(path) {
  path <- to_repository_path(path)
  if (identical(path, "inst/app/app.R")) return("App bootstrap")
  if (path %in% c("inst/app/config.R", "inst/app/global.R", "inst/app/helpers.R")) {
    return("Shared app code")
  }
  if (startsWith(path, "inst/app/params/")) return("Parameters")
  if (startsWith(path, "inst/app/stats/")) return("Statistics")
  if (startsWith(path, "inst/app/graficos/")) return("Graphics")
  if (startsWith(path, "inst/app/server/")) return("Server")
  if (startsWith(path, "inst/app/ui/")) return("User interface")
  "Other app code"
}

line_coverage <- covr::tally_coverage(coverage, by = "line")
if (is.null(line_coverage) || !nrow(line_coverage)) {
  stop("covr returned no instrumented application lines.", call. = FALSE)
}
line_coverage$filename <- to_repository_path(line_coverage$filename)
line_coverage <- line_coverage[
  startsWith(line_coverage$filename, "inst/app/"),
  ,
  drop = FALSE
]
if (!nrow(line_coverage)) {
  stop("covr returned no lines under inst/app.", call. = FALSE)
}
line_coverage$section <- vapply(line_coverage$filename, section_for_path, character(1))
line_coverage$covered <- line_coverage$value > 0

zero_lines <- covr::zero_coverage(coverage, by = "line")
if (nrow(zero_lines)) {
  zero_lines$filename <- to_repository_path(zero_lines$filename)
  zero_lines <- zero_lines[
    startsWith(zero_lines$filename, "inst/app/"),
    ,
    drop = FALSE
  ]
  zero_lines$section <- vapply(zero_lines$filename, section_for_path, character(1))
  zero_lines$function_name <- ifelse(
    is.na(zero_lines$functions) | !nzchar(zero_lines$functions),
    "<top-level or unknown>",
    zero_lines$functions
  )
}

source_inventory <- data.frame(
  section = vapply(app_sources, section_for_path, character(1)),
  filename = to_repository_path(app_sources),
  stringsAsFactors = FALSE
)

file_lines <- aggregate(
  cbind(
    measured_lines = rep.int(1L, nrow(line_coverage)),
    covered_lines = as.integer(line_coverage$covered)
  ),
  by = list(filename = line_coverage$filename),
  FUN = sum
)

file_summary <- merge(source_inventory, file_lines, by = "filename", all.x = TRUE, sort = FALSE)
file_summary <- file_summary[match(source_inventory$filename, file_summary$filename), , drop = FALSE]
file_summary$section <- source_inventory$section
file_summary$measured_lines[is.na(file_summary$measured_lines)] <- 0L
file_summary$covered_lines[is.na(file_summary$covered_lines)] <- 0L
file_summary$uncovered_lines <- file_summary$measured_lines - file_summary$covered_lines
file_summary$coverage_percent <- ifelse(
  file_summary$measured_lines > 0,
  100 * file_summary$covered_lines / file_summary$measured_lines,
  NA_real_
)
file_summary$status <- ifelse(
  file_summary$measured_lines == 0,
  "not instrumented",
  ifelse(
    file_summary$covered_lines == 0,
    "uncovered",
    ifelse(file_summary$uncovered_lines == 0, "fully covered", "partially covered")
  )
)

section_lines <- aggregate(
  cbind(
    measured_lines = rep.int(1L, nrow(line_coverage)),
    covered_lines = as.integer(line_coverage$covered)
  ),
  by = list(section = line_coverage$section),
  FUN = sum
)
section_files <- aggregate(
  list(source_files = source_inventory$filename),
  by = list(section = source_inventory$section),
  FUN = length
)
section_measured_files <- aggregate(
  list(measured_files = as.integer(file_summary$measured_lines > 0)),
  by = list(section = file_summary$section),
  FUN = sum
)
section_summary <- merge(section_files, section_measured_files, by = "section", all = TRUE)
section_summary <- merge(section_summary, section_lines, by = "section", all.x = TRUE)
for (column in c("measured_files", "measured_lines", "covered_lines")) {
  section_summary[[column]][is.na(section_summary[[column]])] <- 0L
}
section_summary$uncovered_lines <- section_summary$measured_lines - section_summary$covered_lines
section_summary$coverage_percent <- ifelse(
  section_summary$measured_lines > 0,
  100 * section_summary$covered_lines / section_summary$measured_lines,
  NA_real_
)

section_order <- c(
  "App bootstrap",
  "Shared app code",
  "Parameters",
  "Statistics",
  "Graphics",
  "Server",
  "User interface",
  "Other app code"
)
section_summary <- section_summary[
  order(match(section_summary$section, section_order), section_summary$section),
  ,
  drop = FALSE
]

total_measured <- nrow(line_coverage)
total_covered <- sum(line_coverage$covered)
total_uncovered <- total_measured - total_covered
total_percent <- 100 * total_covered / total_measured

collapse_line_ranges <- function(lines) {
  lines <- sort(unique(as.integer(lines)))
  if (!length(lines)) return("")
  groups <- cumsum(c(TRUE, diff(lines) > 1L))
  ranges <- vapply(split(lines, groups), function(group) {
    if (length(group) == 1L) as.character(group) else paste0(min(group), "-", max(group))
  }, character(1))
  paste(ranges, collapse = ", ")
}

if (nrow(zero_lines)) {
  zero_lines <- zero_lines[
    order(zero_lines$section, zero_lines$filename, zero_lines$function_name, zero_lines$line),
    ,
    drop = FALSE
  ]
  group_key <- interaction(
    zero_lines$section,
    zero_lines$filename,
    zero_lines$function_name,
    drop = TRUE,
    lex.order = TRUE
  )
  uncovered_ranges <- do.call(rbind, lapply(split(seq_len(nrow(zero_lines)), group_key), function(index) {
    data.frame(
      section = zero_lines$section[index[[1]]],
      filename = zero_lines$filename[index[[1]]],
      function_name = zero_lines$function_name[index[[1]]],
      uncovered_line_ranges = collapse_line_ranges(zero_lines$line[index]),
      stringsAsFactors = FALSE
    )
  }))
  rownames(uncovered_ranges) <- NULL
} else {
  uncovered_ranges <- data.frame(
    section = character(),
    filename = character(),
    function_name = character(),
    uncovered_line_ranges = character(),
    stringsAsFactors = FALSE
  )
}

utils::write.csv(
  section_summary,
  file.path(output_dir, "coverage-by-section.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  file_summary,
  file.path(output_dir, "coverage-by-file.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  uncovered_ranges,
  file.path(output_dir, "uncovered-line-ranges.csv"),
  row.names = FALSE,
  na = ""
)

format_percent <- function(value) {
  ifelse(is.na(value), "N/A", sprintf("%.2f%%", value))
}

escape_markdown <- function(value) {
  value <- gsub("[|]", "\\\\|", as.character(value))
  gsub("[\r\n]+", " ", value)
}

markdown_table <- function(data) {
  if (!nrow(data)) return("_No rows._")
  data[] <- lapply(data, escape_markdown)
  header <- paste0("| ", paste(names(data), collapse = " | "), " |")
  divider <- paste0("| ", paste(rep("---", ncol(data)), collapse = " | "), " |")
  rows <- apply(data, 1L, function(row) paste0("| ", paste(row, collapse = " | "), " |"))
  paste(c(header, divider, rows), collapse = "\n")
}

section_markdown <- data.frame(
  Section = section_summary$section,
  Files = paste0(section_summary$measured_files, "/", section_summary$source_files),
  `Covered lines` = section_summary$covered_lines,
  `Measured lines` = section_summary$measured_lines,
  Coverage = format_percent(section_summary$coverage_percent),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

file_markdown <- data.frame(
  Section = file_summary$section,
  File = paste0("`", file_summary$filename, "`"),
  `Covered lines` = file_summary$covered_lines,
  `Measured lines` = file_summary$measured_lines,
  Coverage = format_percent(file_summary$coverage_percent),
  Status = file_summary$status,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

fully_uncovered <- file_summary$filename[file_summary$status == "uncovered"]
not_instrumented <- file_summary$filename[file_summary$status == "not instrumented"]
uncovered_block <- if (length(fully_uncovered)) {
  c("Files with measured executable lines but no executed line:", paste0("- `", fully_uncovered, "`"))
} else {
  "No measured file was completely uncovered."
}
not_instrumented_block <- if (length(not_instrumented)) {
  c(
    "Files for which `covr` found no instrumentable line:",
    paste0("- `", not_instrumented, "`")
  )
} else {
  "Every source file contributed at least one instrumented line."
}

summary_lines <- c(
  "# BIOSZEN application test coverage",
  "",
  paste0("Generated: ", generated_at),
  "",
  "## Overall result",
  "",
  paste0("**", sprintf("%.2f%%", total_percent), "** (", total_covered, " of ", total_measured, " measured executable lines)."),
  "",
  paste0("Uncovered measured lines: **", total_uncovered, "**."),
  "",
  "## Coverage by section",
  "",
  markdown_table(section_markdown),
  "",
  "## Coverage by file",
  "",
  markdown_table(file_markdown),
  "",
  "## Completely uncovered or not instrumented",
  "",
  uncovered_block,
  "",
  not_instrumented_block,
  "",
  "Detailed function names and uncovered line ranges are in `uncovered-line-ranges.csv`.",
  "",
  "## Interpretation",
  "",
  "- The scope is the R source under `inst/app`; no application source file is edited.",
  "- The percentage is line coverage: a line is covered when the current tests execute an expression instrumented by `covr` on that line.",
  "- The full existing `tests/testthat` suite is run. A test failure stops coverage and makes the workflow fail.",
  "- Browser tests launch the Shiny app in a separate R process. Code executed only in that child process may not increment `file_coverage()` counters, so E2E-only paths can appear conservatively under-covered.",
  "- Coverage measures execution, not assertion quality; it is one confidence indicator rather than proof that behavior is correct.",
  "- No `pkgcheck`, Codecov, Coveralls, SonarQube, or other external coverage upload is used. The workflow artifact contains derived Markdown/CSV reports only, without source-code snippets.",
  "- The workflow reports the percentage but does not enforce a minimum coverage threshold."
)

writeLines(summary_lines, file.path(output_dir, "coverage-summary.md"), useBytes = TRUE)
message(sprintf("BIOSZEN app coverage: %.2f%% (%d/%d measured lines)", total_percent, total_covered, total_measured))
message("Coverage report: ", file.path(output_dir, "coverage-summary.md"))
