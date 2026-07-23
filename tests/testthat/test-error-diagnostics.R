library(testthat)

test_that("diagnostic helpers redact personal paths and email addresses", {
  global_file <- app_test_path("global.R")
  global_txt <- paste(readLines(global_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  helper_env <- new.env(parent = globalenv())
  helper_env$`%||%` <- function(x, y) if (is.null(x)) y else x
  extract_function <- function(name, next_name) {
    pattern <- sprintf(
      "(?s)(%s <- function\\(.*?\\n\\})(?=\\n\\n%s <- function)",
      name,
      next_name
    )
    match <- regmatches(global_txt, regexpr(pattern, global_txt, perl = TRUE))
    expect_length(match, 1L)
    eval(parse(text = match), envir = helper_env)
  }

  extract_function("bioszen_sanitize_diagnostic_text", "bioszen_error_call_text")
  extract_function("bioszen_error_call_text", "bioszen_error_trace_text")
  extract_function("bioszen_error_trace_text", "bioszen_is_expected_shiny_condition")
  extract_function("bioszen_is_expected_shiny_condition", "bioszen_is_user_input_condition")
  extract_function("bioszen_is_user_input_condition", "bioszen_is_reportable_app_error")
  extract_function("bioszen_is_reportable_app_error", "bioszen_build_error_report")
  extract_function("bioszen_build_error_report", "bioszen_plot_workload_is_large")
  workload_match <- regmatches(
    global_txt,
    regexpr(
      "(?s)(bioszen_plot_workload_is_large <- function\\(.*?\\n\\})(?=\\n\\n# Report unexpected)",
      global_txt,
      perl = TRUE
    )
  )
  expect_length(workload_match, 1L)
  eval(parse(text = workload_match), envir = helper_env)

  condition <- simpleError(
    paste(
      "Contact alice@example.org",
      "Failed at \"C:/Users/alice/Documents/private/input.xlsx\"",
      sep = "\n"
    )
  )
  report <- helper_env$bioszen_build_error_report(
    condition = condition,
    user_message = "Plot failed",
    module = "Plots and statistics",
    action = "Render Boxplot",
    version = "2.0.4",
    calls = list(quote(read.csv("/Users/alice/private/input.csv")))
  )

  expect_match(report$text, "BIOSZEN version: 2.0.4", fixed = TRUE)
  expect_match(report$text, "Module: Plots and statistics", fixed = TRUE)
  expect_match(report$text, "Original R condition:", fixed = TRUE)
  expect_match(report$text, "Traceback / active calls:", fixed = TRUE)
  expect_match(report$text, "<redacted-path>", fixed = TRUE)
  expect_match(report$text, "<redacted-email>", fixed = TRUE)
  expect_false(grepl("alice", report$text, fixed = TRUE))
  expect_false(grepl("input.xlsx", report$text, fixed = TRUE))
  expect_false(grepl("input.csv", report$text, fixed = TRUE))

  expect_false(helper_env$bioszen_plot_workload_is_large(12L, threshold = 1000L))
  expect_true(helper_env$bioszen_plot_workload_is_large(5000L, threshold = 1000L))
  expect_false(helper_env$bioszen_plot_workload_is_large(character(0), threshold = 1000L))

  expect_true(helper_env$bioszen_is_user_input_condition(
    simpleError("Invalid data file: required sheet Datos is missing")
  ))
  expect_false(helper_env$bioszen_is_reportable_app_error(
    simpleError("Select a data file first")
  ))
  expect_true(helper_env$bioszen_is_reportable_app_error(
    simpleError("NAs are not allowed in subscripted assignments")
  ))
})
