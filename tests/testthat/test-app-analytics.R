library(testthat)

test_that("app analytics reports one privacy-limited session event", {
  ui_text <- paste(
    readLines(app_test_path("ui", "ui_main.R"), warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )
  server_text <- paste(
    readLines(app_test_path("server", "server_main.R"), warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )

  expect_match(ui_text, "G-Q5FYW8FV3Z", fixed = TRUE)
  expect_false(grepl("BIOSZEN.analytics.enabled", ui_text, fixed = TRUE))
  expect_false(grepl("BIOSZEN_DISABLE_ANALYTICS", ui_text, fixed = TRUE))
  expect_match(ui_text, "bioszenAnalyticsAppOpen", fixed = TRUE)
  expect_match(ui_text, "bioszen_app_open", fixed = TRUE)
  expect_match(ui_text, "app_version", fixed = TRUE)
  expect_match(ui_text, "launch_mode", fixed = TRUE)
  expect_match(ui_text, "app_language", fixed = TRUE)
  expect_match(ui_text, "var sent = false", fixed = TRUE)

  analytics_flush <- regmatches(
    server_text,
    regexpr(
      "on_session_flushed\\(function\\(\\) \\{[\\s\\S]{0,900}?bioszenAnalyticsAppOpen[\\s\\S]{0,500}?\\}, once = TRUE\\)",
      server_text,
      perl = TRUE
    )
  )
  expect_length(analytics_flush, 1L)
  expect_match(analytics_flush, "bioszen_analytics_launch_mode()", fixed = TRUE)
  expect_match(analytics_flush, "app_version", fixed = TRUE)
  expect_false(grepl("input$", analytics_flush, fixed = TRUE))
  expect_false(grepl("uploaded", analytics_flush, ignore.case = TRUE))
  expect_false(grepl("file_path", analytics_flush, fixed = TRUE))
})

test_that("analytics launch labels are deterministic", {
  env <- new.env(parent = globalenv())
  env$`%||%` <- function(x, y) if (is.null(x)) y else x
  env$BIOSZEN_CSS_DPI <- 96
  sys.source(app_test_path("helpers.R"), envir = env)

  old_mode <- getOption("BIOSZEN.launch_mode", NULL)
  on.exit({
    options(BIOSZEN.launch_mode = old_mode)
  }, add = TRUE)

  for (mode in c("r_package", "standalone_bundle", "hosted", "direct_source")) {
    options(BIOSZEN.launch_mode = mode)
    expect_identical(env$bioszen_analytics_launch_mode(), mode)
  }
  options(BIOSZEN.launch_mode = "unexpected")
  expect_true(env$bioszen_analytics_launch_mode() %in% c("hosted", "direct_source"))
})

test_that("each supported launcher sets a fixed analytics launch mode", {
  root <- app_test_root()
  package_launcher <- paste(
    readLines(file.path(root, "R", "run_app.R"), warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )
  source_launcher <- paste(
    readLines(file.path(root, "app.R"), warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )
  standalone_launcher <- paste(
    readLines(file.path(root, "inst", "launchers", "App.R"), warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )

  expect_match(package_launcher, 'BIOSZEN.launch_mode = "r_package"', fixed = TRUE)
  expect_match(source_launcher, 'if (hosted) "hosted" else "direct_source"', fixed = TRUE)
  expect_match(standalone_launcher, 'BIOSZEN.launch_mode = "standalone_bundle"', fixed = TRUE)
})
