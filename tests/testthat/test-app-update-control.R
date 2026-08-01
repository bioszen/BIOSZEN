library(testthat)

test_that("the bilingual app update control is wired to the safe lifecycle", {
  ui_file <- app_test_path("ui", "ui_main.R")
  server_file <- app_test_path("server", "server_main.R")
  en_file <- app_test_path("i18n", "translation_en.csv")
  es_file <- app_test_path("i18n", "translation_es.csv")

  ui_text <- paste(readLines(ui_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  server_text <- paste(readLines(server_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  en <- read.csv(en_file, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  es <- read.csv(es_file, stringsAsFactors = FALSE, fileEncoding = "UTF-8")

  expect_match(ui_text, 'actionButton\\(\\s*"bioszenUpdate"', perl = TRUE)
  expect_match(ui_text, 'actionButton\\(\\s*"bioszenUpdateGrowth"', perl = TRUE)
  expect_match(ui_text, 'class = "btn bioszen-update-button"', fixed = TRUE)
  expect_match(ui_text, 'tr\\("app_update_button"\\)', perl = TRUE)

  expect_match(server_text, 'bioszen_update_available', fixed = TRUE)
  expect_match(server_text, '.bioszen_request_update_after_app', fixed = TRUE)
  expect_match(server_text, 'pending_bioszen_update', fixed = TRUE)
  expect_match(server_text, 'schedule_session_callback(function() shiny::stopApp()', fixed = TRUE)

  expect_identical(en$en[match("app_update_button", en$key)], "Update")
  expect_identical(es$es[match("app_update_button", es$key)], "Actualizar")
})

test_that("update validation states are not treated as application bugs", {
  server_text <- paste(
    readLines(app_test_path("server", "server_main.R"), warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )

  expect_match(server_text, 'tr_text("app_update_check_failed", lang)', fixed = TRUE)
  expect_match(server_text, 'type = "warning"', fixed = TRUE)
  expect_match(server_text, 'tr_text("app_update_none", lang)', fixed = TRUE)
})

test_that("weekly update checks are delayed, non-blocking, and cross-browser", {
  ui_text <- paste(
    readLines(app_test_path("ui", "ui_main.R"), warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )
  server_text <- paste(
    readLines(app_test_path("server", "server_main.R"), warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )

  expect_match(ui_text, "bioszenWeeklyUpdateConfig", fixed = TRUE)
  expect_match(ui_text, "setTimeout(function ()", fixed = TRUE)
  expect_match(ui_text, "AbortController", fixed = TRUE)
  expect_match(ui_text, "credentials: 'omit'", fixed = TRUE)
  expect_match(ui_text, "referrerPolicy: 'no-referrer'", fixed = TRUE)
  expect_match(ui_text, "releaseSummary(notes, available)", fixed = TRUE)
  expect_match(ui_text, "fetchJson(String(config.notes_endpoint), 3500)", fixed = TRUE)
  expect_false(grepl("bioszenWeeklyUpdateLastCheckedAt", ui_text, fixed = TRUE))
  expect_match(server_text, "bioszen_weekly_update_due()", fixed = TRUE)
  expect_match(server_text, 'installed_version <- get_current_version("BIOSZEN")', fixed = TRUE)
  expect_match(server_text, 'if (is.null(installed_version)', fixed = TRUE)
  expect_false(grepl('get_current_version("BIOSZEN") %||% "0"', server_text, fixed = TRUE))
  expect_match(server_text, "bioszen_record_weekly_update_check()", fixed = TRUE)
  expect_match(server_text, 'identical(package, "BIOSZEN")', fixed = TRUE)
  expect_match(server_text, 'endpoint = "https://bioszen.r-universe.dev/BIOSZEN/json"', fixed = TRUE)
  expect_match(server_text, 'notes_endpoint = "https://raw.githubusercontent.com/bioszen/BIOSZEN/main/inst/update-notes.json"', fixed = TRUE)
  expect_match(server_text, 'class = "bioszen-update-release-summary"', fixed = TRUE)
  expect_match(server_text, "duration = NULL", fixed = TRUE)
  expect_match(server_text, "closeButton = TRUE", fixed = TRUE)
  expect_match(server_text, 'actionButton(\n        "bioszenWeeklyUpdate"', fixed = TRUE)

  flushed <- regmatches(
    server_text,
    regexpr(
      "on_session_flushed\\(function\\(\\) \\{[\\s\\S]{0,900}?\\}, once = TRUE\\)",
      server_text,
      perl = TRUE
    )
  )
  expect_length(flushed, 1L)
  expect_false(grepl("bioszen_update_available", flushed, fixed = TRUE))
})

test_that("automatic update checks stay silent unless a newer version exists", {
  server_text <- paste(
    readLines(app_test_path("server", "server_main.R"), warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )

  weekly_handler <- regmatches(
    server_text,
    regexpr(
      "observeEvent\\(input\\$bioszen_weekly_update_result,[\\s\\S]{0,5000}?\\}, ignoreInit = TRUE\\)",
      server_text,
      perl = TRUE
    )
  )
  expect_length(weekly_handler, 1L)
  guard_at <- regexpr("if (!isTRUE(newer)) return()", weekly_handler, fixed = TRUE)[1]
  notification_at <- regexpr("showNotification(", weekly_handler, fixed = TRUE)[1]
  expect_gt(guard_at, 0L)
  expect_gt(notification_at, guard_at)
  expect_false(grepl("app_update_checking", weekly_handler, fixed = TRUE))
  expect_false(grepl("app_update_none", weekly_handler, fixed = TRUE))

  expect_match(server_text, 'message = tr_text("app_update_checking", lang)', fixed = TRUE)
  expect_match(server_text, 'tr_text("app_update_none", lang)', fixed = TRUE)
  expect_match(server_text, "observeEvent(input$bioszenUpdate", fixed = TRUE)
  expect_match(server_text, "observeEvent(input$bioszenUpdateGrowth", fixed = TRUE)
})

test_that("weekly update state retries failures and waits seven days after success", {
  env <- new.env(parent = globalenv())
  env$`%||%` <- function(x, y) if (is.null(x)) y else x
  env$BIOSZEN_CSS_DPI <- 96
  sys.source(app_test_path("helpers.R"), envir = env)

  state <- tempfile(fileext = ".rds")
  on.exit(unlink(state, force = TRUE), add = TRUE)
  start <- as.POSIXct("2026-08-01 12:00:00", tz = "UTC")

  expect_true(env$bioszen_weekly_update_due(state, now = start))
  expect_true(env$bioszen_record_weekly_update_check(state, checked_at = start))
  expect_false(env$bioszen_weekly_update_due(state, now = start + 6 * 24 * 60 * 60))
  expect_true(env$bioszen_weekly_update_due(state, now = start + 7 * 24 * 60 * 60))

  writeLines("not an RDS file", state)
  expect_true(env$bioszen_weekly_update_due(state, now = start))
})

test_that("update notes provide one concise localized sentence for this version", {
  package_root <- app_test_root()
  notes_path <- file.path(package_root, "inst", "update-notes.json")
  if (!file.exists(notes_path)) {
    notes_path <- system.file("update-notes.json", package = "BIOSZEN")
  }
  notes_path <- normalizePath(notes_path, winslash = "/", mustWork = TRUE)
  package_version <- unname(read.dcf(file.path(package_root, "DESCRIPTION"))[, "Version"])
  notes <- jsonlite::fromJSON(notes_path, simplifyVector = FALSE)
  entry <- notes$releases[[package_version]]

  expect_identical(notes$schema_version, 1L)
  expect_type(entry, "list")
  for (language in c("en", "es")) {
    summary <- entry[[language]]
    expect_type(summary, "character")
    expect_length(summary, 1L)
    expect_false(grepl("[\r\n]", summary))
    expect_lte(nchar(summary), 280L)
    expect_match(summary, "[.!?]$")
  }
})
