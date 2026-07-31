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
