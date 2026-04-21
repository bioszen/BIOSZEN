library(testthat)

test_that("mobile pane switch UI is present and uses i18n labels", {
  ui_file <- app_test_path( "ui", "ui_main.R")
  expect_true(file.exists(ui_file))

  txt <- paste(readLines(ui_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_match(txt, 'class\\s*=\\s*"bioszen-pane-fab"', perl = TRUE)
  expect_match(txt, 'actionButton\\(\\s*"mobile_switch_panel"', perl = TRUE)
  expect_match(txt, 'tr\\("mobile_switch_view_graphics"\\)', perl = TRUE)
  expect_match(txt, 'tr\\("mobile_switch_config_panel"\\)', perl = TRUE)
  expect_match(txt, 'id\\s*=\\s*"mobile_switch_label_sidebar"', perl = TRUE)
  expect_match(txt, 'id\\s*=\\s*"mobile_switch_label_main"', perl = TRUE)
})

test_that("mobile pane switch script toggles panes and triggers refresh", {
  ui_file <- app_test_path( "ui", "ui_main.R")
  txt <- paste(readLines(ui_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_match(txt, 'getActiveLayout\\(\\)', perl = TRUE)
  expect_false(grepl('#mainTabs \\\\.tab-pane\\\\.active', txt, perl = TRUE))
  expect_match(txt, 'document\\.querySelectorAll\\(\\s*[\'"]\\.bioszen-main-tabs \\.tab-content > \\.tab-pane\\.active[\'"]\\s*\\)', perl = TRUE)
  expect_match(txt, 'layout\\.classList\\.remove\\(\\s*[\'"]show-sidebar[\'"]\\s*\\)', perl = TRUE)
  expect_match(txt, 'layout\\.classList\\.add\\(\\s*[\'"]show-main[\'"]\\s*\\)', perl = TRUE)
  expect_match(txt, 'layout\\.classList\\.remove\\(\\s*[\'"]show-main[\'"]\\s*\\)', perl = TRUE)
  expect_match(txt, 'layout\\.classList\\.add\\(\\s*[\'"]show-sidebar[\'"]\\s*\\)', perl = TRUE)
  expect_match(txt, 'btn\\.addEventListener\\(\\s*[\'"]click[\'"]', perl = TRUE)
  expect_match(txt, 'Shiny\\.setInputValue\\(\\s*[\'"]mobile_plot_refresh[\'"]', perl = TRUE)
  expect_match(txt, 'Shiny\\.setInputValue\\(\\s*[\'"]mobile_panel_state[\'"]', perl = TRUE)
  expect_match(txt, 'document\\.addEventListener\\(\\s*[\'"]bioszen:lang-changed[\'"]', perl = TRUE)
  expect_match(txt, 'window\\.BIOSZEN_togglePane\\s*=\\s*togglePane', perl = TRUE)
  expect_match(txt, 'window\\.BIOSZEN_setPaneLabels\\s*=\\s*setPaneLabels', perl = TRUE)
  expect_match(txt, 'localStorage\\.getItem\\(\\s*[\'"]appLang[\'"]\\s*\\)', perl = TRUE)
})

test_that("plot outputs stay active when hidden in one-pane mobile mode", {
  server_file <- app_test_path( "server", "server_main.R")
  expect_true(file.exists(server_file))

  txt <- paste(readLines(server_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_match(txt, 'output\\$plotInteractivo\\s*<-\\s*renderPlotly\\(', perl = TRUE)
  expect_match(txt, 'input\\$mobile_plot_refresh', perl = TRUE)
  expect_match(
    txt,
    'outputOptions\\(\\s*output\\s*,\\s*"plotInteractivoUI"\\s*,\\s*suspendWhenHidden\\s*=\\s*FALSE\\s*\\)',
    perl = TRUE
  )
  expect_match(
    txt,
    'outputOptions\\(\\s*output\\s*,\\s*"plotInteractivo"\\s*,\\s*suspendWhenHidden\\s*=\\s*FALSE\\s*\\)',
    perl = TRUE
  )
})

test_that("server syncs floating switch labels when language changes", {
  server_file <- app_test_path( "server", "server_main.R")
  txt <- paste(readLines(server_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_match(txt, 'tr_text\\(\\s*"mobile_switch_view_graphics"', perl = TRUE)
  expect_match(txt, 'tr_text\\(\\s*"mobile_switch_config_panel"', perl = TRUE)
  expect_match(txt, 'window\\.BIOSZEN_setPaneLabels', perl = TRUE)
})
