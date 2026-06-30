library(testthat)

read_app_text <- function(...) {
  path <- app_test_path(...)
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

test_that("language switch refreshes static i18n DOM without reload", {
  ui_txt <- read_app_text("ui", "ui_main.R")
  server_txt <- read_app_text("server", "server_main.R")

  expect_match(ui_txt, "window\\.BIOSZEN_translateStatic\\s*=\\s*translateStatic", perl = TRUE)
  expect_match(ui_txt, "function lookupTranslation\\(key, lang\\)", perl = TRUE)
  expect_match(ui_txt, "document\\.querySelectorAll\\('\\.i18n\\[data-key\\], \\.i18n\\[data-i18n\\]'\\)", perl = TRUE)
  expect_match(ui_txt, "window\\.BIOSZEN_translateStatic\\(lang\\)", perl = TRUE)
  expect_match(ui_txt, "setTimeout\\(function \\(\\) \\{\\s*if \\(window\\.Shiny && typeof Shiny\\.setInputValue === 'function'\\)", perl = TRUE)
  expect_match(ui_txt, "function translateFileInputs\\(lang\\)", perl = TRUE)
  expect_match(ui_txt, "function translateChoiceLabels\\(lang\\)", perl = TRUE)
  expect_match(ui_txt, "function translateDynamicHeadings\\(lang\\)", perl = TRUE)
  expect_match(ui_txt, "function startTranslationObserver\\(\\)", perl = TRUE)
  expect_match(ui_txt, "new MutationObserver\\(function \\(\\)", perl = TRUE)
  expect_match(ui_txt, "scope_by_strain", perl = TRUE)
  expect_match(ui_txt, "sig_mode_bars", perl = TRUE)
  expect_match(ui_txt, "lookupTranslation\\('file_browse', lang\\)", perl = TRUE)
  expect_match(ui_txt, "lookupTranslation\\('file_no_selection', lang\\)", perl = TRUE)
  expect_match(ui_txt, 'tr\\("startup_loading"\\)', perl = TRUE)
  expect_match(ui_txt, "onclick = \"if\\(window\\.BIOSZEN_applyLang\\)\\{window\\.BIOSZEN_applyLang\\('es'\\);\\} return false;\"", perl = TRUE)
  expect_match(ui_txt, "onclick = \"if\\(window\\.BIOSZEN_applyLang\\)\\{window\\.BIOSZEN_applyLang\\('en'\\);\\} return false;\"", perl = TRUE)
  expect_match(server_txt, "window\\.BIOSZEN_translateStatic", perl = TRUE)
  expect_match(server_txt, "observeEvent\\(input\\$app_lang", perl = TRUE)
  expect_match(server_txt, "ignoreInit = FALSE", perl = TRUE)
  expect_false(grepl("data\\('lang', lang\\)\\.trigger\\('change'\\)", ui_txt, perl = TRUE))
  expect_false(grepl("location\\.reload|window\\.location\\.reload", ui_txt, perl = TRUE))
})

test_that("language-refresh title updates keep a real parameter in default titles", {
  server_txt <- read_app_text("server", "server_main.R")

  expect_match(server_txt, "preferred_param <- normalize_param_selection\\(isolate\\(last_param_selection\\(\\) %\\|\\|% \"\"\\), params\\)", perl = TRUE)
  expect_match(server_txt, "if \\(!nzchar\\(param_sel\\) && length\\(params\\)\\)", perl = TRUE)
  expect_match(server_txt, "if \\(!nzchar\\(param_sel\\) && input\\$tipo %in% c\\(\"Boxplot\", \"Barras\", \"Violin\", \"Apiladas\"\\)\\) return\\(\\)", perl = TRUE)
  expect_match(server_txt, "sprintf\\(tr_text\\(\"default_title_strain\", lang\\), type_label, param_sel", perl = TRUE)
  expect_match(server_txt, "update_text_input_if_changed\\(\"plotTitle\", defaultTitle\\)", perl = TRUE)
})

test_that("dynamic Y-axis labels use the active language", {
  server_txt <- read_app_text("server", "server_main.R")
  ui_txt <- read_app_text("ui", "ui_main.R")
  es <- read.csv(app_test_path("i18n", "translation_es.csv"), stringsAsFactors = FALSE, fileEncoding = "UTF-8")

  expect_false(grepl('paste0\\("Y max', server_txt, perl = TRUE))
  expect_false(grepl('paste0\\("Int Y', server_txt, perl = TRUE))
  expect_match(server_txt, 'tr_text\\("y_max", lang\\)', perl = TRUE)
  expect_match(server_txt, 'tr_text\\("y_interval", lang\\)', perl = TRUE)
  expect_match(ui_txt, "bioszen-axis-input-row", fixed = TRUE)
  expect_match(ui_txt, "min-height: 2\\.45em", perl = TRUE)
  expect_match(ui_txt, "numericInput\\('ymax'[\\s\\S]*?numericInput\\('ybreak'", perl = TRUE)
  expect_equal(es$es[match("y_max", es$key)], "Máx. Y")
  expect_equal(es$es[match("y_interval", es$key)], "Intervalo Y")
  expect_equal(es$es[match("file_browse", es$key)], "Examinar...")
  expect_equal(es$es[match("file_no_selection", es$key)], "Ningún archivo seleccionado")
  expect_equal(es$es[match("startup_loading", es$key)], "Cargando...")
  expect_equal(es$es[match("file_upload_complete", es$key)], "Carga completa")
  expect_equal(es$es[match("datatable_search", es$key)], "Buscar:")
})

test_that("dynamic radio choices are repainted with the active language", {
  server_txt <- read_app_text("server", "server_main.R")

  expect_match(server_txt, 'type_labels <- tr_text\\(', perl = TRUE)
  expect_match(server_txt, '"plot_bars"', perl = TRUE)
  expect_match(server_txt, '"plot_curves"', perl = TRUE)
  expect_match(server_txt, 'tr_text\\(c\\("sig_mode_bars", "sig_mode_labels"\\), lang\\)', perl = TRUE)
  expect_match(server_txt, 'tr_text\\(c\\("corr_method_pearson", "corr_method_spearman", "corr_method_kendall"\\), lang\\)', perl = TRUE)
  expect_false(grepl('list\\(tr\\("sig_mode_bars"\\), tr\\("sig_mode_labels"\\)\\)', server_txt, perl = TRUE))
})
