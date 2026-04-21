library(testthat)

read_app_file <- function(...) {
  parts <- as.character(c(...))
  if (length(parts) >= 2 && identical(parts[1:2], c("inst", "app"))) {
    parts <- parts[-c(1, 2)]
  }
  path <- do.call(app_test_path, as.list(parts))
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

test_that("composition panel defaults use 1000x700 and exposes copy button", {
  txt <- read_app_file("inst", "app", "global.R")

  expect_true(grepl(
    "numericInput\\(\"combo_width\"\\s*,\\s*tr\\(\"combo_width\"\\)\\s*,\\s*1000",
    txt,
    perl = TRUE
  ))
  expect_true(grepl(
    "numericInput\\(\"combo_height\"\\s*,\\s*tr\\(\"combo_height\"\\)\\s*,\\s*700",
    txt,
    perl = TRUE
  ))
  expect_true(grepl(
    "actionButton\\(\\s*\"copy_combo_clipboard\"",
    txt,
    perl = TRUE
  ))
  expect_true(grepl(
    "textInput\\(\"combo_title\"\\s*,\\s*tr\\(\"combo_title\"\\)",
    txt,
    perl = TRUE
  ))
  expect_true(grepl(
    "numericInput\\(\"combo_title_size\"\\s*,\\s*tr\\(\"combo_comp_title_size\"\\)",
    txt,
    perl = TRUE
  ))
  expect_true(grepl(
    "numericInput\\(\"fs_title_all\"\\s*,\\s*tr\\(\"combo_plot_title_size\"\\)",
    txt,
    perl = TRUE
  ))
  expect_true(grepl(
    "numericInput\\(\"combo_axis_line_size\"\\s*,\\s*tr\\(\"combo_axis_line_size\"\\)",
    txt,
    perl = TRUE
  ))
  expect_true(grepl(
    "helpText\\(tr\\(\"combo_style_live_hint\"\\)\\)",
    txt,
    perl = TRUE
  ))
  expect_true(grepl(
    "helpText\\(tr\\(\"combo_override_apply_hint\"\\)\\)",
    txt,
    perl = TRUE
  ))
  expect_true(grepl(
    "selectInput\\(\\s*\"combo_legend_scope\"",
    txt,
    perl = TRUE
  ))
  expect_true(grepl(
    "radioButtons\\(\\s*\"combo_legend_side\"",
    txt,
    perl = TRUE
  ))
})

test_that("composition clipboard copy is wired in UI and server", {
  ui_txt <- read_app_file("inst", "app", "ui", "ui_main.R")
  srv_txt <- read_app_file("inst", "app", "server", "server_main.R")

  expect_true(grepl(
    "Shiny\\.addCustomMessageHandler\\('copyStaticPlotToClipboard'",
    ui_txt,
    perl = TRUE
  ))
  expect_true(grepl(
    "observeEvent\\(input\\$copy_combo_clipboard",
    srv_txt,
    perl = TRUE
  ))
  expect_true(grepl(
    "sendCustomMessage\\(\\s*\"copyStaticPlotToClipboard\"",
    srv_txt,
    perl = TRUE
  ))
  expect_true(grepl(
    "elementId\\s*=\\s*\"comboPreview\"",
    srv_txt,
    perl = TRUE
  ))
})

test_that("composition flow keeps stacked plots addable and snapshot-safe", {
  panel_txt <- read_app_file("inst", "app", "server", "panel_module.R")
  srv_txt <- read_app_file("inst", "app", "server", "server_main.R")

  expect_true(grepl(
    "\"Apiladas\"",
    panel_txt,
    perl = TRUE
  ))
  expect_true(grepl(
    "type\\s*=\\s*input\\$tipo",
    panel_txt,
    perl = TRUE
  ))
  expect_true(grepl(
    "if \\(input\\$tipo == \"Apiladas\"\\)",
    srv_txt,
    perl = TRUE
  ))
  expect_true(grepl(
    "build_plot\\(scope_sel, strain_sel, \"Apiladas\"\\)",
    srv_txt,
    perl = TRUE
  ))
})

test_that("override panel pre-fills current values and avoids empty numeric controls", {
  panel_txt <- read_app_file("inst", "app", "server", "panel_module.R")

  expect_true(grepl(
    "current_override_form_values\\s*<-\\s*function",
    panel_txt,
    perl = TRUE
  ))
  expect_true(grepl(
    "override_target_ids\\s*<-\\s*function",
    panel_txt,
    perl = TRUE
  ))
  expect_false(grepl(
    "numericInput\\(\"ov_fs_title\"",
    panel_txt,
    perl = TRUE
  ))
  expect_false(grepl(
    "numericInput\\(\"ov_fs_axis\"",
    panel_txt,
    perl = TRUE
  ))
  expect_false(grepl(
    "numericInput\\(\"ov_axis_size\"",
    panel_txt,
    perl = TRUE
  ))
})

test_that("composition supports global title and shared legend modes", {
  panel_txt <- read_app_file("inst", "app", "server", "panel_module.R")

  expect_true(grepl(
    "plot_annotation\\(\\s*title\\s*=\\s*combo_title",
    panel_txt,
    perl = TRUE
  ))
  expect_true(grepl(
    "legend_scope\\s*<-\\s*as\\.character\\(input\\$combo_legend_scope",
    panel_txt,
    perl = TRUE
  ))
  expect_true(grepl(
    "show_legends && legend_scope %in% c\\(\"by_type\", \"collect\"\\)",
    panel_txt,
    perl = TRUE
  ))
  expect_true(grepl(
    "plot_layout\\(guides\\s*=\\s*\"collect\"\\)",
    panel_txt,
    perl = TRUE
  ))
  expect_true(grepl(
    "combo_legend_side",
    panel_txt,
    perl = TRUE
  ))
  expect_true(grepl(
    "force_enable_plot_legend\\s*<-\\s*function",
    panel_txt,
    perl = TRUE
  ))
  expect_true(grepl(
    "if \\(isFALSE\\(leg\\)\\)\\s*\\{\\s*p\\$layers\\[\\[i\\]\\]\\$show.legend <- NA",
    panel_txt,
    perl = TRUE
  ))
  expect_true(grepl(
    "global_axis_line_size <- clamp_num\\(input\\$combo_axis_line_size",
    panel_txt,
    perl = TRUE
  ))
  expect_true(grepl(
    "axis.line = element_line\\(linewidth = global_axis_line_size",
    panel_txt,
    perl = TRUE
  ))
  expect_true(grepl(
    "axis.title.x = element_text\\(size = global_axis_title_size",
    panel_txt,
    perl = TRUE
  ))
  expect_true(grepl(
    "axis.text.x = element_text\\(size = global_axis_text_size",
    panel_txt,
    perl = TRUE
  ))
})

test_that("composition auto-expands columns and rejects incomplete custom layouts", {
  panel_txt <- read_app_file("inst", "app", "server", "panel_module.R")

  expect_true(grepl(
    "if \\(max\\(present\\) != n_plots\\) return\\(NULL\\)",
    panel_txt,
    perl = TRUE
  ))
  expect_true(grepl(
    "if \\(nrow_combo \\* ncol_eff < n_plots\\)",
    panel_txt,
    perl = TRUE
  ))
  expect_true(grepl(
    "updateNumericInput\\(session, \"ncol_combo\", value = as.integer\\(ncol_eff\\)\\)",
    panel_txt,
    perl = TRUE
  ))
})
