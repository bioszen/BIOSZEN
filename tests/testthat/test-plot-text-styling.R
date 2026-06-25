library(testthat)

test_that("main graph typography controls are wired through UI and server", {
  ui_file <- app_test_path("ui", "ui_main.R")
  server_file <- app_test_path("server", "server_main.R")
  panel_file <- app_test_path("server", "panel_module.R")
  global_file <- app_test_path("global.R")

  ui_txt <- paste(readLines(ui_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  server_txt <- paste(readLines(server_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  panel_txt <- paste(readLines(panel_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  global_txt <- paste(readLines(global_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_match(global_txt, "bioszen_plot_font_choices <- function", fixed = TRUE)
  expect_match(global_txt, "bioszen_plot_text_targets <- function", fixed = TRUE)
  expect_match(global_txt, "bioszen_plot_text_styles <- function", fixed = TRUE)
  for (font in c(
    "Arial Black",
    "Century Gothic",
    "Consolas",
    "Franklin Gothic Medium",
    "Palatino Linotype",
    "Segoe UI",
    "Verdana"
  )) {
    expect_match(global_txt, paste0("\"", font, "\""), fixed = TRUE)
  }

  expect_match(ui_txt, "id = \"plotTextStylePanel\"", fixed = TRUE)
  expect_match(ui_txt, "open = FALSE", fixed = TRUE)
  expect_match(ui_txt, "accordion_panel_safe(\n                      tr(\"plot_text_style_section\")", fixed = TRUE)
  expect_match(ui_txt, "\"plot_font_family\"", fixed = TRUE)
  expect_match(ui_txt, "choices = bioszen_plot_font_choices()", fixed = TRUE)
  for (input_id in c(
    "plot_text_style_title",
    "plot_text_style_axis_titles",
    "plot_text_style_axis_text",
    "plot_text_style_legend",
    "plot_text_style_data_labels",
    "plot_text_style_significance"
  )) {
    expect_match(ui_txt, paste0("\"", input_id, "\""), fixed = TRUE)
  }

  expect_match(server_txt, "style_plot_text <- function", fixed = TRUE)
  expect_match(server_txt, "style_plot_text_layers <- function", fixed = TRUE)
  expect_match(server_txt, "apply_plotly_text_style <- function", fixed = TRUE)
  expect_match(server_txt, "plot_text_face <- function", fixed = TRUE)
  expect_match(server_txt, "plot_text_styles_for_target <- function", fixed = TRUE)

  expect_match(server_txt, "plot_font_family = as.character(input$plot_font_family", fixed = TRUE)
  expect_match(server_txt, "plot_text_style_title = plot_metadata_style_value(\"title\")", fixed = TRUE)
  expect_match(server_txt, "plot_text_style_axis_titles = plot_metadata_style_value(\"axis_titles\")", fixed = TRUE)
  expect_match(server_txt, "plot_text_style_axis_text = plot_metadata_style_value(\"axis_text\")", fixed = TRUE)
  expect_match(server_txt, "plot_text_style_legend = plot_metadata_style_value(\"legend\")", fixed = TRUE)
  expect_match(server_txt, "plot_text_style_data_labels = plot_metadata_style_value(\"data_labels\")", fixed = TRUE)
  expect_match(server_txt, "plot_text_style_significance = plot_metadata_style_value(\"significance\")", fixed = TRUE)
  expect_match(server_txt, "addWorksheet(wb, \"TextStyle\")", fixed = TRUE)
  expect_match(server_txt, "writeData(wb, \"TextStyle\", text_style", fixed = TRUE)
  expect_match(server_txt, "metadata_text_style_sheet_rows(", fixed = TRUE)
  expect_match(server_txt, "updateSelectInput(session, \"plot_font_family\"", fixed = TRUE)
  expect_match(server_txt, "plot_text_style_input_id(target)", fixed = TRUE)
  expect_match(server_txt, "metadata_parse_text_style_value(x)", fixed = TRUE)
  expect_match(server_txt, "legacy_targets <- parse_csv_values(old_targets)", fixed = TRUE)

  expect_match(server_txt, "apply_plotly_text_style(plt)", fixed = TRUE)
  expect_match(server_txt, "return(apply_plotly_text_style(sanitize_plotly_display_labels(plt)))", fixed = TRUE)
  expect_match(server_txt, "bioszen_text_target = \"significance\"", fixed = TRUE)

  expect_match(global_txt, "\"combo_font_family\"", fixed = TRUE)
  for (input_id in c(
    "combo_text_style_title",
    "combo_text_style_axis_titles",
    "combo_text_style_axis_text",
    "combo_text_style_legend",
    "combo_text_style_data_labels",
    "combo_text_style_significance"
  )) {
    expect_match(global_txt, paste0("\"", input_id, "\""), fixed = TRUE)
  }

  expect_match(global_txt, "id = \"comboTextStylePanel\"", fixed = TRUE)
  expect_match(global_txt, "accordion_panel_safe(\n                tr(\"plot_text_style_section\")", fixed = TRUE)
  expect_match(global_txt, "choices = bioszen_plot_font_choices()", fixed = TRUE)
  expect_match(panel_txt, "combo_text_face <- function", fixed = TRUE)
  expect_match(panel_txt, "combo_style_text_layers <- function", fixed = TRUE)
  expect_match(panel_txt, "combo_text_style_title = combo_metadata_style_value(\"title\")", fixed = TRUE)
  expect_match(panel_txt, "addWorksheet(wb, 'TextStyle')", fixed = TRUE)
  expect_match(panel_txt, "writeData(wb, 'TextStyle', collect_combo_text_style_tbl()", fixed = TRUE)
  expect_match(panel_txt, "collect_combo_plot_metadata_tbl <- function()", fixed = TRUE)
  expect_match(panel_txt, "addWorksheet(wb, 'PlotMetadata')", fixed = TRUE)
  expect_match(panel_txt, "writeData(wb, 'PlotMetadata', plot_meta", fixed = TRUE)
  expect_match(panel_txt, "metadata_text_style_sheet_rows(", fixed = TRUE)
  expect_match(panel_txt, "metadata_parse_text_style_value(v)", fixed = TRUE)
  expect_match(panel_txt, "for (target in c(\"title\", \"axis_titles\", \"axis_text\", \"legend\", \"data_labels\", \"significance\"))", fixed = TRUE)
})

test_that("plotly underline styling does not emit literal u tags", {
  server_file <- app_test_path("server", "server_main.R")
  server_txt <- paste(readLines(server_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_match(server_txt, "font$lineposition <- \"under\"", fixed = TRUE)
  expect_match(server_txt, "apply_plotly_underline_render_hook <- function", fixed = TRUE)
  expect_match(server_txt, "node.style.textDecoration = 'underline'", fixed = TRUE)
  expect_match(server_txt, "node.setAttribute('text-decoration', 'underline')", fixed = TRUE)
  expect_match(server_txt, "gd.on('plotly_afterplot', apply)", fixed = TRUE)
  expect_match(server_txt, "gsub(\"</?u\\\\b[^>]*>\", \"\", out, ignore.case = TRUE)", fixed = TRUE)
  expect_false(grepl("paste0\\(\"<u>\"", server_txt, fixed = FALSE))
})
