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
    "plot_text_style_axis_title_x",
    "plot_text_style_axis_title_y",
    "plot_text_style_axis_text",
    "plot_text_style_axis_text_x",
    "plot_text_style_axis_text_y",
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

  for (input_id in c(
    "plot_text_style_title",
    "plot_text_style_axis_titles",
    "plot_text_style_axis_title_x",
    "plot_text_style_axis_title_y"
  )) {
    expect_match(
      ui_txt,
      paste0('"', input_id, '"[\\s\\S]*?selected = character\\(0\\)'),
      perl = TRUE,
      info = paste(input_id, "must start without bold, italic, or underline")
    )
  }
  expect_match(
    server_txt,
    'plot.title = update_text_element\\([\\s\\S]*?"title",[\\s\\S]*?default_face = "plain"',
    perl = TRUE
  )
  expect_match(
    server_txt,
    'axis.title = update_text_element\\([\\s\\S]*?"axis_titles",[\\s\\S]*?default_face = "plain"',
    perl = TRUE
  )

  expect_match(server_txt, "plot_font_family = as.character(input$plot_font_family", fixed = TRUE)
  expect_match(server_txt, "plot_text_style_title = plot_metadata_style_value(\"title\")", fixed = TRUE)
  expect_match(server_txt, "plot_text_style_axis_titles = plot_metadata_style_value(\"axis_titles\")", fixed = TRUE)
  expect_match(server_txt, "plot_text_style_axis_title_x = plot_metadata_style_value(\"axis_title_x\")", fixed = TRUE)
  expect_match(server_txt, "plot_text_style_axis_title_y = plot_metadata_style_value(\"axis_title_y\")", fixed = TRUE)
  expect_match(server_txt, "plot_text_style_axis_text = plot_metadata_style_value(\"axis_text\")", fixed = TRUE)
  expect_match(server_txt, "plot_text_style_axis_text_x = plot_metadata_style_value(\"axis_text_x\")", fixed = TRUE)
  expect_match(server_txt, "plot_text_style_axis_text_y = plot_metadata_style_value(\"axis_text_y\")", fixed = TRUE)
  expect_match(server_txt, "legend_style <- plot_metadata_style_value(\"legend\")", fixed = TRUE)
  expect_match(server_txt, "plot_text_style_legend = legend_style", fixed = TRUE)
  expect_match(server_txt, "plot_text_style_data_labels = plot_metadata_style_value(\"data_labels\")", fixed = TRUE)
  expect_match(server_txt, "plot_text_style_significance = plot_metadata_style_value(\"significance\")", fixed = TRUE)
  expect_match(server_txt, "plot_axis_xy_custom = as.character(input$plot_axis_xy_custom", fixed = TRUE)
  expect_match(server_txt, "group_label_text_style_overrides = encode_named_metadata(", fixed = TRUE)
  for (field in c(
    "legend_applicable",
    "legend_on_right",
    "legend_text_style",
    "legend_bold",
    "legend_italic",
    "legend_underline",
    "legend_font_family",
    "legend_font_size"
  )) {
    expect_match(server_txt, field, fixed = TRUE)
  }
  expect_match(server_txt, "get_val(\"legend_on_right\")", fixed = TRUE)
  expect_match(server_txt, "get_val_allow_blank(\"legend_text_style\")", fixed = TRUE)
  expect_match(server_txt, "addWorksheet(wb, \"TextStyle\")", fixed = TRUE)
  expect_match(server_txt, "writeData(wb, \"TextStyle\", text_style", fixed = TRUE)
  expect_match(server_txt, "metadata_text_style_sheet_rows(", fixed = TRUE)
  expect_match(server_txt, "updateSelectInput(session, \"plot_font_family\"", fixed = TRUE)
  expect_match(server_txt, "updateCheckboxInput(session, \"plot_axis_xy_custom\"", fixed = TRUE)
  expect_match(server_txt, "group_label_style_overrides(decode_named_metadata(v))", fixed = TRUE)
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
    "combo_text_style_axis_title_x",
    "combo_text_style_axis_title_y",
    "combo_text_style_axis_text",
    "combo_text_style_axis_text_x",
    "combo_text_style_axis_text_y",
    "combo_text_style_legend",
    "combo_text_style_data_labels",
    "combo_text_style_significance"
  )) {
    expect_match(global_txt, paste0("\"", input_id, "\""), fixed = TRUE)
  }

  expect_match(global_txt, "id = \"comboTextStylePanel\"", fixed = TRUE)
  expect_match(global_txt, "accordion_panel_safe(\n                tr(\"plot_text_style_section\")", fixed = TRUE)
  expect_match(global_txt, "choices = bioszen_plot_font_choices()", fixed = TRUE)
  for (input_id in c(
    "combo_text_style_title",
    "combo_text_style_axis_titles",
    "combo_text_style_axis_title_x",
    "combo_text_style_axis_title_y"
  )) {
    expect_match(
      global_txt,
      paste0('"', input_id, '"[\\s\\S]*?selected = character\\(0\\)'),
      perl = TRUE,
      info = paste(input_id, "must start without bold, italic, or underline")
    )
  }
  expect_match(panel_txt, '"title", default_face = "plain"', fixed = TRUE)
  expect_match(panel_txt, '"axis_titles", default_face = "plain"', fixed = TRUE)
  expect_match(panel_txt, "combo_text_face <- function", fixed = TRUE)
  expect_match(panel_txt, "combo_style_text_layers <- function", fixed = TRUE)
  expect_match(panel_txt, "combo_axis_xy_custom = input$combo_axis_xy_custom", fixed = TRUE)
  expect_match(panel_txt, "combo_text_style_axis_title_x = combo_metadata_style_value(\"axis_title_x\")", fixed = TRUE)
  expect_match(panel_txt, "combo_text_style_axis_text_y = combo_metadata_style_value(\"axis_text_y\")", fixed = TRUE)
  expect_match(panel_txt, "combo_text_style_title = combo_metadata_style_value(\"title\")", fixed = TRUE)
  for (field in c(
    "combo_legend_on_right",
    "combo_legend_text_style",
    "combo_legend_bold",
    "combo_legend_italic",
    "combo_legend_underline",
    "combo_legend_font_family",
    "combo_legend_font_size"
  )) {
    expect_match(panel_txt, field, fixed = TRUE)
  }
  expect_match(panel_txt, "gv('combo_legend_on_right')", fixed = TRUE)
  expect_match(panel_txt, "gv(\"combo_legend_text_style\")", fixed = TRUE)
  expect_match(panel_txt, "addWorksheet(wb, 'TextStyle')", fixed = TRUE)
  expect_match(panel_txt, "writeData(wb, 'TextStyle', collect_combo_text_style_tbl()", fixed = TRUE)
  expect_match(panel_txt, "collect_combo_plot_metadata_tbl <- function()", fixed = TRUE)
  expect_match(panel_txt, "addWorksheet(wb, 'PlotMetadata')", fixed = TRUE)
  expect_match(panel_txt, "writeData(wb, 'PlotMetadata', plot_meta", fixed = TRUE)
  expect_match(panel_txt, "metadata_text_style_sheet_rows(", fixed = TRUE)
  expect_match(panel_txt, "metadata_parse_text_style_value(v)", fixed = TRUE)
  expect_match(panel_txt, "for (target in combo_text_style_targets())", fixed = TRUE)
})

test_that("plotly underline styling does not emit literal u tags", {
  server_file <- app_test_path("server", "server_main.R")
  server_txt <- paste(readLines(server_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_match(server_txt, "font$lineposition <- \"under\"", fixed = TRUE)
  expect_match(server_txt, "apply_plotly_underline_render_hook <- function", fixed = TRUE)
  expect_match(server_txt, "node.style.textDecoration = 'underline'", fixed = TRUE)
  expect_match(server_txt, "node.setAttribute('text-decoration', 'underline')", fixed = TRUE)
  expect_match(server_txt, "node.style.fontStyle = 'italic'", fixed = TRUE)
  expect_match(server_txt, "node.setAttribute('font-style', 'italic')", fixed = TRUE)
  expect_match(server_txt, "node.style.fontWeight = '700'", fixed = TRUE)
  expect_match(server_txt, "node.setAttribute('font-weight', '700')", fixed = TRUE)
  expect_match(server_txt, "axisTitles: ['.xtitle', '.ytitle'", fixed = TRUE)
  expect_match(server_txt, "axisTitleX: ['.xtitle'", fixed = TRUE)
  expect_match(server_txt, "axisTextY: ['.ytick text'", fixed = TRUE)
  expect_match(server_txt, "groupLabels = group_states", fixed = TRUE)
  expect_match(server_txt, "normalizeLabelText(node.textContent || '')", fixed = TRUE)
  expect_match(server_txt, "scatterlayer .textpoint text", fixed = TRUE)
  expect_match(server_txt, "bartext text", fixed = TRUE)
  expect_match(server_txt, "gd.on('plotly_afterplot', apply)", fixed = TRUE)
  expect_match(server_txt, "gsub(\"</?u\\\\b[^>]*>\", \"\", out, ignore.case = TRUE)", fixed = TRUE)
  expect_false(grepl("paste0\\(\"<u>\"", server_txt, fixed = FALSE))
})
