library(testthat)

root <- app_test_root()
source(app_test_path( "helpers.R"))

test_that("metadata_filter_design_only removes data-selection keys but keeps restorable plot state", {
  meta <- data.frame(
    Campo = c(
      "tipo", "colorMode", "scope", "strain", "param",
      "doNorm", "ctrlMedium", "heat_scale_mode", "heat_params"
    ),
    Valor = c(
      "Heatmap", "Viridis", "Combinado", "S1", "uMax",
      "TRUE", "Control", "row", "uMax,ODmax"
    ),
    stringsAsFactors = FALSE
  )

  out <- metadata_filter_design_only(meta)
  removed <- metadata_data_keys()

  expect_false(any(as.character(out$Campo) %in% removed))
  expect_true(all(c("tipo", "colorMode", "param", "doNorm", "ctrlMedium", "heat_scale_mode") %in% as.character(out$Campo)))
  expect_false(any(c("scope", "strain", "heat_params") %in% as.character(out$Campo)))
})

test_that("metadata text styles encode and parse normal explicitly", {
  expect_equal(metadata_text_style_value(character(0)), "normal")
  expect_equal(metadata_text_style_value(c("bold", "underline")), "bold,underline")
  expect_equal(metadata_parse_text_style_value("normal"), character(0))
  expect_equal(metadata_parse_text_style_value("plain"), character(0))
  expect_equal(metadata_parse_text_style_value("bold, italic"), c("bold", "italic"))
  expect_equal(metadata_parse_text_style_value("normal,underline"), "underline")
})

test_that("parameter selections normalize legacy normalized metadata names", {
  params <- c("P1", "P2", "area_mean")

  expect_equal(normalize_param_selection("P1", params), "P1")
  expect_equal(normalize_param_selection("P2_Norm", params), "P2")
  expect_equal(normalize_param_selection("area_mean_Norm", params), "area_mean")
  expect_equal(normalize_param_selection("missing_Norm", params), "")
  expect_equal(normalize_param_selection("P3_Norm", character(0)), "P3")
})

test_that("metadata TextStyle sheets can be converted back to metadata rows", {
  style_tbl <- data.frame(
    Text = c("Plot title", "Axis titles", "X axis title", "X axis tick labels", "Data labels"),
    Target = c("title", "axis_titles", "axis_title_x", "axis_text_x", "data_labels"),
    FontFamily = rep("Georgia", 5),
    Size = c("22", "14", "15", "11", "10"),
    Style = c("normal", "bold,italic", "italic,underline", "bold", "bold,italic,underline"),
    InputId = c(
      "plot_text_style_title",
      "plot_text_style_axis_titles",
      "plot_text_style_axis_title_x",
      "plot_text_style_axis_text_x",
      "plot_text_style_data_labels"
    ),
    SizeInputId = c("fs_title", "fs_axis", "fs_axis", "fs_axis", "fs_axis"),
    stringsAsFactors = FALSE
  )

  rows <- metadata_text_style_sheet_rows(
    style_tbl,
    font_field = "plot_font_family",
    style_prefix = "plot_text_style_"
  )

  expect_equal(rows$Valor[rows$Campo == "plot_font_family"], "Georgia")
  expect_equal(rows$Valor[rows$Campo == "plot_text_style_title"], "normal")
  expect_equal(rows$Valor[rows$Campo == "plot_text_style_axis_titles"], "bold,italic")
  expect_equal(rows$Valor[rows$Campo == "plot_text_style_axis_title_x"], "italic,underline")
  expect_equal(rows$Valor[rows$Campo == "plot_text_style_axis_text_x"], "bold")
  expect_equal(rows$Valor[rows$Campo == "plot_text_style_data_labels"], "bold,italic,underline")
  expect_equal(rows$Valor[rows$Campo == "fs_title"], "22")
  expect_equal(rows$Valor[rows$Campo == "fs_axis"], "10")
  expect_equal(metadata_text_target_label("axis_title_x"), "X axis title")
  expect_equal(metadata_text_target_label("axis_text_y"), "Y axis tick labels")
})
