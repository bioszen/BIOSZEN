library(testthat)

root <- app_test_root()
source(app_test_path( "helpers.R"))

test_that("metadata_filter_design_only removes data-selection keys", {
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
  expect_true(all(c("tipo", "colorMode", "heat_scale_mode") %in% as.character(out$Campo)))
})

test_that("metadata text styles encode and parse normal explicitly", {
  expect_equal(metadata_text_style_value(character(0)), "normal")
  expect_equal(metadata_text_style_value(c("bold", "underline")), "bold,underline")
  expect_equal(metadata_parse_text_style_value("normal"), character(0))
  expect_equal(metadata_parse_text_style_value("plain"), character(0))
  expect_equal(metadata_parse_text_style_value("bold, italic"), c("bold", "italic"))
  expect_equal(metadata_parse_text_style_value("normal,underline"), "underline")
})

test_that("metadata TextStyle sheets can be converted back to metadata rows", {
  style_tbl <- data.frame(
    Text = c("Plot title", "Axis titles"),
    Target = c("title", "axis_titles"),
    FontFamily = c("Georgia", "Georgia"),
    Size = c("22", "14"),
    Style = c("normal", "bold,italic"),
    InputId = c("plot_text_style_title", "plot_text_style_axis_titles"),
    SizeInputId = c("fs_title", "fs_axis"),
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
  expect_equal(rows$Valor[rows$Campo == "fs_title"], "22")
  expect_equal(rows$Valor[rows$Campo == "fs_axis"], "14")
})
