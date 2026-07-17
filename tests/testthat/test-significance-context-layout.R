library(testthat)

source(app_test_path("helpers.R"))

test_that("significance context keys isolate parameter and normalization state", {
  base <- list(
    dataset_key = "dataset-a",
    scope = "Por Cepa",
    strain = "BY4742",
    plot_type = "Boxplot"
  )

  key_a <- do.call(bioszen_significance_context_key, c(base, list(parameter = "A")))
  key_b <- do.call(bioszen_significance_context_key, c(base, list(parameter = "B")))
  key_a_again <- do.call(bioszen_significance_context_key, c(base, list(parameter = "A")))
  key_a_norm <- do.call(
    bioszen_significance_context_key,
    c(base, list(parameter = "A", normalized = TRUE, normalization_control = "Control"))
  )

  expect_identical(key_a, key_a_again)
  expect_false(identical(key_a, key_b))
  expect_false(identical(key_a, key_a_norm))
  expect_identical(
    bioszen_significance_context_key(
      dataset_key = "dataset-a",
      scope = "Por Cepa",
      strain = "BY4742",
      plot_type = "Boxplot",
      parameter = "A"
    ),
    bioszen_significance_context_key(
      dataset_key = "dataset-a",
      scope = "Por Cepa",
      strain = "BY4742",
      plot_type = "Violin",
      parameter = "A"
    )
  )
})

test_that("stacked significance contexts are stable but independent", {
  key_ab <- bioszen_significance_context_key(
    dataset_key = "dataset-a",
    scope = "Combinado",
    plot_type = "Apiladas",
    stacked_parameters = c("B", "A")
  )
  key_ba <- bioszen_significance_context_key(
    dataset_key = "dataset-a",
    scope = "Combinado",
    plot_type = "Apiladas",
    stacked_parameters = c("A", "B")
  )
  key_ac <- bioszen_significance_context_key(
    dataset_key = "dataset-a",
    scope = "Combinado",
    plot_type = "Apiladas",
    stacked_parameters = c("A", "C")
  )

  expect_identical(key_ab, key_ba)
  expect_false(identical(key_ab, key_ac))
})

test_that("server stores significance bars by the active context", {
  server_txt <- paste(
    readLines(app_test_path("server", "server_main.R"), warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )

  expect_match(server_txt, "sig_store     <- reactiveVal(list())", fixed = TRUE)
  expect_match(server_txt, "bioszen_significance_context_key(", fixed = TRUE)
  expect_match(server_txt, "parameter = input$param", fixed = TRUE)
  expect_match(server_txt, "store[[key]] <- value", fixed = TRUE)
  expect_match(
    server_txt,
    "prepare_sig_results_tbl(\n          sig_raw,\n          adjust_method = multitest_sel",
    fixed = TRUE
  )
})

test_that("sidebar tables stay inside the options panel", {
  ui_txt <- paste(
    readLines(app_test_path("ui", "ui_main.R"), warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )

  expect_match(ui_txt, ".bioszen-sidebar-content .dataTables_wrapper", fixed = TRUE)
  expect_match(ui_txt, "overflow: hidden;", fixed = TRUE)
  expect_match(ui_txt, ".dataTables_paginate > span", fixed = TRUE)
  expect_match(ui_txt, "flex-wrap: wrap;", fixed = TRUE)
  expect_match(
    ui_txt,
    "numericInput('sig_linewidth', tr(\"sig_linewidth\"),\n                                        .4",
    fixed = TRUE
  )
})

test_that("curve plots consume the same typography context as other plots", {
  server_txt <- paste(
    readLines(app_test_path("server", "server_main.R"), warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )
  curve_txt <- paste(
    readLines(app_test_path("graficos", "graficos_curvas.R"), warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )

  expect_match(server_txt, "fs_title = fs_title,\n            fs_axis = fs_axis,\n            fs_legend = fs_legend", fixed = TRUE)
  expect_match(curve_txt, "plot.title = element_text(size = fs_title", fixed = TRUE)
  expect_match(curve_txt, "axis.title = element_text(size = fs_axis", fixed = TRUE)
  expect_false(grepl("plot.title = element_text(size = input$fs_title", curve_txt, fixed = TRUE))
})

test_that("curve point size is independently editable and metadata-backed", {
  ui_txt <- paste(
    readLines(app_test_path("ui", "ui_main.R"), warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )
  server_txt <- paste(
    readLines(app_test_path("server", "server_main.R"), warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )
  curve_txt <- paste(
    readLines(app_test_path("graficos", "graficos_curvas.R"), warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )

  expect_match(ui_txt, "numericInput('curve_pt_size'", fixed = TRUE)
  expect_match(curve_txt, "size = input$curve_pt_size %||% (input$curve_lwd * 2.2)", fixed = TRUE)
  expect_match(server_txt, 'Campo = c("curve_lwd", "curve_pt_size")', fixed = TRUE)
  expect_match(server_txt, 'get_val("curve_pt_size")', fixed = TRUE)
})
