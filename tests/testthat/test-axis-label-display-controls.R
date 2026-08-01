library(testthat)

test_that("axis label display helper preserves the 45 degree default", {
  env <- new.env(parent = globalenv())
  env$`%||%` <- function(x, y) if (is.null(x)) y else x
  env$BIOSZEN_CSS_DPI <- 96
  sys.source(app_test_path("helpers.R"), envir = env)

  unchanged <- env$bioszen_axis_label_spec(c("Long label", "Other label"))
  expect_identical(unchanged$labels, c("Long label", "Other label"))
  expect_equal(unchanged$angle, 45)
  expect_equal(unchanged$hjust, 1)

  wrapped <- env$bioszen_axis_label_spec(
    c("Long group label", "Other group label"),
    angle_input = 0,
    wrap = TRUE,
    wrap_lines = 2,
    wrap_fun = function(x, lines) paste0(x, "::", lines)
  )
  expect_identical(wrapped$labels, c("Long group label::2", "Other group label::2"))
  expect_equal(wrapped$angle, 0)
  expect_equal(wrapped$hjust, 0.5)
})

test_that("heatmap and correlation matrix expose and restore X label controls", {
  ui_text <- paste(
    readLines(app_test_path("ui", "ui_main.R"), warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )
  server_text <- paste(
    readLines(app_test_path("server", "server_main.R"), warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )

  expect_match(
    ui_text,
    "\\['Boxplot','Barras','Violin','Apiladas','Heatmap','MatrizCorrelacion'\\]",
    perl = TRUE
  )
  expect_match(ui_text, 'numericInput\\(\\s*"x_angle"', perl = TRUE)
  expect_match(ui_text, 'checkboxInput\\(\\s*"x_wrap"', perl = TRUE)
  expect_match(ui_text, 'numericInput\\(\\s*"x_wrap_lines"', perl = TRUE)
  expect_equal(
    lengths(regmatches(
      server_text,
      gregexpr("bioszen_axis_label_spec(", server_text, fixed = TRUE)
    )),
    2L
  )
  expect_match(
    server_text,
    "bioszen_x_axis_metadata_rows(",
    fixed = TRUE
  )
  expect_match(server_text, "bioszen_x_axis_metadata_state(meta, metadata_plot_type)", fixed = TRUE)
  expect_match(server_text, "if (identical(tipo, input$tipo)) apply_metadata(meta)", fixed = TRUE)
  expect_match(server_text, "meta_store[[as.character(input$tipo[[1]])]]", fixed = TRUE)
})

test_that("X-axis display metadata roundtrips for every supported plot type only", {
  env <- new.env(parent = globalenv())
  env$`%||%` <- function(x, y) if (is.null(x)) y else x
  env$BIOSZEN_CSS_DPI <- 96
  sys.source(app_test_path("helpers.R"), envir = env)

  supported <- c("Boxplot", "Barras", "Violin", "Apiladas", "Heatmap", "MatrizCorrelacion")
  expect_identical(env$bioszen_x_axis_metadata_plot_types(), supported)

  for (plot_type in supported) {
    metadata <- env$bioszen_x_axis_metadata_rows(
      plot_type,
      angle = 35,
      wrap = TRUE,
      wrap_lines = 3
    )
    expect_identical(metadata$Campo, c("x_angle", "x_wrap", "x_wrap_lines"))
    expect_identical(metadata$Valor, c("35", "TRUE", "3"))

    restored <- env$bioszen_x_axis_metadata_state(metadata, plot_type)
    expect_equal(restored$angle, 35)
    expect_true(restored$wrap)
    expect_equal(restored$wrap_lines, 3L)
  }

  expect_equal(nrow(env$bioszen_x_axis_metadata_rows("Curvas")), 0L)
  expect_null(env$bioszen_x_axis_metadata_state(
    data.frame(Campo = "x_angle", Valor = "35"),
    "Correlacion"
  ))

  automatic <- env$bioszen_x_axis_metadata_state(
    env$bioszen_x_axis_metadata_rows("Heatmap", angle = NA_real_),
    "Heatmap"
  )
  expect_true(automatic$angle_present)
  expect_true(is.na(automatic$angle))
})
