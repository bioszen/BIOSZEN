test_that("p-value display preserves very small values without showing zero", {
  env <- new.env(parent = globalenv())
  env$`%||%` <- function(x, y) if (is.null(x)) y else x
  env$BIOSZEN_CSS_DPI <- 96
  sys.source(app_test_path("helpers.R"), envir = env)

  displayed <- env$bioszen_format_p_value(c(0, 1e-12, 0.0001, 0.05, NA_real_))

  expect_identical(displayed[[1]], "<2.22e-16")
  expect_identical(displayed[[2]], "1.000e-12")
  expect_identical(displayed[[3]], "0.0001")
  expect_identical(displayed[[4]], "0.0500")
  expect_identical(displayed[[5]], "NA")
  expect_false(any(displayed == "0"))
})

test_that("significance stars follow the documented boundary rules exactly", {
  env <- new.env(parent = globalenv())
  env$`%||%` <- function(x, y) if (is.null(x)) y else x
  env$BIOSZEN_CSS_DPI <- 96
  sys.source(app_test_path("helpers.R"), envir = env)

  p <- c(0, 0.000099, 0.0001, 0.000999, 0.001, 0.00999, 0.01, 0.05, 0.050001, NA)
  expected <- c("****", "****", "***", "***", "**", "**", "*", "*", "ns", "")

  expect_identical(
    env$bioszen_significance_stars(p, nonsignificant = "ns"),
    expected
  )
  expect_identical(
    env$bioszen_is_significant(c(0.05, 0.050001, NA_real_)),
    c(TRUE, FALSE, FALSE)
  )
})

test_that("all statistical display surfaces use the shared p-value rules", {
  server_txt <- paste(
    readLines(app_test_path("server", "server_main.R"), warn = FALSE),
    collapse = "\n"
  )
  corr_txt <- paste(
    readLines(app_test_path("graficos", "graficos_correlacion.R"), warn = FALSE),
    collapse = "\n"
  )
  matrix_txt <- paste(
    readLines(app_test_path("graficos", "graficos_corr_matrix.R"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(server_txt, "bioszen_format_p_value(P_value)", fixed = TRUE)
  expect_match(server_txt, "bioszen_significance_stars(P_referencia)", fixed = TRUE)
  expect_match(server_txt, "P_valor_mostrado = bioszen_format_p_value(P_valor)", fixed = TRUE)
  expect_match(corr_txt, "bioszen_format_p_value(cor_res$p.value", fixed = TRUE)
  expect_match(matrix_txt, "stars = bioszen_significance_stars(p.adj)", fixed = TRUE)
})

test_that("significance table removes duplicate p columns only without correction", {
  server_txt <- paste(
    readLines(app_test_path("server", "server_main.R"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(server_txt, 'adjust_method <- input$multitest_method %||% "none"', fixed = TRUE)
  expect_match(server_txt, 'if (identical(adjust_method, "none"))', fixed = TRUE)
  expect_match(server_txt, 'drop_p_cols <- c(drop_p_cols, "P_valor")', fixed = TRUE)
  expect_match(server_txt, 'drop_p_cols <- c("P_referencia", source_p_cols)', fixed = TRUE)
})
