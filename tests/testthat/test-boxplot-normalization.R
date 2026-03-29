library(testthat)

test_that("boxplot build succeeds with normalized data from standard reference fixture", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("plotly")

  data_path <- testthat::test_path(
    "..", "..", "inst", "app", "www", "reference_files", "Ejemplo_platemap_parametros.xlsx"
  )
  expect_true(
    file.exists(data_path),
    info = "Missing standardized reference fixture: inst/app/www/reference_files/Ejemplo_platemap_parametros.xlsx"
  )

  old <- setwd(testthat::test_path("..", "..", "inst", "app"))
  on.exit(setwd(old), add = TRUE)

  source(testthat::test_path("..", "..", "inst", "app", "helpers.R"))
  source(testthat::test_path("..", "..", "inst", "app", "global.R"))

  prep <- prepare_platemap(
    read_excel_tmp(data_path, "Datos"),
    read_excel_tmp(data_path, "PlotSettings")
  )

  params <- prep$cfg$Parameter
  present <- intersect(params, names(prep$datos))

  agg <- prep$datos %>%
    dplyr::filter(!is.na(Strain), !is.na(Media), Strain != "C-") %>%
    dplyr::group_by(Strain, Media, BiologicalReplicate) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(present), ~ mean(.x, na.rm = TRUE)),
      Orden = dplyr::first(Orden),
      .groups = "drop"
    )

  media_vals <- unique(as.character(stats::na.omit(agg$Media)))
  ctrl_medium <- if ("Mock" %in% media_vals) "Mock" else media_vals[[1]]
  agg_norm <- normalize_params(agg, params = present, do_norm = TRUE, ctrl_medium = ctrl_medium)

  norm_cols <- names(agg_norm)[grepl("_Norm$", names(agg_norm))]
  expect_gt(length(norm_cols), 0)

  finite_norm_cols <- norm_cols[vapply(norm_cols, function(col) {
    vals <- suppressWarnings(as.numeric(agg_norm[[col]]))
    any(is.finite(vals))
  }, logical(1))]
  expect_gt(length(finite_norm_cols), 0)

  col_sel <- finite_norm_cols[[1]]
  agg_norm$.norm_val <- suppressWarnings(as.numeric(agg_norm[[col_sel]]))
  df <- agg_norm %>%
    dplyr::filter(is.finite(.norm_val), !is.na(Media))

  expect_gt(nrow(df), 0)
  p <- ggplot2::ggplot(df, ggplot2::aes(Media, .data$.norm_val, fill = Media)) +
    ggplot2::geom_boxplot(coef = 1e6, na.rm = TRUE)
  expect_error(ggplot2::ggplot_build(p), NA)
  expect_error(plotly::ggplotly(p, originalData = TRUE), NA)
})
