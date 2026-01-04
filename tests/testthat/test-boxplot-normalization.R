library(testthat)

test_that("boxplot build succeeds with normalized data from platemapValidacionUR", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("plotly")

  data_path <- testthat::test_path("..", "..", "platemapValidacionUR_22.12.25.xlsx")
  skip_if_not(file.exists(data_path))

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

  agg_norm <- normalize_params(agg, params = present, do_norm = TRUE, ctrl_medium = "Mock")

  scope_df <- agg_norm %>% dplyr::filter(Strain == "AV02")
  names(scope_df) <- enc2utf8(names(scope_df))

  rawParam <- enc2utf8(params[1])
  col_sel  <- paste0(rawParam, "_Norm")
  expect_true(col_sel %in% names(scope_df))

  scope_df[[col_sel]] <- suppressWarnings(as.numeric(scope_df[[col_sel]]))
  df <- scope_df %>%
    dplyr::filter(is.finite(.data[[col_sel]]), !is.na(Media))

  expect_gt(nrow(df), 0)
  p <- ggplot2::ggplot(df, ggplot2::aes(Media, .data[[col_sel]], fill = Media)) +
    ggplot2::geom_boxplot(coef = 1e6, na.rm = TRUE)
  expect_error(ggplot2::ggplot_build(p), NA)
  expect_error(plotly::ggplotly(p, originalData = FALSE), NA)
})
