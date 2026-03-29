library(testthat)

root <- normalizePath(test_path("..", ".."))
load_plot_regression_sources <- function() {
  app_dir <- file.path(root, "inst", "app")
  old <- setwd(app_dir)
  on.exit(setwd(old), add = TRUE)
  source(file.path(app_dir, "global.R"))
  source(file.path(app_dir, "helpers.R"))
  source(file.path(app_dir, "stats", "stats_upgrades.R"))
  source(file.path(app_dir, "graficos", "graficos_heatmap.R"))
  source(file.path(app_dir, "graficos", "graficos_corr_matrix.R"))
}
load_plot_regression_sources()

test_that("heatmap rendering payload structure remains stable", {
  df <- data.frame(
    Strain = rep(c("A", "B"), each = 6),
    Media = rep(c("M1", "M2", "M3"), times = 4),
    BiologicalReplicate = rep(1:2, times = 6),
    Orden = rep(c(1L, 2L, 3L), times = 4),
    TechnicalReplicate = 1L,
    Well = paste0("W", seq_len(12)),
    stringsAsFactors = FALSE
  )
  df$Label <- paste(df$Strain, df$Media, sep = "-")
  df$P1 <- c(1, 2, 3, 2, 3, 4, 4, 5, 6, 5, 6, 7)
  df$P2 <- c(7, 6, 5, 6, 5, 4, 4, 3, 2, 3, 2, 1)
  df$P3 <- c(2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5)

  payload <- prepare_heatmap_payload(
    df_h = df,
    scope = "Combinado",
    label_mode = FALSE,
    all_params = c("P1", "P2", "P3"),
    params_raw = c("P1", "P2", "P3"),
    cluster_rows = TRUE,
    cluster_cols = TRUE,
    k_rows = 2L,
    k_cols = 2L,
    scale_mode = "row"
  )

  expect_equal(payload$row_cluster_count, 2L)
  expect_equal(payload$col_cluster_count, 2L)
  expect_true(all(grepl("^\\[R[0-9]+\\] ", payload$row_labels)))
  expect_true(all(grepl("^\\[C[0-9]+\\] ", payload$col_labels)))
  expect_true(length(payload$row_boundaries) >= 1L)
  expect_true(length(payload$col_boundaries) >= 1L)
  expect_true(is.matrix(payload$mat_vals))
  expect_equal(nrow(payload$mat_vals), 3L)
  expect_equal(ncol(payload$mat_vals), 6L)
})

test_that("correlation matrix payload structure remains stable", {
  set.seed(123)
  df <- data.frame(
    P1 = rnorm(24),
    P2 = rnorm(24),
    P3 = rnorm(24),
    P4 = rnorm(24)
  )
  df$P3 <- df$P1 * 0.7 + df$P3 * 0.3
  df$P4 <- -df$P2 * 0.5 + df$P4 * 0.5

  payload <- prepare_corr_matrix_payload(
    df_m = df,
    all_params = c("P1", "P2", "P3", "P4"),
    params_raw = c("P1", "P2", "P3", "P4"),
    high_dim = FALSE,
    do_norm = FALSE,
    has_ctrl_selected = FALSE,
    corr_method = "spearman",
    adjust_method = "holm",
    order_profile = TRUE,
    show_sig_only = TRUE
  )

  mat <- payload$mat_df %>% dplyr::arrange(param_x, param_y)
  expect_equal(nrow(mat), 16L)
  expect_true(all(is.finite(mat$r)))
  expect_true(all(mat$r <= 1 & mat$r >= -1))
  expect_true(all(c("param_x", "param_y", "r", "p.adj", "label_txt") %in% names(mat)))
  expect_true(any(mat$label_txt != ""))
})
