library(testthat)

root <- normalizePath(test_path("..", ".."))
load_numeric_stability_sources <- function() {
  app_dir <- file.path(root, "inst", "app")
  old <- setwd(app_dir)
  on.exit(setwd(old), add = TRUE)
  source(file.path(app_dir, "global.R"))
  source(file.path(app_dir, "helpers.R"))
  source(file.path(app_dir, "stats", "stats_upgrades.R"))
  source(file.path(app_dir, "graficos", "graficos_heatmap.R"))
}
load_numeric_stability_sources()

test_that("correlation_matrix_with_p is stable with constants, ties, and sparse pairs", {
  df <- data.frame(
    A = c(1, 1, 1, 1, 1, 1, 1, 1),
    B = c(2, 3, 4, 5, 6, 7, 8, 9),
    C = c(9, 8, 7, 6, 5, 4, 3, 2),
    D = c(NA, NA, 4, 4, 4, 4, NA, NA),
    E = c(0, 0, 0, 0, 0, 0, 0, 0),
    stringsAsFactors = FALSE
  )

  out <- correlation_matrix_with_p(
    df = df,
    params = c("A", "B", "C", "D", "E"),
    method = "spearman",
    adjust_method = "holm"
  )

  expect_true(is.matrix(out$cor))
  expect_true(is.matrix(out$p))
  expect_equal(nrow(out$cor), 5L)
  expect_equal(unname(diag(out$cor)), rep(1, 5))
  expect_equal(unname(diag(out$p)), rep(0, 5))
  expect_true(is.data.frame(out$tidy))
  expect_equal(nrow(out$tidy), 25L)
})

test_that("heatmap clustering remains stable for near-degenerate parameter rows", {
  set.seed(42)
  df <- data.frame(
    Strain = rep(c("S1", "S2"), each = 12),
    Media = rep(rep(c("M1", "M2", "M3"), each = 4), 2),
    BiologicalReplicate = rep(1:4, 6),
    Orden = rep(c(1L, 2L, 3L), each = 4, times = 2),
    TechnicalReplicate = 1L,
    Well = paste0("W", seq_len(24)),
    stringsAsFactors = FALSE
  )
  df$Label <- paste(df$Strain, df$Media, sep = "-")
  df$P1 <- 1
  df$P2 <- c(rep(0, 12), rep(1, 12))
  df$P3 <- rnorm(24, mean = 0, sd = 1e-6)
  df$P4 <- c(rep(NA_real_, 8), rep(2, 16))
  df$P5 <- seq_len(24) / 10

  payload <- prepare_heatmap_payload(
    df_h = df,
    scope = "Combinado",
    label_mode = FALSE,
    all_params = c("P1", "P2", "P3", "P4", "P5"),
    params_raw = c("P1", "P2", "P3", "P4", "P5"),
    cluster_rows = TRUE,
    cluster_cols = TRUE,
    hclust_method = "average",
    k_rows = 3L,
    k_cols = 2L,
    scale_mode = "row"
  )

  expect_true(is.matrix(payload$mat_vals))
  expect_equal(nrow(payload$mat_vals), 5L)
  expect_true(any(is.finite(payload$mat_vals)))
  expect_true(payload$row_cluster_count >= 1L)
  expect_true(payload$col_cluster_count >= 1L)
})
