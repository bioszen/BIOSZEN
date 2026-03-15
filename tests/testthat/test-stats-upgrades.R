library(testthat)

root <- normalizePath(testthat::test_path("..", ".."))
source(file.path(root, "inst", "app", "helpers.R"))
source(file.path(root, "inst", "app", "stats", "stats_upgrades.R"))

make_upgrade_df <- function() {
  data.frame(
    Label = factor(rep(c("A", "B", "C"), each = 8)),
    BiologicalReplicate = rep(rep(1:4, each = 2), times = 3),
    Valor = c(
      rnorm(8, mean = 1.0, sd = 0.1),
      rnorm(8, mean = 1.4, sd = 0.1),
      rnorm(8, mean = 1.8, sd = 0.1)
    )
  )
}

test_that("pairwise_effect_sizes computes CI and adjusted bootstrap p-values", {
  set.seed(123)
  df <- make_upgrade_df()
  res <- pairwise_effect_sizes(df, n_boot = 200, adjust_method = "holm")

  expect_s3_class(res, "tbl_df")
  expect_true(nrow(res) > 0)
  expect_true(all(c(
    "group1", "group2", "cohen_d", "cliffs_delta",
    "fold_change", "p.bootstrap", "p.bootstrap.adj"
  ) %in% names(res)))
  expect_true(all(is.na(res$p.bootstrap.adj) | (res$p.bootstrap.adj >= 0 & res$p.bootstrap.adj <= 1)))
})

test_that("pairwise_resampling_tests supports permutation mode", {
  set.seed(321)
  df <- make_upgrade_df()
  res <- pairwise_resampling_tests(
    df,
    method = "permutation",
    n_iter = 300,
    adjust_method = "fdr"
  )

  expect_s3_class(res, "tbl_df")
  expect_true(nrow(res) > 0)
  expect_true(all(c("mean_diff", "ci_low", "ci_high", "p.value", "p.adj") %in% names(res)))
  expect_true(all(is.na(res$p.adj) | (res$p.adj >= 0 & res$p.adj <= 1)))
})

test_that("apply_multitest_preset adds adjusted column", {
  df <- data.frame(p.value = c(0.01, 0.03, 0.2, NA_real_))
  out <- apply_multitest_preset(df, method = "holm", out_col = "p.holm")
  expect_true("p.holm" %in% names(out))
  expect_true(is.na(out$p.holm[4]))
  expect_true(all(out$p.holm[1:3] >= out$p.value[1:3], na.rm = TRUE))
})

test_that("mixed_model_summary returns a one-row summary table", {
  df <- make_upgrade_df()
  res <- mixed_model_summary(df)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 1)
  expect_true(all(c("model", "term", "p.value", "n", "note") %in% names(res)))
})

test_that("correlation_matrix_with_p returns square matrices and tidy table", {
  df <- data.frame(
    A = 1:20,
    B = (1:20) * 2 + rnorm(20, 0, 0.01),
    C = rev(1:20)
  )
  res <- correlation_matrix_with_p(df, params = c("A", "B", "C"), method = "spearman", adjust_method = "holm")

  expect_true(is.matrix(res$cor))
  expect_true(is.matrix(res$p))
  expect_equal(nrow(res$cor), 3)
  expect_equal(ncol(res$cor), 3)
  expect_s3_class(res$tidy, "data.frame")
  expect_true(nrow(res$tidy) == 9)
})
