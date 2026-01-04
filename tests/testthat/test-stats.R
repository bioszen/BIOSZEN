library(testthat)

root <- normalizePath(testthat::test_path("..", ".."))
source(file.path(root, "inst", "app", "helpers.R"))
source(file.path(root, "inst", "app", "stats", "stats_tests.R"))
source(file.path(root, "inst", "app", "stats", "stats_correlation.R"))

make_stats_df <- function() {
  data.frame(
    Label = factor(rep(c("A", "B", "C"), each = 5)),
    Valor = c(
      1, 1.1, 0.9, 1.05, 0.95,
      2, 2.1, 1.9, 2.05, 1.95,
      3, 3.1, 2.9, 3.05, 2.95
    )
  )
}

test_that("do_anova returns tidy results for Tukey", {
  skip_if_not_installed("broom")
  df <- make_stats_df()
  res <- do_anova(df, post_hoc = "Tukey")
  expect_true("p.adj" %in% names(res))
  expect_true(nrow(res) > 0)
})

test_that("do_anova returns results for Bonferroni", {
  skip_if_not_installed("rstatix")
  df <- make_stats_df()
  res <- do_anova(df, post_hoc = "Bonferroni")
  expect_true("p.adj" %in% names(res))
  expect_true(nrow(res) > 0)
})

test_that("do_anova returns results for Dunnett with control", {
  skip_if_not_installed("DescTools")
  df <- make_stats_df()
  res <- do_anova(df, post_hoc = "Dunnett", control_group = "A")
  expect_true(all(c("grupo1", "grupo2", "p.adj") %in% names(res)))
  expect_true(nrow(res) > 0)
})

test_that("do_kw returns results for Dunn", {
  skip_if_not_installed("rstatix")
  df <- make_stats_df()
  res <- do_kw(df, post_hoc = "Dunn")
  expect_true("p.adj" %in% names(res))
  expect_true(nrow(res) > 0)
})

test_that("run_correlation returns expected estimate", {
  res <- run_correlation(1:10, 1:10)
  expect_equal(unname(res$estimate), 1, tolerance = 1e-12)
})

test_that("safe_pairwise_t returns empty tibble for single group", {
  skip_if_not_installed("rstatix")
  df <- data.frame(Label = factor(rep("A", 3)), Valor = c(1, 1.1, 0.9))
  res <- safe_pairwise_t(df)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 0)
})

test_that("safe_pairwise_wilcox computes pairwise comparison", {
  skip_if_not_installed("rstatix")
  df <- data.frame(
    Label = factor(rep(c("A", "B"), each = 4)),
    Valor = c(1, 1.2, 1.1, 0.9, 2, 2.1, 1.9, 2.2)
  )
  res <- safe_pairwise_wilcox(df)
  expect_s3_class(res, "tbl_df")
  expect_gt(nrow(res), 0)
  expect_true("p.adj" %in% names(res))
  expect_true(all(is.finite(res$p.adj)))
})

test_that("filter_min_obs drops groups with insufficient replicates", {
  df <- data.frame(
    Label = factor(c("A", "A", "B")),
    Valor = c(1, 2, 3)
  )
  trimmed <- filter_min_obs(df, min_n = 2)

  expect_true(all(trimmed$Label == "A"))
  expect_false("B" %in% trimmed$Label)
})

test_that("can_paired detects balanced replication", {
  df_bal <- data.frame(Label = factor(c("A", "A", "B", "B")), Valor = 1:4)
  df_unbal <- data.frame(Label = factor(c("A", "B", "B")), Valor = 1:3)

  expect_true(can_paired(df_bal))
  expect_false(can_paired(df_unbal))
})
