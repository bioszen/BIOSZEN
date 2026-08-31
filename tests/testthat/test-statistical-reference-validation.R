library(testthat)

load_reference_statistics_env <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)
    suppressPackageStartupMessages({
      library(dplyr)
      library(tibble)
    })
    cached <<- app_test_source_env(
      paths = c(
        app_test_path("helpers.R"),
        app_test_path("stats", "stats_tests.R"),
        app_test_path("stats", "stats_upgrades.R")
      ),
      required = c(
        "do_anova", "do_kw", "safe_pairwise_t", "safe_pairwise_wilcox",
        "can_paired", "bioszen_is_significant", "bioszen_significance_stars",
        "apply_multitest_preset"
      )
    )
    cached
  }
})

order_pairwise_result <- function(result) {
  pair_key <- paste(as.character(result$group1), as.character(result$group2), sep = "|")
  result[order(pair_key), , drop = FALSE]
}

test_that("NIST SiRstv one-way ANOVA matches all certified statistics", {
  dat <- bioszen_reference_test_df("nist_sirstv.csv")
  fit <- stats::aov(Valor ~ Label, data = dat)
  table <- summary(fit)[[1L]]

  observed <- c(
    between_df = table[1L, "Df"],
    between_ss = table[1L, "Sum Sq"],
    between_ms = table[1L, "Mean Sq"],
    within_df = table[2L, "Df"],
    within_ss = table[2L, "Sum Sq"],
    within_ms = table[2L, "Mean Sq"],
    f_value = table[1L, "F value"],
    r_squared = table[1L, "Sum Sq"] / sum(table[, "Sum Sq"]),
    residual_sd = sqrt(table[2L, "Mean Sq"])
  )
  certified <- c(
    between_df = 4,
    between_ss = 5.11462616000000e-02,
    between_ms = 1.27865654000000e-02,
    within_df = 20,
    within_ss = 2.16636560000000e-01,
    within_ms = 1.08318280000000e-02,
    f_value = 1.18046237440255,
    r_squared = 1.90999039051129e-01,
    residual_sd = 1.04076068334656e-01
  )

  expect_equal(nrow(dat), 25L)
  expect_equal(observed[c("between_df", "within_df")], certified[c("between_df", "within_df")])
  expect_equal(observed[setdiff(names(observed), c("between_df", "within_df"))],
               certified[setdiff(names(certified), c("between_df", "within_df"))],
               tolerance = 5e-13)
})

test_that("BIOSZEN ANOVA and Kruskal post-hoc results match rstatix", {
  skip_if_not_installed("rstatix")
  env <- load_reference_statistics_env()
  dat <- bioszen_reference_test_df("three_groups.csv")

  observed_anova <- order_pairwise_result(env$do_anova(dat, post_hoc = "Tukey"))
  expected_anova <- order_pairwise_result(rstatix::tukey_hsd(dat, Valor ~ Label))
  expect_equal(observed_anova$p.adj, expected_anova$p.adj, tolerance = 1e-15)
  expect_equal(observed_anova$estimate, expected_anova$estimate, tolerance = 1e-15)

  observed_kw <- order_pairwise_result(env$do_kw(dat, post_hoc = "Dunn"))
  expected_kw <- order_pairwise_result(
    rstatix::dunn_test(dat, Valor ~ Label, p.adjust.method = "bonferroni")
  )
  expect_equal(observed_kw$p, expected_kw$p, tolerance = 1e-15)
  expect_equal(observed_kw$p.adj, expected_kw$p.adj, tolerance = 1e-15)

  global_anova <- summary(stats::aov(Valor ~ Label, data = dat))[[1L]][1L, "Pr(>F)"]
  global_kw <- stats::kruskal.test(Valor ~ Label, data = dat)$p.value
  expect_lt(global_anova, 0.05)
  expect_lt(global_kw, 0.05)
})

test_that("paired t and Wilcoxon helpers match their R package references", {
  skip_if_not_installed("rstatix")
  env <- load_reference_statistics_env()
  paired <- bioszen_reference_test_df("paired.csv")
  expect_true(env$can_paired(paired))

  observed_t <- env$safe_pairwise_t(paired, method = "holm")
  expected_t <- rstatix::t_test(paired, Valor ~ Label, paired = TRUE) |>
    rstatix::adjust_pvalue(method = "holm")
  expect_equal(observed_t$p, expected_t$p, tolerance = 1e-15)
  expect_equal(observed_t$p.adj, expected_t$p.adj, tolerance = 1e-15)

  ties <- bioszen_reference_test_df("ties.csv")
  observed_w <- suppressWarnings(env$safe_pairwise_wilcox(ties, method = "holm"))
  expected_w <- suppressWarnings(
    rstatix::wilcox_test(ties, Valor ~ Label, paired = TRUE) |>
      rstatix::adjust_pvalue(method = "holm")
  )
  expect_equal(observed_w$p, expected_w$p, tolerance = 1e-15)
  expect_equal(observed_w$p.adj, expected_w$p.adj, tolerance = 1e-15)
})

test_that("unequal sample sizes are unpaired and match Welch t-test parity", {
  skip_if_not_installed("rstatix")
  env <- load_reference_statistics_env()
  dat <- bioszen_reference_test_df("unequal_n.csv")
  expect_false(env$can_paired(dat))

  observed <- env$safe_pairwise_t(dat, method = "none")
  expected <- rstatix::t_test(dat, Valor ~ Label, paired = FALSE) |>
    rstatix::adjust_pvalue(method = "none")
  expect_equal(observed$p, expected$p, tolerance = 1e-15)
  expect_equal(observed$p.adj, expected$p.adj, tolerance = 1e-15)
})

test_that("edge-case fixtures are handled without invalid adjusted probabilities", {
  skip_if_not_installed("rstatix")
  env <- load_reference_statistics_env()

  with_na <- bioszen_reference_test_df("with_na.csv")
  with_na <- with_na[is.finite(with_na$Valor), , drop = FALSE]
  expect_no_error(na_result <- env$safe_pairwise_t(with_na, method = "holm"))
  expect_true("p.adj" %in% names(na_result))
  expect_true(all(is.na(na_result$p.adj) | (na_result$p.adj >= 0 & na_result$p.adj <= 1)))

  identical_values <- bioszen_reference_test_df("identical_values.csv")
  expect_no_error(constant_t <- env$safe_pairwise_t(identical_values, method = "holm"))
  expect_no_error(constant_w <- suppressWarnings(
    env$safe_pairwise_wilcox(identical_values, method = "holm")
  ))
  expect_true(is.data.frame(constant_t))
  expect_true(is.data.frame(constant_w))

  extreme <- bioszen_reference_test_df("extreme_values.csv")
  expect_no_error(extreme_result <- env$safe_pairwise_t(extreme, method = "holm"))
  expect_true(all(is.na(extreme_result$p.adj) |
                    (is.finite(extreme_result$p.adj) & extreme_result$p.adj >= 0 & extreme_result$p.adj <= 1)))
})

test_that("BIOSZEN p adjustments and significance decisions match base R", {
  env <- load_reference_statistics_env()
  raw <- c(0.00005, 0.0005, 0.005, 0.05, 0.05001, NA_real_)
  input <- data.frame(p.value = raw)

  for (method in c("holm", "fdr", "bonferroni", "none")) {
    observed <- env$apply_multitest_preset(
      input,
      p_col = "p.value",
      method = method,
      out_col = "p.adjusted"
    )
    expected <- if (identical(method, "none")) raw else stats::p.adjust(raw, method = method)
    expect_equal(observed$p.adjusted, expected, tolerance = 1e-15, info = method)
  }

  expect_identical(env$bioszen_significance_stars(raw, nonsignificant = "ns"),
                   c("****", "***", "**", "*", "ns", ""))
  expect_identical(env$bioszen_is_significant(raw),
                   c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE))
})
