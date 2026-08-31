library(testthat)

load_curve_reference_env <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)
    suppressPackageStartupMessages(library(dplyr))
    cached <<- app_test_source_env(
      paths = app_test_path("helpers.R"),
      required = c("curve_pointwise_fisher", "bioszen_significance_stars")
    )
    cached
  }
})

test_that("curve pointwise Fisher result matches an independent calculation", {
  env <- load_curve_reference_env()
  summary <- bioszen_curve_summary_reference()
  d1 <- summary[summary$Label == "A", , drop = FALSE]
  d2 <- summary[summary$Label == "B", , drop = FALSE]

  observed <- env$curve_pointwise_fisher(d1, d2)
  se <- sqrt((d1$SD^2 / d1$N) + (d2$SD^2 / d2$N))
  differences <- d1$Avg - d2$Avg
  point_p <- pmax(
    2 * stats::pnorm(-abs(differences / se)),
    .Machine$double.xmin
  )
  fisher_stat <- -2 * sum(log(point_p))
  expected_p <- stats::pchisq(fisher_stat, df = 2 * length(point_p), lower.tail = FALSE)
  expected_estimate <- stats::weighted.mean(differences, w = 1 / se^2)

  expect_equal(observed$n_points, nrow(d1))
  expect_equal(observed$estimate, expected_estimate, tolerance = 1e-15)
  expect_equal(observed$p_value, expected_p, tolerance = 1e-15)
})

test_that("curve endpoint implementation matches the reference z calculation", {
  endpoint_runner <- bioszen_extract_curve_method_function("S3")
  summary <- bioszen_curve_summary_reference()
  d1 <- summary[summary$Label == "A", , drop = FALSE]
  d2 <- summary[summary$Label == "B", , drop = FALSE]

  observed <- endpoint_runner(d1, d2)
  endpoint <- max(intersect(d1$Time, d2$Time))
  a <- d1[d1$Time == endpoint, , drop = FALSE]
  b <- d2[d2$Time == endpoint, , drop = FALSE]
  expected_estimate <- a$Avg - b$Avg
  expected_se <- sqrt((a$SD^2 / a$N) + (b$SD^2 / b$N))
  expected_p <- 2 * stats::pnorm(-abs(expected_estimate / expected_se))

  expect_equal(observed$estimate, expected_estimate, tolerance = 1e-15)
  expect_equal(observed$p_value, expected_p, tolerance = 1e-15)
  expect_match(observed$comparison_suffix, "t=6", fixed = TRUE)
})

test_that("global curve-shape comparison produces the expected spline interaction test", {
  summary <- bioszen_curve_summary_reference()
  summary$Label <- factor(summary$Label)
  weights <- ifelse(is.finite(summary$SD) & summary$SD > 0, 1 / summary$SD^2, 1)

  null_model <- stats::lm(
    Avg ~ Label + splines::ns(Time, df = 4),
    data = summary,
    weights = weights
  )
  interaction_model <- stats::lm(
    Avg ~ Label * splines::ns(Time, df = 4),
    data = summary,
    weights = weights
  )
  comparison <- stats::anova(null_model, interaction_model)
  p_value <- as.numeric(comparison[2L, "Pr(>F)"])

  expect_true(is.finite(p_value))
  expect_gte(p_value, 0)
  expect_lte(p_value, 1)
  expect_lt(p_value, 0.05)
})

test_that("replicate curve AUC matches trapezoidal and gcplyr references", {
  skip_if_not_installed("gcplyr")
  long <- bioszen_curve_long_reference()
  series <- split(long, list(long$Label, long$BiologicalReplicate), drop = TRUE)

  for (series_name in names(series)) {
    dat <- series[[series_name]]
    expected <- bioszen_trapezoid_auc(dat$Time, dat$Value)
    observed <- gcplyr::auc(x = dat$Time, y = dat$Value)
    expect_equal(as.numeric(observed), expected, tolerance = 1e-15, info = series_name)
  }

  auc_values <- vapply(series, function(dat) {
    as.numeric(gcplyr::auc(x = dat$Time, y = dat$Value))
  }, numeric(1))
  labels <- sub("[.].*$", "", names(auc_values))
  expect_gt(mean(auc_values[labels == "B"]), mean(auc_values[labels == "A"]))

  normality_p <- vapply(
    split(auc_values, labels),
    function(values) stats::shapiro.test(values)$p.value,
    numeric(1)
  )
  selected_test <- if (all(normality_p > 0.05)) "Welch t-test" else "Wilcoxon rank-sum"
  expect_identical(selected_test, "Welch t-test")
  expected_p <- stats::t.test(auc_values ~ factor(labels), var.equal = FALSE)$p.value
  expect_true(is.finite(expected_p))
  expect_lt(expected_p, 0.05)
})

test_that("curve p-value decisions use the same BIOSZEN significance labels", {
  env <- load_curve_reference_env()
  summary <- bioszen_curve_summary_reference()
  observed <- env$curve_pointwise_fisher(
    summary[summary$Label == "A", , drop = FALSE],
    summary[summary$Label == "B", , drop = FALSE]
  )
  expect_identical(
    env$bioszen_significance_stars(observed$p_value, nonsignificant = "ns"),
    if (observed$p_value < 0.0001) "****" else if (observed$p_value < 0.001) "***" else if (observed$p_value < 0.01) "**" else if (observed$p_value <= 0.05) "*" else "ns"
  )
})
