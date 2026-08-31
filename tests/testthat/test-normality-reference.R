library(testthat)

test_that("BIOSZEN normality guards match the R reference functions", {
  skip_if_not_installed("nortest")

  safe_shapiro <- bioszen_extract_server_function("safe_shapiro_test")
  safe_ks <- bioszen_extract_server_function("safe_ks_test")
  safe_ad <- bioszen_extract_server_function("safe_ad_test")
  normal <- bioszen_read_reference("normal.csv")

  for (label in unique(normal$Media)) {
    values <- normal$Value[normal$Media == label]

    observed_shapiro <- safe_shapiro(values)
    expected_shapiro <- stats::shapiro.test(values)
    expect_equal(observed_shapiro$stat, unname(expected_shapiro$statistic), tolerance = 1e-15)
    expect_equal(observed_shapiro$p, expected_shapiro$p.value, tolerance = 1e-15)

    observed_ks <- safe_ks(values)
    expected_ks <- stats::ks.test(values, "pnorm", mean(values), stats::sd(values))
    expect_equal(observed_ks$stat, unname(expected_ks$statistic), tolerance = 1e-15)
    expect_equal(observed_ks$p, expected_ks$p.value, tolerance = 1e-15)

    observed_ad <- safe_ad(values)
    expected_ad <- nortest::ad.test(values)
    expect_equal(observed_ad$stat, unname(expected_ad$statistic), tolerance = 1e-15)
    expect_equal(observed_ad$p, expected_ad$p.value, tolerance = 1e-15)
  }
})

test_that("normality decisions distinguish reference normal and skewed data", {
  safe_shapiro <- bioszen_extract_server_function("safe_shapiro_test")
  normal <- bioszen_read_reference("normal.csv")
  skewed <- bioszen_read_reference("non_normal.csv")

  normal_p <- vapply(split(normal$Value, normal$Media), function(x) safe_shapiro(x)$p, numeric(1))
  skewed_p <- vapply(split(skewed$Value, skewed$Media), function(x) safe_shapiro(x)$p, numeric(1))

  expect_true(all(normal_p > 0.05))
  expect_true(all(skewed_p <= 0.05))
})

test_that("normality guards remove missing values and reject constant data", {
  safe_shapiro <- bioszen_extract_server_function("safe_shapiro_test")
  safe_ks <- bioszen_extract_server_function("safe_ks_test")
  safe_ad <- bioszen_extract_server_function("safe_ad_test")

  with_na <- bioszen_read_reference("with_na.csv")$Value
  finite <- with_na[is.finite(with_na)]
  expect_equal(safe_shapiro(with_na)$p, stats::shapiro.test(finite)$p.value, tolerance = 1e-15)

  constant <- bioszen_read_reference("identical_values.csv")$Value
  for (runner in list(safe_shapiro, safe_ks, safe_ad)) {
    result <- runner(constant)
    expect_true(is.na(result$stat))
    expect_true(is.na(result$p))
  }
})
