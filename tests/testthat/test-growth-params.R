library(testthat)

root <- normalizePath(testthat::test_path("..", ".."))
source(file.path(root, "inst", "app", "params", "params_growth.R"))

make_growth <- function(well, rate = 0.1, n = 20) {
  time <- seq(0, n - 1)
  jitter <- 1 + 0.01 * sin(time)
  data.frame(
    Well = rep(well, length(time)),
    Time = time,
    Measurements = exp(rate * time) * jitter
  )
}

make_flat_growth <- function(well, value = 1, n = 15) {
  data.frame(
    Well = rep(well, n),
    Time = seq(0, n - 1),
    Measurements = rep(value, n)
  )
}

with_parallel_disabled <- function(code) {
  old <- Sys.getenv("BIOSZEN_PARALLEL", unset = NA)
  on.exit({
    if (is.na(old)) {
      Sys.unsetenv("BIOSZEN_PARALLEL")
    } else {
      Sys.setenv(BIOSZEN_PARALLEL = old)
    }
  }, add = TRUE)
  Sys.setenv(BIOSZEN_PARALLEL = "false")
  force(code)
}

test_that("calculate_growth_rates_robust returns stable order and metrics", {
  skip_if_not_installed("gcplyr")
  with_parallel_disabled({
    df <- rbind(make_growth("B"), make_growth("A"))
    res <- calculate_growth_rates_robust(df)
    expect_equal(nrow(res), 2)
    expect_equal(as.character(res$Well), c("B", "A"))
    mu <- res[[2]]
    expect_true(all(is.finite(mu)))
    expect_true(all(mu > 0))
    expect_equal(res$doub_time, log(2) / mu, tolerance = 1e-6)
    expect_true(all(res$ODmax > 0))
    expect_true(all(res$AUC > 0))
  })
})

test_that("combine_growth_results fills gaps with permissive estimates", {
  skip_if_not_installed("gcplyr")
  with_parallel_disabled({
    df <- rbind(
      make_growth("B", rate = 0.4, n = 15),  # fuera del rango robusto
      make_growth("A", rate = 0.1, n = 15)
    )
    rob <- calculate_growth_rates_robust(df)
    perm <- calculate_growth_rates_permissive(df)
    merged <- combine_growth_results(rob, perm)

    expect_equal(as.character(merged$Well), c("B", "A"))
    b_mu <- merged$µMax[merged$Well == "B"]
    expect_false(any(is.na(b_mu)))
    expect_equal(unname(b_mu), unname(perm$µMax[perm$Well == "B"]))
    expect_true(all(is.finite(merged$AUC[merged$Well == "B"])))
    expect_equal(
      unname(merged$doub_time[merged$Well == "A"]),
      unname(rob$doub_time[rob$Well == "A"])
    )
  })
})

test_that("calculate_growth_rates_permissive returns stable order and metrics", {
  skip_if_not_installed("gcplyr")
  with_parallel_disabled({
    df <- rbind(make_growth("B"), make_growth("A"))
    res <- calculate_growth_rates_permissive(df)
    expect_equal(nrow(res), 2)
    expect_equal(as.character(res$Well), c("B", "A"))
    mu <- res[[2]]
    expect_true(all(is.finite(mu)))
    expect_true(all(mu > 0))
    expect_equal(res$doub_time, log(2) / mu, tolerance = 1e-6)
    expect_true(all(res$ODmax > 0))
    expect_true(all(res$AUC > 0))
  })
})

test_that("calculate_growth_rates_robust tolerates flat growth curves", {
  skip_if_not_installed("gcplyr")
  with_parallel_disabled({
    df <- make_flat_growth("Z", value = 1, n = 12)
    res <- calculate_growth_rates_robust(df)

    expect_equal(nrow(res), 1)
    expect_true(is.na(res[[2]]))
    expect_true(is.na(res$doub_time))
    expect_equal(as.character(res$Well), "Z")
    expect_equal(res$ODmax, 1)
    expect_true(is.na(res$AUC))
  })
})
