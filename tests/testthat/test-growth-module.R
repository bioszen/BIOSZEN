library(testthat)

root <- normalizePath(testthat::test_path("..", ".."))
source(file.path(root, "inst", "app", "server", "growth_module.R"))
source(file.path(root, "inst", "app", "params", "params_growth.R"))

test_that("growth module generates curve and parameter workbooks", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")
  skip_if_not_installed("gcplyr")
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("dplyr")

  library(shiny)
  library(dplyr)

  n_points <- 24
  tmp_input <- tempfile("growth_input_", fileext = ".xlsx")
  on.exit(unlink(tmp_input, recursive = TRUE), add = TRUE)

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Sheet1")
  mod1 <- 1 + 0.01 * sin(seq_len(n_points))
  mod2 <- 1 + 0.015 * cos(seq_len(n_points))
  raw_data <- data.frame(
    Ignore1 = seq_len(n_points),
    Ignore2 = seq_len(n_points),
    W1 = exp(seq(0, by = 0.12, length.out = n_points)) * mod1,
    W2 = exp(seq(0, by = 0.09, length.out = n_points)) * 1.05 * mod2
  )
  openxlsx::writeData(wb, "Sheet1", raw_data, startRow = 3, colNames = TRUE)
  openxlsx::saveWorkbook(wb, tmp_input, overwrite = TRUE)

  shiny::testServer(setup_growth_module, {
    session$setInputs(
      maxTime = n_points - 1,
      timeInterval = 1,
      growthFiles = data.frame(
        datapath = tmp_input,
        name = basename(tmp_input),
        stringsAsFactors = FALSE
      ),
      runGrowth = 1
    )
    session$flushReact()

    growth_dir <- file.path(tempdir(), "growth_results")
    on.exit(unlink(growth_dir, recursive = TRUE), add = TRUE)
    expect_true(dir.exists(growth_dir))

    curvas <- list.files(growth_dir, pattern = "^(Curvas|Curves)_.*\\.xlsx$", full.names = TRUE)
    params <- list.files(growth_dir, pattern = "^(Parametros|Parameters)_.*\\.xlsx$", full.names = TRUE)
    expect_length(curvas, 1)
    expect_length(params, 1)
    expect_equal(openxlsx::getSheetNames(params[[1]]), "Resultados Combinados")

    res <- readxl::read_excel(params[[1]])
    expect_gt(nrow(res), 0)
    expect_true(all(res$ODmax > 0))

    mu_col <- names(res)[2]
    expect_true(is.numeric(res[[mu_col]]))
    expect_true(all(is.finite(res[[mu_col]])))
  })
})

test_that("growth module extracts growth parameters per well with expected fields", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")
  skip_if_not_installed("gcplyr")
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("dplyr")

  library(shiny)
  library(dplyr)

  n_points <- 28
  tmp_input <- tempfile("growth_input_fields_", fileext = ".xlsx")
  on.exit(unlink(tmp_input, recursive = TRUE), add = TRUE)

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Sheet1")
  t <- seq_len(n_points)
  raw_data <- data.frame(
    Ignore1 = t,
    Ignore2 = t,
    W1 = exp(seq(0, by = 0.11, length.out = n_points)) * (1 + 0.01 * sin(t)),
    W2 = exp(seq(0, by = 0.08, length.out = n_points)) * 1.03 * (1 + 0.01 * cos(t)),
    W3 = exp(seq(0, by = 0.06, length.out = n_points)) * 0.98 * (1 + 0.012 * sin(t / 2))
  )
  openxlsx::writeData(wb, "Sheet1", raw_data, startRow = 3, colNames = TRUE)
  openxlsx::saveWorkbook(wb, tmp_input, overwrite = TRUE)

  shiny::testServer(setup_growth_module, {
    session$setInputs(
      maxTime = n_points - 1,
      timeInterval = 1,
      growthFiles = data.frame(
        datapath = tmp_input,
        name = basename(tmp_input),
        stringsAsFactors = FALSE
      ),
      runGrowth = 1
    )
    session$flushReact()

    growth_dir <- file.path(tempdir(), "growth_results")
    on.exit(unlink(growth_dir, recursive = TRUE), add = TRUE)
    params <- list.files(growth_dir, pattern = "^(Parametros|Parameters)_.*\\.xlsx$", full.names = TRUE)
    expect_length(params, 1)

    res <- readxl::read_excel(params[[1]])
    expected_cols <- c("Well", "µMax", "ODmax", "AUC", "lag_time", "max_percap_time", "doub_time", "max_time")
    expect_equal(names(res), expected_cols)

    expect_equal(nrow(res), 3)
    expect_setequal(as.character(res$Well), c("W1", "W2", "W3"))

    expect_true(all(is.finite(res$ODmax)))
    expect_true(all(res$ODmax > 0))
    expect_true(all(is.finite(res$AUC)))
    expect_true(all(res$AUC > 0))

    expect_true(all(is.finite(res$`µMax`)))
    expect_true(all(res$`µMax` > 0))

    expect_equal(
      unname(res$doub_time),
      unname(log(2) / res$`µMax`),
      tolerance = 1e-6
    )
  })
})

test_that("batch growth path preserves legacy per-well results on synthetic curves", {
  skip_if_not_installed("gcplyr")
  skip_if_not_installed("dplyr")
  library(dplyr)

  legacy_growth_results <- function(tidy_df) {
    wells <- unique(tidy_df$Well)
    all_results <- vector("list", length(wells))
    for (k in seq_along(wells)) {
      w <- wells[k]
      df_w <- tidy_df %>%
        dplyr::filter(Well == w) %>%
        dplyr::mutate(Well = factor(Well, levels = wells), Time = as.numeric(Time))
      robust <- calculate_growth_rates_robust(df_w)
      permissive <- calculate_growth_rates_permissive(df_w)
      combined <- combine_growth_results(robust, permissive)
      combined$Well <- w
      all_results[[k]] <- combined
    }
    dplyr::bind_rows(all_results) %>%
      dplyr::mutate(Well = factor(Well, levels = wells)) %>%
      dplyr::arrange(Well) %>%
      dplyr::select(Well, µMax, ODmax, AUC, lag_time, max_percap_time, doub_time, max_time)
  }

  n_points <- 30
  time <- rep(seq(0, n_points - 1), 2)
  tidy_df <- data.frame(
    Time = time,
    Well = rep(c("W1", "W2"), each = n_points),
    Measurements = c(
      exp(seq(0, by = 0.12, length.out = n_points)) * (1 + 0.01 * sin(seq_len(n_points))),
      exp(seq(0, by = 0.42, length.out = n_points)) * (1 + 0.01 * cos(seq_len(n_points)))
    )
  )

  legacy <- legacy_growth_results(tidy_df)
  batch <- compute_growth_results_batch(tidy_df)

  expect_identical(as.character(batch$Well), as.character(legacy$Well))
  expect_identical(names(batch), names(legacy))
  for (col in setdiff(names(batch), "Well")) {
    expect_equal(unname(batch[[col]]), unname(legacy[[col]]), tolerance = sqrt(.Machine$double.eps))
  }
})
