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

    curvas <- list.files(growth_dir, pattern = "^Curvas_.*\\.xlsx$", full.names = TRUE)
    params <- list.files(growth_dir, pattern = "^Parametros_.*\\.xlsx$", full.names = TRUE)
    expect_length(curvas, 1)
    expect_length(params, 1)

    res <- readxl::read_excel(params[[1]])
    expect_gt(nrow(res), 0)
    expect_true(all(res$ODmax > 0))

    mu_col <- names(res)[2]
    expect_true(is.numeric(res[[mu_col]]))
    expect_true(all(is.finite(res[[mu_col]])))
  })
})
