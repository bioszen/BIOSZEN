library(testthat)

root <- app_test_root()
source(app_test_path( "server", "growth_module.R"))
source(app_test_path( "params", "params_growth.R"))

make_growth_input_file <- function(path, n_points = 24, n_wells = 3) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Sheet1")
  t <- seq_len(n_points)
  dat <- data.frame(Ignore1 = t, Ignore2 = t)
  for (i in seq_len(n_wells)) {
    rate <- 0.06 + 0.02 * i
    dat[[paste0("W", i)]] <- exp(seq(0, by = rate, length.out = n_points)) * (1 + 0.01 * sin(t + i))
  }
  openxlsx::writeData(wb, "Sheet1", dat, startRow = 3, colNames = TRUE)
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}

test_that("growth module async execution completes without hanging", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("later")
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")
  skip_if_not_installed("gcplyr")
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("dplyr")

  old_force <- getOption("bioszen_growth_force_sync", NA)
  on.exit(options(bioszen_growth_force_sync = old_force), add = TRUE)
  options(bioszen_growth_force_sync = FALSE)

  tmp_input <- tempfile("growth_async_", fileext = ".xlsx")
  on.exit(unlink(tmp_input, recursive = TRUE), add = TRUE)
  make_growth_input_file(tmp_input, n_points = 22, n_wells = 3)

  shiny::testServer(setup_growth_module, {
    session$setInputs(
      maxTime = 21,
      timeInterval = 1,
      growthFiles = data.frame(
        datapath = tmp_input,
        name = basename(tmp_input),
        stringsAsFactors = FALSE
      ),
      runGrowth = 1
    )

    growth_dir <- file.path(tempdir(), "growth_results")
    done <- FALSE
    params <- character(0)
    for (i in seq_len(800)) {
      later::run_now(0.01)
      session$flushReact()
      params <- list.files(growth_dir, pattern = "^(Parametros|Parameters)_.*\\.xlsx$", full.names = TRUE)
      if (length(params) == 1) {
        done <- TRUE
        break
      }
    }

    expect_true(done)
    res <- readxl::read_excel(params[[1]])
    expect_gt(nrow(res), 0)
  })
})

test_that("compute_growth_results_batch supports fast cancellation", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("gcplyr")

  n_wells <- 8
  n_points <- 80
  wells <- paste0("W", seq_len(n_wells))
  time <- seq(0, n_points - 1)
  rows <- lapply(seq_along(wells), function(i) {
    data.frame(
      Time = time,
      Well = wells[i],
      Measurements = exp((0.05 + i * 0.01) * time) * (1 + 0.01 * cos(time + i))
    )
  })
  tidy_df <- dplyr::bind_rows(rows)

  tick <- 0L
  should_abort <- function() {
    tick <<- tick + 1L
    tick > 400L
  }

  elapsed <- system.time({
    expect_error(
      compute_growth_results_batch(tidy_df, should_abort = should_abort),
      class = "bioszen_growth_cancelled"
    )
  })[["elapsed"]]

  expect_lt(elapsed, 20)
})

test_that("growth module can execute repeated runs in one session", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")
  skip_if_not_installed("gcplyr")
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("dplyr")

  old_force <- getOption("bioszen_growth_force_sync", NA)
  on.exit(options(bioszen_growth_force_sync = old_force), add = TRUE)
  options(bioszen_growth_force_sync = TRUE)

  tmp1 <- tempfile("growth_run1_", fileext = ".xlsx")
  tmp2 <- tempfile("growth_run2_", fileext = ".xlsx")
  on.exit(unlink(c(tmp1, tmp2), recursive = TRUE), add = TRUE)
  make_growth_input_file(tmp1, n_points = 20, n_wells = 2)
  make_growth_input_file(tmp2, n_points = 18, n_wells = 2)

  shiny::testServer(setup_growth_module, {
    session$setInputs(
      maxTime = 19,
      timeInterval = 1,
      growthFiles = data.frame(datapath = tmp1, name = basename(tmp1), stringsAsFactors = FALSE),
      runGrowth = 1
    )
    session$flushReact()

    growth_dir <- file.path(tempdir(), "growth_results")
    params1 <- list.files(growth_dir, pattern = "^(Parametros|Parameters)_.*\\.xlsx$", full.names = TRUE)
    expect_length(params1, 1)

    session$setInputs(
      maxTime = 17,
      timeInterval = 1,
      growthFiles = data.frame(datapath = tmp2, name = basename(tmp2), stringsAsFactors = FALSE),
      runGrowth = 2
    )
    session$flushReact()

    params2 <- list.files(growth_dir, pattern = "^(Parametros|Parameters)_.*\\.xlsx$", full.names = TRUE)
    expect_length(params2, 1)
    expect_false(identical(basename(params1), basename(params2)))
  })
})

test_that("growth module reports errors and remains usable for a new run", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")
  skip_if_not_installed("gcplyr")
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("dplyr")

  old_force <- getOption("bioszen_growth_force_sync", NA)
  on.exit(options(bioszen_growth_force_sync = old_force), add = TRUE)
  options(bioszen_growth_force_sync = TRUE)

  bad_input <- tempfile("growth_bad_", fileext = ".xlsx")
  good_input <- tempfile("growth_good_", fileext = ".xlsx")
  on.exit(unlink(c(bad_input, good_input), recursive = TRUE), add = TRUE)

  writexl::write_xlsx(
    data.frame(
      A = letters[1:8],
      B = letters[1:8],
      stringsAsFactors = FALSE
    ),
    bad_input
  )
  make_growth_input_file(good_input, n_points = 18, n_wells = 2)

  shiny::testServer(setup_growth_module, {
    session$setInputs(
      maxTime = 17,
      timeInterval = 1,
      growthFiles = data.frame(datapath = bad_input, name = basename(bad_input), stringsAsFactors = FALSE),
      runGrowth = 1
    )
    session$flushReact()

    expect_true(grepl("^Error:", status_text()))
    expect_false(isTRUE(growth_running()))

    session$setInputs(
      maxTime = 17,
      timeInterval = 1,
      growthFiles = data.frame(datapath = good_input, name = basename(good_input), stringsAsFactors = FALSE),
      runGrowth = 2
    )
    session$flushReact()

    growth_dir <- file.path(tempdir(), "growth_results")
    params <- list.files(growth_dir, pattern = "^(Parametros|Parameters)_.*\\.xlsx$", full.names = TRUE)
    expect_length(params, 1)
    res <- readxl::read_excel(params[[1]])
    expect_gt(nrow(res), 0)
    expect_identical(status_text(), "Completed.")
    expect_false(isTRUE(growth_running()))
  })
})
