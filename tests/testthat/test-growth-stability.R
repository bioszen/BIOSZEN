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

test_that("growth uploads are copied before processing", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("readxl")

  tmp_input <- tempfile("growth_upload_original_", fileext = ".xlsx")
  make_growth_input_file(tmp_input, n_points = 12, n_wells = 2)

  cache_parent <- tempfile("growth_upload_cache_")
  copied <- .bioszen_copy_growth_uploads(
    files = tmp_input,
    names = basename(tmp_input),
    parent_dir = cache_parent
  )
  on.exit(unlink(cache_parent, recursive = TRUE), add = TRUE)

  expect_true(file.exists(copied$files[[1]]))
  unlink(tmp_input)

  built <- .bioszen_build_curves_sheet(copied$files[[1]], max_time = 11, time_interval = 1)
  expect_identical(built$format, "raw")
  expect_true(all(c("Time", "W1", "W2") %in% names(built$new_data)))
})

test_that("growth output stems are filesystem-safe and unique", {
  names <- c("Plate 1: Growth?.xlsx", "Plate 1/Growth*.xlsx", "", NA)
  stems <- .bioszen_unique_growth_stems(names)

  expect_length(stems, length(names))
  expect_length(unique(stems), length(stems))
  expect_false(any(grepl('[<>:"/\\\\|?*]', stems)))
  expect_true(all(nzchar(stems)))
})

test_that("growth backgrounding is not wired as cancellation", {
  ui_file <- app_test_path("ui", "ui_main.R")
  module_file <- app_test_path("server", "growth_module.R")
  server_file <- app_test_path("server", "server_main.R")

  ui_txt <- paste(readLines(ui_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  module_txt <- paste(readLines(module_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  server_txt <- paste(readLines(server_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_false(grepl("growth_progress_closed", ui_txt, fixed = TRUE))
  expect_false(grepl("growth_progress_closed", module_txt, fixed = TRUE))
  expect_match(module_txt, "session_closed\\s*<-\\s*TRUE", perl = TRUE)
  expect_match(module_txt, "\\.bioszen_copy_growth_uploads\\(", perl = TRUE)
  expect_match(server_txt, "allowReconnect\\(TRUE\\)", fixed = FALSE)
  expect_match(server_txt, "\\.bioszen_growth_has_active_jobs", fixed = FALSE)
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

test_that("same growth file rerun starts from clean results", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")
  skip_if_not_installed("gcplyr")
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("dplyr")

  old_force <- getOption("bioszen_growth_force_sync", NA)
  on.exit(options(bioszen_growth_force_sync = old_force), add = TRUE)
  options(bioszen_growth_force_sync = TRUE)

  tmp_input <- tempfile("growth_same_file_", fileext = ".xlsx")
  on.exit(unlink(tmp_input, recursive = TRUE), add = TRUE)
  make_growth_input_file(tmp_input, n_points = 18, n_wells = 2)

  shiny::testServer(setup_growth_module, {
    growth_payload <- data.frame(datapath = tmp_input, name = basename(tmp_input), stringsAsFactors = FALSE)
    session$setInputs(maxTime = 17, timeInterval = 1, growthFiles = growth_payload, runGrowth = 1)
    session$flushReact()

    growth_dir <- file.path(tempdir(), "growth_results")
    stale_file <- file.path(growth_dir, "Parameters_stale_previous_error.xlsx")
    openxlsx::write.xlsx(data.frame(Well = "STALE", value = 1), stale_file, rowNames = FALSE)
    expect_true(file.exists(stale_file))

    session$setInputs(maxTime = 17, timeInterval = 1, growthFiles = growth_payload, runGrowth = 2)
    session$flushReact()

    params <- list.files(growth_dir, pattern = "^(Parametros|Parameters)_.*\\.xlsx$", full.names = TRUE)
    expect_length(params, 1)
    expect_false(any(basename(params) == basename(stale_file)))
    expect_identical(status_text(), "Completed.")
  })
})

test_that("failed growth upload preparation clears stale results first", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("openxlsx")

  stale_dir <- file.path(tempdir(), "growth_results")
  dir.create(stale_dir, recursive = TRUE, showWarnings = FALSE)
  stale_file <- file.path(stale_dir, "Parameters_stale_failed_attempt.xlsx")
  openxlsx::write.xlsx(data.frame(Well = "STALE", value = 1), stale_file, rowNames = FALSE)
  expect_true(file.exists(stale_file))

  shiny::testServer(setup_growth_module, {
    session$setInputs(
      maxTime = 17,
      timeInterval = 1,
      growthFiles = data.frame(
        datapath = file.path(tempdir(), "missing_growth_input.xlsx"),
        name = "missing_growth_input.xlsx",
        stringsAsFactors = FALSE
      ),
      runGrowth = 1
    )
    session$flushReact()

    params <- list.files(stale_dir, pattern = "^(Parametros|Parameters)_.*\\.xlsx$", full.names = TRUE)
    expect_length(params, 0)
    expect_true(grepl("^Error:", status_text()))
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
