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

test_that("checkpointed growth results match the original strong-first calculation", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("gcplyr")
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("digest")

  n_wells <- 4
  n_points <- 28
  wells <- paste0("W", seq_len(n_wells))
  time <- seq(0, n_points - 1)
  tidy_df <- dplyr::bind_rows(lapply(seq_along(wells), function(i) {
    data.frame(
      Time = time,
      Well = wells[[i]],
      Measurements = exp((0.055 + i * 0.012) * time) * (1 + 0.01 * cos(time + i))
    )
  }))

  full <- compute_growth_results_batch(tidy_df)
  checkpoint_dir <- tempfile("growth_checkpoint_resume_")
  dir.create(checkpoint_dir, recursive = TRUE)
  on.exit(unlink(checkpoint_dir, recursive = TRUE, force = TRUE), add = TRUE)
  checkpoint <- .bioszen_growth_checkpoint(
    checkpoint_dir,
    stem = "checkpoint_plate",
    source_name = "checkpoint_plate.xlsx",
    source_hash = "hash-for-test",
    max_time = max(time),
    time_interval = 1,
    format = "raw"
  )

  partial <- full[1, , drop = FALSE]
  .bioszen_write_growth_checkpoint(checkpoint, partial, completed = FALSE)
  resumed <- compute_growth_results_batch(tidy_df, checkpoint = checkpoint)

  normalize_growth <- function(x) {
    x <- as.data.frame(x)
    x$Well <- as.character(x$Well)
    x
  }
  expect_equal(normalize_growth(resumed), normalize_growth(full), tolerance = 1e-12)
  expect_true(file.exists(checkpoint$rds_file))
  expect_true(file.exists(checkpoint$partial_file))
  saved <- readRDS(checkpoint$rds_file)
  expect_true(isTRUE(saved$completed))
  expect_equal(sort(as.character(saved$results$Well)), sort(wells))
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
  expect_match(ui_txt, "runButton.disabled = running", fixed = TRUE)
  expect_match(ui_txt, "stopButton.disabled = !running", fixed = TRUE)
  expect_match(ui_txt, "browseGrowthOutputDir", fixed = TRUE)
  expect_match(ui_txt, "placeholder = tr_text\\(\"growth_output_dir_placeholder\"", perl = TRUE)
  expect_false(grepl('placeholder = tr\\("growth_output_dir_placeholder"', ui_txt, fixed = TRUE))
  expect_match(module_txt, "safe_show_growth_notification", fixed = TRUE)
  expect_match(module_txt, "with_growth_progress", fixed = TRUE)
  expect_match(module_txt, "\\.bioszen_choose_growth_output_dir", fixed = FALSE)
  expect_match(module_txt, "updateTextInput\\(session, \"growthOutputDir\"", perl = TRUE)
  expect_match(module_txt, "shiny::incProgress\\(amount, detail = detail, session = progress_session\\)", perl = TRUE)
  expect_false(grepl("(^|[^:])\\bwithProgress\\(", module_txt, perl = TRUE))
  expect_match(module_txt, "session_closed\\s*<-\\s*TRUE", perl = TRUE)
  expect_match(module_txt, "growth_state\\$cancel_requested\\s*<-\\s*TRUE", perl = TRUE)
  expect_match(module_txt, "cancel_requested\\(TRUE\\)", fixed = FALSE)
  expect_match(module_txt, "\\.bioszen_copy_growth_uploads\\(", perl = TRUE)
  expect_match(module_txt, "should_stop_on_last_session", fixed = TRUE)
  expect_match(server_txt, "allowReconnect\\(TRUE\\)", fixed = FALSE)
  expect_match(server_txt, "BIOSZEN_STOP_ON_LAST_SESSION", fixed = TRUE)
  expect_match(server_txt, "BIOSZEN.stop_on_last_session\", \"true\"", fixed = TRUE)
  expect_false(grepl("active_sessions == 0 && !keep_running_for_growth\\) shiny::stopApp", server_txt))
  expect_match(server_txt, "\\.bioszen_growth_has_active_jobs", fixed = FALSE)
})

test_that("growth notifications do not crash when Shiny notification transport is unavailable", {
  skip_if_not_installed("shiny")

  shiny::testServer(setup_growth_module, {
    session$sendNotification <- NULL
    session$setInputs(runGrowth = 1)
    session$flushReact()
    expect_match(status_text(), "Select at least one growth file", fixed = TRUE)
    expect_false(isTRUE(growth_running()))
  })
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
      growthFiles = data.frame(datapath = tmp2, name = basename(tmp2), stringsAsFactors = FALSE)
    )
    session$flushReact()
    selected_rows <- growth_files_selected()
    second_id <- selected_rows$id[selected_rows$name == basename(tmp2)]
    session$setInputs(
      maxTime = 17,
      timeInterval = 1,
      growthFilesKeep = second_id,
      runGrowth = 2
    )
    session$flushReact()

    params2 <- list.files(growth_dir, pattern = "^(Parametros|Parameters)_.*\\.xlsx$", full.names = TRUE)
    expect_length(params2, 1)
    expect_false(identical(basename(params1), basename(params2)))
  })
})

test_that("growth output directory receives final files and cleans completed checkpoints", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")
  skip_if_not_installed("gcplyr")
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("digest")

  old_force <- getOption("bioszen_growth_force_sync", NA)
  on.exit(options(bioszen_growth_force_sync = old_force), add = TRUE)
  options(bioszen_growth_force_sync = TRUE)

  tmp_input <- tempfile("growth_autosave_", fileext = ".xlsx")
  output_dir <- tempfile("growth_autosave_output_")
  dir.create(output_dir, recursive = TRUE)
  on.exit(unlink(c(tmp_input, output_dir), recursive = TRUE, force = TRUE), add = TRUE)
  make_growth_input_file(tmp_input, n_points = 18, n_wells = 2)
  prepared <- .bioszen_build_curves_sheet(tmp_input, max_time = 17, time_interval = 1)
  full_tidy <- gcplyr::trans_wide_to_tidy(prepared$new_data, id_cols = "Time")
  full_expected <- compute_growth_results_batch(full_tidy)
  stem <- .bioszen_unique_growth_stems(basename(tmp_input))[[1]]
  checkpoint <- .bioszen_growth_checkpoint(
    output_dir,
    stem = stem,
    source_name = basename(tmp_input),
    source_hash = .bioszen_growth_file_hash(tmp_input),
    max_time = 17,
    time_interval = 1,
    format = prepared$format
  )
  .bioszen_write_growth_checkpoint(checkpoint, full_expected[1, , drop = FALSE], completed = FALSE)
  expect_true(file.exists(checkpoint$rds_file))
  expect_true(file.exists(checkpoint$partial_file))

  shiny::testServer(setup_growth_module, {
    session$setInputs(
      maxTime = 17,
      timeInterval = 1,
      growthOutputDir = output_dir,
      growthFiles = data.frame(datapath = tmp_input, name = basename(tmp_input), stringsAsFactors = FALSE),
      runGrowth = 1
    )
    session$flushReact()

    expect_identical(status_text(), "Completed.")
    final_params <- list.files(output_dir, pattern = "^(Parametros|Parameters)_.*\\.xlsx$", full.names = TRUE)
    final_curves <- list.files(output_dir, pattern = "^(Curvas|Curves)_.*\\.xlsx$", full.names = TRUE)
    expect_length(final_params, 1)
    expect_length(final_curves, 1)
    res <- readxl::read_excel(final_params[[1]])
    normalize_growth <- function(x) {
      x <- as.data.frame(x)
      x$Well <- as.character(x$Well)
      x
    }
    expect_equal(normalize_growth(res), normalize_growth(full_expected), tolerance = 1e-12)
    expect_false(dir.exists(file.path(output_dir, "BIOSZEN_growth_checkpoints")))
  })
})

test_that("growth file selection appends uploads and selected subset drives processing", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")
  skip_if_not_installed("gcplyr")
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("dplyr")

  old_force <- getOption("bioszen_growth_force_sync", NA)
  on.exit(options(bioszen_growth_force_sync = old_force), add = TRUE)
  options(bioszen_growth_force_sync = TRUE)

  tmp1 <- tempfile("growth_append_one_", fileext = ".xlsx")
  tmp2 <- tempfile("growth_append_two_", fileext = ".xlsx")
  on.exit(unlink(c(tmp1, tmp2), recursive = TRUE), add = TRUE)
  make_growth_input_file(tmp1, n_points = 18, n_wells = 2)
  make_growth_input_file(tmp2, n_points = 18, n_wells = 2)

  shiny::testServer(setup_growth_module, {
    session$setInputs(
      growthFiles = data.frame(datapath = tmp1, name = basename(tmp1), stringsAsFactors = FALSE)
    )
    session$flushReact()
    expect_equal(nrow(growth_files_selected()), 1L)

    session$setInputs(
      growthFiles = data.frame(datapath = tmp2, name = basename(tmp2), stringsAsFactors = FALSE)
    )
    session$flushReact()
    selected_rows <- growth_files_selected()
    expect_equal(nrow(selected_rows), 2L)

    second_id <- selected_rows$id[[2]]
    session$setInputs(growthFilesKeep = second_id, maxTime = 17, timeInterval = 1, runGrowth = 1)
    session$flushReact()

    growth_dir <- file.path(tempdir(), "growth_results")
    params <- list.files(growth_dir, pattern = "^(Parametros|Parameters)_.*\\.xlsx$", full.names = TRUE)
    expect_length(params, 1)
    expect_match(basename(params), tools::file_path_sans_ext(basename(tmp2)), fixed = TRUE)
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

test_that("growth stop button leaves session reusable without clearing file selection", {
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

  tmp_input <- tempfile("growth_stop_", fileext = ".xlsx")
  on.exit(unlink(tmp_input, recursive = TRUE), add = TRUE)
  make_growth_input_file(tmp_input, n_points = 120, n_wells = 8)

  shiny::testServer(setup_growth_module, {
    session$setInputs(
      growthFiles = data.frame(datapath = tmp_input, name = basename(tmp_input), stringsAsFactors = FALSE)
    )
    session$flushReact()
    expect_equal(nrow(growth_files_selected()), 1L)

    session$setInputs(maxTime = 119, timeInterval = 1, runGrowth = 1)
    session$flushReact()
    later::later(function() {
      session$setInputs(stopGrowth = 1)
      session$flushReact()
    }, delay = 0)

    stopped <- FALSE
    for (i in seq_len(1200)) {
      later::run_now(0.01)
      session$flushReact()
      if (!isTRUE(growth_running())) {
        stopped <- TRUE
        break
      }
    }

    expect_true(stopped)
    expect_false(isTRUE(growth_running()))
    expect_false(isTRUE(cancel_requested()))
    expect_equal(nrow(growth_files_selected()), 1L)
    expect_match(status_text(), "stopped|Stop requested|Completed", ignore.case = TRUE)
  })
})

test_that("growth can run again after stop and a new upload", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("later")
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")
  skip_if_not_installed("gcplyr")
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("dplyr")

  old_force <- getOption("bioszen_growth_force_sync", NA)
  on.exit(options(bioszen_growth_force_sync = old_force), add = TRUE)

  tmp1 <- tempfile("growth_stop_then_rerun_one_", fileext = ".xlsx")
  tmp2 <- tempfile("growth_stop_then_rerun_two_", fileext = ".xlsx")
  on.exit(unlink(c(tmp1, tmp2), recursive = TRUE), add = TRUE)
  make_growth_input_file(tmp1, n_points = 120, n_wells = 8)
  make_growth_input_file(tmp2, n_points = 18, n_wells = 2)

  shiny::testServer(setup_growth_module, {
    options(bioszen_growth_force_sync = FALSE)
    session$setInputs(
      growthFiles = data.frame(datapath = tmp1, name = basename(tmp1), stringsAsFactors = FALSE)
    )
    session$flushReact()
    expect_equal(nrow(growth_files_selected()), 1L)

    session$setInputs(maxTime = 119, timeInterval = 1, runGrowth = 1)
    session$flushReact()
    later::later(function() {
      session$setInputs(stopGrowth = 1)
      session$flushReact()
    }, delay = 0)

    stopped <- FALSE
    for (i in seq_len(1200)) {
      later::run_now(0.01)
      session$flushReact()
      if (!isTRUE(growth_running())) {
        stopped <- TRUE
        break
      }
    }

    expect_true(stopped)
    expect_false(isTRUE(growth_running()))
    expect_false(isTRUE(cancel_requested()))

    session$setInputs(
      growthFiles = data.frame(datapath = tmp2, name = basename(tmp2), stringsAsFactors = FALSE)
    )
    session$flushReact()
    selected_rows <- growth_files_selected()
    second_id <- selected_rows$id[selected_rows$name == basename(tmp2)]
    expect_length(second_id, 1L)
    expect_false(isTRUE(growth_running()))
    expect_false(isTRUE(cancel_requested()))

    options(bioszen_growth_force_sync = TRUE)
    session$setInputs(
      maxTime = 17,
      timeInterval = 1,
      growthFilesKeep = second_id,
      runGrowth = 2
    )
    session$flushReact()

    growth_dir <- file.path(tempdir(), "growth_results")
    params <- list.files(growth_dir, pattern = "^(Parametros|Parameters)_.*\\.xlsx$", full.names = TRUE)
    expect_length(params, 1)
    expect_match(basename(params), tools::file_path_sans_ext(basename(tmp2)), fixed = TRUE)
    expect_identical(status_text(), "Completed.")
    expect_false(isTRUE(growth_running()))
    expect_false(isTRUE(cancel_requested()))
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
      growthFiles = data.frame(datapath = good_input, name = basename(good_input), stringsAsFactors = FALSE)
    )
    session$flushReact()
    selected_rows <- growth_files_selected()
    good_id <- selected_rows$id[selected_rows$name == basename(good_input)]
    session$setInputs(
      maxTime = 17,
      timeInterval = 1,
      growthFilesKeep = good_id,
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
