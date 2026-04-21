library(testthat)

root <- app_test_root()
source(app_test_path( "server", "growth_module.R"))
source(app_test_path( "params", "params_growth.R"))

infer_time_interval_hours <- function(time_col) {
  seconds <- suppressWarnings(
    as.numeric(gsub("[^0-9.+-]", "", as.character(time_col)))
  )
  seconds <- seconds[is.finite(seconds)]
  if (length(seconds) < 2) {
    return(1)
  }
  step_seconds <- stats::median(diff(seconds), na.rm = TRUE)
  if (!is.finite(step_seconds) || step_seconds <= 0) {
    return(1)
  }
  step_seconds / 3600
}

extract_time_hours <- function(time_col) {
  seconds <- suppressWarnings(
    as.numeric(gsub("[^0-9.+-]", "", as.character(time_col)))
  )
  seconds <- seconds[is.finite(seconds)]
  if (!length(seconds)) {
    return(numeric(0))
  }
  seconds / 3600
}

full_permissive_batch <- function(tidy_df) {
  wells <- unique(tidy_df$Well)
  tidy_df <- tidy_df %>%
    dplyr::mutate(
      Well = factor(Well, levels = wells),
      Time = as.numeric(Time)
    )
  robust <- calculate_growth_rates_robust(tidy_df)
  permissive <- calculate_growth_rates_permissive(tidy_df)
  combine_growth_results(robust, permissive) %>%
    dplyr::mutate(Well = factor(Well, levels = wells)) %>%
    dplyr::arrange(Well) %>%
    dplyr::select(Well, µMax, ODmax, AUC, lag_time, max_percap_time, doub_time, max_time)
}

test_that("Rapa workbook subset keeps exact values and order with selective permissive path", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")
  skip_if_not_installed("gcplyr")
  skip_if_not_installed("dplyr")
  library(dplyr)

  curves_path <- "C:/Users/user/Downloads/Curvas_Rapa_paper.xlsx"
  params_path <- "C:/Users/user/Downloads/Parametros_Curvas_Rapa_paper.xlsx"
  skip_if_not(file.exists(curves_path), "Curvas_Rapa_paper.xlsx not available")

  raw <- readxl::read_excel(curves_path, skip = 2, .name_repair = "minimal")
  time_hours <- extract_time_hours(raw[[1]])
  if (!length(time_hours)) {
    interval_hours <- infer_time_interval_hours(raw[[1]])
    time_hours <- seq(0, by = interval_hours, length.out = nrow(raw))
  }
  raw <- raw[seq_len(min(length(time_hours), nrow(raw))), , drop = FALSE]
  curves_sheet <- data.frame(
    Time = time_hours[seq_len(nrow(raw))],
    raw[, -c(1, 2), drop = FALSE],
    check.names = FALSE
  )

  tmp_curves <- tempfile("rapa_curves_", fileext = ".xlsx")
  on.exit(unlink(tmp_curves, recursive = TRUE), add = TRUE)
  writexl::write_xlsx(list(Sheet1 = curves_sheet), path = tmp_curves)

  raw_wide <- gcplyr::read_wides(tmp_curves, sheet = "Sheet1", startrow = 1, startcol = 1)
  tidy_df <- gcplyr::trans_wide_to_tidy(raw_wide[, -1], id_cols = "Time")
  all_wells <- unique(as.character(tidy_df$Well))
  selected_wells <- all_wells[seq_len(min(6, length(all_wells)))]
  tidy_df <- tidy_df %>%
    dplyr::filter(as.character(Well) %in% selected_wells) %>%
    dplyr::mutate(Well = factor(as.character(Well), levels = selected_wells))

  baseline <- full_permissive_batch(tidy_df)
  batch <- compute_growth_results_batch(tidy_df)
  expect_identical(names(batch), names(baseline))
  expect_identical(as.character(batch$Well), as.character(baseline$Well))

  numeric_cols <- setdiff(names(batch), "Well")
  for (col in numeric_cols) {
    expect_equal(unname(batch[[col]]), unname(baseline[[col]]), tolerance = sqrt(.Machine$double.eps))
  }

  if (file.exists(params_path)) {
    expected <- readxl::read_excel(params_path, sheet = "Resultados Combinados")
    expect_identical(names(batch), names(expected))
    expected_subset <- expected[expected$Well %in% selected_wells, , drop = FALSE]
    expect_identical(as.character(batch$Well), as.character(expected_subset$Well))
  }
})
