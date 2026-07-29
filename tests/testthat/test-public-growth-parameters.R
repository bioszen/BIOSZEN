library(testthat)

public_growth_api <- new.env(parent = globalenv())
for (file in c("growth_core.R", "growth_parameters.R")) {
  sys.source(file.path(app_test_root(), "R", file), envir = public_growth_api)
}

make_public_growth_curves <- function() {
  time <- seq(0, 20, by = 1)
  data.frame(
    Time = time,
    RobustWell = 0.05 * exp(0.10 * time),
    FallbackWell = 0.03 * exp(0.30 * time),
    check.names = FALSE
  )
}

test_that("growth_parameters is exactly equal to the Shiny growth core", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("gcplyr")

  curves <- make_public_growth_curves()
  tidy <- gcplyr::trans_wide_to_tidy(curves, id_cols = "Time")

  ui_env <- new.env(parent = globalenv())
  sys.source(app_test_path("server", "growth_module.R"), envir = ui_env)
  sys.source(app_test_path("params", "params_growth.R"), envir = ui_env)

  ui_result <- ui_env$compute_growth_results_batch(tidy)
  command_result <- public_growth_api$growth_parameters(curves)

  expect_identical(names(command_result), names(ui_result))
  expect_identical(as.character(command_result$Well), as.character(ui_result$Well))
  for (column in setdiff(names(ui_result), "Well")) {
    expect_equal(
      unname(command_result[[column]]),
      unname(ui_result[[column]]),
      tolerance = sqrt(.Machine$double.eps),
      info = paste("UI/command mismatch in", column)
    )
  }
  expect_identical(attr(command_result, "method"), "robust_with_permissive_fallback")
  expect_null(attr(command_result, "output_file"))
})

test_that("growth_parameters supports tidy data without writing files", {
  skip_if_not_installed("gcplyr")

  curves <- make_public_growth_curves()
  tidy <- gcplyr::trans_wide_to_tidy(curves, id_cols = "Time")
  scratch <- tempfile("bioszen_growth_no_write_")
  dir.create(scratch)
  before <- list.files(scratch, all.files = TRUE, no.. = TRUE)

  old_wd <- setwd(scratch)
  on.exit(setwd(old_wd), add = TRUE)
  result <- public_growth_api$growth_parameters(tidy)

  expect_s3_class(result, "bioszen_growth_parameters")
  expect_identical(list.files(scratch, all.files = TRUE, no.. = TRUE), before)
  expect_identical(names(result), public_growth_api$.bioszen_growth_result_columns)
})

test_that("growth_parameters reads files and protects explicit output", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")

  input_file <- tempfile("bioszen_growth_input_", fileext = ".xlsx")
  output_dir <- tempfile("bioszen_growth_output_")
  writexl::write_xlsx(make_public_growth_curves(), input_file)

  result <- public_growth_api$growth_parameters(input_file, output_dir = output_dir)
  output_file <- attr(result, "output_file")

  expect_true(file.exists(output_file))
  expect_identical(basename(output_file), paste0("Parameters_", tools::file_path_sans_ext(basename(input_file)), ".xlsx"))
  saved <- readxl::read_excel(output_file, sheet = "Resultados Combinados", .name_repair = "minimal")
  expect_identical(names(saved), names(result))
  expect_identical(as.character(saved$Well), as.character(result$Well))
  for (column in setdiff(names(result), "Well")) {
    expect_equal(
      unname(saved[[column]]),
      unname(result[[column]]),
      tolerance = sqrt(.Machine$double.eps)
    )
  }
  expect_error(
    public_growth_api$growth_parameters(input_file, output_dir = output_dir),
    "already exists"
  )
  expect_silent(public_growth_api$growth_parameters(input_file, output_dir = output_dir, overwrite = TRUE))
})

test_that("growth_parameters processes every supported file in a directory", {
  skip_if_not_installed("writexl")

  input_dir <- tempfile("bioszen_growth_directory_")
  dir.create(input_dir)
  first <- file.path(input_dir, "first.xlsx")
  second <- file.path(input_dir, "second.xlsx")
  curves <- make_public_growth_curves()[, c("Time", "RobustWell"), drop = FALSE]
  writexl::write_xlsx(curves, first)
  curves$RobustWell <- curves$RobustWell * 1.1
  writexl::write_xlsx(curves, second)

  result <- public_growth_api$growth_parameters(input_dir)

  expect_s3_class(result, "bioszen_growth_parameters_list")
  expect_identical(names(result), c("first", "second"))
  expect_true(all(vapply(result, inherits, logical(1), "bioszen_growth_parameters")))
  expect_true(all(vapply(result, function(x) is.null(attr(x, "output_file")), logical(1))))
})

test_that("growth_parameters validates unsupported or missing inputs", {
  unsupported <- tempfile(fileext = ".txt")
  writeLines("not growth data", unsupported)
  expect_error(public_growth_api$growth_parameters(character()), "data frame, file path")
  expect_error(public_growth_api$growth_parameters("missing-growth-file.xlsx"), "does not exist")
  expect_error(public_growth_api$growth_parameters(unsupported), "Unsupported growth input format")
  expect_error(public_growth_api$growth_parameters(data.frame()), "empty")
  expect_error(public_growth_api$growth_parameters(make_public_growth_curves(), time_interval = 0), "greater than zero")
})
