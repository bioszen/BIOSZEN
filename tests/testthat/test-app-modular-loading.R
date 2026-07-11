test_that("inst/app/app.R loads UI and server modules via source_dir", {
  app_file <- app_test_path( "app.R")
  expect_true(file.exists(app_file))

  txt <- paste(readLines(app_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_match(txt, "resolve_bioszen_app_dir\\s*<-\\s*function\\s*\\(", perl = TRUE)
  expect_match(txt, "resolve_bioszen_source_root\\s*<-\\s*function\\s*\\(", perl = TRUE)
  expect_match(txt, "sys\\.source\\(file\\.path\\(app_dir, \"config\\.R\"\\)\\s*,\\s*envir\\s*=\\s*app_env\\)", perl = TRUE)
  expect_match(txt, "BIOSZEN\\.skip_direct_run_setup", perl = TRUE)
  expect_match(txt, "_R_CHECK_PACKAGE_NAME_", fixed = TRUE)
  expect_match(txt, "source_dir\\(file\\.path\\(app_dir, \"server\"\\)\\s*,\\s*envir\\s*=\\s*app_env\\)", perl = TRUE)
  expect_match(txt, "source_dir\\(file\\.path\\(app_dir, \"ui\"\\)\\s*,\\s*envir\\s*=\\s*app_env\\)", perl = TRUE)
  expect_match(txt, "file\\.path\\(app_dir, \"\\.\\.\"\\)", perl = TRUE)
  expect_no_match(txt, "sys\\.source\\(\"ui/ui_main\\.R\"", perl = TRUE)
  expect_no_match(txt, "sys\\.source\\(\"server/server_main\\.R\"", perl = TRUE)
})

test_that("inst/app/app.R can be sourced from the package root", {
  root <- normalizePath(app_test_root(), winslash = "/", mustWork = TRUE)
  app_file <- normalizePath(app_test_path("app.R"), winslash = "/", mustWork = TRUE)
  expect_true(file.exists(app_file))

  old_wd <- setwd(root)
  on.exit(setwd(old_wd), add = TRUE)

  app <- source(app_file, local = new.env(parent = globalenv()))$value
  expect_s3_class(app, "shiny.appobj")
})

test_that("shared helpers are defined only in helpers.R", {
  global_file <- app_test_path( "global.R")
  helpers_file <- app_test_path( "helpers.R")
  expect_true(file.exists(global_file))
  expect_true(file.exists(helpers_file))

  global_txt <- paste(readLines(global_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  helpers_txt <- paste(readLines(helpers_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  canonical_helpers <- c(
    "split_comparison",
    "dunnett_to_tibble",
    "safe_pairwise_t",
    "safe_file"
  )

  for (fn in canonical_helpers) {
    def_re <- sprintf("\\b%s\\s*<-\\s*function\\s*\\(", fn)
    expect_no_match(global_txt, def_re, perl = TRUE)
    expect_match(helpers_txt, def_re, perl = TRUE)
  }
})
