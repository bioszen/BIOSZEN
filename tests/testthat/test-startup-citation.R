test_that("BIOSZEN startup citation includes the Zenodo DOI", {
  run_app_file <- file.path(app_test_root(), "R", "run_app.R")
  if (file.exists(run_app_file)) {
    run_app_env <- new.env(parent = globalenv())
    source(run_app_file, local = run_app_env)
    startup_citation <- get(".bioszen_startup_citation", envir = run_app_env)
  } else {
    startup_citation <- getFromNamespace(".bioszen_startup_citation", "BIOSZEN")
  }

  old_show <- getOption("BIOSZEN.show_startup_citation", NULL)
  old_shown <- getOption("BIOSZEN.startup_citation_shown", NULL)
  on.exit({
    if (is.null(old_show)) {
      options(BIOSZEN.show_startup_citation = NULL)
    } else {
      options(BIOSZEN.show_startup_citation = old_show)
    }
    if (is.null(old_shown)) {
      options(BIOSZEN.startup_citation_shown = NULL)
    } else {
      options(BIOSZEN.startup_citation_shown = old_shown)
    }
  }, add = TRUE)

  options(BIOSZEN.show_startup_citation = TRUE)
  options(BIOSZEN.startup_citation_shown = FALSE)
  msg <- capture.output(startup_citation(), type = "message")
  expected <- c(
    "##",
    "## BIOSZEN",
    "## See https://github.com/bioszen/BIOSZEN for additional documentation and source code.",
    "## Please cite software as:",
    "##   Szenfeld, B. (2026). BIOSZEN. Zenodo. https://doi.org/10.5281/zenodo.18217210",
    "##"
  )
  expect_equal(msg, expected)

  repeated <- capture.output(startup_citation(), type = "message")
  expect_length(repeated, 0)

  forced <- capture.output(startup_citation(force = TRUE), type = "message")
  expect_equal(forced, expected)

  options(BIOSZEN.startup_citation_shown = FALSE)
  options(BIOSZEN.show_startup_citation = FALSE)
  muted <- capture.output(startup_citation(), type = "message")
  expect_length(muted, 0)
})

test_that("standalone launcher emits the approved citation block", {
  launcher_file <- file.path(app_test_root(), "inst", "launchers", "App.R")
  if (!file.exists(launcher_file)) {
    skip("The standalone launcher is a repository file and is not installed in the package.")
  }
  launcher_exprs <- parse(launcher_file)
  citation_assignment <- Filter(function(expr) {
    is.call(expr) &&
      identical(expr[[1]], as.name("<-")) &&
      identical(expr[[2]], as.name("bioszen_startup_citation"))
  }, as.list(launcher_exprs))

  expect_length(citation_assignment, 1)
  launcher_env <- new.env(parent = globalenv())
  eval(citation_assignment[[1]], envir = launcher_env)

  old_show <- getOption("BIOSZEN.show_startup_citation", NULL)
  old_shown <- getOption("BIOSZEN.startup_citation_shown", NULL)
  on.exit({
    options(BIOSZEN.show_startup_citation = old_show)
    options(BIOSZEN.startup_citation_shown = old_shown)
  }, add = TRUE)

  options(BIOSZEN.show_startup_citation = TRUE)
  options(BIOSZEN.startup_citation_shown = FALSE)
  msg <- capture.output(
    get("bioszen_startup_citation", envir = launcher_env)(),
    type = "message"
  )

  expect_equal(msg, c(
    "##",
    "## BIOSZEN",
    "## See https://github.com/bioszen/BIOSZEN for additional documentation and source code.",
    "## Please cite software as:",
    "##   Szenfeld, B. (2026). BIOSZEN. Zenodo. https://doi.org/10.5281/zenodo.18217210",
    "##"
  ))
})

test_that("direct source launchers emit the BIOSZEN startup citation", {
  startup_file <- file.path(app_test_root(), "R", "app_startup.R")
  if (!file.exists(startup_file)) {
    skip("Direct source launchers are repository files and are not installed in the package.")
  }

  startup_env <- new.env(parent = globalenv())
  source(startup_file, local = startup_env)
  expect_true(exists(".bioszen_startup_citation", envir = startup_env, inherits = FALSE))

  old_show <- getOption("BIOSZEN.show_startup_citation", NULL)
  old_shown <- getOption("BIOSZEN.startup_citation_shown", NULL)
  on.exit({
    if (is.null(old_show)) {
      options(BIOSZEN.show_startup_citation = NULL)
    } else {
      options(BIOSZEN.show_startup_citation = old_show)
    }
    if (is.null(old_shown)) {
      options(BIOSZEN.startup_citation_shown = NULL)
    } else {
      options(BIOSZEN.startup_citation_shown = old_shown)
    }
  }, add = TRUE)

  options(BIOSZEN.show_startup_citation = TRUE)
  options(BIOSZEN.startup_citation_shown = FALSE)
  msg <- capture.output(
    get(".bioszen_startup_citation", envir = startup_env)(),
    type = "message"
  )
  expect_true(any(grepl("BIOSZEN", msg, fixed = TRUE)))
  expect_true(any(grepl("10.5281/zenodo.18217210", msg, fixed = TRUE)))

  app_launcher <- paste(readLines(file.path(app_test_root(), "App.R"), warn = FALSE), collapse = "\n")
  embedded_launcher <- paste(readLines(app_test_path("app.R"), warn = FALSE), collapse = "\n")
  run_app_launcher <- paste(readLines(file.path(app_test_root(), "R", "run_app.R"), warn = FALSE), collapse = "\n")
  standalone_launcher <- paste(
    readLines(file.path(app_test_root(), "inst", "launchers", "App.R"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(app_launcher, "bioszen_prepare_direct_run\\(\\)", perl = TRUE)
  expect_match(app_launcher, "shiny::shinyAppDir\\(app_dir\\)", perl = TRUE)
  expect_match(run_app_launcher, "\\.bioszen_startup_citation\\s*<-\\s*function\\(force = FALSE\\)", perl = TRUE)
  expect_false(
    grepl("app_dir <- system\\.file\\(\"app\", package = \"BIOSZEN\"\\)[\\s\\S]*?\\.bioszen_startup_citation\\(", run_app_launcher, perl = TRUE),
    info = "run_app should let inst/app/app.R print the citation after app dependencies are loaded."
  )
  expect_match(embedded_launcher, "\\.bioszen_emit_app_startup_citation\\s*<-\\s*function", perl = TRUE)
  expect_match(embedded_launcher, "BIOSZEN_LAUNCHER_CITATION_SHOWN", fixed = TRUE)
  expect_match(embedded_launcher, "\\.bioszen_emit_app_startup_citation\\(\\)", perl = TRUE)
  expect_match(
    embedded_launcher,
    "source_dir\\(file\\.path\\(app_dir, \"ui\"\\)[\\s\\S]*?\\.bioszen_emit_app_startup_citation\\(\\)[\\s\\S]*?shiny::shinyApp",
    perl = TRUE,
    info = "Direct inst/app startup should print the citation at the end of app startup, after modules load."
  )
  expect_match(standalone_launcher, "bioszen_startup_citation\\s*<-\\s*function", perl = TRUE)
  expect_match(standalone_launcher, "Szenfeld, B\\. \\(2026\\)\\. BIOSZEN\\. Zenodo\\. https://doi\\.org/10\\.5281/zenodo\\.18217210", perl = TRUE)
  expect_false(grepl("installed_app_late_citation", standalone_launcher, fixed = TRUE))
  expect_true(
    grepl("if \\(restart_in_clean_process\\)[\\s\\S]*?cat\\(\"\\\\nBIOSZEN version:[\\s\\S]*?bioszen_startup_citation\\(\\)[\\s\\S]*?launch_fresh_launcher_process\\(script_path\\)", standalone_launcher, perl = TRUE),
    info = "The parent launcher should print the citation once before a clean-process handoff."
  )
  expect_match(standalone_launcher, "options\\(BIOSZEN\\.startup_citation_shown = FALSE\\)", perl = TRUE)
})
