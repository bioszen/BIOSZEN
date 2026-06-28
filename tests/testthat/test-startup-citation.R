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
  on.exit({
    if (is.null(old_show)) {
      options(BIOSZEN.show_startup_citation = NULL)
    } else {
      options(BIOSZEN.show_startup_citation = old_show)
    }
  }, add = TRUE)

  options(BIOSZEN.show_startup_citation = TRUE)
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

  options(BIOSZEN.show_startup_citation = FALSE)
  muted <- capture.output(startup_citation(), type = "message")
  expect_length(muted, 0)
})
