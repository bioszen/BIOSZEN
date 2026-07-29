library(testthat)

public_package_api <- new.env(parent = globalenv())
for (file in c("citation.R", "update.R", "run_app.R")) {
  sys.source(file.path(app_test_root(), "R", file), envir = public_package_api)
}

mock_public_package_api <- function(bindings) {
  binding_names <- names(bindings)
  originals <- mget(binding_names, envir = public_package_api, inherits = FALSE)
  for (name in binding_names) {
    assign(name, bindings[[name]], envir = public_package_api)
  }
  function() {
    for (name in binding_names) {
      assign(name, originals[[name]], envir = public_package_api)
    }
    invisible(TRUE)
  }
}

test_that("BIOSZEN launcher delegates to run_app without changing arguments", {
  received <- NULL
  restore <- mock_public_package_api(list(
    run_app = function(host, port, launch.browser) {
      received <<- list(host = host, port = port, launch.browser = launch.browser)
      "launched"
    }
  ))
  on.exit(restore(), add = TRUE)

  browser_fun <- function(url) invisible(url)
  result <- public_package_api$BIOSZEN(host = "127.0.0.1", port = 5432, launch.browser = browser_fun)

  expect_identical(result, "launched")
  expect_identical(received$host, "127.0.0.1")
  expect_identical(received$port, 5432)
  expect_identical(received$launch.browser, browser_fun)
  expect_true(is.function(public_package_api$run_app))
})

test_that("update checks compare versions without installing anything", {
  available <- matrix(
    c("2.0.5"),
    nrow = 1,
    dimnames = list("BIOSZEN", "Version")
  )
  restore <- mock_public_package_api(list(
    .bioszen_installed_version = function() numeric_version("2.0.4"),
    .bioszen_available_packages = function(repos) available
  ))
  on.exit(restore(), add = TRUE)

  status <- public_package_api$bioszen_update_available(repos = "https://example.invalid")

  expect_true(status)
  expect_identical(attr(status, "installed_version"), "2.0.4")
  expect_identical(attr(status, "available_version"), "2.0.5")
})

test_that("update checks fail gracefully when the repository is unavailable", {
  restore <- mock_public_package_api(list(
    .bioszen_installed_version = function() numeric_version("2.0.4"),
    .bioszen_available_packages = function(repos) stop("offline")
  ))
  on.exit(restore(), add = TRUE)

  status <- public_package_api$bioszen_update_available(repos = "https://example.invalid", quiet = TRUE)

  expect_true(is.na(status))
  expect_identical(attr(status, "error"), "offline")
})

test_that("bioszen_update installs only after consent and outside the app", {
  installed <- FALSE
  available <- matrix(
    c("2.0.5"),
    nrow = 1,
    dimnames = list("BIOSZEN", "Version")
  )
  restore <- mock_public_package_api(list(
    .bioszen_installed_version = function() numeric_version("2.0.4"),
    .bioszen_available_packages = function(repos) available,
    .bioszen_install_package = function(repos, lib) installed <<- TRUE
  ))
  on.exit(restore(), add = TRUE)

  old_running <- getOption("BIOSZEN.app_running", NULL)
  on.exit(options(BIOSZEN.app_running = old_running), add = TRUE)
  options(BIOSZEN.app_running = FALSE)
  expect_message(public_package_api$bioszen_update(ask = FALSE, repos = "https://example.invalid"), "Restart R")
  expect_true(installed)

  installed <- FALSE
  options(BIOSZEN.app_running = TRUE)
  expect_error(public_package_api$bioszen_update(ask = FALSE, repos = "https://example.invalid"), "Close the BIOSZEN app")
  expect_false(installed)
})

test_that("citation commands expose one official DOI and citation", {
  expected_text <- "Szenfeld, B. (2026). BIOSZEN. Zenodo. https://doi.org/10.5281/zenodo.18217210"

  expect_identical(public_package_api$bioszen_citation("text"), expected_text)
  expect_identical(public_package_api$bioszen_citation("doi"), "10.5281/zenodo.18217210")
  expect_s3_class(public_package_api$bioszen_citation("bibentry"), "bibentry")
  expect_match(public_package_api$bioszen_citation("bibtex"), "10.5281/zenodo.18217210", fixed = TRUE)
})

test_that("the package installs into a clean library with its public API and citation", {
  package_root <- normalizePath(app_test_root(), winslash = "/", mustWork = TRUE)
  clean_library <- tempfile("bioszen-clean-library-")
  dir.create(clean_library)
  clean_library <- normalizePath(clean_library, winslash = "/", mustWork = TRUE)
  dependency_libraries <- normalizePath(.libPaths(), winslash = "/", mustWork = TRUE)
  previous_r_libs_user <- Sys.getenv("R_LIBS_USER", unset = NA_character_)
  previous_r_libs <- Sys.getenv("R_LIBS", unset = NA_character_)
  on.exit({
    if (is.na(previous_r_libs_user)) {
      Sys.unsetenv("R_LIBS_USER")
    } else {
      Sys.setenv(R_LIBS_USER = previous_r_libs_user)
    }
    if (is.na(previous_r_libs)) {
      Sys.unsetenv("R_LIBS")
    } else {
      Sys.setenv(R_LIBS = previous_r_libs)
    }
  }, add = TRUE)
  Sys.setenv(
    R_LIBS_USER = dependency_libraries[[1]],
    R_LIBS = paste(dependency_libraries, collapse = .Platform$path.sep)
  )

  r_executable <- file.path(
    R.home("bin"),
    if (.Platform$OS.type == "windows") "R.exe" else "R"
  )
  install_output <- system2(
    r_executable,
    c(
      "CMD", "INSTALL", "--no-multiarch",
      paste0("--library=", shQuote(clean_library)),
      shQuote(package_root)
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  install_status <- attr(install_output, "status")
  if (is.null(install_status)) install_status <- 0L
  if (!identical(install_status, 0L)) {
    fail(paste(install_output, collapse = "\n"))
    return(invisible(NULL))
  }

  namespace <- loadNamespace("BIOSZEN", lib.loc = clean_library)
  on.exit(try(unloadNamespace("BIOSZEN"), silent = TRUE), add = TRUE)
  expected_exports <- c(
    "BIOSZEN", "run_app", "growth_parameters", "bioszen_citation",
    "bioszen_update_available", "bioszen_update"
  )
  expect_true(all(expected_exports %in% getNamespaceExports(namespace)))

  package_citation <- utils::citation("BIOSZEN", lib.loc = clean_library)
  expect_s3_class(package_citation, "bibentry")
  expect_match(
    paste(format(package_citation), collapse = " "),
    "10.5281/zenodo.18217210",
    fixed = TRUE
  )
})
