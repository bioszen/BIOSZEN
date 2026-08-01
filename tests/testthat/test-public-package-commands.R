library(testthat)

public_package_api <- new.env(parent = globalenv())
public_package_sources <- file.path(
  app_test_root(),
  "R",
  c("citation.R", "update.R", "browser_launcher.R", "run_app.R")
)
if (all(file.exists(public_package_sources))) {
  for (file in public_package_sources) {
    sys.source(file, envir = public_package_api)
  }
} else {
  package_namespace <- asNamespace("BIOSZEN")
  for (name in ls(package_namespace, all.names = TRUE)) {
    value <- get(name, envir = package_namespace, inherits = FALSE)
    if (is.function(value) && identical(environment(value), package_namespace)) {
      environment(value) <- public_package_api
    }
    assign(name, value, envir = public_package_api)
  }
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

test_that("BIOSZEN uses the operating-system default browser by default", {
  run_app_default <- eval(
    formals(public_package_api$run_app)$launch.browser,
    envir = environment(public_package_api$run_app)
  )
  received <- NULL
  restore <- mock_public_package_api(list(
    run_app = function(host, port, launch.browser) {
      received <<- list(host = host, port = port, launch.browser = launch.browser)
      "launched"
    }
  ))
  on.exit(restore(), add = TRUE)

  old_browser <- getOption("shiny.launch.browser", NULL)
  on.exit(options(shiny.launch.browser = old_browser), add = TRUE)
  options(shiny.launch.browser = function(url) stop("RStudio Viewer should not be inherited"))

  expect_identical(public_package_api$BIOSZEN(), "launched")
  expect_identical(received$launch.browser, public_package_api$.bioszen_open_default_browser)
  expect_true(isTRUE(run_app_default))
  expect_false(eval(formals(public_package_api$BIOSZEN)$app_window))
})

test_that("BIOSZEN preserves regular-browser and no-browser overrides", {
  received <- NULL
  restore <- mock_public_package_api(list(
    run_app = function(host, port, launch.browser) {
      received <<- launch.browser
      "launched"
    }
  ))
  on.exit(restore(), add = TRUE)

  expect_identical(public_package_api$BIOSZEN(app_window = TRUE), "launched")
  expect_identical(received, public_package_api$.bioszen_open_app_browser)

  expect_identical(public_package_api$BIOSZEN(launch.browser = FALSE), "launched")
  expect_false(received)

  custom <- function(url) invisible(url)
  expect_identical(public_package_api$BIOSZEN(launch.browser = custom), "launched")
  expect_identical(received, custom)
})

test_that("default-browser launcher dispatches by operating system", {
  launched <- character()
  fallback <- FALSE
  restore <- mock_public_package_api(list(
    .bioszen_system_name = function() "Windows",
    .bioszen_open_default_windows = function(url) {
      launched <<- c(launched, "Windows")
      TRUE
    },
    .bioszen_open_default_macos = function(url) {
      launched <<- c(launched, "Darwin")
      TRUE
    },
    .bioszen_open_default_unix = function(url) {
      launched <<- c(launched, "Unix")
      TRUE
    },
    .bioszen_open_default_fallback = function(url) {
      fallback <<- TRUE
      TRUE
    }
  ))
  on.exit(restore(), add = TRUE)

  expect_true(public_package_api$.bioszen_open_default_browser("http://127.0.0.1:4321"))
  expect_identical(launched, "Windows")
  expect_false(fallback)

  public_package_api$.bioszen_system_name <- function() "Darwin"
  expect_true(public_package_api$.bioszen_open_default_browser("http://127.0.0.1:4321"))
  expect_identical(launched, c("Windows", "Darwin"))

  public_package_api$.bioszen_system_name <- function() "Linux"
  expect_true(public_package_api$.bioszen_open_default_browser("http://127.0.0.1:4321"))
  expect_identical(launched, c("Windows", "Darwin", "Unix"))
})

test_that("default-browser launcher uses a safe fallback", {
  fallback <- FALSE
  restore <- mock_public_package_api(list(
    .bioszen_system_name = function() "Linux",
    .bioszen_open_default_unix = function(url) FALSE,
    .bioszen_open_default_fallback = function(url) {
      fallback <<- TRUE
      TRUE
    }
  ))
  on.exit(restore(), add = TRUE)

  expect_true(public_package_api$.bioszen_open_default_browser("http://127.0.0.1:4321"))
  expect_true(fallback)
})

test_that("app-window browser selection stops after the first success", {
  launches <- character()
  restore <- mock_public_package_api(list(
    .bioszen_find_chromium_windows = function() c("first-browser", "second-browser"),
    .bioszen_default_browser_windows = function() "",
    .bioszen_launch_executable_app = function(executable, url) {
      launches <<- c(launches, executable)
      TRUE
    }
  ))
  on.exit(restore(), add = TRUE)

  expect_true(public_package_api$.bioszen_open_app_windows("http://127.0.0.1:4321"))
  expect_identical(launches, "first-browser")
})

test_that("RStudio registers the browser launcher as an interactive addin", {
  package_root <- app_test_root()
  addins_candidates <- c(
    file.path(package_root, "inst", "rstudio", "addins.dcf"),
    file.path(package_root, "rstudio", "addins.dcf"),
    system.file("rstudio", "addins.dcf", package = "BIOSZEN")
  )
  addins_file <- addins_candidates[
    nzchar(addins_candidates) & file.exists(addins_candidates)
  ][1]

  expect_true(length(addins_file) == 1L && !is.na(addins_file))

  addins <- read.dcf(addins_file)
  expect_true(any(addins[, "Name"] == "Launch BIOSZEN in Browser"))
  browser_addin <- addins[addins[, "Name"] == "Launch BIOSZEN in Browser", , drop = FALSE]
  expect_identical(unname(browser_addin[1, "Binding"]), "BIOSZEN")
  expect_identical(tolower(unname(browser_addin[1, "Interactive"])), "true")
})

test_that("update checks compare versions without installing anything", {
  available <- matrix(
    c("2.1.1"),
    nrow = 1,
    dimnames = list("BIOSZEN", "Version")
  )
  restore <- mock_public_package_api(list(
    .bioszen_installed_version = function() numeric_version("2.0.5"),
    .bioszen_available_packages = function(repos) available
  ))
  on.exit(restore(), add = TRUE)

  status <- public_package_api$bioszen_update_available(repos = "https://example.invalid")

  expect_true(status)
  expect_identical(attr(status, "installed_version"), "2.0.5")
  expect_identical(attr(status, "available_version"), "2.1.1")
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
    c("2.1.1"),
    nrow = 1,
    dimnames = list("BIOSZEN", "Version")
  )
  restore <- mock_public_package_api(list(
    .bioszen_installed_version = function() numeric_version("2.0.5"),
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

test_that("bioszen_update leaves the installation untouched when already current", {
  installed <- FALSE
  available <- matrix(
    c("2.1.1"),
    nrow = 1,
    dimnames = list("BIOSZEN", "Version")
  )
  restore <- mock_public_package_api(list(
    .bioszen_installed_version = function() numeric_version("2.1.1"),
    .bioszen_available_packages = function(repos) available,
    .bioszen_install_package = function(repos, lib) installed <<- TRUE
  ))
  on.exit(restore(), add = TRUE)

  old_running <- getOption("BIOSZEN.app_running", NULL)
  on.exit(options(BIOSZEN.app_running = old_running), add = TRUE)
  options(BIOSZEN.app_running = FALSE)

  expect_message(
    result <- public_package_api$bioszen_update(ask = FALSE, repos = "https://example.invalid"),
    "already up to date"
  )
  expect_false(result)
  expect_false(installed)
})

test_that("the app can request an update only while it is running", {
  old_running <- getOption("BIOSZEN.app_running", NULL)
  old_request <- getOption("BIOSZEN.update_after_app", NULL)
  on.exit(options(
    BIOSZEN.app_running = old_running,
    BIOSZEN.update_after_app = old_request
  ), add = TRUE)

  options(BIOSZEN.app_running = FALSE, BIOSZEN.update_after_app = FALSE)
  expect_false(public_package_api$.bioszen_request_update_after_app())
  expect_false(isTRUE(getOption("BIOSZEN.update_after_app", FALSE)))

  options(BIOSZEN.app_running = TRUE)
  expect_true(public_package_api$.bioszen_request_update_after_app())
  expect_true(isTRUE(getOption("BIOSZEN.update_after_app", FALSE)))
})

test_that("run_app installs a confirmed update only after Shiny stops", {
  updated <- FALSE
  running_during_install <- NA
  restore <- mock_public_package_api(list(
    .bioszen_installed_app_dir = function() tempdir(),
    .bioszen_run_shiny_app = function(app_dir, ...) {
      expect_true(isTRUE(getOption("BIOSZEN.app_running", FALSE)))
      expect_true(public_package_api$.bioszen_request_update_after_app())
      "app-closed"
    },
    bioszen_update = function(ask = TRUE, repos = NULL) {
      running_during_install <<- isTRUE(getOption("BIOSZEN.app_running", FALSE))
      expect_false(ask)
      updated <<- TRUE
      TRUE
    }
  ))
  on.exit(restore(), add = TRUE)

  old_running <- getOption("BIOSZEN.app_running", NULL)
  old_request <- getOption("BIOSZEN.update_after_app", NULL)
  on.exit(options(
    BIOSZEN.app_running = old_running,
    BIOSZEN.update_after_app = old_request
  ), add = TRUE)
  options(BIOSZEN.app_running = FALSE, BIOSZEN.update_after_app = FALSE)

  result <- public_package_api$run_app(launch.browser = FALSE)

  expect_identical(result, "app-closed")
  expect_true(updated)
  expect_false(running_during_install)
  expect_false(isTRUE(getOption("BIOSZEN.app_running", FALSE)))
})

test_that("run_app does not update when the user did not request it", {
  update_calls <- 0L
  restore <- mock_public_package_api(list(
    .bioszen_installed_app_dir = function() tempdir(),
    .bioszen_run_shiny_app = function(app_dir, ...) "app-closed",
    bioszen_update = function(...) {
      update_calls <<- update_calls + 1L
      TRUE
    }
  ))
  on.exit(restore(), add = TRUE)

  old_running <- getOption("BIOSZEN.app_running", NULL)
  old_request <- getOption("BIOSZEN.update_after_app", NULL)
  on.exit(options(
    BIOSZEN.app_running = old_running,
    BIOSZEN.update_after_app = old_request
  ), add = TRUE)
  options(BIOSZEN.app_running = FALSE, BIOSZEN.update_after_app = FALSE)

  expect_identical(public_package_api$run_app(launch.browser = FALSE), "app-closed")
  expect_identical(update_calls, 0L)
})

test_that("a post-shutdown installation failure does not crash run_app", {
  restore <- mock_public_package_api(list(
    .bioszen_installed_app_dir = function() tempdir(),
    .bioszen_run_shiny_app = function(app_dir, ...) {
      public_package_api$.bioszen_request_update_after_app()
      "app-closed"
    },
    bioszen_update = function(...) stop("library is locked")
  ))
  on.exit(restore(), add = TRUE)

  old_running <- getOption("BIOSZEN.app_running", NULL)
  old_request <- getOption("BIOSZEN.update_after_app", NULL)
  on.exit(options(
    BIOSZEN.app_running = old_running,
    BIOSZEN.update_after_app = old_request
  ), add = TRUE)
  options(BIOSZEN.app_running = FALSE, BIOSZEN.update_after_app = FALSE)

  expect_warning(
    result <- public_package_api$run_app(launch.browser = FALSE),
    "library is locked"
  )
  expect_identical(result, "app-closed")
  expect_false(isTRUE(getOption("BIOSZEN.app_running", FALSE)))
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
  expected_exports <- c(
    "BIOSZEN", "run_app", "growth_parameters", "bioszen_citation",
    "bioszen_update_available", "bioszen_update"
  )

  if (!file.exists(file.path(package_root, "R", "run_app.R"))) {
    namespace <- asNamespace("BIOSZEN")
    expect_true(all(expected_exports %in% getNamespaceExports(namespace)))

    package_citation <- utils::citation("BIOSZEN")
    expect_s3_class(package_citation, "bibentry")
    expect_match(
      paste(format(package_citation), collapse = " "),
      "10.5281/zenodo.18217210",
      fixed = TRUE
    )
    return(invisible(NULL))
  }

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
  expect_true(all(expected_exports %in% getNamespaceExports(namespace)))

  installed_addin <- system.file(
    "rstudio", "addins.dcf",
    package = "BIOSZEN",
    lib.loc = clean_library
  )
  expect_true(file.exists(installed_addin))
  expect_identical(unname(read.dcf(installed_addin)[1, "Binding"]), "BIOSZEN")

  package_citation <- utils::citation("BIOSZEN", lib.loc = clean_library)
  expect_s3_class(package_citation, "bibentry")
  expect_match(
    paste(format(package_citation), collapse = " "),
    "10.5281/zenodo.18217210",
    fixed = TRUE
  )
})
