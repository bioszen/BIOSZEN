load_launcher_functions <- function(function_names) {
  root <- app_test_root()
  launcher_candidates <- c(
    file.path(root, "inst", "launchers", "App.R"),
    file.path(root, "launchers", "App.R"),
    system.file("launchers", "App.R", package = "BIOSZEN")
  )
  launcher_candidates <- launcher_candidates[nzchar(launcher_candidates)]
  launcher_matches <- launcher_candidates[file.exists(launcher_candidates)]
  if (!length(launcher_matches)) {
    stop("Unable to resolve standalone launcher App.R path for tests.", call. = FALSE)
  }
  launcher <- launcher_matches[[1]]
  exprs <- parse(launcher)
  env <- new.env(parent = globalenv())

  for (expr in exprs) {
    if (
      is.call(expr) &&
      identical(expr[[1]], as.symbol("<-")) &&
      is.symbol(expr[[2]]) &&
      as.character(expr[[2]]) %in% function_names
    ) {
      eval(expr, envir = env)
    }
  }

  env
}

launcher_source_path <- function() {
  root <- app_test_root()
  candidates <- c(
    file.path(root, "inst", "launchers", "App.R"),
    file.path(root, "launchers", "App.R"),
    system.file("launchers", "App.R", package = "BIOSZEN")
  )
  candidates <- candidates[nzchar(candidates)]
  matches <- candidates[file.exists(candidates)]
  if (!length(matches)) {
    skip("Standalone launcher source file is not available in this test context.")
  }
  matches[[1]]
}

root_launcher_source_paths <- function() {
  root <- app_test_root()
  paths <- file.path(root, c("App.R", "app.R"))
  paths[file.exists(paths)]
}

write_launcher_desc <- function(path, version, package = "BIOSZEN") {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  writeLines(
    c(
      paste0("Package: ", package),
      paste0("Version: ", version),
      "Imports: shiny, ggplot2"
    ),
    file.path(path, "DESCRIPTION"),
    useBytes = TRUE
  )
}

test_that("standalone launcher discovers extracted BIOSZEN source folders", {
  env <- load_launcher_functions(c(
    "parse_pkg_version",
    "extract_version_from_dirname",
    "read_source_description",
    "read_source_description_version",
    "get_source_version",
    "is_package_source_dir",
    "find_package_source_dirs",
    "normalize_dep_field",
    "deps_from_dcf",
    "get_source_dependencies"
  ))

  base_dir <- tempfile("bioszen-launcher-src-")
  dir.create(base_dir)
  direct_dir <- file.path(base_dir, "BIOSZEN_2.0.3")
  nested_dir <- file.path(base_dir, "BIOSZEN_2.1.2", "BIOSZEN")
  other_dir <- file.path(base_dir, "BIOSZEN_other")

  write_launcher_desc(direct_dir, "2.0.3")
  write_launcher_desc(nested_dir, "2.1.2")
  write_launcher_desc(other_dir, "9.9.9", package = "OTHER")

  found <- env$find_package_source_dirs(base_dir, "BIOSZEN")

  expect_true(normalizePath(direct_dir, winslash = "/", mustWork = TRUE) %in% found)
  expect_true(normalizePath(nested_dir, winslash = "/", mustWork = TRUE) %in% found)
  expect_false(normalizePath(other_dir, winslash = "/", mustWork = TRUE) %in% found)
  expect_equal(as.character(env$get_source_version(direct_dir, "BIOSZEN")), "2.0.3")
  expect_equal(as.character(env$get_source_version(nested_dir, "BIOSZEN")), "2.1.2")
  expect_setequal(env$get_source_dependencies(direct_dir), c("shiny", "ggplot2"))
})

test_that("standalone launcher can use the current folder as extracted BIOSZEN source", {
  env <- load_launcher_functions(c(
    "read_source_description",
    "is_package_source_dir",
    "find_package_source_dirs"
  ))

  source_dir <- tempfile("BIOSZEN_2.0.5-")
  write_launcher_desc(source_dir, "2.0.5")

  found <- env$find_package_source_dirs(source_dir, "BIOSZEN")

  expect_equal(found, normalizePath(source_dir, winslash = "/", mustWork = TRUE))
})

test_that("standalone launcher keeps archive candidates primary when both exist", {
  env <- load_launcher_functions(c(
    "pick_best_archive",
    "pick_best_package_candidate"
  ))

  archive_choice <- env$pick_best_archive(list(list(
    path = "BIOSZEN_2.0.3.tar.gz",
    kind = "archive",
    version = package_version("2.0.3"),
    mtime = Sys.time()
  )))
  source_choice <- env$pick_best_archive(list(list(
    path = "BIOSZEN_2.1.2",
    kind = "source_dir",
    version = package_version("2.1.2"),
    mtime = Sys.time()
  )))

  expect_identical(env$pick_best_package_candidate(archive_choice, source_choice)$kind, "archive")
  expect_identical(env$pick_best_package_candidate(NULL, source_choice)$kind, "source_dir")
})

test_that("standalone launcher closes the startup log sink on source exit", {
  launcher <- launcher_source_path()
  txt <- paste(readLines(launcher, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_match(txt, "log_sink_depth <- sink.number\\(\\)", perl = TRUE)
  expect_match(txt, "sink\\(log_file, append = TRUE, split = TRUE\\)", perl = TRUE)
  expect_match(txt, "close_launcher_log_sink <- function\\(\\)", perl = TRUE)
  expect_match(txt, "on.exit\\(close_launcher_log_sink\\(\\), add = TRUE\\)", perl = TRUE)
})

test_that("standalone launcher resolves fresh Rscript paths on Windows and macOS", {
  env <- load_launcher_functions("launcher_rscript_path")
  fake_home <- tempfile("bioszen-r-home-")
  dir.create(file.path(fake_home, "bin"), recursive = TRUE)
  windows_rscript <- file.path(fake_home, "bin", "Rscript.exe")
  macos_rscript <- file.path(fake_home, "bin", "Rscript")
  file.create(windows_rscript, macos_rscript)

  expect_equal(
    env$launcher_rscript_path(fake_home, "windows"),
    normalizePath(windows_rscript, winslash = "/", mustWork = TRUE)
  )
  expect_equal(
    env$launcher_rscript_path(fake_home, "unix"),
    normalizePath(macos_rscript, winslash = "/", mustWork = TRUE)
  )
})

test_that("standalone launcher honors an explicit source-wrapper path", {
  env <- load_launcher_functions("get_script_path")
  launcher <- tempfile("BIOSZEN launcher ", fileext = ".R")
  file.create(launcher)

  old_value <- Sys.getenv("BIOSZEN_LAUNCHER_SCRIPT", unset = NA_character_)
  on.exit({
    if (is.na(old_value)) {
      Sys.unsetenv("BIOSZEN_LAUNCHER_SCRIPT")
    } else {
      Sys.setenv(BIOSZEN_LAUNCHER_SCRIPT = old_value)
    }
  }, add = TRUE)
  Sys.setenv(BIOSZEN_LAUNCHER_SCRIPT = launcher)

  expect_equal(
    env$get_script_path(),
    normalizePath(launcher, winslash = "/", mustWork = TRUE)
  )
})

test_that("standalone launcher starts a one-time vanilla child process", {
  env <- load_launcher_functions("launch_fresh_launcher_process")
  env$launcher_fresh_process_env <- "BIOSZEN_LAUNCHER_FRESH_PROCESS"
  env$launcher_citation_shown_env <- "BIOSZEN_LAUNCHER_CITATION_SHOWN"
  script <- tempfile("BIOSZEN App ", fileext = ".R")
  rscript <- tempfile("Rscript ")
  writeLines("invisible(TRUE)", script)
  file.create(rscript)

  captured <- new.env(parent = emptyenv())
  fake_system2 <- function(command, args, stdout, stderr, wait) {
    captured$command <- command
    captured$args <- args
    captured$stdout <- stdout
    captured$stderr <- stderr
    captured$wait <- wait
    captured$fresh_env <- Sys.getenv("BIOSZEN_LAUNCHER_FRESH_PROCESS")
    captured$citation_env <- Sys.getenv("BIOSZEN_LAUNCHER_CITATION_SHOWN")
    0L
  }

  expect_true(env$launch_fresh_launcher_process(
    script,
    system2_fun = fake_system2,
    rscript = rscript
  ))
  expect_equal(captured$command, rscript)
  expect_identical(captured$args[[1]], "--vanilla")
  expect_match(captured$args[[2]], "BIOSZEN App", fixed = TRUE)
  expect_true(captured$wait)
  expect_identical(captured$fresh_env, "1")
  expect_identical(captured$citation_env, "1")
  expect_identical(Sys.getenv("BIOSZEN_LAUNCHER_FRESH_PROCESS", unset = "<unset>"), "<unset>")
  expect_identical(Sys.getenv("BIOSZEN_LAUNCHER_CITATION_SHOWN", unset = "<unset>"), "<unset>")
})

test_that("standalone launcher detects namespaces loaded outside its local library", {
  env <- load_launcher_functions(c("same_launcher_path", "loaded_namespace_conflicts"))
  local_lib <- tempfile("bioszen-local-lib-")
  dir.create(file.path(local_lib, "stats"), recursive = TRUE)

  expect_identical(env$loaded_namespace_conflicts("stats", local_lib), "stats")
  expect_true(env$same_launcher_path("C:/Temp/BIOSZEN", "c:/temp/bioszen", "windows"))
})

test_that("standalone launcher defers missing packages locked by the current session", {
  env <- load_launcher_functions("ensure_dependencies")
  state <- new.env(parent = emptyenv())
  state$missing_calls <- 0L
  state$installed <- character(0)

  env$launcher_is_fresh_process <- FALSE
  env$launcher_library_changed <- FALSE
  env$launcher_deferred_packages <- character(0)
  env$resolve_dependency_closure <- identity
  env$loadedNamespaces <- function() "shiny"
  env$packages_missing_from_local_library <- function(packages, lib_dir) {
    state$missing_calls <- state$missing_calls + 1L
    if (state$missing_calls == 1L) c("shiny", "later") else "shiny"
  }
  env$install_dependency_packages <- function(packages, lib_dir) {
    state$installed <- packages
    invisible(TRUE)
  }

  output <- capture.output(env$ensure_dependencies(c("shiny", "later"), tempdir()))

  expect_identical(state$installed, "later")
  expect_identical(env$launcher_deferred_packages, "shiny")
  expect_true(any(grepl("Deferring packages already loaded", output, fixed = TRUE)))
  expect_true(any(grepl("Clean-process installation still required for: shiny", output, fixed = TRUE)))
})

test_that("standalone launcher installs missing packages normally in its fresh process", {
  env <- load_launcher_functions("ensure_dependencies")
  state <- new.env(parent = emptyenv())
  state$missing_calls <- 0L
  state$installed <- character(0)

  env$launcher_is_fresh_process <- TRUE
  env$launcher_library_changed <- FALSE
  env$launcher_deferred_packages <- character(0)
  env$resolve_dependency_closure <- identity
  env$loadedNamespaces <- function() "shiny"
  env$packages_missing_from_local_library <- function(packages, lib_dir) {
    state$missing_calls <- state$missing_calls + 1L
    if (state$missing_calls == 1L) "shiny" else character(0)
  }
  env$install_dependency_packages <- function(packages, lib_dir) {
    state$installed <- packages
    invisible(TRUE)
  }
  env$loaded_namespace_conflicts <- function(packages, lib_dir) character(0)
  env$loadable_with_local_library <- function(package, lib_dir) TRUE

  capture.output(env$ensure_dependencies("shiny", tempdir()))

  expect_identical(state$installed, "shiny")
  expect_length(env$launcher_deferred_packages, 0L)
})

test_that("standalone launcher defers app loading after local library changes", {
  launcher <- launcher_source_path()
  txt <- paste(readLines(launcher, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_match(txt, "launcher_library_changed <<- TRUE", fixed = TRUE)
  expect_match(txt, "launcher_library_changed <- TRUE", fixed = TRUE)
  expect_match(txt, "launcher_deferred_packages <- character(0)", fixed = TRUE)
  expect_match(txt, "length(launcher_deferred_packages)", fixed = TRUE)
  expect_match(txt, "loaded_namespace_conflicts(unique(c(deps, pkg)), local_lib)", fixed = TRUE)
  expect_match(txt, "restart_in_clean_process <- !isTRUE(launcher_is_fresh_process)", fixed = TRUE)
  expect_true(grepl(
    "if \\(restart_in_clean_process\\)[\\s\\S]*?launch_fresh_launcher_process\\(script_path\\)[\\s\\S]*?else \\{[\\s\\S]*?do.call\\(run_fun, args\\)",
    txt,
    perl = TRUE
  ))
})

test_that("root App.R launchers resolve paths from the script location", {
  paths <- root_launcher_source_paths()
  skip_if_not(length(paths) > 0, "Root App.R/app.R launchers are source-tree files and are not installed.")
  for (path in paths) {
    txt <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

    expect_match(txt, "find_bioszen_app_file <- function", fixed = TRUE)
    expect_match(txt, "frame\\$ofile", perl = TRUE)
    expect_match(txt, "startup_file <- file.path\\(app_root, \"R\", \"app_startup.R\"\\)", perl = TRUE)
    expect_match(txt, "file.path\\(app_root, \"inst\", \"app\"\\)", perl = TRUE)
    expect_match(txt, "file.path\\(app_root, \"inst\", \"launchers\", \"App.R\"\\)", perl = TRUE)
    expect_match(txt, "BIOSZEN_LAUNCHER_SCRIPT", fixed = TRUE)
    expect_match(txt, "keep App.R beside BIOSZEN-v\\*\\.tar\\.gz", perl = TRUE)
    expect_false(grepl('file.path\\("inst", "app"\\)', txt, perl = TRUE))
  }
})

test_that("root app launcher delegates missing app folders to the standalone launcher", {
  root_launcher <- file.path(app_test_root(), "app.R")
  skip_if_not(
    file.exists(root_launcher),
    "The root app.R launcher is a repository file and is not installed in the package."
  )

  root <- tempfile("bioszen-root-launcher-")
  launcher_dir <- file.path(root, "inst", "launchers")
  dir.create(launcher_dir, recursive = TRUE)
  expect_true(file.copy(root_launcher, file.path(root, "app.R")))

  capture_path <- tempfile("bioszen-launcher-script-", fileext = ".rds")
  standalone_launcher <- file.path(launcher_dir, "App.R")
  writeLines(
    "saveRDS(Sys.getenv('BIOSZEN_LAUNCHER_SCRIPT'), Sys.getenv('BIOSZEN_TEST_CAPTURE'))",
    standalone_launcher,
    useBytes = TRUE
  )

  old_capture <- Sys.getenv("BIOSZEN_TEST_CAPTURE", unset = NA_character_)
  old_launcher <- Sys.getenv("BIOSZEN_LAUNCHER_SCRIPT", unset = NA_character_)
  on.exit({
    if (is.na(old_capture)) Sys.unsetenv("BIOSZEN_TEST_CAPTURE") else Sys.setenv(BIOSZEN_TEST_CAPTURE = old_capture)
    if (is.na(old_launcher)) Sys.unsetenv("BIOSZEN_LAUNCHER_SCRIPT") else Sys.setenv(BIOSZEN_LAUNCHER_SCRIPT = old_launcher)
  }, add = TRUE)
  Sys.setenv(BIOSZEN_TEST_CAPTURE = capture_path)
  Sys.unsetenv("BIOSZEN_LAUNCHER_SCRIPT")

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(root)
  sys.source(file.path(root, "app.R"), envir = new.env(parent = globalenv()))

  expect_true(file.exists(capture_path))
  expect_equal(
    readRDS(capture_path),
    normalizePath(standalone_launcher, winslash = "/", mustWork = TRUE)
  )
})
