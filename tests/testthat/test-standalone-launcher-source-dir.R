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
  expect_match(txt, "on.exit\\(\\{\\s*while \\(sink.number\\(\\) > log_sink_depth\\)", perl = TRUE)
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
    expect_false(grepl('file.path\\("inst", "app"\\)', txt, perl = TRUE))
  }
})
