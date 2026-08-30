app_test_path <- function(...) {
  rel_parts <- list(...)

  source_path <- do.call(
    testthat::test_path,
    c(list("..", "..", "inst", "app"), rel_parts)
  )
  if (file.exists(source_path) || dir.exists(source_path)) {
    return(source_path)
  }

  check_package <- Sys.getenv("_R_CHECK_PACKAGE_NAME_", unset = "BIOSZEN")
  check_tree_path <- do.call(
    testthat::test_path,
    c(list("..", "..", "00_pkg_src", check_package, "inst", "app"), rel_parts)
  )
  if (file.exists(check_tree_path) || dir.exists(check_tree_path)) {
    return(normalizePath(check_tree_path, winslash = "/", mustWork = TRUE))
  }

  check_source_path <- do.call(
    testthat::test_path,
    c(list("..", "inst", "app"), rel_parts)
  )
  if (file.exists(check_source_path) || dir.exists(check_source_path)) {
    return(check_source_path)
  }

  installed_path <- do.call(
    system.file,
    c(list("app"), rel_parts, list(package = "BIOSZEN"))
  )
  if (nzchar(installed_path) && (file.exists(installed_path) || dir.exists(installed_path))) {
    return(installed_path)
  }

  requested <- paste(unlist(rel_parts), collapse = "/")
  if (!nzchar(requested)) {
    requested <- "."
  }
  stop(sprintf("Unable to resolve app test path for '%s'.", requested), call. = FALSE)
}

app_test_root <- function() {
  source_root <- testthat::test_path("..", "..")
  if (
    dir.exists(source_root) &&
    file.exists(file.path(source_root, "DESCRIPTION")) &&
    dir.exists(file.path(source_root, "inst", "app"))
  ) {
    return(source_root)
  }

  check_package <- Sys.getenv("_R_CHECK_PACKAGE_NAME_", unset = "BIOSZEN")
  check_source_root <- file.path(source_root, "00_pkg_src", check_package)
  if (
    dir.exists(check_source_root) &&
    file.exists(file.path(check_source_root, "DESCRIPTION")) &&
    dir.exists(file.path(check_source_root, "inst", "app"))
  ) {
    return(normalizePath(check_source_root, winslash = "/", mustWork = TRUE))
  }

  check_root <- testthat::test_path("..")
  if (
    dir.exists(check_root) &&
    file.exists(file.path(check_root, "DESCRIPTION")) &&
    dir.exists(file.path(check_root, "inst", "app"))
  ) {
    return(check_root)
  }

  pkg_root <- system.file(package = "BIOSZEN")
  if (nzchar(pkg_root) && file.exists(file.path(pkg_root, "DESCRIPTION"))) {
    return(pkg_root)
  }

  app_dir <- app_test_path()
  parent <- normalizePath(file.path(app_dir, ".."), winslash = "/", mustWork = FALSE)
  grandparent <- normalizePath(file.path(app_dir, "..", ".."), winslash = "/", mustWork = FALSE)
  if (file.exists(file.path(grandparent, "DESCRIPTION"))) return(grandparent)
  if (file.exists(file.path(parent, "DESCRIPTION"))) return(parent)
  stop("Unable to resolve package root for tests.", call. = FALSE)
}

app_test_launch_dir <- function() {
  root <- app_test_root()
  if (file.exists(file.path(root, "app.R"))) return(root)
  app_test_path()
}

# covr::file_coverage() preloads instrumented app functions into the same
# environment that sources the coverage test driver. Focused tests normally
# source private copies so they can also run outside covr; during coverage,
# reuse the instrumented environment instead so those executions are counted.
app_test_source_env <- function(paths,
                                required,
                                parent = globalenv(),
                                initialize = NULL) {
  coverage_env <- getOption("BIOSZEN.coverage_source_env", NULL)
  required <- unique(as.character(required))
  has_required <- function(env) {
    is.environment(env) && all(vapply(
      required,
      exists,
      logical(1),
      envir = env,
      inherits = FALSE
    ))
  }

  if (has_required(coverage_env)) {
    return(coverage_env)
  }

  env <- new.env(parent = parent)
  env$`%||%` <- function(x, y) if (is.null(x)) y else x
  if (is.function(initialize)) initialize(env)

  paths <- normalizePath(paths, winslash = "/", mustWork = TRUE)
  for (path in paths) sys.source(path, envir = env)

  missing <- required[!vapply(
    required,
    exists,
    logical(1),
    envir = env,
    inherits = FALSE
  )]
  if (length(missing)) {
    stop(
      "Focused app test source did not define: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  env
}

# Some focused tests source global.R directly instead of the full app bootstrap.
# Load the shared constants first so those tests mirror the production order.
dpi_config_path <- app_test_path("config.R")
if (file.exists(dpi_config_path)) {
  sys.source(dpi_config_path, envir = globalenv())
}
