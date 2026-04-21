app_test_path <- function(...) {
  rel_parts <- list(...)

  source_path <- do.call(
    testthat::test_path,
    c(list("..", "..", "inst", "app"), rel_parts)
  )
  if (file.exists(source_path) || dir.exists(source_path)) {
    return(source_path)
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
