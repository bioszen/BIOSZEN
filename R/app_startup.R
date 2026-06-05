bioszen_r_version_key <- function() {
  minor <- strsplit(R.version$minor, ".", fixed = TRUE)[[1]][1]
  paste(R.version$major, minor, sep = ".")
}

bioszen_find_source_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = FALSE)
  repeat {
    if (file.exists(file.path(current, "DESCRIPTION")) &&
        file.exists(file.path(current, "inst", "app", "app.R"))) {
      return(current)
    }

    parent <- dirname(current)
    if (identical(parent, current)) break
    current <- parent
  }

  NULL
}

bioszen_project_library <- function(root = bioszen_find_source_root()) {
  if (is.null(root)) return(NULL)
  file.path(root, "R_libs", bioszen_r_version_key())
}

bioszen_is_stale_user_library <- function(path) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  expected <- paste0("/win-library/", bioszen_r_version_key())
  grepl("/win-library/[0-9]+\\.[0-9]+$", path) && !grepl(expected, path, fixed = TRUE)
}

bioszen_configure_project_library <- function(root = bioszen_find_source_root()) {
  lib <- bioszen_project_library(root)
  if (is.null(lib)) return(invisible(.libPaths()))

  if (!dir.exists(lib)) {
    dir.create(lib, recursive = TRUE, showWarnings = FALSE)
  }

  existing <- .libPaths()
  existing <- existing[!vapply(existing, bioszen_is_stale_user_library, logical(1))]
  .libPaths(unique(c(lib, existing)))
  options(BIOSZEN.project_lib = lib)
  invisible(.libPaths())
}

bioszen_description_dependencies <- function(root = bioszen_find_source_root()) {
  if (is.null(root)) return(character())

  desc <- read.dcf(file.path(root, "DESCRIPTION"))
  fields <- intersect(c("Depends", "Imports"), colnames(desc))
  deps <- unlist(strsplit(paste(desc[1, fields], collapse = ","), ",", fixed = TRUE))
  deps <- trimws(gsub("\n", " ", deps, fixed = TRUE))
  deps <- sub("\\s*\\([^)]*\\)", "", deps)
  unique(deps[nzchar(deps) & deps != "R"])
}

bioszen_missing_dependencies <- function(packages) {
  packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
}

bioszen_configure_package_download <- function() {
  repos <- getOption("repos")
  if (is.null(repos) || identical(unname(repos["CRAN"]), "@CRAN@")) {
    options(repos = c(CRAN = "https://cloud.r-project.org"))
  }

  if (.Platform$OS.type == "windows" &&
      !nzchar(getOption("download.file.method", ""))) {
    method <- if (isTRUE(capabilities("libcurl"))) "libcurl" else "wininet"
    options(download.file.method = method)
  }

  invisible(TRUE)
}

bioszen_install_missing_dependencies <- function(root = bioszen_find_source_root()) {
  bioszen_configure_project_library(root)

  packages <- bioszen_description_dependencies(root)
  missing <- bioszen_missing_dependencies(packages)
  if (!length(missing)) return(invisible(character()))

  bioszen_configure_package_download()

  lib <- getOption("BIOSZEN.project_lib")
  message(
    "Installing BIOSZEN packages for R ", bioszen_r_version_key(),
    " into ", normalizePath(lib, winslash = "/", mustWork = FALSE)
  )
  utils::install.packages(missing, lib = lib, dependencies = NA)

  still_missing <- bioszen_missing_dependencies(packages)
  if (length(still_missing)) {
    stop(
      "BIOSZEN could not load these required packages after installation: ",
      paste(still_missing, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(missing)
}

bioszen_prepare_direct_run <- function(root = bioszen_find_source_root(),
                                       install = TRUE) {
  if (getRversion() < "4.1.0") {
    stop("BIOSZEN requires R >= 4.1.0.", call. = FALSE)
  }

  bioszen_configure_project_library(root)

  if (isTRUE(install)) {
    bioszen_install_missing_dependencies(root)
  }

  invisible(.libPaths())
}
