.bioszen_default_repositories <- function() {
  c(
    BIOSZEN = "https://bioszen.r-universe.dev",
    CRAN = "https://cloud.r-project.org"
  )
}

.bioszen_available_packages <- function(repos) {
  utils::available.packages(repos = repos)
}

.bioszen_install_package <- function(repos, lib) {
  utils::install.packages("BIOSZEN", repos = repos, lib = lib)
}

.bioszen_normalize_library_paths <- function(paths) {
  paths <- unique(as.character(paths))
  paths <- paths[!is.na(paths) & nzchar(paths)]
  if (!length(paths)) return(character(0))

  unique(vapply(paths, function(path) {
    normalizePath(path.expand(path), winslash = "/", mustWork = FALSE)
  }, character(1), USE.NAMES = FALSE))
}

.bioszen_same_library <- function(left, right) {
  left <- .bioszen_normalize_library_paths(left)
  right <- .bioszen_normalize_library_paths(right)
  if (!length(left) || !length(right)) return(FALSE)
  if (identical(.Platform$OS.type, "windows")) {
    left <- tolower(left[[1]])
    right <- tolower(right[[1]])
  }
  identical(left[[1]], right[[1]])
}

.bioszen_standalone_library <- function() {
  local_library <- getOption(
    "BIOSZEN.local_lib",
    Sys.getenv("BIOSZEN_LOCAL_LIB", unset = "")
  )
  if (!is.character(local_library) || !length(local_library) ||
      is.na(local_library[[1]]) || !nzchar(trimws(local_library[[1]]))) {
    return("")
  }
  .bioszen_normalize_library_paths(trimws(local_library[[1]]))[[1]]
}

.bioszen_standard_libraries <- function(
    libraries = .libPaths(),
    standalone_library = .bioszen_standalone_library()) {
  libraries <- .bioszen_normalize_library_paths(libraries)
  if (!nzchar(standalone_library)) return(libraries)
  libraries[!vapply(
    libraries,
    .bioszen_same_library,
    logical(1),
    right = standalone_library
  )]
}

.bioszen_user_library_candidates <- function(
    libraries = .libPaths(),
    standalone_library = .bioszen_standalone_library(),
    user_libraries = Sys.getenv("R_LIBS_USER", unset = "")) {
  user_libraries <- as.character(user_libraries)
  user_libraries <- user_libraries[!is.na(user_libraries) & nzchar(user_libraries)]
  user_libraries <- if (length(user_libraries)) {
    unlist(
      strsplit(user_libraries, .Platform$path.sep, fixed = TRUE),
      use.names = FALSE
    )
  } else character(0)
  standard_libraries <- .bioszen_standard_libraries(
    libraries = libraries,
    standalone_library = standalone_library
  )
  system_library <- .bioszen_normalize_library_paths(R.home("library"))
  non_system_libraries <- standard_libraries[!vapply(
    standard_libraries,
    .bioszen_same_library,
    logical(1),
    right = system_library
  )]
  candidates <- .bioszen_normalize_library_paths(c(user_libraries, non_system_libraries))
  if (nzchar(standalone_library)) {
    candidates <- candidates[!vapply(
      candidates,
      .bioszen_same_library,
      logical(1),
      right = standalone_library
    )]
  }
  candidates
}

.bioszen_user_package_library <- function(...) {
  candidates <- .bioszen_user_library_candidates(...)
  library <- bioszen_select_writable_library(candidates)
  if (!nzchar(library)) {
    stop(
      "BIOSZEN could not find or create a writable personal R package library.",
      call. = FALSE
    )
  }
  library
}

.bioszen_standard_package_status <- function(
    libraries = .libPaths(),
    standalone_library = .bioszen_standalone_library(),
    user_libraries = Sys.getenv("R_LIBS_USER", unset = "")) {
  standard_libraries <- .bioszen_standard_libraries(
    libraries = libraries,
    standalone_library = standalone_library
  )
  user_candidates <- .bioszen_user_library_candidates(
    libraries = libraries,
    standalone_library = standalone_library,
    user_libraries = user_libraries
  )
  candidates <- .bioszen_normalize_library_paths(c(user_candidates, standard_libraries))
  package_paths <- file.path(candidates, "BIOSZEN")
  present <- which(
    dir.exists(package_paths) &
      file.exists(file.path(package_paths, "DESCRIPTION"))
  )
  if (!length(present)) {
    return(list(
      installed = FALSE,
      version = NA_character_,
      library = "",
      path = ""
    ))
  }

  index <- present[[1]]
  version <- tryCatch(
    as.character(utils::packageVersion("BIOSZEN", lib.loc = candidates[[index]])),
    error = function(e) NA_character_
  )
  list(
    installed = TRUE,
    version = version,
    library = candidates[[index]],
    path = normalizePath(package_paths[[index]], winslash = "/", mustWork = TRUE)
  )
}

.bioszen_install_user_package <- function(
    repos = .bioszen_default_repositories(),
    lib = .bioszen_user_package_library()) {
  if (isTRUE(getOption("BIOSZEN.app_running", FALSE))) {
    stop("Close the BIOSZEN app before installing the package.", call. = FALSE)
  }
  lib <- normalizePath(lib, winslash = "/", mustWork = TRUE)
  previous_libraries <- .libPaths()
  standard_libraries <- .bioszen_standard_libraries(previous_libraries)
  # Exclude the standalone library while resolving dependencies so a fresh R
  # session does not depend on packages available only inside the bundle.
  .libPaths(unique(c(lib, standard_libraries)))
  on.exit(.libPaths(previous_libraries), add = TRUE)

  .bioszen_install_package(repos = repos, lib = lib)
  installed_path <- file.path(lib, "BIOSZEN")
  if (!dir.exists(installed_path) ||
      !file.exists(file.path(installed_path, "DESCRIPTION"))) {
    stop("BIOSZEN was not found in the personal R library after installation.", call. = FALSE)
  }
  version <- tryCatch(
    as.character(utils::packageVersion("BIOSZEN", lib.loc = lib)),
    error = function(e) ""
  )
  version_text <- if (nzchar(version)) paste0(" ", version) else ""
  message(
    "BIOSZEN", version_text, " was installed as an R package.\n",
    "Restart R, then launch it with:\n",
    "  BIOSZEN::BIOSZEN()"
  )
  invisible(TRUE)
}

.bioszen_update_library <- function(package_path = tryCatch(
                                      find.package("BIOSZEN"),
                                      error = function(e) ""
                                    ),
                                    libraries = .libPaths()) {
  package_library <- if (nzchar(package_path)) dirname(package_path) else ""
  candidates <- unique(c(package_library, libraries))
  current_candidates <- Filter(
    Negate(is.null),
    lapply(candidates[nzchar(candidates)], bioszen_current_library_from_stale)
  )
  target <- if (length(current_candidates)) {
    current_candidates[[1]]
  } else if (nzchar(package_library)) {
    package_library
  } else {
    libraries[[1]]
  }

  if (!dir.exists(target) && !dir.create(target, recursive = TRUE, showWarnings = FALSE)) {
    stop("BIOSZEN could not create the current R package library at ", target, ".", call. = FALSE)
  }
  normalizePath(target, winslash = "/", mustWork = TRUE)
}

.bioszen_installed_version <- function() {
  tryCatch(utils::packageVersion("BIOSZEN"), error = function(e) numeric_version("0"))
}

.bioszen_request_update_after_app <- function() {
  if (!isTRUE(getOption("BIOSZEN.app_running", FALSE))) {
    return(invisible(FALSE))
  }
  options(
    BIOSZEN.update_after_app = TRUE,
    BIOSZEN.install_after_app = FALSE
  )
  invisible(TRUE)
}

.bioszen_request_install_after_app <- function() {
  if (!isTRUE(getOption("BIOSZEN.app_running", FALSE)) ||
      !identical(getOption("BIOSZEN.launch_mode", ""), "standalone_bundle")) {
    return(invisible(FALSE))
  }
  options(
    BIOSZEN.install_after_app = TRUE,
    BIOSZEN.update_after_app = FALSE
  )
  invisible(TRUE)
}

#' Check for a BIOSZEN update
#'
#' Checks the BIOSZEN R-universe repository without installing anything.
#'
#' @param repos Package repositories to query. The BIOSZEN R-universe and CRAN
#'   are used by default.
#' @param quiet Whether connection or repository errors should be returned
#'   silently as `NA`.
#'
#' @return A logical scalar. Attributes contain the installed and available
#'   versions. `NA` means that the check could not be completed.
#' @export
bioszen_update_available <- function(repos = .bioszen_default_repositories(), quiet = FALSE) {
  installed <- .bioszen_installed_version()
  available <- tryCatch(
    .bioszen_available_packages(repos),
    error = function(e) e
  )
  if (inherits(available, "error")) {
    if (!isTRUE(quiet)) {
      warning("BIOSZEN could not check for updates: ", conditionMessage(available), call. = FALSE)
    }
    out <- NA
    attr(out, "installed_version") <- as.character(installed)
    attr(out, "available_version") <- NA_character_
    attr(out, "error") <- conditionMessage(available)
    return(out)
  }

  if (!"BIOSZEN" %in% rownames(available)) {
    if (!isTRUE(quiet)) {
      warning("BIOSZEN is not currently listed by the configured repositories.", call. = FALSE)
    }
    out <- NA
    attr(out, "installed_version") <- as.character(installed)
    attr(out, "available_version") <- NA_character_
    attr(out, "error") <- "BIOSZEN was not found in the configured repositories."
    return(out)
  }

  candidate <- numeric_version(available["BIOSZEN", "Version"])
  out <- candidate > installed
  attr(out, "installed_version") <- as.character(installed)
  attr(out, "available_version") <- as.character(candidate)
  attr(out, "error") <- NULL
  out
}

#' Update BIOSZEN
#'
#' Checks R-universe for a newer BIOSZEN version, requests consent, and installs
#' it into the library containing the current package. BIOSZEN never updates
#' while its Shiny application is running.
#'
#' @param ask Whether to ask for interactive confirmation before installation.
#'   Set to `FALSE` only when prior user consent has already been obtained.
#' @param repos Package repositories used for the check and installation.
#'
#' @return `TRUE` when an update was installed and `FALSE` when no installation
#'   was needed or the user declined.
#' @export
bioszen_update <- function(ask = TRUE, repos = .bioszen_default_repositories()) {
  if (!is.logical(ask) || length(ask) != 1L || is.na(ask)) {
    stop("ask must be TRUE or FALSE.", call. = FALSE)
  }
  if (isTRUE(getOption("BIOSZEN.app_running", FALSE))) {
    stop("Close the BIOSZEN app before updating the package.", call. = FALSE)
  }

  available <- bioszen_update_available(repos = repos, quiet = FALSE)
  if (is.na(available)) return(invisible(FALSE))
  if (!isTRUE(available)) {
    message("BIOSZEN is already up to date (", attr(available, "installed_version"), ").")
    return(invisible(FALSE))
  }

  prompt <- sprintf(
    "Update BIOSZEN from %s to %s?",
    attr(available, "installed_version"),
    attr(available, "available_version")
  )
  if (isTRUE(ask)) {
    if (!interactive()) {
      message(prompt, " Run bioszen_update(ask = FALSE) after obtaining user consent.")
      return(invisible(FALSE))
    }
    confirmed <- utils::askYesNo(prompt, default = FALSE)
    if (!isTRUE(confirmed)) return(invisible(FALSE))
  }

  lib <- .bioszen_update_library()
  .bioszen_install_package(repos = repos, lib = lib)
  message("BIOSZEN was updated. Restart R before launching the app again.")
  invisible(TRUE)
}
