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
  options(BIOSZEN.update_after_app = TRUE)
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
