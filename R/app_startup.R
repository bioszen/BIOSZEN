bioszen_r_version_key <- function() {
  minor <- strsplit(R.version$minor, ".", fixed = TRUE)[[1]][1]
  paste(R.version$major, minor, sep = ".")
}

bioszen_current_library_from_stale <- function(path,
                                                version_key = bioszen_r_version_key()) {
  path <- gsub("\\\\", "/", as.character(path[[1]]))
  substitutions <- c(
    "(?i)(/win-library/)[0-9]+\\.[0-9]+$" = paste0("\\1", version_key),
    "(?i)(/[^/]+-library/)[0-9]+\\.[0-9]+$" = paste0("\\1", version_key),
    "(?i)(/Library/R/(?:arm64|x86_64)/)[0-9]+\\.[0-9]+(/library)$" =
      paste0("\\1", version_key, "\\2"),
    "(?i)(/Library/R/)[0-9]+\\.[0-9]+(/library)$" =
      paste0("\\1", version_key, "\\2")
  )

  for (pattern in names(substitutions)) {
    if (!grepl(pattern, path, perl = TRUE)) next
    candidate <- sub(pattern, substitutions[[pattern]], path, perl = TRUE)
    if (!identical(candidate, path)) return(candidate)
  }
  NULL
}

bioszen_is_current_versioned_library <- function(
    path,
    version_key = bioszen_r_version_key()) {
  path <- gsub("\\\\", "/", as.character(path[[1]]))
  version_pattern <- gsub("\\.", "\\\\.", version_key)
  patterns <- c(
    paste0("(?i)/win-library/", version_pattern, "$"),
    paste0("(?i)/[^/]+-library/", version_pattern, "$"),
    paste0("(?i)/Library/R/(?:arm64|x86_64)/", version_pattern, "/library$"),
    paste0("(?i)/Library/R/", version_pattern, "/library$")
  )
  any(vapply(patterns, grepl, logical(1), x = path, perl = TRUE))
}

bioszen_managed_runtime_library <- function(
    version_key = bioszen_r_version_key(),
    platform = R.version$platform,
    root = getOption("BIOSZEN.runtime_root", NULL)) {
  if (is.null(root) || !length(root) || is.na(root[[1]]) || !nzchar(root[[1]])) {
    root <- tools::R_user_dir("BIOSZEN", which = "data")
  }
  file.path(
    path.expand(as.character(root[[1]])),
    "runtime-library",
    version_key,
    platform
  )
}

bioszen_configure_graphics_cache <- function(
    root = getOption("BIOSZEN.graphics_cache_root", NULL)) {
  if (is.null(root) || !length(root) || is.na(root[[1]]) || !nzchar(root[[1]])) {
    root <- file.path(tools::R_user_dir("BIOSZEN", which = "cache"), "gdtools")
  }
  root <- path.expand(as.character(root[[1]]))
  ready <- dir.exists(root) ||
    dir.create(root, recursive = TRUE, showWarnings = FALSE)
  if (!ready || file.access(root, 2) != 0) {
    root <- file.path(tempdir(), "BIOSZEN-gdtools-cache")
    dir.create(root, recursive = TRUE, showWarnings = FALSE)
  }
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  options(GDTOOLS_CACHE_DIR = root)
  Sys.setenv(GDTOOLS_CACHE_DIR = root)
  invisible(root)
}

bioszen_select_writable_library <- function(candidates) {
  candidates <- unique(as.character(candidates))
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  for (candidate in candidates) {
    candidate <- normalizePath(candidate, winslash = "/", mustWork = FALSE)
    ready <- dir.exists(candidate) || suppressWarnings(
      dir.create(candidate, recursive = TRUE, showWarnings = FALSE)
    )
    if (!ready || file.access(candidate, 2) != 0) next
    return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
  }
  ""
}

bioszen_find_package_in_libraries <- function(package, libraries = .libPaths()) {
  candidates <- file.path(libraries, package)
  present <- candidates[
    dir.exists(candidates) & file.exists(file.path(candidates, "DESCRIPTION"))
  ]
  if (length(present)) {
    normalizePath(present[[1]], winslash = "/", mustWork = TRUE)
  } else {
    ""
  }
}

bioszen_package_build_info <- function(package, libraries = .libPaths()) {
  package_path <- bioszen_find_package_in_libraries(package, libraries)
  if (!nzchar(package_path)) {
    return(list(path = "", library = "", built_r = NA_character_, compiled = NA))
  }

  description <- read.dcf(file.path(package_path, "DESCRIPTION"))
  built <- if ("Built" %in% colnames(description)) {
    unname(description[1, "Built"])
  } else {
    ""
  }
  built <- if (is.na(built)) "" else built
  built_r <- if (grepl("^R [0-9]+\\.[0-9]+", built)) {
    sub("^R ([0-9]+\\.[0-9]+).*$", "\\1", built)
  } else {
    NA_character_
  }
  needs_compilation <- if ("NeedsCompilation" %in% colnames(description)) {
    unname(description[1, "NeedsCompilation"])
  } else {
    "no"
  }
  compiled <- identical(
    tolower(if (is.na(needs_compilation)) "no" else needs_compilation),
    "yes"
  )

  list(
    path = package_path,
    library = dirname(package_path),
    built_r = built_r,
    compiled = compiled
  )
}

bioszen_pptx_runtime_packages <- function() {
  c("Rcpp", "systemfonts", "xml2", "gdtools", "officer", "rvg")
}

bioszen_pptx_runtime_compatible <- function(libraries = .libPaths(),
                                             required_library = NULL) {
  expected <- bioszen_r_version_key()
  required_library <- if (is.null(required_library)) NULL else {
    normalizePath(required_library, winslash = "/", mustWork = FALSE)
  }

  all(vapply(bioszen_pptx_runtime_packages(), function(package) {
    info <- bioszen_package_build_info(package, libraries)
    if (!nzchar(info$path)) return(FALSE)
    if (!is.null(required_library) && !identical(info$library, required_library)) {
      return(FALSE)
    }
    !isTRUE(info$compiled) || identical(info$built_r, expected)
  }, logical(1)))
}

bioszen_prepare_installed_runtime <- function(
    libraries = .libPaths(),
    repos = c(
      Officeverse = "https://davidgohel.r-universe.dev",
      CRAN = "https://cloud.r-project.org"
    ),
    install_fun = function(packages, lib, repos) {
      utils::install.packages(
        packages,
        lib = lib,
        repos = repos,
        dependencies = NA
      )
    },
    set_library_paths = .libPaths,
    loaded_namespaces = loadedNamespaces()) {
  bioszen_configure_graphics_cache()

  if (bioszen_pptx_runtime_compatible(libraries)) {
    return(invisible(list(repaired = FALSE, library = NULL)))
  }

  rvg_info <- bioszen_package_build_info("rvg", libraries)
  candidate_libraries <- unique(c(rvg_info$library, libraries))
  mapped_targets <- Filter(
    Negate(is.null),
    lapply(candidate_libraries[nzchar(candidate_libraries)], bioszen_current_library_from_stale)
  )
  current_targets <- candidate_libraries[
    nzchar(candidate_libraries) &
      vapply(candidate_libraries, bioszen_is_current_versioned_library, logical(1))
  ]
  targets <- unique(c(
    mapped_targets,
    current_targets,
    bioszen_managed_runtime_library()
  ))

  target <- bioszen_select_writable_library(targets)
  if (!nzchar(target)) {
    warning(
      "BIOSZEN could not create a writable per-user library for the editable ",
      "PowerPoint runtime. The app will continue and PowerPoint export will ",
      "use its compatibility fallback.",
      call. = FALSE
    )
    return(invisible(list(
      repaired = FALSE,
      library = NULL,
      vector_available = FALSE,
      error = "no writable runtime library"
    )))
  }
  runtime_libraries <- unique(c(target, libraries))
  set_library_paths(runtime_libraries)
  options(BIOSZEN.runtime_lib = target)

  if (bioszen_pptx_runtime_compatible(runtime_libraries, target)) {
    return(invisible(list(repaired = FALSE, library = target)))
  }

  message(
    "BIOSZEN detected packages from an older R library. Preparing compatible ",
    "PowerPoint graphics packages for R ", bioszen_r_version_key(), " in ", target, "."
  )
  bioszen_configure_package_download()
  install_error <- tryCatch(
    {
      install_fun(bioszen_pptx_runtime_packages(), lib = target, repos = repos)
      NULL
    },
    error = function(e) e
  )

  runtime_ready <- is.null(install_error) &&
    bioszen_pptx_runtime_compatible(runtime_libraries, target)
  if (!runtime_ready) {
    set_library_paths(libraries)
    options(BIOSZEN.runtime_lib = NULL)
    reason <- if (is.null(install_error)) {
      "the installed packages did not pass the compatibility check"
    } else {
      conditionMessage(install_error)
    }
    warning(
      "BIOSZEN could not prepare the editable PowerPoint runtime for R ",
      bioszen_r_version_key(), " (", reason, "). The app will continue; ",
      "PowerPoint export will use its compatibility fallback until the runtime ",
      "can be prepared.",
      call. = FALSE
    )
    return(invisible(list(
      repaired = FALSE,
      library = target,
      vector_available = FALSE,
      error = reason
    )))
  }
  if ("rvg" %in% loaded_namespaces) {
    warning(
      "A previous rvg binary is already loaded. BIOSZEN installed the compatible ",
      "runtime in ", target, "; editable PowerPoint export will be available after ",
      "R is restarted once.",
      call. = FALSE
    )
  }

  invisible(list(
    repaired = TRUE,
    library = target,
    vector_available = !"rvg" %in% loaded_namespaces,
    restart_required = "rvg" %in% loaded_namespaces
  ))
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
  !is.null(bioszen_current_library_from_stale(path))
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
  options(BIOSZEN.local_lib = lib)
  Sys.setenv(BIOSZEN_LOCAL_LIB = lib)
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
  bioszen_configure_graphics_cache()

  if (isTRUE(install)) {
    bioszen_install_missing_dependencies(root)
  }

  invisible(.libPaths())
}
