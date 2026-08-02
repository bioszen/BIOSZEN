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

bioszen_runtime_repositories <- function() {
  # Native graphics binaries must match the running R minor version. CRAN
  # publishes platform-specific binaries for current R releases; development
  # R-universe binaries can otherwise satisfy the package version while still
  # carrying an incompatible graphics ABI.
  c(CRAN = "https://cloud.r-project.org")
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

bioszen_parse_package_dependencies <- function(value) {
  if (!length(value) || is.na(value[[1]]) || !nzchar(value[[1]])) {
    return(character())
  }
  dependencies <- unlist(strsplit(gsub("\n", " ", value[[1]], fixed = TRUE), ","))
  dependencies <- trimws(sub("\\s*\\([^)]*\\)", "", dependencies))
  unique(dependencies[nzchar(dependencies) & dependencies != "R"])
}

bioszen_package_dependencies_in_libraries <- function(
    package,
    libraries = .libPaths(),
    fields = c("Depends", "Imports", "LinkingTo")) {
  package_path <- bioszen_find_package_in_libraries(package, libraries)
  if (!nzchar(package_path)) return(character())

  description <- read.dcf(file.path(package_path, "DESCRIPTION"))
  fields <- intersect(fields, colnames(description))
  if (!length(fields)) return(character())
  unique(unlist(lapply(fields, function(field) {
    bioszen_parse_package_dependencies(unname(description[1, field]))
  })))
}

bioszen_runtime_dependency_closure <- function(packages,
                                                libraries = .libPaths()) {
  pending <- unique(as.character(packages))
  pending <- pending[!is.na(pending) & nzchar(pending) & pending != "R"]
  resolved <- character()

  while (length(pending)) {
    package <- pending[[1]]
    pending <- pending[-1]
    if (package %in% resolved) next
    resolved <- c(resolved, package)
    dependencies <- bioszen_package_dependencies_in_libraries(package, libraries)
    pending <- unique(c(pending, setdiff(dependencies, resolved)))
  }
  resolved
}

bioszen_installed_runtime_packages <- function(libraries = .libPaths()) {
  direct <- bioszen_package_dependencies_in_libraries(
    "BIOSZEN",
    libraries,
    fields = c("Depends", "Imports")
  )
  bioszen_runtime_dependency_closure(direct, libraries)
}

bioszen_runtime_repair_packages <- function(libraries = .libPaths()) {
  runtime_packages <- bioszen_installed_runtime_packages(libraries)
  expected <- bioszen_r_version_key()
  repair <- runtime_packages[vapply(runtime_packages, function(package) {
    info <- bioszen_package_build_info(package, libraries)
    !nzchar(info$path) ||
      (isTRUE(info$compiled) && !identical(info$built_r, expected))
  }, logical(1))]

  if (!bioszen_pptx_runtime_compatible(libraries)) {
    repair <- c(bioszen_pptx_runtime_packages(), repair)
  }
  unique(repair)
}

bioszen_prepare_installed_runtime <- function(
    libraries = .libPaths(),
    repos = bioszen_runtime_repositories(),
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

  repair_packages <- bioszen_runtime_repair_packages(libraries)
  if (!length(repair_packages)) {
    return(invisible(list(repaired = FALSE, library = NULL)))
  }

  package_libraries <- vapply(repair_packages, function(package) {
    bioszen_package_build_info(package, libraries)$library
  }, character(1))
  candidate_libraries <- unique(c(package_libraries, libraries))
  mapped_targets <- Filter(
    Negate(is.null),
    lapply(candidate_libraries[nzchar(candidate_libraries)], bioszen_current_library_from_stale)
  )
  current_targets <- candidate_libraries[
    nzchar(candidate_libraries) &
      vapply(candidate_libraries, bioszen_is_current_versioned_library, logical(1))
  ]
  existing_targets <- candidate_libraries[
    nzchar(candidate_libraries) &
      !vapply(candidate_libraries, bioszen_is_stale_user_library, logical(1))
  ]
  targets <- unique(c(
    mapped_targets,
    current_targets,
    existing_targets
  ))

  target <- bioszen_select_writable_library(targets)
  if (!nzchar(target)) {
    warning(
      "BIOSZEN could not create a writable per-user library for its compiled ",
      "runtime dependencies. The app will continue; unavailable features will ",
      "report their normal compatibility error or use their supported fallback.",
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

  repair_packages <- bioszen_runtime_repair_packages(runtime_libraries)
  if (!length(repair_packages)) {
    return(invisible(list(repaired = FALSE, library = target)))
  }

  message(
    "BIOSZEN detected missing packages or compiled packages from another R ",
    "version. Preparing a compatible runtime for R ", bioszen_r_version_key(),
    " in ", target, "."
  )
  bioszen_configure_package_download()
  install_error <- tryCatch(
    {
      install_fun(repair_packages, lib = target, repos = repos)
      NULL
    },
    error = function(e) e
  )

  remaining_repairs <- if (is.null(install_error)) {
    bioszen_runtime_repair_packages(runtime_libraries)
  } else {
    repair_packages
  }
  runtime_ready <- is.null(install_error) && !length(remaining_repairs)
  if (!runtime_ready) {
    set_library_paths(libraries)
    options(BIOSZEN.runtime_lib = NULL)
    reason <- if (is.null(install_error)) {
      "the installed packages did not pass the compatibility check"
    } else {
      conditionMessage(install_error)
    }
    warning(
      "BIOSZEN could not prepare all runtime dependencies for R ",
      bioszen_r_version_key(), " (", reason, "). The app will continue; ",
      "affected features will remain unavailable or use their supported ",
      "compatibility fallback until the runtime can be prepared.",
      call. = FALSE
    )
    return(invisible(list(
      repaired = FALSE,
      library = target,
      vector_available = FALSE,
      error = reason
    )))
  }
  loaded_repairs <- intersect(repair_packages, loaded_namespaces)
  if (length(loaded_repairs)) {
    warning(
      "Previously loaded package binaries were repaired in ", target,
      ". Restart R once before using: ", paste(loaded_repairs, collapse = ", "), ".",
      call. = FALSE
    )
  }

  invisible(list(
    repaired = TRUE,
    library = target,
    packages = repair_packages,
    vector_available = !any(bioszen_pptx_runtime_packages() %in% loaded_repairs),
    restart_required = length(loaded_repairs) > 0L
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
