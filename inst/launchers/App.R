# app.R (backend) - BIOSZEN launcher
# Goals:
# 1) Fix host/port for the launcher
# 2) Open the app in a browser app window when possible (prefer Chrome; else default browser)
# 3) Use a local ./R_libs/<R major.minor>
# 4) Install BIOSZEN from embedded/nearby archives or extracted sources if needed

options(
  shiny.host = "127.0.0.1",
  shiny.port = 4321
)

# -------- paths --------
get_script_path <- function() {
  args0 <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args0, value = TRUE)
  script_path <- if (length(file_arg)) sub("^--file=", "", file_arg[1]) else ""
  script_path <- if (nzchar(script_path)) normalizePath(script_path, winslash = "/", mustWork = TRUE) else ""

  probe_source_calls <- function() {
    calls <- rev(sys.calls())
    for (call in calls) {
      if (!is.call(call)) next
      if (!identical(call[[1]], as.symbol("source"))) next

      args <- as.list(call)
      file_arg <- args$file
      if (is.null(file_arg) && length(args) >= 2) {
        file_arg <- args[[2]]
      }

      if (is.character(file_arg) && nzchar(file_arg)) {
        candidate <- tryCatch(
          normalizePath(file_arg, winslash = "/", mustWork = TRUE),
          error = function(e) ""
        )
        if (nzchar(candidate)) return(candidate)
      }
    }
    ""
  }

  probe_srcfile <- function(env) {
    srcfile <- tryCatch(attr(env, "srcfile"), error = function(e) NULL)
    if (is.null(srcfile)) return("")
    filename <- tryCatch(attr(srcfile, "filename"), error = function(e) NULL)
    if (!is.character(filename) || !nzchar(filename)) return("")
    filename
  }

  if (!nzchar(script_path)) {
    frames <- rev(sys.frames())
    candidates <- c(

      probe_source_calls(),
      tryCatch(sys.frame(1)$ofile, error = function(e) ""),
      vapply(frames, probe_srcfile, character(1)),
      tryCatch(parent.frame()$ofile, error = function(e) "")
    )
    candidates <- candidates[nzchar(candidates) & file.exists(candidates)]
    if (length(candidates)) {
      script_path <- normalizePath(candidates[[1]], winslash = "/", mustWork = TRUE)
    }
  }

  script_path
}

script_path <- get_script_path()
script_dir <- if (nzchar(script_path)) dirname(script_path) else getwd()
script_dir <- normalizePath(script_dir, winslash = "/", mustWork = TRUE)

root_dir <- script_dir
if (grepl("/inst/launchers$", script_dir)) {
  root_dir <- dirname(dirname(script_dir))
}
root_dir <- normalizePath(root_dir, winslash = "/", mustWork = FALSE)

# -------- log --------
log_file <- file.path(script_dir, "bioszen_r.log")
log_sink_depth <- sink.number()
try({
  sink(log_file, append = TRUE, split = TRUE)
  on.exit({
    while (sink.number() > log_sink_depth) {
      before <- sink.number()
      try(sink(), silent = TRUE)
      if (sink.number() >= before) break
    }
  }, add = TRUE)
  cat("\n=== BIOSZEN App.R start ===\n")
  cat("Time: ", as.character(Sys.time()), "\n", sep = "")
  cat("R: ", R.version.string, "\n", sep = "")
  cat("script_dir: ", script_dir, "\n", sep = "")
  cat("root_dir  : ", root_dir, "\n", sep = "")
}, silent = TRUE)

if (getRversion() < "4.1.0") {
  stop("BIOSZEN requires R >= 4.1.0.")
}

# -------- local library (self-contained) --------
r_version_key <- function() {
  minor <- strsplit(R.version$minor, ".", fixed = TRUE)[[1]][1]
  paste(R.version$major, minor, sep = ".")
}

is_stale_windows_user_library <- function(path) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  expected <- paste0("/win-library/", r_version_key())
  grepl("/win-library/[0-9]+\\.[0-9]+$", path) && !grepl(expected, path, fixed = TRUE)
}

configure_local_library <- function(root) {
  lib_dir <- file.path(root, "R_libs", r_version_key())
  dir.create(lib_dir, showWarnings = FALSE, recursive = TRUE)

  existing <- .libPaths()
  existing <- existing[!vapply(existing, is_stale_windows_user_library, logical(1))]
  .libPaths(unique(c(lib_dir, existing)))

  normalizePath(lib_dir, winslash = "/", mustWork = TRUE)
}

local_lib <- configure_local_library(root_dir)
Sys.setenv(BIOSZEN_LOCAL_LIB = local_lib)
options(BIOSZEN.local_lib = local_lib)
cat(".libPaths():\n")
print(.libPaths())

pkg <- "BIOSZEN"

bioszen_startup_citation <- function() {
  if (!isTRUE(getOption("BIOSZEN.show_startup_citation", TRUE))) {
    return(invisible(FALSE))
  }
  if (isTRUE(getOption("BIOSZEN.startup_citation_shown", FALSE))) {
    return(invisible(FALSE))
  }

  options(BIOSZEN.startup_citation_shown = TRUE)
  packageStartupMessage(paste(
    "##",
    "## BIOSZEN",
    "## See https://github.com/bioszen/BIOSZEN for additional documentation and source code.",
    "## Please cite software as:",
    "##   Szenfeld, B. (2026). BIOSZEN. Zenodo. https://doi.org/10.5281/zenodo.18217210",
    "##",
    sep = "\n"
  ))
  invisible(TRUE)
}

# -------- base64 decode (pure R) --------
base64_decode <- function(x) {
  x <- gsub("\\s", "", x)
  if (!nzchar(x)) return(raw(0))

  pad_count <- nchar(gsub("[^=]", "", x))
  x <- gsub("=", "A", x, fixed = TRUE)

  alphabet <- c(LETTERS, letters, 0:9, "+", "/")
  chars <- strsplit(x, "", fixed = TRUE)[[1]]
  vals <- match(chars, alphabet) - 1L
  if (any(is.na(vals))) stop("Invalid base64 payload.")
  if (length(vals) %% 4 != 0) stop("Invalid base64 length.")

  m <- matrix(vals, ncol = 4, byrow = TRUE)
  b1 <- bitwShiftL(m[, 1], 2) + bitwShiftR(m[, 2], 4)
  b2 <- bitwShiftL(bitwAnd(m[, 2], 15), 4) + bitwShiftR(m[, 3], 2)
  b3 <- bitwShiftL(bitwAnd(m[, 3], 3), 6) + m[, 4]
  bytes <- as.raw(c(rbind(b1, b2, b3)))

  if (pad_count > 0) {
    keep <- length(bytes) - pad_count
    if (keep > 0) {
      bytes <- bytes[seq_len(keep)]
    } else {
      bytes <- raw(0)
    }
  }
  bytes
}

read_embedded_payload <- function(path, with_raw = TRUE) {
  if (!nzchar(path) || !file.exists(path)) return(NULL)
  lines <- readLines(path, warn = FALSE)
  marker_idx <- grep("^##==BIOSZEN_PAYLOAD:", lines)
  if (!length(marker_idx)) return(NULL)

  marker <- lines[marker_idx[1]]
  payload_name <- sub("^##==BIOSZEN_PAYLOAD:(.*)==##$", "\\1", marker)
  if (!nzchar(payload_name) || identical(payload_name, marker)) {
    payload_name <- "BIOSZEN_payload.bin"
  }

  if (marker_idx[1] >= length(lines)) return(NULL)
  if (!with_raw) {
    return(list(name = payload_name, raw = NULL))
  }

  payload_lines <- lines[(marker_idx[1] + 1L):length(lines)]
  if (!length(payload_lines)) return(NULL)

  payload_lines <- sub("^#\\s?", "", payload_lines)
  payload_b64 <- paste(payload_lines, collapse = "")
  if (!nzchar(payload_b64)) return(NULL)

  list(name = payload_name, raw = base64_decode(payload_b64))
}

# -------- version helpers --------
parse_pkg_version <- function(x) {
  x <- unname(x)[1]
  if (!is.character(x) || !nzchar(x)) return(NULL)
  tryCatch(package_version(x), error = function(e) NULL)
}

extract_version_from_filename <- function(path, pkg_name = "BIOSZEN") {
  base <- basename(path)
  pattern <- paste0("^", pkg_name, "[-_]v?([0-9A-Za-z\\.\\-]+)\\.(tar\\.gz|tgz|tar|zip)$")
  match <- regexec(pattern, base, ignore.case = TRUE)
  parts <- regmatches(base, match)[[1]]
  if (length(parts) >= 2) return(parse_pkg_version(parts[2]))
  NULL
}

read_archive_description <- function(archive_path) {
  if (!file.exists(archive_path)) return(NULL)

  is_zip <- grepl("\\.zip$", archive_path, ignore.case = TRUE)
  entries <- tryCatch(
    if (is_zip) utils::unzip(archive_path, list = TRUE)$Name else utils::untar(archive_path, list = TRUE),
    error = function(e) character(0)
  )
  if (!length(entries)) return(NULL)

  desc_entry <- entries[grepl("(^|/|\\\\)DESCRIPTION$", entries)][1]
  if (!nzchar(desc_entry)) return(NULL)
  desc_entry <- sub("^\\./", "", desc_entry)

  exdir <- tempfile("bioszen_desc_")
  dir.create(exdir)
  on.exit(unlink(exdir, recursive = TRUE, force = TRUE), add = TRUE)

  ok <- tryCatch({
    if (is_zip) {
      utils::unzip(archive_path, files = desc_entry, exdir = exdir)
    } else {
      utils::untar(archive_path, files = desc_entry, exdir = exdir)
    }
    TRUE
  }, error = function(e) FALSE)
  if (!ok) return(NULL)

  desc_path <- file.path(exdir, desc_entry)
  if (!file.exists(desc_path)) return(NULL)
  dcf <- tryCatch(read.dcf(desc_path), error = function(e) NULL)
  if (is.null(dcf) || nrow(dcf) < 1) return(NULL)
  dcf
}

read_archive_description_version <- function(archive_path, pkg_name = "BIOSZEN") {
  dcf <- read_archive_description(archive_path)
  if (is.null(dcf)) return(NULL)
  if (!"Version" %in% colnames(dcf)) return(NULL)
  if ("Package" %in% colnames(dcf)) {
    pkg <- unname(dcf[1, "Package"])
    if (nzchar(pkg) && !identical(tolower(pkg), tolower(pkg_name))) return(NULL)
  }

  parse_pkg_version(unname(dcf[1, "Version"]))
}

get_archive_version <- function(archive_path, pkg_name = "BIOSZEN") {
  ver <- extract_version_from_filename(archive_path, pkg_name)
  if (!is.null(ver)) return(ver)
  read_archive_description_version(archive_path, pkg_name)
}

extract_version_from_dirname <- function(path, pkg_name = "BIOSZEN") {
  base <- basename(normalizePath(path, winslash = "/", mustWork = FALSE))
  pattern <- paste0("^", pkg_name, "[-_]v?([0-9A-Za-z\\.\\-]+)$")
  match <- regexec(pattern, base, ignore.case = TRUE)
  parts <- regmatches(base, match)[[1]]
  if (length(parts) >= 2) return(parse_pkg_version(parts[2]))
  NULL
}

read_source_description <- function(source_dir) {
  desc_path <- file.path(source_dir, "DESCRIPTION")
  if (!file.exists(desc_path)) return(NULL)

  dcf <- tryCatch(read.dcf(desc_path), error = function(e) NULL)
  if (is.null(dcf) || nrow(dcf) < 1) return(NULL)
  dcf
}

read_source_description_version <- function(source_dir, pkg_name = "BIOSZEN") {
  dcf <- read_source_description(source_dir)
  if (is.null(dcf)) return(NULL)
  if (!"Version" %in% colnames(dcf)) return(NULL)
  if ("Package" %in% colnames(dcf)) {
    pkg <- unname(dcf[1, "Package"])
    if (nzchar(pkg) && !identical(tolower(pkg), tolower(pkg_name))) return(NULL)
  }

  parse_pkg_version(unname(dcf[1, "Version"]))
}

get_source_version <- function(source_dir, pkg_name = "BIOSZEN") {
  ver <- read_source_description_version(source_dir, pkg_name)
  if (!is.null(ver)) return(ver)
  extract_version_from_dirname(source_dir, pkg_name)
}

is_package_source_dir <- function(source_dir, pkg_name = "BIOSZEN") {
  dcf <- read_source_description(source_dir)
  if (is.null(dcf) || !"Package" %in% colnames(dcf)) return(FALSE)
  pkg <- unname(dcf[1, "Package"])
  nzchar(pkg) && identical(tolower(pkg), tolower(pkg_name))
}

find_package_source_dirs <- function(base_dir, pkg_name = "BIOSZEN") {
  if (!dir.exists(base_dir)) return(character(0))

  self <- if (is_package_source_dir(base_dir, pkg_name)) base_dir else character(0)
  top_dirs <- list.dirs(base_dir, recursive = FALSE, full.names = TRUE)
  if (!length(top_dirs)) {
    if (!length(self)) return(character(0))
    return(normalizePath(self, winslash = "/", mustWork = TRUE))
  }

  direct <- top_dirs[vapply(top_dirs, is_package_source_dir, logical(1), pkg_name = pkg_name)]
  versioned_dir <- paste0("^", pkg_name, "([-_].*)?$")
  source_parents <- top_dirs[grepl(versioned_dir, basename(top_dirs), ignore.case = TRUE)]
  nested <- unlist(lapply(source_parents, function(parent) {
    child_dirs <- list.dirs(parent, recursive = FALSE, full.names = TRUE)
    child_dirs[vapply(child_dirs, is_package_source_dir, logical(1), pkg_name = pkg_name)]
  }), use.names = FALSE)

  candidates <- unique(c(self, direct, nested))
  candidates <- candidates[dir.exists(candidates)]
  normalizePath(candidates, winslash = "/", mustWork = TRUE)
}

source_dir_info <- function(path, pkg_name = "BIOSZEN") {
  list(
    path = path,
    kind = "source_dir",
    version = get_source_version(path, pkg_name),
    mtime = tryCatch(file.info(path)$mtime, error = function(e) as.POSIXct(NA))
  )
}

get_local_version <- function(pkg_name, lib_dir) {
  tryCatch(packageVersion(pkg_name, lib.loc = lib_dir), error = function(e) NULL)
}

archive_fingerprint <- function(archive_path) {
  if (is.null(archive_path) || !file.exists(archive_path)) return("")
  value <- tryCatch(unname(tools::md5sum(archive_path)), error = function(e) "")
  if (is.na(value)) "" else value
}

archive_marker_path <- function(lib_dir, pkg_name = "BIOSZEN") {
  file.path(lib_dir, paste0(".", pkg_name, "_archive_md5"))
}

archive_matches_local_install <- function(archive_path, lib_dir, pkg_name = "BIOSZEN") {
  fingerprint <- archive_fingerprint(archive_path)
  marker <- archive_marker_path(lib_dir, pkg_name)
  if (!nzchar(fingerprint) || !file.exists(marker)) return(FALSE)

  recorded <- tryCatch(readLines(marker, warn = FALSE), error = function(e) character(0))
  length(recorded) && identical(recorded[[1]], fingerprint)
}

write_archive_marker <- function(archive_path, lib_dir, pkg_name = "BIOSZEN") {
  fingerprint <- archive_fingerprint(archive_path)
  if (!nzchar(fingerprint)) return(invisible(FALSE))
  tryCatch({
    writeLines(fingerprint, archive_marker_path(lib_dir, pkg_name), useBytes = TRUE)
    TRUE
  }, error = function(e) FALSE)
}

pick_best_archive <- function(infos) {
  if (!length(infos)) return(NULL)

  with_version <- Filter(function(x) !is.null(x$version), infos)
  if (length(with_version)) {
    best <- with_version[[1]]
    if (length(with_version) > 1) {
      for (info in with_version[-1]) {
        if (info$version > best$version) {
          best <- info
        } else if (info$version == best$version) {
          if (is.na(best$mtime) || (!is.na(info$mtime) && info$mtime > best$mtime)) {
            best <- info
          }
        }
      }
    }
    return(best)
  }

  best <- infos[[1]]
  if (length(infos) > 1) {
    for (info in infos[-1]) {
      if (is.na(best$mtime)) {
        best <- info
      } else if (!is.na(info$mtime) && info$mtime > best$mtime) {
        best <- info
      }
    }
  }
  best
}

pick_best_package_candidate <- function(archive_choice, source_choice) {
  if (is.null(archive_choice)) return(source_choice)
  archive_choice
}

# -------- dependency helpers --------
normalize_dep_field <- function(x) {
  if (is.null(x) || !nzchar(x)) return(character(0))
  x <- gsub("[\r\n]", " ", x)
  x <- gsub("\\s*\\([^\\)]+\\)", "", x)
  parts <- unlist(strsplit(x, ",", fixed = TRUE), use.names = FALSE)
  parts <- trimws(parts)
  parts[nzchar(parts)]
}

deps_from_dcf <- function(dcf) {
  if (is.null(dcf) || nrow(dcf) < 1) return(character(0))

  fields <- c("Depends", "Imports", "LinkingTo")
  deps <- unlist(lapply(fields, function(field) {
    if (!field %in% colnames(dcf)) return(character(0))
    normalize_dep_field(dcf[1, field])
  }), use.names = FALSE)
  deps <- unique(deps)
  deps[deps != "R"]
}

get_pkg_dependencies <- function(pkg_name, lib_dir) {
  desc_path <- file.path(lib_dir, pkg_name, "DESCRIPTION")
  if (!file.exists(desc_path)) return(character(0))

  dcf <- tryCatch(read.dcf(desc_path), error = function(e) NULL)
  deps_from_dcf(dcf)
}

get_archive_dependencies <- function(archive_path) {
  dcf <- read_archive_description(archive_path)
  deps_from_dcf(dcf)
}

get_source_dependencies <- function(source_dir) {
  dcf <- read_source_description(source_dir)
  deps_from_dcf(dcf)
}

ensure_cran_repo <- function() {
  repos <- getOption("repos")
  cran <- if (is.null(repos)) "" else repos[["CRAN"]]
  if (is.null(cran) || is.na(cran) || !nzchar(cran) || cran == "@CRAN@") {
    options(repos = c(CRAN = "https://cran.rstudio.com"))
  }

  invisible(TRUE)
}

cran_repo_candidates <- function() {
  repos <- getOption("repos")
  current <- if (is.null(repos)) "" else unname(repos[["CRAN"]])
  current <- current[!is.na(current)]
  unique(c(current, "https://cran.rstudio.com", "https://cloud.r-project.org", "https://cran.r-project.org"))
}

download_method_candidates <- function() {
  current <- getOption("download.file.method", "")
  current <- if (is.null(current)) "" else as.character(current)
  if (.Platform$OS.type == "windows") {
    unique(c(current, "auto", "wininet", "libcurl"))
  } else {
    unique(c(current, "auto", "libcurl"))
  }
}

read_available_packages_once <- function(repo, method) {
  options(repos = c(CRAN = repo))
  if (nzchar(method)) {
    options(download.file.method = method)
  }

  warnings <- character(0)
  db <- tryCatch(
    withCallingHandlers(
      utils::available.packages(),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      warnings <<- c(warnings, conditionMessage(e))
      NULL
    }
  )

  if (!is.null(db) && nrow(db) > 0) {
    method_label <- if (nzchar(method)) method else "default"
    cat("[deps] Using CRAN mirror ", repo, " with download method ", method_label, ".\n", sep = "")
    return(db)
  }

  if (length(warnings)) {
    method_label <- if (nzchar(method)) method else "default"
    cat(
      "[deps] CRAN metadata unavailable via ", repo,
      " (", method_label, "): ", warnings[[1]], "\n",
      sep = ""
    )
  }
  NULL
}

read_available_package_db <- function() {
  ensure_cran_repo()

  repos <- cran_repo_candidates()
  repos <- repos[nzchar(repos) & repos != "@CRAN@"]
  methods <- download_method_candidates()

  for (repo in repos) {
    for (method in methods) {
      db <- read_available_packages_once(repo, method)
      if (!is.null(db)) return(db)
    }
  }

  NULL
}

loadable_package <- function(package, lib_dir = NULL) {
  tryCatch(
    requireNamespace(package, quietly = TRUE, lib.loc = lib_dir),
    error = function(e) FALSE
  )
}

missing_packages <- function(packages, lib_dir = NULL) {
  packages <- unique(packages[nzchar(packages) & packages != "R"])
  packages[!vapply(packages, loadable_package, logical(1), lib_dir = lib_dir)]
}

default_library_packages <- function() {
  rownames(installed.packages(priority = c("base", "recommended")))
}

installed_in_local_library <- function(lib_dir) {
  if (!dir.exists(lib_dir)) return(character(0))
  rownames(utils::installed.packages(lib.loc = lib_dir, noCache = TRUE))
}

packages_missing_from_local_library <- function(packages, lib_dir) {
  packages <- unique(packages[nzchar(packages) & packages != "R"])
  setdiff(packages, unique(c(installed_in_local_library(lib_dir), default_library_packages())))
}

loadable_with_local_library <- function(package, lib_dir) {
  old_libs <- .libPaths()
  on.exit(.libPaths(old_libs), add = TRUE)

  libs <- old_libs[!vapply(old_libs, is_stale_windows_user_library, logical(1))]
  .libPaths(unique(c(lib_dir, libs)))

  loadable_package(package, lib_dir = lib_dir)
}

install_dependency_packages <- function(packages, lib_dir) {
  if (!length(packages)) return(invisible(TRUE))

  ensure_cran_repo()
  cat("[deps] Installing missing packages: ", paste(packages, collapse = ", "), "\n", sep = "")
  ok <- tryCatch({
    utils::install.packages(
      packages,
      lib = lib_dir,
      dependencies = NA
    )
    TRUE
  }, error = function(e) {
    cat("[deps] install.packages failed: ", conditionMessage(e), "\n", sep = "")
    FALSE
  })
  if (!ok) stop("Failed installing dependencies. Check bioszen_r.log.")
  invisible(TRUE)
}

repair_unloadable_packages <- function(packages, lib_dir) {
  packages <- unique(packages[nzchar(packages) & packages != "R"])
  if (!length(packages)) return(invisible(TRUE))

  cat("[deps] Repairing packages not loadable from local library: ", paste(packages, collapse = ", "), "\n", sep = "")
  package_dirs <- file.path(lib_dir, packages)
  unlink(package_dirs[dir.exists(package_dirs)], recursive = TRUE, force = TRUE)
  install_dependency_packages(packages, lib_dir)
  invisible(TRUE)
}

resolve_dependency_closure <- function(deps) {
  deps <- unique(deps[nzchar(deps) & deps != "R"])
  if (!length(deps)) return(character(0))

  ensure_cran_repo()
  db <- read_available_package_db()
  if (is.null(db)) return(deps)

  available <- rownames(db)
  packages_with_metadata <- intersect(deps, available)
  packages_without_metadata <- setdiff(deps, available)
  if (length(packages_without_metadata)) {
    cat("[deps] No CRAN metadata for: ", paste(packages_without_metadata, collapse = ", "), "\n", sep = "")
  }

  recursive <- character(0)
  if (length(packages_with_metadata)) {
    recursive <- tryCatch({
      deps_map <- tools::package_dependencies(
        packages_with_metadata,
        db = db,
        which = c("Depends", "Imports", "LinkingTo"),
        recursive = TRUE
      )
      unlist(deps_map, use.names = FALSE)
    }, error = function(e) {
      cat("[deps] Could not resolve recursive CRAN dependencies: ", conditionMessage(e), "\n", sep = "")
      character(0)
    })
  }

  setdiff(unique(c(deps, recursive)), c("R", default_library_packages()))
}

ensure_dependencies <- function(deps, lib_dir) {
  if (!length(deps)) {
    cat("[deps] No dependency metadata found.\n")
    return(invisible(TRUE))
  }

  all_deps <- resolve_dependency_closure(deps)
  missing <- packages_missing_from_local_library(all_deps, lib_dir)
  if (length(missing)) {
    install_dependency_packages(missing, lib_dir)
  } else {
    cat("[deps] All dependency packages are present in the local library.\n")
  }

  missing_after <- packages_missing_from_local_library(all_deps, lib_dir)
  if (length(missing_after)) {
    stop("Missing packages after install: ", paste(missing_after, collapse = ", "))
  }

  not_loadable <- all_deps[!vapply(all_deps, loadable_with_local_library, logical(1), lib_dir = lib_dir)]
  if (length(not_loadable)) {
    repair_unloadable_packages(not_loadable, lib_dir)
  }

  not_loadable <- deps[!vapply(deps, loadable_with_local_library, logical(1), lib_dir = lib_dir)]
  if (length(not_loadable)) {
    stop("Packages installed but not loadable from local library: ", paste(not_loadable, collapse = ", "))
  }
  invisible(TRUE)
}

# -------- browser launch helpers (multi-browser app mode) --------

# Windows: find installed Chromium browsers (try in this order)
find_chromium_windows <- function() {
  candidates <- c(
    # From PATH
    Sys.which("chrome.exe"),
    Sys.which("msedge.exe"),
    Sys.which("brave.exe"),
    Sys.which("vivaldi.exe"),
    Sys.which("opera.exe"),
    Sys.which("chromium.exe"),

    # Common install locations
    "C:/Program Files/Google/Chrome/Application/chrome.exe",
    "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe",
    file.path(Sys.getenv("LOCALAPPDATA"), "Google/Chrome/Application/chrome.exe"),

    "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe",
    "C:/Program Files/Microsoft/Edge/Application/msedge.exe",
    file.path(Sys.getenv("LOCALAPPDATA"), "Microsoft/Edge/Application/msedge.exe"),

    file.path(Sys.getenv("LOCALAPPDATA"), "BraveSoftware/Brave-Browser/Application/brave.exe"),
    "C:/Program Files/BraveSoftware/Brave-Browser/Application/brave.exe",
    "C:/Program Files (x86)/BraveSoftware/Brave-Browser/Application/brave.exe",

    file.path(Sys.getenv("LOCALAPPDATA"), "Vivaldi/Application/vivaldi.exe"),
    "C:/Program Files/Vivaldi/Application/vivaldi.exe",
    "C:/Program Files (x86)/Vivaldi/Application/vivaldi.exe",

    file.path(Sys.getenv("LOCALAPPDATA"), "Programs/Opera/opera.exe"),
    "C:/Program Files/Opera/opera.exe",
    "C:/Program Files (x86)/Opera/opera.exe"
  )

  candidates <- candidates[nzchar(candidates)]
  candidates <- unique(candidates)
  candidates <- candidates[file.exists(candidates)]
  if (!length(candidates)) return(character(0))
  normalizePath(candidates, winslash = "/", mustWork = TRUE)
}

# Windows: locate default browser executable (fallback)
extract_exe_from_cmd <- function(cmd) {
  if (is.null(cmd) || !nzchar(cmd)) return(NULL)
  cmd <- trimws(cmd)
  if (grepl("^\"[^\"]+\"", cmd)) {
    path <- sub("^\"([^\"]+)\".*$", "\\1", cmd)
  } else {
    path <- sub("^([^ ]+).*$", "\\1", cmd)
  }
  if (!file.exists(path)) return(NULL)
  path
}

get_default_browser_exe_windows <- function() {
  cmd <- NULL
  try({
    reg <- utils::readRegistry(
      "HKEY_CURRENT_USER\\Software\\Microsoft\\Windows\\Shell\\Associations\\UrlAssociations\\http\\UserChoice"
    )
    prog_id <- reg[["ProgId"]]
    if (!is.null(prog_id) && nzchar(prog_id)) {
      reg2 <- utils::readRegistry(paste0("HKEY_CLASSES_ROOT\\", prog_id, "\\shell\\open\\command"))
      cmd <- reg2[[""]]
    }
  }, silent = TRUE)

  if (is.null(cmd) || !nzchar(cmd)) {
    try({
      reg <- utils::readRegistry("HKEY_CLASSES_ROOT\\http\\shell\\open\\command")
      cmd <- reg[[""]]
    }, silent = TRUE)
  }

  extract_exe_from_cmd(cmd)
}

is_chromium_exe <- function(exe) {
  if (is.null(exe) || !nzchar(exe)) return(FALSE)
  exe_name <- tolower(basename(exe))
  exe_name %in% c("chrome.exe", "msedge.exe", "brave.exe", "vivaldi.exe", "opera.exe", "chromium.exe")
}

open_app_windows <- function(url) {
  # 1) Try any installed Chromium browser as app
  exes <- find_chromium_windows()
  if (length(exes)) {
    for (exe in exes) {
      ok <- tryCatch({
        system2(exe, args = c(paste0("--app=", url), "--new-window"), wait = FALSE)
        TRUE
      }, error = function(e) FALSE)
      if (ok) return(TRUE)
    }
  }

  # 2) Try default browser if it is Chromium-based
  def <- get_default_browser_exe_windows()
  if (!is.null(def) && is_chromium_exe(def)) {
    ok <- tryCatch({
      system2(def, args = c(paste0("--app=", url), "--new-window"), wait = FALSE)
      TRUE
    }, error = function(e) FALSE)
    if (ok) return(TRUE)
  }

  FALSE
}

# macOS: try common Chromium apps (if installed), else default browser (normal)
mac_app_exists <- function(app_name) {
  out <- tryCatch(system2("open", c("-Ra", app_name), stdout = TRUE, stderr = TRUE), error = function(e) NULL)
  if (is.null(out)) return(FALSE)
  status <- attr(out, "status")
  is.null(status) || status == 0
}

open_app_macos <- function(url) {
  apps <- c("Google Chrome", "Microsoft Edge", "Brave Browser", "Vivaldi", "Opera", "Chromium")
  for (app in apps) {
    if (mac_app_exists(app)) {
      ok <- tryCatch({
        system2("open", args = c("-a", app, "--args", paste0("--app=", url)), wait = FALSE)
        TRUE
      }, error = function(e) FALSE)
      if (ok) return(TRUE)
    }
  }
  FALSE
}

open_app_browser <- function(url) {
  sys <- Sys.info()[["sysname"]]
  opened <- FALSE
  if (identical(sys, "Windows")) {
    opened <- open_app_windows(url)
  } else if (identical(sys, "Darwin")) {
    opened <- open_app_macos(url)
  }

  if (!opened) {
    utils::browseURL(url)
  }
  invisible(opened)
}

options(shiny.launch.browser = open_app_browser)

# -------- helper: install extracted source with R CMD INSTALL --------
run_r_cmd_install <- function(package_path, lib_dir) {
  r_bin <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R")
  if (!file.exists(r_bin)) r_bin <- file.path(R.home("bin"), "R")
  quote_arg <- function(x) shQuote(x, type = if (.Platform$OS.type == "windows") "cmd" else "sh")

  status <- tryCatch(
    system2(r_bin, c("CMD", "INSTALL", paste0("--library=", quote_arg(lib_dir)), quote_arg(package_path))),
    error = function(e) {
      cat("[install] R CMD INSTALL failed to start: ", conditionMessage(e), "\n", sep = "")
      1L
    }
  )
  identical(status, 0L)
}

# -------- helper: install from zip by unzip --------
install_from_zip_by_unzip <- function(zip_path, lib_dir, pkg_name = "BIOSZEN") {
  zip_path <- normalizePath(zip_path, winslash = "/", mustWork = TRUE)
  lib_dir <- normalizePath(lib_dir, winslash = "/", mustWork = TRUE)

  cat("\n[install] Installing win.binary zip via install.packages(): ", zip_path, "\n", sep = "")
  ok <- tryCatch({
    utils::install.packages(zip_path, repos = NULL, type = "win.binary", lib = lib_dir, quiet = FALSE)
    TRUE
  }, error = function(e) {
    cat("[install] install.packages() failed: ", conditionMessage(e), "\n", sep = "")
    FALSE
  })
  if (!ok) stop("Failed to install BIOSZEN from zip: ", zip_path)

  invisible(TRUE)
}

install_from_tarball <- function(tarball_path, lib_dir, pkg_name = "BIOSZEN") {
  tarball_path <- normalizePath(tarball_path, winslash = "/", mustWork = TRUE)
  lib_dir <- normalizePath(lib_dir, winslash = "/", mustWork = TRUE)

  cat("\n[install] Installing source tarball via install.packages(): ", tarball_path, "\n", sep = "")

  ok <- tryCatch({
    utils::install.packages(tarball_path, repos = NULL, type = "source", lib = lib_dir, quiet = FALSE)
    TRUE
  }, error = function(e) {
    cat("[install] install.packages() failed: ", conditionMessage(e), "\n", sep = "")
    FALSE
  })
  if (!ok) stop("Failed to install BIOSZEN from tar.gz: ", tarball_path)
  invisible(TRUE)
}

install_from_uncompressed_tar <- function(tar_path, lib_dir, pkg_name = "BIOSZEN") {
  tar_path <- normalizePath(tar_path, winslash = "/", mustWork = TRUE)
  lib_dir <- normalizePath(lib_dir, winslash = "/", mustWork = TRUE)

  cat("\n[install] Installing uncompressed source tar via R CMD INSTALL: ", tar_path, "\n", sep = "")
  ok <- run_r_cmd_install(tar_path, lib_dir)
  if (!ok) stop("Failed to install BIOSZEN from uncompressed tar: ", tar_path)
  invisible(TRUE)
}

install_from_source_dir <- function(source_dir, lib_dir, pkg_name = "BIOSZEN") {
  source_dir <- normalizePath(source_dir, winslash = "/", mustWork = TRUE)
  lib_dir <- normalizePath(lib_dir, winslash = "/", mustWork = TRUE)

  if (!is_package_source_dir(source_dir, pkg_name)) {
    stop("Not a BIOSZEN source directory: ", source_dir)
  }

  cat("\n[install] Installing source directory via R CMD INSTALL: ", source_dir, "\n", sep = "")
  ok <- run_r_cmd_install(source_dir, lib_dir)
  if (!ok) stop("Failed to install BIOSZEN from source directory: ", source_dir)
  invisible(TRUE)
}

remove_incomplete_install <- function(lib_dir, pkg_name = "BIOSZEN") {
  pkg_dir <- file.path(lib_dir, pkg_name)
  meta_rds <- file.path(pkg_dir, "Meta", "package.rds")
  if (dir.exists(pkg_dir) && !file.exists(meta_rds)) {
    cat("[install] Removing incomplete installation at ", pkg_dir, "\n", sep = "")
    unlink(pkg_dir, recursive = TRUE, force = TRUE)
  }
}

unload_package_namespace <- function(pkg_name) {
  attached_name <- paste0("package:", pkg_name)
  if (attached_name %in% search()) {
    tryCatch(detach(attached_name, unload = TRUE, character.only = TRUE), error = function(e) {
      cat("[install] Could not detach loaded package ", pkg_name, ": ", conditionMessage(e), "\n", sep = "")
    })
  }

  if (pkg_name %in% loadedNamespaces()) {
    tryCatch(unloadNamespace(pkg_name), error = function(e) {
      cat("[install] Could not unload namespace ", pkg_name, ": ", conditionMessage(e), "\n", sep = "")
    })
  }
  invisible(TRUE)
}

# -------- ensure BIOSZEN --------
remove_incomplete_install(local_lib, pkg)

local_version <- get_local_version(pkg, local_lib)
if (!is.null(local_version)) {
  cat("\nLocal BIOSZEN version: ", as.character(local_version), "\n", sep = "")
} else {
  cat("\nLocal BIOSZEN not installed.\n")
}

embedded <- read_embedded_payload(script_path, with_raw = FALSE)
embedded_path <- NULL
if (!is.null(embedded)) {
  embedded_path <- file.path(script_dir, embedded$name)
}

archive_dir <- script_dir
archives <- c(
  embedded_path,
  list.files(archive_dir, pattern = "^BIOSZEN[-_].*\\.(tar\\.gz|tgz|tar)$", full.names = TRUE, ignore.case = TRUE),
  list.files(archive_dir, pattern = "^BIOSZEN[-_].*\\.zip$", full.names = TRUE, ignore.case = TRUE)
)
archives <- unique(archives[!is.na(archives) & nzchar(archives)])
source_dirs <- find_package_source_dirs(archive_dir, pkg)

archive_info <- list()
if (length(archives)) {
  archive_info <- lapply(archives, function(path) {
    list(
      path = path,
      kind = "archive",
      version = get_archive_version(path, pkg),
      mtime = tryCatch(file.info(path)$mtime, error = function(e) as.POSIXct(NA))
    )
  })
}
source_info <- list()
if (length(source_dirs)) {
  source_info <- lapply(source_dirs, source_dir_info, pkg_name = pkg)
}

archive_choice <- pick_best_archive(archive_info)
source_choice <- pick_best_archive(source_info)
package_choice <- pick_best_package_candidate(archive_choice, source_choice)
package_path <- if (!is.null(package_choice)) package_choice$path else NULL
package_kind <- if (!is.null(package_choice) && !is.null(package_choice$kind)) package_choice$kind else ""
package_version <- if (!is.null(package_choice)) package_choice$version else NULL
archive_path <- if (identical(package_kind, "archive")) package_path else NULL
source_dir <- if (identical(package_kind, "source_dir")) package_path else NULL

if (is.null(package_path) && is.null(local_version)) {
  stop(
    "No BIOSZEN-*.tar.gz, BIOSZEN-*.tgz, BIOSZEN-*.tar, BIOSZEN-*.zip, ",
    "or extracted BIOSZEN source folder found in: ",
    archive_dir
  )
}

if (!is.null(package_path)) {
  candidate_label <- if (identical(package_kind, "source_dir")) "Source directory candidate" else "Archive candidate"
  cat("\n", candidate_label, ": ", package_path, "\n", sep = "")
  if (!is.null(package_version)) {
    cat("Candidate version: ", as.character(package_version), "\n", sep = "")
  } else {
    cat("Candidate version: unknown\n")
  }
}

install_needed <- FALSE
if (is.null(local_version)) {
  install_needed <- !is.null(package_path)
} else if (!is.null(package_version)) {
  install_needed <- package_version > local_version
  if (
    !install_needed &&
    package_version == local_version &&
    identical(package_kind, "archive") &&
    !is.null(archive_path) &&
    file.exists(archive_path)
  ) {
    install_needed <- !archive_matches_local_install(archive_path, local_lib, pkg)
  }
}

if (install_needed && identical(package_kind, "archive") && !is.null(archive_path) && !file.exists(archive_path)) {
  embedded_full <- read_embedded_payload(script_path, with_raw = TRUE)
  if (is.null(embedded_full) || is.null(embedded_path) || !identical(archive_path, embedded_path)) {
    stop("Archive not found: ", archive_path)
  }
  if (!file.exists(embedded_path) || file.info(embedded_path)$size == 0) {
    cat("[install] Writing embedded archive: ", embedded_path, "\n", sep = "")
    writeBin(embedded_full$raw, embedded_path)
  }
}

deps <- character(0)
deps_source <- ""
if (install_needed && identical(package_kind, "archive") && !is.null(archive_path) && file.exists(archive_path)) {
  deps <- get_archive_dependencies(archive_path)
  deps_source <- "archive"
} else if (install_needed && identical(package_kind, "source_dir") && !is.null(source_dir) && dir.exists(source_dir)) {
  deps <- get_source_dependencies(source_dir)
  deps_source <- "source directory"
} else if (!is.null(local_version)) {
  deps <- get_pkg_dependencies(pkg, local_lib)
  deps_source <- "installed"
}
if (length(deps) && nzchar(deps_source)) {
  cat("[deps] Using ", deps_source, " metadata.\n", sep = "")
}
ensure_dependencies(deps, local_lib)

if (install_needed) {
  unload_package_namespace(pkg)
  install_source_label <- if (identical(package_kind, "source_dir")) "source directory" else "archive"

  if (!is.null(local_version) && !is.null(package_version)) {
    if (package_version > local_version) {
      cat("[install] Updating BIOSZEN: ", as.character(local_version), " -> ", as.character(package_version), "\n", sep = "")
    } else {
      cat("[install] Installing BIOSZEN from ", install_source_label, ".\n", sep = "")
    }
  } else {
    cat("[install] Installing BIOSZEN from ", install_source_label, ".\n", sep = "")
  }

  if (identical(package_kind, "source_dir")) {
    install_from_source_dir(source_dir, local_lib, pkg)
  } else if (grepl("\\.zip$", archive_path, ignore.case = TRUE)) {
    install_from_zip_by_unzip(archive_path, local_lib, pkg)
  } else if (grepl("\\.tar$", archive_path, ignore.case = TRUE)) {
    install_from_uncompressed_tar(archive_path, local_lib, pkg)
  } else {
    install_from_tarball(archive_path, local_lib, pkg)
  }
  remove_incomplete_install(local_lib, pkg)
  if (identical(package_kind, "archive")) {
    write_archive_marker(archive_path, local_lib, pkg)
  }
} else if (!is.null(local_version)) {
  cat("[install] Local BIOSZEN is up to date; skipping install.\n")
}

if (!loadable_with_local_library(pkg, local_lib)) {
  stop("BIOSZEN is still not loadable. Check bioszen_r.log.")
}

cat("\nBIOSZEN version: ", as.character(packageVersion(pkg, lib.loc = local_lib)), "\n", sep = "")
options(BIOSZEN.startup_citation_shown = FALSE)

# -------- run app --------
run_fun <- tryCatch(getExportedValue(pkg, "run_app"), error = function(e) NULL)
if (is.null(run_fun)) stop("Package does not export run_app().")

fm <- names(formals(run_fun))
args <- list()
if ("host" %in% fm) args$host <- getOption("shiny.host", "127.0.0.1")
if ("port" %in% fm) args$port <- getOption("shiny.port", 4321)
if ("launch.browser" %in% fm) args$launch.browser <- open_app_browser

cat("\nLaunching BIOSZEN::run_app() ...\n")
do.call(run_fun, args)
