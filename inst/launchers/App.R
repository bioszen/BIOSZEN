# app.R (backend) - BIOSZEN launcher
# Goals:
# 1) Fix host/port for the launcher
# 2) Open the app in a browser app window when possible (prefer Chrome; else default browser)
# 3) Use a local ./R_libs
# 4) Install BIOSZEN from embedded or nearby archives if needed

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
try({
  sink(log_file, append = TRUE, split = TRUE)
  cat("\n=== BIOSZEN App.R start ===\n")
  cat("Time: ", as.character(Sys.time()), "\n", sep = "")
  cat("R: ", R.version.string, "\n", sep = "")
  cat("script_dir: ", script_dir, "\n", sep = "")
  cat("root_dir  : ", root_dir, "\n", sep = "")
}, silent = TRUE)

# -------- local library (self-contained) --------
local_lib <- file.path(root_dir, "R_libs")
dir.create(local_lib, showWarnings = FALSE, recursive = TRUE)
.libPaths(c(local_lib, .libPaths()))
cat(".libPaths():\n")
print(.libPaths())

pkg <- "BIOSZEN"

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
  if (!is.character(x) || !nzchar(x)) return(NULL)
  tryCatch(package_version(x), error = function(e) NULL)
}

extract_version_from_filename <- function(path, pkg_name = "BIOSZEN") {
  base <- basename(path)
  pattern <- paste0("^", pkg_name, "_([0-9A-Za-z\\.\\-]+)\\.(tar\\.gz|zip)$")
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
    pkg <- dcf[1, "Package"]
    if (nzchar(pkg) && !identical(tolower(pkg), tolower(pkg_name))) return(NULL)
  }

  parse_pkg_version(dcf[1, "Version"])
}

get_archive_version <- function(archive_path, pkg_name = "BIOSZEN") {
  ver <- extract_version_from_filename(archive_path, pkg_name)
  if (!is.null(ver)) return(ver)
  read_archive_description_version(archive_path, pkg_name)
}

get_local_version <- function(pkg_name, lib_dir) {
  tryCatch(packageVersion(pkg_name, lib.loc = lib_dir), error = function(e) NULL)
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

ensure_cran_repo <- function() {
  repos <- getOption("repos")
  cran <- if (is.null(repos)) "" else repos[["CRAN"]]
  if (is.null(cran) || is.na(cran) || !nzchar(cran) || cran == "@CRAN@") {
    options(repos = c(CRAN = "https://cran.rstudio.com"))
  }
}

ensure_dependencies <- function(deps, lib_dir) {
  if (!length(deps)) {
    cat("[deps] No dependency metadata found.\n")
    return(invisible(TRUE))
  }

  missing <- deps[!vapply(deps, requireNamespace, logical(1), quietly = TRUE)]
  if (!length(missing)) {
    cat("[deps] All dependencies are already installed.\n")
    return(invisible(TRUE))
  }

  ensure_cran_repo()
  cat("[deps] Installing missing packages: ", paste(missing, collapse = ", "), "\n", sep = "")
  ok <- tryCatch({
    utils::install.packages(
      missing,
      lib = lib_dir,
      dependencies = c("Depends", "Imports", "LinkingTo")
    )
    TRUE
  }, error = function(e) {
    cat("[deps] install.packages failed: ", conditionMessage(e), "\n", sep = "")
    FALSE
  })
  if (!ok) stop("Failed installing dependencies. Check bioszen_r.log.")

  missing_after <- missing[!vapply(missing, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_after)) {
    stop("Missing packages after install: ", paste(missing_after, collapse = ", "))
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

remove_incomplete_install <- function(lib_dir, pkg_name = "BIOSZEN") {
  pkg_dir <- file.path(lib_dir, pkg_name)
  meta_rds <- file.path(pkg_dir, "Meta", "package.rds")
  if (dir.exists(pkg_dir) && !file.exists(meta_rds)) {
    cat("[install] Removing incomplete installation at ", pkg_dir, "\n", sep = "")
    unlink(pkg_dir, recursive = TRUE, force = TRUE)
  }
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
  list.files(archive_dir, pattern = "^BIOSZEN_.*\\.tar\\.gz$", full.names = TRUE, ignore.case = TRUE),
  list.files(archive_dir, pattern = "^BIOSZEN_.*\\.zip$", full.names = TRUE, ignore.case = TRUE)
)
archives <- unique(archives[!is.na(archives) & nzchar(archives)])

archive_info <- list()
if (length(archives)) {
  archive_info <- lapply(archives, function(path) {
    list(
      path = path,
      version = get_archive_version(path, pkg),
      mtime = tryCatch(file.info(path)$mtime, error = function(e) as.POSIXct(NA))
    )
  })
}

archive_choice <- pick_best_archive(archive_info)
archive_path <- if (!is.null(archive_choice)) archive_choice$path else NULL
archive_version <- if (!is.null(archive_choice)) archive_choice$version else NULL

if (is.null(archive_path) && is.null(local_version)) {
  stop("No BIOSZEN_*.tar.gz or BIOSZEN_*.zip found in: ", archive_dir)
}

if (!is.null(archive_path)) {
  cat("\nArchive candidate: ", archive_path, "\n", sep = "")
  if (!is.null(archive_version)) {
    cat("Archive version : ", as.character(archive_version), "\n", sep = "")
  } else {
    cat("Archive version : unknown\n")
  }
}

install_needed <- FALSE
if (is.null(local_version)) {
  install_needed <- !is.null(archive_path)
} else if (!is.null(archive_version)) {
  install_needed <- archive_version > local_version
}

if (install_needed && !is.null(archive_path) && !file.exists(archive_path)) {
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
if (install_needed && !is.null(archive_path) && file.exists(archive_path)) {
  deps <- get_archive_dependencies(archive_path)
  deps_source <- "archive"
} else if (!is.null(local_version)) {
  deps <- get_pkg_dependencies(pkg, local_lib)
  deps_source <- "installed"
}
if (length(deps) && nzchar(deps_source)) {
  cat("[deps] Using ", deps_source, " metadata.\n", sep = "")
}
ensure_dependencies(deps, local_lib)

if (install_needed) {
  if (!is.null(local_version) && !is.null(archive_version)) {
    cat("[install] Updating BIOSZEN: ", as.character(local_version), " -> ", as.character(archive_version), "\n", sep = "")
  } else {
    cat("[install] Installing BIOSZEN from archive.\n")
  }

  if (grepl("\\.zip$", archive_path, ignore.case = TRUE)) {
    install_from_zip_by_unzip(archive_path, local_lib, pkg)
  } else {
    install_from_tarball(archive_path, local_lib, pkg)
  }
  remove_incomplete_install(local_lib, pkg)
} else if (!is.null(local_version)) {
  cat("[install] Local BIOSZEN is up to date; skipping install.\n")
}

if (!requireNamespace(pkg, quietly = TRUE, lib.loc = local_lib)) {
  stop("BIOSZEN is still not loadable. Check bioszen_r.log.")
}

cat("\nBIOSZEN version: ", as.character(packageVersion(pkg, lib.loc = local_lib)), "\n", sep = "")

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
