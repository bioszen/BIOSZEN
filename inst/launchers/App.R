# app.R (backend) - BIOSZEN launcher
# Goals:
# 1) Fix host/port for the launcher
# 2) Open the app in a browser app window when possible
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

read_embedded_payload <- function(path) {
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
  payload_lines <- lines[(marker_idx[1] + 1L):length(lines)]
  if (!length(payload_lines)) return(NULL)

  payload_lines <- sub("^#\\s?", "", payload_lines)
  payload_b64 <- paste(payload_lines, collapse = "")
  if (!nzchar(payload_b64)) return(NULL)

  list(name = payload_name, raw = base64_decode(payload_b64))
}

# -------- browser launch helpers --------
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

open_app_windows <- function(url) {
  cmd <- NULL
  try({
    reg <- utils::readRegistry(
      "HKEY_CURRENT_USER\\Software\\Microsoft\\Windows\\Shell\\Associations\\UrlAssociations\\http\\UserChoice"
    )
    prog_id <- reg[["ProgId"]]
    if (!is.null(prog_id) && nzchar(prog_id)) {
      reg2 <- utils::readRegistry(
        paste0("HKEY_CLASSES_ROOT\\", prog_id, "\\shell\\open\\command")
      )
      cmd <- reg2[[""]]
    }
  }, silent = TRUE)

  if (is.null(cmd) || !nzchar(cmd)) {
    try({
      reg <- utils::readRegistry("HKEY_CLASSES_ROOT\\http\\shell\\open\\command")
      cmd <- reg[[""]]
    }, silent = TRUE)
  }

  exe <- extract_exe_from_cmd(cmd)
  if (is.null(exe)) return(FALSE)

  exe_name <- tolower(basename(exe))
  chromium <- c("chrome.exe", "msedge.exe", "brave.exe", "vivaldi.exe", "opera.exe", "chromium.exe")
  if (!exe_name %in% chromium) return(FALSE)

  args <- c(paste0("--app=", url), "--new-window")
  ok <- tryCatch({
    system2(exe, args = args, wait = FALSE)
    TRUE
  }, error = function(e) FALSE)
  ok
}

get_default_browser_bundle_id <- function() {
  out <- tryCatch(
    system2(
      "osascript",
      c("-e", "id of application (path to default application for \"http:\")"),
      stdout = TRUE,
      stderr = TRUE
    ),
    error = function(e) NULL
  )
  if (is.null(out)) return(NULL)
  status <- attr(out, "status")
  if (!is.null(status) && status != 0) return(NULL)
  id <- trimws(out[1])
  if (!nzchar(id)) return(NULL)
  id
}

open_app_macos <- function(url) {
  bundle_id <- get_default_browser_bundle_id()
  if (is.null(bundle_id)) return(FALSE)

  chromium_ids <- c(
    "com.google.Chrome",
    "com.microsoft.edgemac",
    "com.brave.Browser",
    "com.brave.browser",
    "org.chromium.Chromium",
    "com.vivaldi.Vivaldi",
    "com.operasoftware.Opera"
  )
  if (!bundle_id %in% chromium_ids) return(FALSE)

  args <- c("-b", bundle_id, "--args", paste0("--app=", url))
  ok <- tryCatch({
    system2("open", args = args, wait = FALSE)
    TRUE
  }, error = function(e) FALSE)
  ok
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

if (!requireNamespace(pkg, quietly = TRUE)) {
  cat("\nBIOSZEN not found in .libPaths(). Looking for archives...\n")

embedded <- read_embedded_payload(script_path)
  embedded_path <- NULL
  if (!is.null(embedded)) {
    embedded_path <- file.path(script_dir, embedded$name)
    if (!file.exists(embedded_path) || file.info(embedded_path)$size == 0) {
      cat("[install] Writing embedded archive: ", embedded_path, "\n", sep = "")
      writeBin(embedded$raw, embedded_path)
    }
  }

  archive_dir <- script_dir
  archives <- c(
    embedded_path,
    list.files(archive_dir, pattern = "^BIOSZEN_.*\\.tar\\.gz$", full.names = TRUE),
    list.files(archive_dir, pattern = "^BIOSZEN_.*\\.zip$", full.names = TRUE)
  )
  archives <- unique(archives[!is.na(archives) & nzchar(archives)])

  if (length(archives) == 0) {
    stop("No BIOSZEN_*.tar.gz or BIOSZEN_*.zip found in: ", archive_dir)
  }

  archives <- archives[order(file.info(archives)$mtime, decreasing = TRUE)]
  archive_path <- archives[[1]]
  cat("Using archive: ", archive_path, "\n", sep = "")

  if (grepl("\\.zip$", archive_path, ignore.case = TRUE)) {
    install_from_zip_by_unzip(archive_path, local_lib, pkg)
  } else {
    install_from_tarball(archive_path, local_lib, pkg)
  }
  remove_incomplete_install(local_lib, pkg)
}

if (!requireNamespace(pkg, quietly = TRUE)) {
  stop("BIOSZEN is still not loadable. Check bioszen_r.log.")
}

cat("\nBIOSZEN version: ", as.character(packageVersion(pkg)), "\n", sep = "")

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
