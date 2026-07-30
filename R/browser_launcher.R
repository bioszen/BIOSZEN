.bioszen_existing_browser_paths <- function(candidates) {
  candidates <- unique(candidates[nzchar(candidates)])
  candidates <- candidates[file.exists(candidates)]
  if (!length(candidates)) return(character())
  normalizePath(candidates, winslash = "/", mustWork = TRUE)
}

.bioszen_find_chromium_windows <- function() {
  .bioszen_existing_browser_paths(c(
    Sys.which("chrome.exe"),
    Sys.which("msedge.exe"),
    Sys.which("brave.exe"),
    Sys.which("vivaldi.exe"),
    Sys.which("opera.exe"),
    Sys.which("chromium.exe"),
    "C:/Program Files/Google/Chrome/Application/chrome.exe",
    "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe",
    file.path(Sys.getenv("LOCALAPPDATA"), "Google/Chrome/Application/chrome.exe"),
    "C:/Program Files/Microsoft/Edge/Application/msedge.exe",
    "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe",
    file.path(Sys.getenv("LOCALAPPDATA"), "Microsoft/Edge/Application/msedge.exe"),
    "C:/Program Files/BraveSoftware/Brave-Browser/Application/brave.exe",
    "C:/Program Files (x86)/BraveSoftware/Brave-Browser/Application/brave.exe",
    file.path(Sys.getenv("LOCALAPPDATA"), "BraveSoftware/Brave-Browser/Application/brave.exe"),
    "C:/Program Files/Vivaldi/Application/vivaldi.exe",
    "C:/Program Files (x86)/Vivaldi/Application/vivaldi.exe",
    file.path(Sys.getenv("LOCALAPPDATA"), "Vivaldi/Application/vivaldi.exe"),
    "C:/Program Files/Opera/opera.exe",
    "C:/Program Files (x86)/Opera/opera.exe",
    file.path(Sys.getenv("LOCALAPPDATA"), "Programs/Opera/opera.exe")
  ))
}

.bioszen_extract_browser_executable <- function(command) {
  if (is.null(command) || !length(command) || !nzchar(command[[1]])) return("")
  command <- trimws(command[[1]])
  path <- if (grepl("^\"[^\"]+\"", command)) {
    sub("^\"([^\"]+)\".*$", "\\1", command)
  } else {
    sub("^([^ ]+).*$", "\\1", command)
  }
  if (!file.exists(path)) return("")
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

.bioszen_default_browser_windows <- function() {
  command <- tryCatch({
    choice <- utils::readRegistry(
      "HKEY_CURRENT_USER\\Software\\Microsoft\\Windows\\Shell\\Associations\\UrlAssociations\\http\\UserChoice"
    )
    prog_id <- choice[["ProgId"]]
    if (is.null(prog_id) || !nzchar(prog_id)) stop("No browser ProgId")
    handler <- utils::readRegistry(
      paste0("HKEY_CLASSES_ROOT\\", prog_id, "\\shell\\open\\command")
    )
    handler[[""]]
  }, error = function(e) "")

  if (!nzchar(command)) {
    command <- tryCatch({
      handler <- utils::readRegistry("HKEY_CLASSES_ROOT\\http\\shell\\open\\command")
      handler[[""]]
    }, error = function(e) "")
  }
  .bioszen_extract_browser_executable(command)
}

.bioszen_is_chromium_executable <- function(path) {
  if (!length(path) || !nzchar(path[[1]])) return(FALSE)
  tolower(basename(path[[1]])) %in% c(
    "chrome.exe", "msedge.exe", "brave.exe", "vivaldi.exe",
    "opera.exe", "chromium.exe"
  )
}

.bioszen_launch_executable_app <- function(executable, url) {
  tryCatch({
    system2(
      executable,
      args = c(paste0("--app=", url), "--new-window"),
      wait = FALSE
    )
    TRUE
  }, error = function(e) FALSE)
}

.bioszen_open_app_windows <- function(url) {
  candidates <- .bioszen_find_chromium_windows()
  default_browser <- .bioszen_default_browser_windows()
  if (.bioszen_is_chromium_executable(default_browser)) {
    candidates <- unique(c(default_browser, candidates))
  }
  if (!length(candidates)) return(FALSE)
  for (candidate in candidates) {
    if (.bioszen_launch_executable_app(candidate, url)) return(TRUE)
  }
  FALSE
}

.bioszen_system_name <- function() Sys.info()[["sysname"]]

.bioszen_open_default_windows <- function(url) {
  shell.exec(url)
  TRUE
}

.bioszen_open_default_macos <- function(url) {
  status <- system2("open", shQuote(url), wait = FALSE)
  is.null(status) || identical(status, 0L)
}

.bioszen_open_default_unix <- function(url) {
  opener <- unname(Sys.which("xdg-open"))
  if (!nzchar(opener)) return(FALSE)
  status <- system2(opener, shQuote(url), wait = FALSE)
  is.null(status) || identical(status, 0L)
}

.bioszen_open_default_fallback <- function(url) {
  utils::browseURL(url)
  TRUE
}

.bioszen_open_default_browser <- function(url) {
  system_name <- .bioszen_system_name()
  opened <- tryCatch(
    if (identical(system_name, "Windows")) {
      .bioszen_open_default_windows(url)
    } else if (identical(system_name, "Darwin")) {
      .bioszen_open_default_macos(url)
    } else {
      .bioszen_open_default_unix(url)
    },
    error = function(e) FALSE
  )

  if (!isTRUE(opened)) {
    opened <- tryCatch(
      .bioszen_open_default_fallback(url),
      error = function(e) FALSE
    )
  }
  invisible(isTRUE(opened))
}

.bioszen_macos_app_exists <- function(app_name) {
  output <- tryCatch(
    system2("open", c("-Ra", shQuote(app_name)), stdout = TRUE, stderr = TRUE),
    error = function(e) NULL
  )
  if (is.null(output)) return(FALSE)
  status <- attr(output, "status")
  is.null(status) || identical(status, 0L)
}

.bioszen_open_app_macos <- function(url) {
  apps <- c(
    "Google Chrome", "Microsoft Edge", "Brave Browser",
    "Vivaldi", "Opera", "Chromium"
  )
  for (app in apps) {
    if (!.bioszen_macos_app_exists(app)) next
    opened <- tryCatch({
      system2(
        "open",
        c("-na", shQuote(app), "--args", paste0("--app=", url)),
        wait = FALSE
      )
      TRUE
    }, error = function(e) FALSE)
    if (opened) return(TRUE)
  }
  FALSE
}

.bioszen_find_chromium_unix <- function() {
  candidates <- unname(Sys.which(c(
    "google-chrome", "google-chrome-stable", "chromium",
    "chromium-browser", "microsoft-edge", "microsoft-edge-stable",
    "brave-browser", "vivaldi", "opera"
  )))
  candidates <- unique(candidates[nzchar(candidates)])
  if (!length(candidates)) return(character())
  normalizePath(candidates, winslash = "/", mustWork = TRUE)
}

.bioszen_open_app_unix <- function(url) {
  candidates <- .bioszen_find_chromium_unix()
  if (!length(candidates)) return(FALSE)
  for (candidate in candidates) {
    if (.bioszen_launch_executable_app(candidate, url)) return(TRUE)
  }
  FALSE
}

.bioszen_open_app_browser <- function(url) {
  system_name <- Sys.info()[["sysname"]]
  opened <- if (identical(system_name, "Windows")) {
    .bioszen_open_app_windows(url)
  } else if (identical(system_name, "Darwin")) {
    .bioszen_open_app_macos(url)
  } else {
    .bioszen_open_app_unix(url)
  }

  if (!isTRUE(opened)) utils::browseURL(url)
  invisible(isTRUE(opened))
}

.bioszen_browser_launcher <- function(launch.browser, app_window) {
  if (!isTRUE(launch.browser)) return(launch.browser)
  if (isTRUE(app_window)) return(.bioszen_open_app_browser)
  .bioszen_open_default_browser
}
