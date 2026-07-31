#' Launch BIOSZEN
#'
#' Starts the interactive BIOSZEN Shiny application. `BIOSZEN()` is the concise
#' public launcher; `run_app()` remains available for backward compatibility.
#'
#' @param host Host interface for the local Shiny server.
#' @param port Port for the local Shiny server.
#' @param launch.browser Browser launcher passed to [shiny::runApp()]. The
#'   default, `TRUE`, opens the app automatically. Supply `FALSE` to disable
#'   automatic opening or a function to use a custom launcher.
#' @param app_window Logical. When `FALSE` (the default), `BIOSZEN()` opens the
#'   operating system's configured default browser. Set it to `TRUE` to prefer
#'   a dedicated Chromium app window and fall back to the default browser. This
#'   argument is used by `BIOSZEN()`; `run_app()` retains its original
#'   browser-launch interface.
#'
#' @return The value returned by [shiny::runApp()], invisibly when appropriate.
#' @export
BIOSZEN <- function(host = getOption("shiny.host", "127.0.0.1"),
                    port = getOption("shiny.port", 4321),
                    launch.browser = TRUE,
                    app_window = FALSE) {
  browser_launcher <- .bioszen_browser_launcher(launch.browser, app_window)
  run_app(host = host, port = port, launch.browser = browser_launcher)
}

#' @rdname BIOSZEN
#' @export
run_app <- function(host = getOption("shiny.host", "127.0.0.1"),
                    port = getOption("shiny.port", 4321),
                    launch.browser = getOption("shiny.launch.browser", TRUE)) {
  local_lib <- getOption("BIOSZEN.local_lib", Sys.getenv("BIOSZEN_LOCAL_LIB", ""))
  if (is.character(local_lib) && length(local_lib) && nzchar(local_lib[[1]]) && dir.exists(local_lib[[1]])) {
    local_lib <- normalizePath(local_lib[[1]], winslash = "/", mustWork = TRUE)
    .libPaths(unique(c(local_lib, .libPaths())))
  }

  app_dir <- .bioszen_installed_app_dir()
  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    stop("The installed BIOSZEN Shiny application could not be found.", call. = FALSE)
  }

  previous_running <- getOption("BIOSZEN.app_running", NULL)
  previous_update_request <- getOption("BIOSZEN.update_after_app", NULL)
  options(
    BIOSZEN.app_running = TRUE,
    BIOSZEN.update_after_app = FALSE
  )
  on.exit({
    options(
      BIOSZEN.app_running = previous_running,
      BIOSZEN.update_after_app = previous_update_request
    )
  }, add = TRUE)

  app_result <- .bioszen_run_shiny_app(
    app_dir,
    host = host,
    port = port,
    launch.browser = launch.browser,
    display.mode = "normal"
  )

  update_requested <- isTRUE(getOption("BIOSZEN.update_after_app", FALSE))
  options(
    BIOSZEN.app_running = previous_running,
    BIOSZEN.update_after_app = FALSE
  )
  if (update_requested) {
    tryCatch(
      bioszen_update(ask = FALSE),
      error = function(e) {
        warning(
          "BIOSZEN closed, but the update could not be installed: ",
          conditionMessage(e),
          ". Run BIOSZEN::bioszen_update() in a fresh R session.",
          call. = FALSE
        )
        FALSE
      }
    )
  }
  app_result
}

.bioszen_installed_app_dir <- function() {
  system.file("app", package = "BIOSZEN")
}

.bioszen_run_shiny_app <- function(app_dir, ...) {
  shiny::runApp(app_dir, ...)
}
