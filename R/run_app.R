#' Launch BIOSZEN
#'
#' Starts the interactive BIOSZEN Shiny application. `BIOSZEN()` is the concise
#' public launcher; `run_app()` remains available for backward compatibility.
#'
#' @param host Host interface for the local Shiny server.
#' @param port Port for the local Shiny server.
#' @param launch.browser Browser launcher passed to [shiny::runApp()].
#'
#' @return The value returned by [shiny::runApp()], invisibly when appropriate.
#' @export
BIOSZEN <- function(host = getOption("shiny.host", "127.0.0.1"),
                    port = getOption("shiny.port", 4321),
                    launch.browser = getOption("shiny.launch.browser", TRUE)) {
  run_app(host = host, port = port, launch.browser = launch.browser)
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

  app_dir <- system.file("app", package = "BIOSZEN")
  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    stop("The installed BIOSZEN Shiny application could not be found.", call. = FALSE)
  }

  previous_running <- getOption("BIOSZEN.app_running", NULL)
  options(BIOSZEN.app_running = TRUE)
  on.exit(options(BIOSZEN.app_running = previous_running), add = TRUE)

  shiny::runApp(
    app_dir,
    host = host,
    port = port,
    launch.browser = launch.browser,
    display.mode = "normal"
  )
}
