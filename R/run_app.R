.bioszen_startup_citation <- function() {
  if (!isTRUE(getOption("BIOSZEN.show_startup_citation", TRUE))) {
    return(invisible(FALSE))
  }
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

#'
#' This function launches the interactive app that ships with the package.
#'
#' @param host Host interface for the local Shiny server.
#' @param port Port for the local Shiny server.
#' @param launch.browser Browser launcher passed to `shiny::runApp()`.
#'
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
  .bioszen_startup_citation()
  shiny::runApp(
    app_dir,
    host = host,
    port = port,
    launch.browser = launch.browser,
    display.mode = "normal"
  )
}
