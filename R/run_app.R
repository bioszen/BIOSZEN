#'
#' This function launches the interactive app that ships with the package.
#'
#' @export
run_app <- function() {
  app_dir <- system.file("app", package = "BIOSZEN")
  shiny::runApp(app_dir, display.mode = "normal")
}
