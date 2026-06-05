startup_file <- file.path("R", "app_startup.R")
if (file.exists(startup_file)) {
  sys.source(startup_file, envir = globalenv())
  bioszen_prepare_direct_run()
}

app_dir <- normalizePath(file.path("inst", "app"), winslash = "/", mustWork = TRUE)
shiny::shinyAppDir(app_dir)
