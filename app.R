find_bioszen_app_file <- function(default_name = "app.R") {
  frames <- sys.frames()
  frame_paths <- vapply(frames, function(frame) {
    path <- frame$ofile
    if (is.null(path) || !length(path) || is.na(path[[1]]) || !nzchar(path[[1]])) {
      ""
    } else {
      path[[1]]
    }
  }, character(1))
  frame_paths <- frame_paths[nzchar(frame_paths) & file.exists(frame_paths)]
  if (length(frame_paths)) {
    return(normalizePath(frame_paths[[length(frame_paths)]], winslash = "/", mustWork = TRUE))
  }

  cmd_args <- commandArgs(FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)
  if (length(file_arg)) {
    path <- sub("^--file=", "", file_arg[[1]])
    if (file.exists(path)) {
      return(normalizePath(path, winslash = "/", mustWork = TRUE))
    }
  }

  normalizePath(file.path(getwd(), default_name), winslash = "/", mustWork = FALSE)
}

app_root <- dirname(find_bioszen_app_file("app.R"))
if (!dir.exists(file.path(app_root, "inst", "app"))) {
  app_root <- getwd()
}
app_root <- normalizePath(app_root, winslash = "/", mustWork = TRUE)

startup_file <- file.path(app_root, "R", "app_startup.R")
if (file.exists(startup_file)) {
  sys.source(startup_file, envir = globalenv())
  bioszen_prepare_direct_run()
}

app_dir <- normalizePath(file.path(app_root, "inst", "app"), winslash = "/", mustWork = TRUE)
shiny::shinyAppDir(app_dir)
