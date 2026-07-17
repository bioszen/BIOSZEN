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
app_dir <- file.path(app_root, "inst", "app")
if (!dir.exists(app_dir)) {
  working_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  working_app_dir <- file.path(working_root, "inst", "app")
  if (dir.exists(working_app_dir)) {
    app_root <- working_root
    app_dir <- working_app_dir
  }
}

if (!dir.exists(app_dir)) {
  standalone_launcher <- file.path(app_root, "inst", "launchers", "App.R")
  if (!file.exists(standalone_launcher)) {
    stop(
      "Could not find the BIOSZEN app or standalone launcher. ",
      "For a release bundle, keep App.R beside BIOSZEN-v*.tar.gz and run App.R.",
      call. = FALSE
    )
  }

  source_standalone_launcher <- function(path) {
    previous_launcher_script <- Sys.getenv("BIOSZEN_LAUNCHER_SCRIPT", unset = NA_character_)
    on.exit({
      if (is.na(previous_launcher_script)) {
        Sys.unsetenv("BIOSZEN_LAUNCHER_SCRIPT")
      } else {
        Sys.setenv(BIOSZEN_LAUNCHER_SCRIPT = previous_launcher_script)
      }
    }, add = TRUE)

    Sys.setenv(BIOSZEN_LAUNCHER_SCRIPT = normalizePath(path, winslash = "/", mustWork = TRUE))
    sys.source(path, envir = globalenv())
  }

  source_standalone_launcher(standalone_launcher)
} else {
  app_root <- normalizePath(app_root, winslash = "/", mustWork = TRUE)
  app_dir <- normalizePath(app_dir, winslash = "/", mustWork = TRUE)

  startup_file <- file.path(app_root, "R", "app_startup.R")
  if (file.exists(startup_file)) {
    sys.source(startup_file, envir = globalenv())
    bioszen_prepare_direct_run()
  }

  shiny::shinyAppDir(app_dir)
}
