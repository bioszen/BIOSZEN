use_bioszen_standalone_library <- function() {
  local_lib <- getOption("BIOSZEN.local_lib", Sys.getenv("BIOSZEN_LOCAL_LIB", ""))
  if (!is.character(local_lib) || !length(local_lib) || !nzchar(local_lib[[1]])) {
    return(invisible(FALSE))
  }
  if (!dir.exists(local_lib[[1]])) {
    return(invisible(FALSE))
  }

  local_lib <- normalizePath(local_lib[[1]], winslash = "/", mustWork = TRUE)
  .libPaths(unique(c(local_lib, .libPaths())))
  invisible(TRUE)
}

use_bioszen_standalone_library()

resolve_bioszen_app_dir <- function() {
  frame_files <- unlist(lapply(sys.frames(), function(frame) {
    path <- frame$ofile
    if (is.character(path) && length(path) && nzchar(path[[1]])) path[[1]] else character()
  }), use.names = FALSE)

  candidates <- unique(c(
    dirname(normalizePath(frame_files, winslash = "/", mustWork = FALSE)),
    normalizePath(getwd(), winslash = "/", mustWork = FALSE),
    normalizePath(file.path(getwd(), "inst", "app"), winslash = "/", mustWork = FALSE)
  ))
  candidates <- candidates[nzchar(candidates)]

  for (candidate in candidates) {
    if (file.exists(file.path(candidate, "config.R")) &&
        file.exists(file.path(candidate, "global.R")) &&
        file.exists(file.path(candidate, "helpers.R")) &&
        dir.exists(file.path(candidate, "server")) &&
        dir.exists(file.path(candidate, "ui"))) {
      return(candidate)
    }
  }

  stop("Could not locate the BIOSZEN Shiny app directory.", call. = FALSE)
}

app_dir <- resolve_bioszen_app_dir()

resolve_bioszen_source_root <- function(app_dir) {
  candidates <- unique(c(
    normalizePath(file.path(app_dir, "..", ".."), winslash = "/", mustWork = FALSE),
    normalizePath(file.path(app_dir, ".."), winslash = "/", mustWork = FALSE),
    normalizePath(getwd(), winslash = "/", mustWork = FALSE),
    normalizePath(file.path(getwd(), ".."), winslash = "/", mustWork = FALSE)
  ))
  candidates <- candidates[nzchar(candidates)]

  for (candidate in candidates) {
    if (file.exists(file.path(candidate, "DESCRIPTION"))) {
      return(candidate)
    }
  }

  normalizePath(file.path(app_dir, "..", ".."), winslash = "/", mustWork = FALSE)
}

source_root <- resolve_bioszen_source_root(app_dir)

startup_files <- unique(c(
  file.path(source_root, "R", "app_startup.R"),
  file.path("R", "app_startup.R"),
  file.path("..", "..", "R", "app_startup.R")
))
startup_file <- startup_files[file.exists(startup_files)][1]
if (!is.na(startup_file)) {
  sys.source(startup_file, envir = globalenv())
  in_r_cmd_check <- nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_", unset = "")) ||
    grepl("\\.Rcheck(/|$)", normalizePath(getwd(), winslash = "/", mustWork = FALSE))
  skip_direct_setup <- isTRUE(getOption("BIOSZEN.skip_direct_run_setup", FALSE)) ||
    isTRUE(in_r_cmd_check)
  if (!skip_direct_setup) {
    bioszen_prepare_direct_run(root = source_root)
  }
}

source_dir <- function(path, envir) {
  files <- sort(list.files(path, pattern = "\\.R$", full.names = TRUE))
  for (f in files) sys.source(f, envir = envir)
}

configure_shiny_upload_limit <- function() {
  # Keep file parsing unchanged: only raise Shiny's HTTP upload ceiling.
  max_upload_mb <- suppressWarnings(as.numeric(Sys.getenv("BIOSZEN_MAX_UPLOAD_MB", "350")))
  if (!is.finite(max_upload_mb) || max_upload_mb <= 0) {
    max_upload_mb <- 350
  }
  options(shiny.maxRequestSize = max_upload_mb * 1024^2)
}

resolve_app_parent_env <- function() {
  # global.R attaches the app dependencies with library(); use the global
  # search chain so installed-package runs resolve those symbols correctly.
  globalenv()
}

app_env <- new.env(parent = resolve_app_parent_env())

configure_shiny_upload_limit()

sys.source(file.path(app_dir, "config.R"), envir = app_env)          # constantes compartidas

sys.source(file.path(app_dir, "global.R"), envir = app_env)          # paquetes y configuraciones generales

sys.source(file.path(app_dir, "helpers.R"), envir = app_env)         # funciones compartidas

# Carga automatica de modulos; permite agregar nuevos .R facilmente
source_dir(file.path(app_dir, "params"), envir = app_env)        # utilidades de parametros
source_dir(file.path(app_dir, "stats"), envir = app_env)         # funciones estadisticas
source_dir(file.path(app_dir, "graficos"), envir = app_env)      # helpers de graficos
source_dir(file.path(app_dir, "server"), envir = app_env)        # modulos de servidor
source_dir(file.path(app_dir, "ui"), envir = app_env)            # interfaz

shiny::addResourcePath("www", file.path(app_dir, "www"))

.bioszen_emit_app_startup_citation <- function() {
  if (identical(Sys.getenv("BIOSZEN_LAUNCHER_CITATION_SHOWN", unset = ""), "1")) {
    options(BIOSZEN.startup_citation_shown = TRUE)
    return(invisible(FALSE))
  }
  if (!isTRUE(getOption("BIOSZEN.show_startup_citation", TRUE))) {
    return(invisible(FALSE))
  }
  if (isTRUE(getOption("BIOSZEN.startup_citation_shown", FALSE))) {
    return(invisible(FALSE))
  }

  options(BIOSZEN.startup_citation_shown = TRUE)
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

.bioszen_emit_app_startup_citation()

shiny::shinyApp(app_env$ui, app_env$server)
