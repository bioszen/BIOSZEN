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
  # Use globalenv() so symbols attached by library() during app bootstrap
  # (e.g., ggplot2::theme_update) are resolvable in sourced app files.
  # Using a package/namespace parent can hide attached package symbols.
  globalenv()
}

app_env <- new.env(parent = resolve_app_parent_env())

configure_shiny_upload_limit()

sys.source("global.R", envir = app_env)          # paquetes y configuraciones generales

sys.source("helpers.R", envir = app_env)         # funciones compartidas

# Carga automatica de modulos; permite agregar nuevos .R facilmente
source_dir("params", envir = app_env)        # utilidades de parametros
source_dir("stats", envir = app_env)         # funciones estadisticas
source_dir("graficos", envir = app_env)      # helpers de graficos
source_dir("server", envir = app_env)        # modulos de servidor
source_dir("ui", envir = app_env)            # interfaz

shiny::addResourcePath("www", "www")

shiny::shinyApp(app_env$ui, app_env$server)
