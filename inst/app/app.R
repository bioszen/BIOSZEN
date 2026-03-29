source_dir <- function(path, envir) {
  files <- sort(list.files(path, pattern = "\\.R$", full.names = TRUE))
  for (f in files) sys.source(f, envir = envir)
}

resolve_app_parent_env <- function() {
  if ("package:BIOSZEN" %in% search()) {
    return(as.environment("package:BIOSZEN"))
  }
  if (requireNamespace("BIOSZEN", quietly = TRUE)) {
    return(asNamespace("BIOSZEN"))
  }
  globalenv()
}

app_env <- new.env(parent = resolve_app_parent_env())

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
