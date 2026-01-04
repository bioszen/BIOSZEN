source_dir <- function(path, envir) {
  files <- list.files(path, pattern = "\\.R$", full.names = TRUE)
  for (f in files) sys.source(f, envir = envir)
}

app_env <- new.env(parent = globalenv())

sys.source("global.R", envir = app_env)          # paquetes y configuraciones generales
pkg_parent <- NULL
search_path <- search()
if (length(search_path) >= 2 && grepl("^package:", search_path[2])) {
  pkg_parent <- as.environment(search_path[2])
}
if (!is.null(pkg_parent)) parent.env(app_env) <- pkg_parent

sys.source("helpers.R", envir = app_env)         # funciones compartidas

# Carga automatica de modulos; permite agregar nuevos .R facilmente
source_dir("params", envir = app_env)        # utilidades de parametros
source_dir("stats", envir = app_env)         # funciones estadisticas
source_dir("graficos", envir = app_env)      # helpers de graficos
source_dir("server", envir = app_env)        # modulos de servidor

sys.source("ui/ui_main.R", envir = app_env)      # interfaz
sys.source("server/server_main.R", envir = app_env)  # logica

shiny::addResourcePath("www", "www")

shiny::shinyApp(app_env$ui, app_env$server)
