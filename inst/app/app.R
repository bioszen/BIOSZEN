source_dir <- function(path) {
  files <- list.files(path, pattern = "\\.R$", full.names = TRUE)
  for (f in files) source(f)
}

source('global.R')          # paquetes y configuraciones generales
source('helpers.R')         # funciones compartidas

# Carga automática de módulos; permite agregar nuevos .R fácilmente
source_dir('params')        # utilidades de parámetros
source_dir('stats')         # funciones estadísticas
source_dir('graficos')      # helpers de gráficos
source_dir('server')        # módulos de servidor

source('ui/ui_main.R')      # interfaz
source('server/server_main.R')  # lógica

shiny::addResourcePath('www', 'www')

shinyApp(ui, server)
