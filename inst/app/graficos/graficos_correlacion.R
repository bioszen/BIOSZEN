# Helpers para gráficos de correlación ---------------------------------------

# Se asegura de que las funciones de correlación estén disponibles
source("stats/stats_correlation.R")

plot_correlacion <- function(scope, strain = NULL) {
  build_plot(scope, strain, "Correlacion")
}
