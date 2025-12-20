# Funciones para pruebas de correlación ---------------------------------------

run_correlation <- function(x, y, method = "pearson") {
  stats::cor.test(x, y, method = method)
}
