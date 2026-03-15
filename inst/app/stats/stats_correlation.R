# Funciones para pruebas de correlación ---------------------------------------

run_correlation <- function(x, y, method = "pearson") {
  cor_args <- list(x = x, y = y, method = method)
  if (method %in% c("spearman", "kendall")) cor_args$exact <- FALSE
  suppressWarnings(do.call(stats::cor.test, cor_args))
}
