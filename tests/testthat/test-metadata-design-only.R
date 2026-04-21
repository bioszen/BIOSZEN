library(testthat)

root <- app_test_root()
source(app_test_path( "helpers.R"))

test_that("metadata_filter_design_only removes data-selection keys", {
  meta <- data.frame(
    Campo = c(
      "tipo", "colorMode", "scope", "strain", "param",
      "doNorm", "ctrlMedium", "heat_scale_mode", "heat_params"
    ),
    Valor = c(
      "Heatmap", "Viridis", "Combinado", "S1", "uMax",
      "TRUE", "Control", "row", "uMax,ODmax"
    ),
    stringsAsFactors = FALSE
  )

  out <- metadata_filter_design_only(meta)
  removed <- metadata_data_keys()

  expect_false(any(as.character(out$Campo) %in% removed))
  expect_true(all(c("tipo", "colorMode", "heat_scale_mode") %in% as.character(out$Campo)))
})
