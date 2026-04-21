library(testthat)

root <- normalizePath(test_path("..", ".."))
load_heatmap_perf_sources <- function() {
  app_dir <- app_test_path()
  old <- setwd(app_dir)
  on.exit(setwd(old), add = TRUE)
  source(file.path(app_dir, "global.R"))
  source(file.path(app_dir, "helpers.R"))
  source(file.path(app_dir, "graficos", "graficos_heatmap.R"))
}
load_heatmap_perf_sources()

build_large_heatmap_fixture <- function(n_strains = 8L, n_media = 8L, reps = 4L, n_params = 140L) {
  strains <- paste0("S", seq_len(n_strains))
  medias <- paste0("M", seq_len(n_media))
  grid <- expand.grid(
    Strain = strains,
    Media = medias,
    BiologicalReplicate = seq_len(reps),
    stringsAsFactors = FALSE
  )
  grid$Orden <- as.integer(match(grid$Media, medias))
  grid$TechnicalReplicate <- 1L
  grid$Well <- paste0("W", seq_len(nrow(grid)))
  grid$Label <- paste(grid$Strain, grid$Media, sep = "-")

  for (i in seq_len(n_params)) {
    pattern <- (as.integer(factor(grid$Strain)) * 0.8) +
      (as.integer(factor(grid$Media)) * 0.4) +
      (grid$BiologicalReplicate * 0.02) +
      sin(i / 10)
    grid[[paste0("P", i)]] <- pattern + (i / 20)
  }
  grid
}

test_that("performance smoke: large heatmap payload computes within budget", {
  skip_on_cran()

  df <- build_large_heatmap_fixture()
  params <- paste0("P", seq_len(140))

  elapsed <- unname(system.time({
    payload <- prepare_heatmap_payload(
      df_h = df,
      scope = "Combinado",
      label_mode = FALSE,
      all_params = params,
      params_raw = params,
      cluster_rows = TRUE,
      cluster_cols = TRUE,
      hclust_method = "average",
      k_rows = 6L,
      k_cols = 5L,
      scale_mode = "row"
    )
    expect_true(nrow(payload$plot_df) > 0L)
  })["elapsed"])

  # Smoke budget to catch obvious performance regressions.
  expect_lt(elapsed, 12)
})

test_that("performance smoke: cluster workbook export remains responsive", {
  skip_on_cran()

  df <- build_large_heatmap_fixture(n_params = 120L)
  params <- paste0("P", seq_len(120))
  payload <- prepare_heatmap_payload(
    df_h = df,
    scope = "Combinado",
    label_mode = FALSE,
    all_params = params,
    params_raw = params,
    cluster_rows = TRUE,
    cluster_cols = TRUE,
    k_rows = 5L,
    k_cols = 4L
  )

  out <- tempfile("heatmap_perf_", fileext = ".xlsx")
  on.exit(unlink(out), add = TRUE)

  elapsed <- unname(system.time({
    write_heatmap_cluster_workbook(payload, out)
  })["elapsed"])

  expect_true(file.exists(out))
  expect_lt(elapsed, 8)
})
