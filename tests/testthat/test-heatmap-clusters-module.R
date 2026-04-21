library(testthat)

root <- normalizePath(test_path("..", ".."))
load_heatmap_cluster_sources <- function() {
  app_dir <- app_test_path()
  old <- setwd(app_dir)
  on.exit(setwd(old), add = TRUE)
  source(file.path(app_dir, "global.R"))
  source(file.path(app_dir, "helpers.R"))
  source(file.path(app_dir, "graficos", "graficos_heatmap.R"))
}
load_heatmap_cluster_sources()

make_heatmap_fixture <- function(n_strains = 3L, n_media = 4L, reps = 5L, n_params = 12L) {
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
    # Deterministic pattern with slight noise by replicate.
    base <- (as.integer(factor(grid$Strain)) * 1.7) + (as.integer(factor(grid$Media)) * 0.9) + i / 7
    grid[[paste0("P", i)]] <- base + (grid$BiologicalReplicate * 0.03)
  }
  grid
}

test_that("prepare_heatmap_payload honors requested cluster counts", {
  df <- make_heatmap_fixture(n_params = 10L)
  all_params <- paste0("P", seq_len(10))

  payload <- prepare_heatmap_payload(
    df_h = df,
    scope = "Combinado",
    label_mode = FALSE,
    all_params = all_params,
    params_raw = all_params,
    high_dim = FALSE,
    do_norm = FALSE,
    has_ctrl_selected = FALSE,
    scale_mode = "row",
    cluster_rows = TRUE,
    cluster_cols = TRUE,
    hclust_method = "average",
    k_rows = 3L,
    k_cols = 2L
  )

  expect_equal(payload$row_cluster_count, 3L)
  expect_equal(payload$col_cluster_count, 2L)
  expect_true(length(payload$row_boundaries) >= 1L)
  expect_true(length(payload$col_boundaries) >= 1L)
  expect_true(nrow(payload$plot_df) > 0L)
})

test_that("prepare_heatmap_payload adds visual gaps between clusters", {
  df <- make_heatmap_fixture(n_params = 24L)
  params <- paste0("P", seq_len(24))

  payload <- prepare_heatmap_payload(
    df_h = df,
    scope = "Combinado",
    label_mode = FALSE,
    all_params = params,
    params_raw = params,
    scale_mode = "row",
    cluster_rows = TRUE,
    cluster_cols = TRUE,
    hclust_method = "average",
    k_rows = 4L,
    k_cols = 3L
  )

  row_steps <- diff(sort(unique(payload$row_breaks)))
  col_steps <- diff(sort(unique(payload$col_breaks)))
  expect_true(any(row_steps > 1, na.rm = TRUE))
  expect_true(any(col_steps > 1, na.rm = TRUE))
})

test_that("prepare_heatmap_payload keeps contiguous positions without clustering", {
  df <- make_heatmap_fixture(n_params = 16L)
  params <- paste0("P", seq_len(16))

  payload <- prepare_heatmap_payload(
    df_h = df,
    scope = "Combinado",
    label_mode = FALSE,
    all_params = params,
    params_raw = params,
    scale_mode = "row",
    cluster_rows = FALSE,
    cluster_cols = FALSE
  )

  row_steps <- diff(sort(unique(payload$row_breaks)))
  col_steps <- diff(sort(unique(payload$col_breaks)))
  expect_true(all(abs(row_steps - 1) < 1e-9))
  expect_true(all(abs(col_steps - 1) < 1e-9))
})

test_that("prepare_heatmap_payload keeps side dendrogram spacing compact", {
  df <- make_heatmap_fixture(n_strains = 2L, n_media = 1L, reps = 5L, n_params = 60L)
  params <- paste0("P", seq_len(60))

  payload <- prepare_heatmap_payload(
    df_h = df,
    scope = "Combinado",
    label_mode = FALSE,
    all_params = params,
    params_raw = params,
    scale_mode = "row",
    cluster_rows = TRUE,
    cluster_cols = FALSE,
    hclust_method = "average",
    k_rows = 4L
  )

  expect_true(is.numeric(payload$side_dend_gap))
  expect_true(is.numeric(payload$side_space))
  expect_lt(payload$side_dend_gap, 0.15)
  expect_lt(payload$side_space, 0.7)
})

test_that("write_heatmap_cluster_workbook exports row-cluster member sheets", {
  skip_if_not_installed("readxl")

  df <- make_heatmap_fixture(n_params = 8L)
  params <- paste0("P", seq_len(8))
  payload <- prepare_heatmap_payload(
    df_h = df,
    scope = "Combinado",
    label_mode = FALSE,
    all_params = params,
    params_raw = params,
    cluster_rows = TRUE,
    cluster_cols = TRUE,
    k_rows = 4L,
    k_cols = 2L
  )

  out <- tempfile("heatmap_clusters_", fileext = ".xlsx")
  on.exit(unlink(out), add = TRUE)
  write_heatmap_cluster_workbook(payload, out)

  expect_true(file.exists(out))
  sheets <- readxl::excel_sheets(out)
  expect_true(all(c("Summary", "HeatmapMatrix", "RowClusters", "ColumnClusters") %in% sheets))

  row_tbl <- readxl::read_excel(out, sheet = "RowClusters")
  expect_true("Cluster" %in% names(row_tbl))
  expect_true(any(is.finite(as.numeric(row_tbl$Cluster)), na.rm = TRUE))

  dynamic_row_cluster_sheets <- grep("^RowCluster_", sheets, value = TRUE)
  expect_true(length(dynamic_row_cluster_sheets) >= 1L)
})

test_that("prepare_heatmap_payload includes all parameters at omics scale", {
  df <- make_heatmap_fixture(n_strains = 5L, n_media = 6L, reps = 4L, n_params = 260L)
  all_params <- paste0("P", seq_len(260))

  payload <- prepare_heatmap_payload(
    df_h = df,
    scope = "Combinado",
    label_mode = FALSE,
    all_params = all_params,
    params_raw = character(0),
    high_dim = TRUE,
    do_norm = FALSE,
    has_ctrl_selected = FALSE,
    scale_mode = "row",
    cluster_rows = TRUE,
    cluster_cols = TRUE,
    hclust_method = "average",
    k_rows = 6L,
    k_cols = 5L
  )

  expect_equal(length(payload$row_order), length(all_params))
  expect_equal(length(payload$col_order), length(unique(df$Label)))
  expect_equal(nrow(payload$mat_vals), length(all_params))
  expect_equal(ncol(payload$mat_vals), length(unique(df$Label)))
  expect_equal(dplyr::n_distinct(payload$plot_df$Parametro), length(all_params))
})

test_that("prepare_heatmap_payload supports transposed orientation and hidden parameter labels", {
  df <- make_heatmap_fixture(n_strains = 3L, n_media = 3L, reps = 3L, n_params = 20L)
  params <- paste0("P", seq_len(20))

  payload <- prepare_heatmap_payload(
    df_h = df,
    scope = "Combinado",
    label_mode = FALSE,
    all_params = params,
    params_raw = params,
    show_param_labels = FALSE,
    orientation = "params_cols",
    scale_mode = "row",
    cluster_rows = TRUE,
    cluster_cols = TRUE,
    hclust_method = "average",
    k_rows = 3L,
    k_cols = 4L
  )

  expect_equal(payload$row_entity, "group")
  expect_equal(payload$col_entity, "parameter")
  expect_equal(ncol(payload$mat_vals), length(params))
  expect_true(all(payload$col_labels == ""))
  expect_gt(length(payload$row_labels), 0L)
})

test_that("prepare_heatmap_payload can abort when requested", {
  df <- make_heatmap_fixture(n_strains = 4L, n_media = 4L, reps = 6L, n_params = 120L)
  params <- paste0("P", seq_len(120))

  expect_error(
    prepare_heatmap_payload(
      df_h = df,
      scope = "Combinado",
      label_mode = FALSE,
      all_params = params,
      params_raw = params,
      show_param_labels = TRUE,
      orientation = "params_rows",
      scale_mode = "row",
      cluster_rows = TRUE,
      cluster_cols = TRUE,
      should_abort = function() TRUE
    ),
    "cancelled",
    ignore.case = TRUE
  )
})
