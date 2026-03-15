library(testthat)

extract_i18n_keys <- function(file_path) {
  txt <- paste(readLines(file_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  m_tr <- regmatches(txt, gregexpr('tr\\("([^"]+)"\\)', txt, perl = TRUE))[[1]]
  keys_tr <- if (length(m_tr)) sub('^tr\\("([^"]+)"\\)$', "\\1", m_tr) else character(0)

  m_tr_text <- regmatches(txt, gregexpr('tr_text\\("([^"]+)"', txt, perl = TRUE))[[1]]
  keys_tr_text <- if (length(m_tr_text)) sub('^tr_text\\("([^"]+)".*$', "\\1", m_tr_text) else character(0)

  unique(c(keys_tr, keys_tr_text))
}

test_that("translation files stay synchronized and include new keys", {
  root <- normalizePath(testthat::test_path("..", ".."))
  en_file <- file.path(root, "inst", "app", "i18n", "translation_en.csv")
  es_file <- file.path(root, "inst", "app", "i18n", "translation_es.csv")

  en <- read.csv(en_file, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  es <- read.csv(es_file, stringsAsFactors = FALSE, fileEncoding = "UTF-8")

  expect_true(all(c("key", "en") %in% names(en)))
  expect_true(all(c("key", "es") %in% names(es)))

  en_keys <- as.character(en$key)
  es_keys <- as.character(es$key)

  expect_equal(length(en_keys), length(unique(en_keys)))
  expect_equal(length(es_keys), length(unique(es_keys)))
  expect_equal(sort(en_keys), sort(es_keys))

  required_new_keys <- c(
    "plot_raincloud", "plot_estimation", "plot_heatmap", "plot_corr_matrix",
    "multitest_method", "multitest_holm", "multitest_fdr", "multitest_bonferroni", "multitest_none",
    "corr_method_kendall", "corr_matrix_settings", "corr_matrix_params", "corr_matrix_show_sig",
    "heatmap_settings", "heatmap_params", "heatmap_zscore", "heatmap_cluster_rows", "heatmap_cluster_cols",
    "heatmap_show_values", "estimation_settings", "estimation_boot_n",
    "curves_show_ci", "curves_show_reps", "curves_rep_alpha",
    "stats_tab_advanced", "resample_method", "resample_bootstrap", "resample_permutation", "resample_iter",
    "run_mixed_model", "run_advanced_stats", "mixed_model_not_available",
    "progress_advanced_stats", "progress_normality", "progress_normalization",
    "download_repro_report", "tab_qc", "qc_title", "qc_subtitle", "qc_missing_table", "qc_outlier_table",
    "qc_sample_table"
  )
  expect_true(all(required_new_keys %in% en_keys))
  expect_true(all(required_new_keys %in% es_keys))
})

test_that("all direct tr/tr_text keys used by app R files exist in translation_en", {
  root <- normalizePath(testthat::test_path("..", ".."))
  app_dir <- file.path(root, "inst", "app")
  en_file <- file.path(app_dir, "i18n", "translation_en.csv")
  en_keys <- as.character(read.csv(en_file, stringsAsFactors = FALSE, fileEncoding = "UTF-8")$key)

  r_files <- list.files(app_dir, pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
  used_keys <- unique(unlist(lapply(r_files, extract_i18n_keys), use.names = FALSE))
  used_keys <- used_keys[nzchar(used_keys)]

  missing_keys <- setdiff(used_keys, en_keys)
  expect_equal(
    length(missing_keys),
    0,
    info = paste("Missing i18n keys:", paste(sort(missing_keys), collapse = ", "))
  )
})
