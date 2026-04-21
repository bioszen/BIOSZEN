library(testthat)

root <- app_test_root()
source(app_test_path( "helpers.R"))

test_that("qc outlier strictness changes detected replicate count", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")
  skip_if_not_installed("tibble")

  df <- data.frame(
    Group = rep("G1", 6),
    BiologicalReplicate = paste0("R", 1:6),
    P = c(10, 10.5, 9.8, 10.2, 11, 12.0),
    stringsAsFactors = FALSE
  )

  permissive <- qc_outlier_replicates(
    df = df,
    params = "P",
    group_col = "Group",
    iqr_mult = 1.5
  )
  strict <- qc_outlier_replicates(
    df = df,
    params = "P",
    group_col = "Group",
    iqr_mult = 0.5
  )

  expect_lt(nrow(permissive), nrow(strict))
  expect_true("R6" %in% strict$Replicate)
})

test_that("qc summary includes outlier counts by group", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")
  skip_if_not_installed("tibble")

  df <- data.frame(
    Group = rep(c("A", "B"), each = 6),
    BiologicalReplicate = paste0("R", 1:12),
    P = c(10, 10.5, 9.8, 10.2, 11, 13.0, 5, 5.1, 5.0, 4.9, 5.2, 7.0),
    stringsAsFactors = FALSE
  )

  detected <- qc_detect_iqr_outliers(
    df = df,
    params = "P",
    group_col = "Group",
    iqr_mult = 1.5
  )
  out_tbl <- qc_summarize_outliers(detected)
  by_group <- out_tbl |>
    dplyr::group_by(Group) |>
    dplyr::summarise(Outliers = sum(Outliers), .groups = "drop")

  expect_true(all(c("A", "B") %in% by_group$Group))
  expect_true(all(by_group$Outliers > 0))
})

test_that("qc top-N reproducibility keeps the closest replicates", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")
  skip_if_not_installed("tibble")

  df <- data.frame(
    Group = rep("G1", 5),
    BiologicalReplicate = paste0("R", 1:5),
    P1 = c(10, 10.1, 9.9, 15, 16),
    P2 = c(10, 10.0, 10.1, 15, 14),
    stringsAsFactors = FALSE
  )

  scores <- qc_rank_replicates(
    df = df,
    params = c("P1", "P2"),
    group_col = "Group"
  )
  picked <- qc_pick_top_replicates(scores, keep_n = 2)

  expect_equal(nrow(picked), 2)
  expect_false(any(picked$Replicate %in% c("R4", "R5")))
})

test_that("qc helpers return empty results when data are insufficient", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")
  skip_if_not_installed("tibble")

  df <- data.frame(
    Group = "G1",
    BiologicalReplicate = "R1",
    P = NA_real_
  )

  out_reps <- qc_outlier_replicates(df = df, params = "P", group_col = "Group")
  scores <- qc_rank_replicates(df = df, params = "P", group_col = "Group")
  picked <- qc_pick_top_replicates(scores, keep_n = 2)

  expect_equal(nrow(out_reps), 0)
  expect_equal(nrow(scores), 0)
  expect_equal(nrow(picked), 0)
})
