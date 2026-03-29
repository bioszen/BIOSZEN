library(testthat)

root <- normalizePath(testthat::test_path("..", ".."))
source(file.path(root, "inst", "app", "helpers.R"))

test_that("qc technical outlier detection works within biological replicate subgroups", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")
  skip_if_not_installed("tibble")

  df <- data.frame(
    Group = rep("G1", 8),
    BiologicalReplicate = rep(c("B1", "B2"), each = 4),
    TechnicalReplicate = rep(c("T1", "T2", "T3", "T4"), 2),
    P = c(10.0, 10.1, 10.2, 14.0, 5.0, 5.1, 5.2, 8.0),
    stringsAsFactors = FALSE
  )

  out_reps <- qc_outlier_replicates(
    df = df,
    params = "P",
    group_col = "Group",
    rep_col = "TechnicalReplicate",
    subgroup_col = "BiologicalReplicate",
    iqr_mult = 0.5
  )

  expect_true(all(c("Group", "Subgroup", "Replicate") %in% names(out_reps)))
  expect_true(any(out_reps$Subgroup == "B1" & out_reps$Replicate == "T4"))
  expect_true(any(out_reps$Subgroup == "B2" & out_reps$Replicate == "T4"))
})

test_that("qc technical top-N selection is computed independently per biological replicate", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")
  skip_if_not_installed("tibble")

  df <- data.frame(
    Group = rep("G1", 6),
    BiologicalReplicate = rep(c("B1", "B2"), each = 3),
    TechnicalReplicate = rep(c("T1", "T2", "T3"), 2),
    P1 = c(10.0, 10.1, 20.0, 5.0, 5.1, 12.0),
    P2 = c(10.0, 10.2, 19.0, 5.0, 4.9, 11.0),
    stringsAsFactors = FALSE
  )

  scores <- qc_rank_replicates(
    df = df,
    params = c("P1", "P2"),
    group_col = "Group",
    rep_col = "TechnicalReplicate",
    subgroup_col = "BiologicalReplicate"
  )
  picked <- qc_pick_top_replicates(scores, keep_n = 2)

  expect_true(all(c("Group", "Subgroup", "Replicate") %in% names(picked)))
  expect_equal(nrow(picked), 4)
  expect_false(any(picked$Replicate == "T3"))
  by_sub <- picked |>
    dplyr::group_by(Group, Subgroup) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop")
  expect_true(all(by_sub$n == 2))
})

test_that("qc technical outlier summary keeps subgroup column", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")
  skip_if_not_installed("tibble")

  df <- data.frame(
    Group = rep("G1", 8),
    BiologicalReplicate = rep(c("B1", "B2"), each = 4),
    TechnicalReplicate = rep(c("T1", "T2", "T3", "T4"), 2),
    P = c(10.0, 10.1, 10.2, 14.0, 5.0, 5.1, 5.2, 8.0),
    stringsAsFactors = FALSE
  )

  detected <- qc_detect_iqr_outliers(
    df = df,
    params = "P",
    group_col = "Group",
    rep_col = "TechnicalReplicate",
    subgroup_col = "BiologicalReplicate",
    iqr_mult = 0.5
  )
  out_tbl <- qc_summarize_outliers(detected)

  expect_true("Subgroup" %in% names(out_tbl))
  expect_true(any(out_tbl$Subgroup == "B1"))
  expect_true(any(out_tbl$Subgroup == "B2"))
})

test_that("technical selection filter updates biological aggregate inputs", {
  df <- data.frame(
    Strain = c("S1", "S1", "S1", "S1"),
    Media = c("M1", "M1", "M1", "M1"),
    BiologicalReplicate = c("B1", "B1", "B2", "B2"),
    TechnicalReplicate = c("T1", "T2", "T1", "T2"),
    Value = c(10, 100, 20, 20),
    stringsAsFactors = FALSE
  )

  baseline <- stats::aggregate(Value ~ BiologicalReplicate, data = df, FUN = mean)
  tech_map <- list(
    "S1||M1||B1" = "T1",
    "S1||M1||B2" = c("T1", "T2")
  )
  filtered <- qc_filter_by_technical_selection(df, tech_map = tech_map)
  updated <- stats::aggregate(Value ~ BiologicalReplicate, data = filtered, FUN = mean)

  expect_equal(baseline$Value[baseline$BiologicalReplicate == "B1"], 55)
  expect_equal(updated$Value[updated$BiologicalReplicate == "B1"], 10)
  expect_equal(updated$Value[updated$BiologicalReplicate == "B2"], 20)
})

test_that("technical selection filter falls back to group keys when needed", {
  df <- data.frame(
    Group = c("G1", "G1", "G1"),
    BiologicalReplicate = c("B1", "B1", "B1"),
    TechnicalReplicate = c("T1", "T2", "T3"),
    Value = c(1, 2, 3),
    stringsAsFactors = FALSE
  )

  filtered <- qc_filter_by_technical_selection(
    df,
    tech_map = list("G1||B1" = c("T2", "T3")),
    group_col = "Group",
    biorep_col = "BiologicalReplicate",
    tech_col = "TechnicalReplicate"
  )

  expect_identical(sort(filtered$TechnicalReplicate), c("T2", "T3"))
  expect_equal(nrow(filtered), 2)
})
