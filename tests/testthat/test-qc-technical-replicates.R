library(testthat)

root <- app_test_root()
source(app_test_path( "helpers.R"))

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

test_that("technical selection filter can match blank-media group keys during export", {
  df <- data.frame(
    Strain = c("LysoR-", "LysoR-", "LysoR-"),
    Media = c("", "", ""),
    BiologicalReplicate = c("1", "1", "1"),
    TechnicalReplicate = c("2461", "2462", "2463"),
    Value = c(1, 2, 30),
    stringsAsFactors = FALSE
  )

  filtered <- qc_filter_by_technical_selection(
    df,
    tech_map = list("LysoR--||1" = c("2461", "2462")),
    group_col = NULL,
    biorep_col = "BiologicalReplicate",
    tech_col = "TechnicalReplicate",
    strain_col = "Strain",
    media_col = "Media"
  )

  expect_equal(nrow(filtered), 2)
  expect_identical(sort(filtered$TechnicalReplicate), c("2461", "2462"))
  expect_false("2463" %in% filtered$TechnicalReplicate)
})

test_that("technical outlier exclusion builds canonical maps from source rows", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tibble")

  df <- data.frame(
    Label = c("LysoR--", "LysoR--", "LysoR--", "Filipina-siNEG"),
    Strain = c("LysoR-", "LysoR-", "LysoR-", "Filipina"),
    Media = c("", "", "", "siNEG"),
    BiologicalReplicate = c("1", "1", "1", "7"),
    TechnicalReplicate = c("2461", "2462", "2463", "501"),
    P = c(1, 2, 30, 4),
    stringsAsFactors = FALSE
  )
  out_reps <- tibble::tibble(
    Group = "LysoR--",
    Subgroup = "1",
    Replicate = "2463"
  )

  result <- qc_build_technical_outlier_selection(
    df = df,
    out_reps = out_reps,
    group_col = "Label",
    current_map = list("LysoR--||1" = c("2461", "2462", "2463"))
  )

  expect_equal(result$changed, 1L)
  expect_identical(result$map[["LysoR--||1"]], c("2461", "2462"))
  expect_identical(result$map[["Filipina||siNEG||7"]], "501")

  filtered <- qc_filter_by_technical_selection(df, tech_map = result$map, group_col = NULL)
  expect_false(any(filtered$Label == "LysoR--" & filtered$TechnicalReplicate == "2463"))
  expect_true(any(filtered$Label == "Filipina-siNEG" & filtered$TechnicalReplicate == "501"))
})

test_that("technical filtered detail table lists only selected technical replicates", {
  df <- data.frame(
    Strain = c("S1", "S1", "S1", "S1"),
    Media = c("M1", "M1", "M1", "M1"),
    Orden = c(1, 1, 1, 1),
    Well = c("A1", "A2", "A3", "A4"),
    BiologicalReplicate = c("B1", "B1", "B2", "B2"),
    TechnicalReplicate = c("T1", "T2", "T1", "T2"),
    P = c(10, 100, 20, 30),
    stringsAsFactors = FALSE
  )

  tbl <- build_technical_filtered_detail_table(
    df = df,
    param = "P",
    tech_map = list(
      "S1||M1||B1" = "T1",
      "S1||M1||B2" = c("T1", "T2")
    )
  )

  expect_equal(nrow(tbl), 3)
  expect_true(all(c("Version", "Strain", "Media", "RepBiol", "RepTec", "Well", "Parameter", "Valor") %in% names(tbl)))
  expect_true(all(tbl$Version == "filt"))
  expect_false(any(tbl$RepBiol == "B1" & tbl$RepTec == "T2"))
  expect_true(any(tbl$RepBiol == "B1" & tbl$RepTec == "T1"))
  expect_true(any(tbl$RepBiol == "B2" & tbl$RepTec == "T2"))
})
