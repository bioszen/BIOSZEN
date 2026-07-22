library(testthat)

root <- app_test_root()
app_dir <- app_test_path()

load_app_sources <- function() {
  source(file.path(app_dir, "helpers.R"))
  source(file.path(app_dir, "global.R"))
}

extract_repbiol_blocks <- function(path, sheet) {
  raw <- readxl::read_excel(path, sheet = sheet, col_names = FALSE)
  c1 <- trimws(as.character(raw[[1]]))
  c1[is.na(c1)] <- ""
  hdr <- which(c1 == "RepBiol")
  if (!length(hdr)) return(list())

  blocks <- vector("list", length(hdr))
  for (i in seq_along(hdr)) {
    vals <- c1[(hdr[[i]] + 1):length(c1)]
    stop_ix <- which(vals == "")
    end_ix <- if (length(stop_ix)) (min(stop_ix) - 1L) else length(vals)
    if (end_ix <= 0) {
      blocks[[i]] <- character(0)
      next
    }
    block <- vals[seq_len(end_ix)]
    blocks[[i]] <- block[nzchar(block)]
  }
  blocks
}

read_strain_detail_block <- function(path, sheet, strain) {
  raw <- readxl::read_excel(path, sheet = sheet, col_names = FALSE)
  target <- paste("Strain:", strain)
  c1 <- trimws(as.character(raw[[1]]))
  c1[is.na(c1)] <- ""
  strain_row <- which(c1 == target)
  if (!length(strain_row)) return(tibble::tibble())

  header_row <- strain_row[[1]] + 1L
  if (header_row > nrow(raw)) return(tibble::tibble())

  headers <- trimws(as.character(unlist(raw[header_row, ], use.names = FALSE)))
  blank <- is.na(headers) | !nzchar(headers)
  headers[blank] <- paste0("V", which(blank))
  headers <- make.unique(headers, sep = "_")

  data_start <- header_row + 1L
  if (data_start > nrow(raw)) return(tibble::tibble())
  data_c1 <- trimws(as.character(raw[[1]][data_start:nrow(raw)]))
  data_c1[is.na(data_c1)] <- ""
  stop_ix <- which(!nzchar(data_c1))
  data_end <- if (length(stop_ix)) data_start + min(stop_ix) - 2L else nrow(raw)
  if (data_end < data_start) return(tibble::tibble())

  block <- raw[data_start:data_end, seq_along(headers), drop = FALSE]
  block <- tibble::as_tibble(block, .name_repair = "minimal")
  names(block) <- headers
  block
}

read_biological_summary_block <- function(path, sheet) {
  raw <- readxl::read_excel(path, sheet = sheet, col_names = FALSE)
  c1 <- trimws(as.character(raw[[1]]))
  c1[is.na(c1)] <- ""
  title_row <- which(c1 == "Resumen por réplica biológica")
  if (!length(title_row) || title_row[[1]] >= nrow(raw)) return(tibble::tibble())
  header_row <- title_row[[1]] + 1L
  headers <- trimws(as.character(unlist(raw[header_row, ], use.names = FALSE)))
  headers[is.na(headers) | !nzchar(headers)] <- paste0("V", which(is.na(headers) | !nzchar(headers)))
  data_start <- header_row + 1L
  if (data_start > nrow(raw)) return(tibble::tibble())
  block <- raw[data_start:nrow(raw), seq_along(headers), drop = FALSE]
  block <- tibble::as_tibble(block, .name_repair = "minimal")
  names(block) <- make.unique(headers, sep = "_")
  block[!is.na(block[[1]]) & nzchar(trimws(as.character(block[[1]]))), , drop = FALSE]
}

expect_grouped_workbook_sheets <- function(
    path,
    required = character(0),
    expected_filtered = character(0),
    absent_filtered = character(0)) {
  sheets <- openxlsx::getSheetNames(path)
  sheet_list <- paste(sheets, collapse = ", ")

  expect_true(
    all(required %in% sheets),
    info = paste("Missing required workbook sheets. Available sheets:", sheet_list)
  )
  expect_true(
    all(expected_filtered %in% sheets),
    info = paste("Missing expected filtered workbook sheets. Available sheets:", sheet_list)
  )
  expect_false(
    any(absent_filtered %in% sheets),
    info = paste("Unexpected filtered workbook sheets. Available sheets:", sheet_list)
  )

  readable <- unique(c(required, expected_filtered))
  for (sheet in readable[readable %in% sheets]) {
    expect_no_error(
      readxl::read_excel(path, sheet = sheet, col_names = FALSE, n_max = 20)
    )
  }

  filtered <- grep("_filt$", sheets, value = TRUE)
  for (sheet in filtered) {
    base_sheet <- sub("_filt$", "", sheet)
    expect_true(
      base_sheet %in% sheets,
      info = sprintf("Filtered sheet %s should have matching base sheet %s.", sheet, base_sheet)
    )
    expect_no_error(
      readxl::read_excel(path, sheet = sheet, col_names = FALSE, n_max = 20)
    )
  }

  invisible(sheets)
}

build_download_fixture <- function(drop_replicate = NULL, na_param_for_rep = NULL) {
  datos <- expand.grid(
    Strain = c("S1", "S2"),
    Media = c("M1", "M2"),
    BiologicalReplicate = c("R1", "R2", "R3"),
    TechnicalReplicate = c("T1", "T2"),
    stringsAsFactors = FALSE
  )

  datos$Orden <- ifelse(datos$Media == "M1", 1L, 2L)
  datos$Well <- paste0("W", seq_len(nrow(datos)))

  rep_ix <- as.numeric(factor(datos$BiologicalReplicate, levels = c("R1", "R2", "R3")))
  media_ix <- as.numeric(factor(datos$Media, levels = c("M1", "M2")))
  strain_ix <- as.numeric(factor(datos$Strain, levels = c("S1", "S2")))
  tech_ix <- as.numeric(factor(datos$TechnicalReplicate, levels = c("T1", "T2")))

  datos$uMax <- 0.20 + 0.03 * strain_ix + 0.02 * media_ix + 0.01 * rep_ix + 0.005 * tech_ix
  datos$ODmax <- 0.70 + 0.15 * strain_ix + 0.08 * media_ix + 0.03 * rep_ix + 0.01 * tech_ix

  if (!is.null(na_param_for_rep) && length(na_param_for_rep) == 2) {
    rep_id <- as.character(na_param_for_rep[[1]])
    param_id <- as.character(na_param_for_rep[[2]])
    if (nzchar(rep_id) && param_id %in% names(datos)) {
      idx <- as.character(datos$BiologicalReplicate) == rep_id
      datos[idx, param_id] <- NA_real_
    }
  }

  if (!is.null(drop_replicate)) {
    datos <- datos[datos$BiologicalReplicate != drop_replicate, , drop = FALSE]
  }

  time_points <- c(0, 30, 60, 90)
  curves <- data.frame(Time = time_points)
  for (i in seq_len(nrow(datos))) {
    well <- datos$Well[[i]]
    curves[[well]] <- 0.05 * i + 0.002 * time_points
  }

  list(
    datos = datos,
    params = c("uMax", "ODmax"),
    curves = curves
  )
}

test_that("grouped-parameter workbook download builds without errors", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("readxl")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")

  old <- setwd(app_dir)
  on.exit(setwd(old), add = TRUE)
  load_app_sources()

  fixture <- build_download_fixture()
  out_path <- tempfile("bioszen_grouped_download_", fileext = ".xlsx")
  on.exit(unlink(out_path), add = TRUE)

  expect_no_error({
    wb <- generate_summary_wb(fixture$datos, fixture$params)
    wb <- add_curves_by_group_sheet(
      wb = wb,
      curve_wide = fixture$curves,
      meta_df = fixture$datos
    )
    openxlsx::saveWorkbook(wb, out_path, overwrite = TRUE)
  })

  expect_true(file.exists(out_path))
  expect_gt(file.info(out_path)$size, 0)

  expect_grouped_workbook_sheets(
    out_path,
    required = c("uMax", "ODmax", "Curvas por grupo")
  )

  detail <- read_strain_detail_block(out_path, "uMax", "S1")
  expect_true(all(c("RepBiol", "RepTec", "M1", "M2") %in% names(detail)))
  expect_true(any(as.character(detail$RepBiol) == "R1" & as.character(detail$RepTec) == "T1"))
  expect_true(any(as.character(detail$RepBiol) == "R1" & as.character(detail$RepTec) == "T2"))
})

test_that("download workbook adds filtered tabs only for affected parameters", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("readxl")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")

  old <- setwd(app_dir)
  on.exit(setwd(old), add = TRUE)
  load_app_sources()

  fixture <- build_download_fixture(na_param_for_rep = c("R2", "ODmax"))
  out_path <- tempfile("bioszen_grouped_download_filtered_", fileext = ".xlsx")
  on.exit(unlink(out_path), add = TRUE)

  groups <- unique(paste(fixture$datos$Strain, fixture$datos$Media, sep = "-"))
  group_map <- setNames(rep(list(c("R1", "R3")), length(groups)), groups)
  filtered <- filter_export_replicates_for_download(
    df = fixture$datos,
    reps_group_map = group_map,
    active_strain = NULL
  )
  expect_true(isTRUE(filtered$has_changes))
  affected <- detect_filtered_params_for_download(
    raw_df = fixture$datos,
    filtered_df = filtered$df,
    params = fixture$params
  )
  expect_equal(affected, "uMax")
  datos_filt <- renumber_replicates_for_export(filtered$df)

  expect_no_error({
    wb <- generate_summary_wb(fixture$datos, fixture$params)
    wb <- add_curves_by_group_sheet(
      wb = wb,
      curve_wide = fixture$curves,
      meta_df = fixture$datos
    )
    wb <- generate_summary_wb(
      datos = datos_filt,
      params = affected,
      wb = wb,
      sheet_suffix = "_filt"
    )
    wb <- add_curves_by_group_sheet(
      wb = wb,
      curve_wide = filter_curve_wide_for_export(fixture$curves, datos_filt),
      meta_df = datos_filt,
      sheet_name = "Curvas por grupo_filt"
    )
    openxlsx::saveWorkbook(wb, out_path, overwrite = TRUE)
  })

  expect_true(file.exists(out_path))
  expect_gt(file.info(out_path)$size, 0)

  expect_grouped_workbook_sheets(
    out_path,
    required = c("uMax", "ODmax", "Curvas por grupo"),
    expected_filtered = c("uMax_filt", "Curvas por grupo_filt"),
    absent_filtered = "ODmax_filt"
  )

  umax_raw <- readxl::read_excel(out_path, sheet = "uMax_filt", col_names = FALSE)
  curves_raw <- readxl::read_excel(out_path, sheet = "Curvas por grupo_filt", col_names = FALSE)

  umax_vals <- trimws(as.character(unlist(umax_raw, use.names = FALSE)))
  umax_vals <- umax_vals[!is.na(umax_vals) & nzchar(umax_vals)]
  curve_vals <- trimws(as.character(unlist(curves_raw, use.names = FALSE)))
  curve_vals <- curve_vals[!is.na(curve_vals) & nzchar(curve_vals)]

  expect_false("R2" %in% umax_vals)
  expect_false("R3" %in% umax_vals)
  expect_false("R2" %in% curve_vals)
  expect_false("R3" %in% curve_vals)
})

test_that("replicates are naturally ordered in normal and filtered export tabs", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("readxl")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")

  old <- setwd(app_dir)
  on.exit(setwd(old), add = TRUE)
  load_app_sources()

  fixture <- build_download_fixture()
  fixture$datos$BiologicalReplicate <- rep_len(c("1", "10", "2", "20", "3", "1"), nrow(fixture$datos))
  out_path <- tempfile("bioszen_grouped_download_order_", fileext = ".xlsx")
  on.exit(unlink(out_path), add = TRUE)

  filtered <- fixture$datos %>%
    dplyr::filter(BiologicalReplicate %in% c("1", "2", "3", "10"))
  filtered <- renumber_replicates_for_export(filtered)

  expect_no_error({
    wb <- generate_summary_wb(fixture$datos, fixture$params)
    wb <- generate_summary_wb(
      datos = filtered,
      params = "uMax",
      wb = wb,
      sheet_suffix = "_filt"
    )
    openxlsx::saveWorkbook(wb, out_path, overwrite = TRUE)
  })

  normal_blocks <- extract_repbiol_blocks(out_path, "uMax")
  filt_blocks <- extract_repbiol_blocks(out_path, "uMax_filt")
  expect_gt(length(normal_blocks), 0)
  expect_gt(length(filt_blocks), 0)

  for (block in normal_blocks) {
    nums <- suppressWarnings(as.numeric(block))
    nums <- nums[is.finite(nums)]
    expect_equal(nums, sort(nums))
  }
  for (block in filt_blocks) {
    nums <- suppressWarnings(as.numeric(block))
    nums <- nums[is.finite(nums)]
    expect_equal(nums, sort(nums))
  }
})

test_that("same replicate ID can be filtered in one group and kept in another", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")

  fixture <- build_download_fixture()
  fixture$datos$BiologicalReplicate <- sub("^R", "", as.character(fixture$datos$BiologicalReplicate))

  filtered <- filter_export_replicates_for_download(
    df = fixture$datos,
    reps_group_map = list(
      "S1-M1" = c("1", "2"),
      "S2-M1" = c("1", "2", "3")
    ),
    active_strain = NULL
  )

  expect_false(any(
    filtered$df$Strain == "S1" &
      filtered$df$Media == "M1" &
      as.character(filtered$df$BiologicalReplicate) == "3"
  ))
  expect_true(any(
    filtered$df$Strain == "S2" &
      filtered$df$Media == "M1" &
      as.character(filtered$df$BiologicalReplicate) == "3"
  ))
})

test_that("sparse grouping rows do not break filtered parameter exports", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")

  fixture <- build_download_fixture()
  sparse_rows <- fixture$datos[seq_len(2), , drop = FALSE]
  sparse_rows$Well <- c("G12", "H12")
  sparse_rows$Strain <- NA_character_
  sparse_rows$Media <- NA_character_
  sparse_rows$BiologicalReplicate <- NA_character_
  sparse_rows$TechnicalReplicate <- NA_character_
  sparse <- rbind(fixture$datos, sparse_rows)

  expect_no_error(
    filtered <- filter_export_replicates_for_download(
      df = sparse,
      reps_strain_map = list(
        S1 = list(M1 = c("R1", "R2"), M2 = c("R1", "R2", "R3")),
        S2 = list(M1 = c("R1", "R2", "R3"), M2 = c("R1", "R2", "R3"))
      ),
      active_strain = "S1"
    )
  )

  expect_true(isTRUE(filtered$has_changes))
  expect_setequal(filtered$df$Well[is.na(filtered$df$Strain)], c("G12", "H12"))
  expect_false(any(
    filtered$df$Strain == "S1" &
      filtered$df$Media == "M1" &
      filtered$df$BiologicalReplicate == "R3",
    na.rm = TRUE
  ))

  expect_no_error(
    param_exports <- build_filtered_param_export_data(
      df = sparse,
      params = fixture$params,
      reps_group_map = list(
        "S1-M1" = c("R1", "R2"),
        "S1-M2" = c("R1", "R2", "R3"),
        "S2-M1" = c("R1", "R2", "R3"),
        "S2-M2" = c("R1", "R2", "R3")
      )
    )
  )
  expect_true(length(param_exports) >= 1L)
})

test_that("strain-level filtering applies only to the active strain", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")

  fixture <- build_download_fixture()
  fixture$datos$BiologicalReplicate <- sub("^R", "", as.character(fixture$datos$BiologicalReplicate))

  filtered <- filter_export_replicates_for_download(
    df = fixture$datos,
    reps_strain_map = list(M1 = c("1", "2")),
    active_strain = "S1"
  )

  expect_false(any(
    filtered$df$Strain == "S1" &
      filtered$df$Media == "M1" &
      as.character(filtered$df$BiologicalReplicate) == "3"
  ))
  expect_true(any(
    filtered$df$Strain == "S2" &
      filtered$df$Media == "M1" &
      as.character(filtered$df$BiologicalReplicate) == "3"
  ))
})

test_that("nested strain map filtering applies across all strains in export", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")

  fixture <- build_download_fixture()
  fixture$datos$BiologicalReplicate <- sub("^R", "", as.character(fixture$datos$BiologicalReplicate))

  filtered <- filter_export_replicates_for_download(
    df = fixture$datos,
    reps_strain_map = list(
      S1 = list(
        M1 = c("1", "2", "3"),
        M2 = c("1", "2", "3")
      ),
      S2 = list(
        M1 = c("1", "2"),
        M2 = c("1", "2", "3")
      )
    ),
    active_strain = "S1"
  )

  expect_true(isTRUE(filtered$has_changes))
  expect_true(any(
    filtered$df$Strain == "S1" &
      filtered$df$Media == "M1" &
      as.character(filtered$df$BiologicalReplicate) == "3"
  ))
  expect_false(any(
    filtered$df$Strain == "S2" &
      filtered$df$Media == "M1" &
      as.character(filtered$df$BiologicalReplicate) == "3"
  ))
})

test_that("group-map and strain-map filters produce equivalent exports", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")

  fixture <- build_download_fixture()
  fixture$datos$BiologicalReplicate <- sub("^R", "", as.character(fixture$datos$BiologicalReplicate))

  strain_filtered <- filter_export_replicates_for_download(
    df = fixture$datos,
    reps_strain_map = list(
      S1 = list(M1 = c("1", "2"), M2 = c("1", "2", "3")),
      S2 = list(M1 = c("1", "2", "3"), M2 = c("1", "2"))
    ),
    active_strain = NULL
  )$df %>%
    dplyr::arrange(Strain, Media, BiologicalReplicate, Well)

  group_filtered <- filter_export_replicates_for_download(
    df = fixture$datos,
    reps_group_map = list(
      "S1-M1" = c("1", "2"),
      "S1-M2" = c("1", "2", "3"),
      "S2-M1" = c("1", "2", "3"),
      "S2-M2" = c("1", "2")
    ),
    active_strain = NULL
  )$df %>%
    dplyr::arrange(Strain, Media, BiologicalReplicate, Well)

  expect_identical(strain_filtered, group_filtered)
})

test_that("technical replicate map is applied when filtering grouped export data", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")

  fixture <- build_download_fixture()
  tech_map <- list("S1||M1||R1" = "T1")

  filtered <- filter_export_replicates_for_download(
    df = fixture$datos,
    tech_selection_map = tech_map
  )

  expect_true(isTRUE(filtered$has_changes))
  expect_equal(filtered$dropped_rows, 1L)
  expect_false(any(
    filtered$df$Strain == "S1" &
      filtered$df$Media == "M1" &
      as.character(filtered$df$BiologicalReplicate) == "R1" &
      as.character(filtered$df$TechnicalReplicate) == "T2"
  ))
  expect_true(any(
    filtered$df$Strain == "S1" &
      filtered$df$Media == "M1" &
      as.character(filtered$df$BiologicalReplicate) == "R1" &
      as.character(filtered$df$TechnicalReplicate) == "T1"
  ))
  expect_true(any(
    filtered$df$Strain == "S2" &
      filtered$df$Media == "M2" &
      as.character(filtered$df$BiologicalReplicate) == "R3" &
      as.character(filtered$df$TechnicalReplicate) == "T2"
  ))
})

test_that("technical replicate filtering produces filtered grouped workbook sheets", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("readxl")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")

  old <- setwd(app_dir)
  on.exit(setwd(old), add = TRUE)
  load_app_sources()

  fixture <- build_download_fixture()
  out_path <- tempfile("bioszen_grouped_download_tech_filtered_", fileext = ".xlsx")
  on.exit(unlink(out_path), add = TRUE)

  filtered <- filter_export_replicates_for_download(
    df = fixture$datos,
    tech_selection_map = list(
      "S1||M1||R1" = "T1",
      "S1||M2||R1" = "T1"
    )
  )
  expect_true(isTRUE(filtered$has_changes))

  affected <- detect_filtered_params_for_download(
    raw_df = fixture$datos,
    filtered_df = filtered$df,
    params = fixture$params
  )
  expect_setequal(affected, fixture$params)

  datos_filt <- renumber_replicates_for_export(filtered$df)
  expect_no_error({
    wb <- generate_summary_wb(fixture$datos, fixture$params)
    wb <- add_curves_by_group_sheet(
      wb = wb,
      curve_wide = fixture$curves,
      meta_df = fixture$datos
    )
    wb <- generate_summary_wb(
      datos = datos_filt,
      params = affected,
      wb = wb,
      sheet_suffix = "_filt"
    )
    wb <- add_curves_by_group_sheet(
      wb = wb,
      curve_wide = filter_curve_wide_for_export(fixture$curves, datos_filt),
      meta_df = datos_filt,
      sheet_name = "Curvas por grupo_filt"
    )
    openxlsx::saveWorkbook(wb, out_path, overwrite = TRUE)
  })

  expect_grouped_workbook_sheets(
    out_path,
    required = c("uMax", "ODmax", "Curvas por grupo"),
    expected_filtered = c("uMax_filt", "ODmax_filt", "Curvas por grupo_filt")
  )

  raw_detail <- read_strain_detail_block(out_path, "uMax", "S1")
  filt_detail <- read_strain_detail_block(out_path, "uMax_filt", "S1")
  expect_true(all(c("RepBiol", "RepTec") %in% names(raw_detail)))
  expect_true(all(c("RepBiol", "RepTec") %in% names(filt_detail)))
  expect_true(any(as.character(raw_detail$RepBiol) == "R1" & as.character(raw_detail$RepTec) == "T2"))
  expect_false(any(as.character(filt_detail$RepBiol) == "1" & as.character(filt_detail$RepTec) == "T2"))
  expect_true(any(as.character(filt_detail$RepBiol) == "1" & as.character(filt_detail$RepTec) == "T1"))

  raw_blocks <- extract_repbiol_blocks(out_path, "uMax")
  filt_blocks <- extract_repbiol_blocks(out_path, "uMax_filt")
  expect_gt(length(raw_blocks), 0)
  expect_gt(length(filt_blocks), 0)
  expect_lt(sum(lengths(filt_blocks)), sum(lengths(raw_blocks)))
})

test_that("technical outlier deselection creates filtered parameter workbook tabs like biological deselection", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("readxl")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")
  skip_if_not_installed("tibble")

  old <- setwd(app_dir)
  on.exit(setwd(old), add = TRUE)
  load_app_sources()

  datos <- data.frame(
    Strain = rep("LysoR-", 8),
    Media = rep("", 8),
    BiologicalReplicate = rep(c("1", "2"), each = 4),
    TechnicalReplicate = rep(c("T1", "T2", "T3", "T4"), 2),
    Orden = rep(1L, 8),
    Well = paste0("W", seq_len(8)),
    uMax = c(10.0, 10.1, 10.2, 40.0, 20.0, 20.1, 20.2, 20.3),
    ODmax = c(1.0, 1.1, 1.2, 8.0, 2.0, 2.1, 2.2, 2.3),
    stringsAsFactors = FALSE
  )
  source_df <- datos %>% dplyr::mutate(Label = paste(.data$Strain, .data$Media, sep = "-"))

  biological_filtered <- build_filtered_param_export_data(
    df = datos,
    params = "uMax",
    reps_group_map = list("LysoR--" = "2")
  )
  expect_identical(names(biological_filtered), "uMax")

  out_reps <- qc_outlier_replicates(
    df = source_df,
    params = "uMax",
    group_col = "Label",
    rep_col = "TechnicalReplicate",
    subgroup_col = "BiologicalReplicate",
    iqr_mult = 1.5
  )
  expect_true(any(out_reps$Group == "LysoR--" & out_reps$Subgroup == "1" & out_reps$Replicate == "T4"))

  selection_result <- qc_build_technical_outlier_selection(
    df = source_df,
    out_reps = out_reps,
    group_col = "Label",
    current_map = list(
      "LysoR--||1" = c("T1", "T2", "T3", "T4"),
      "LysoR--||2" = c("T1", "T2", "T3", "T4")
    )
  )
  expect_equal(selection_result$changed, 1L)
  expect_identical(selection_result$map[["LysoR--||1"]], c("T1", "T2", "T3"))
  expect_identical(selection_result$map[["LysoR--||2"]], c("T1", "T2", "T3", "T4"))

  filtered_by_param <- build_filtered_param_export_data(
    df = datos,
    params = "uMax",
    tech_selection_map = selection_result$map,
    tech_selection_by_param = list(uMax = list()),
    active_tech_param = "uMax"
  )
  expect_identical(names(filtered_by_param), "uMax")

  out_path <- tempfile("bioszen_grouped_download_tech_outlier_filt_", fileext = ".xlsx")
  on.exit(unlink(out_path), add = TRUE)

  expect_no_error({
    wb <- generate_summary_wb(datos, "uMax")
    wb <- generate_summary_wb(
      datos = renumber_replicates_for_export(filtered_by_param$uMax),
      params = "uMax",
      wb = wb,
      sheet_suffix = "_filt"
    )
    openxlsx::saveWorkbook(wb, out_path, overwrite = TRUE)
  })

  expect_grouped_workbook_sheets(
    out_path,
    required = "uMax",
    expected_filtered = "uMax_filt"
  )

  raw_detail <- read_strain_detail_block(out_path, "uMax", "LysoR-")
  filt_detail <- read_strain_detail_block(out_path, "uMax_filt", "LysoR-")
  expect_true(any(as.character(raw_detail$RepBiol) == "1" & as.character(raw_detail$RepTec) == "T4"))
  expect_false(any(as.character(filt_detail$RepBiol) == "1" & as.character(filt_detail$RepTec) == "T4"))
  expect_true(any(as.character(filt_detail$RepBiol) == "2" & as.character(filt_detail$RepTec) == "T4"))

  raw_summary <- read_biological_summary_block(out_path, "uMax")
  filt_summary <- read_biological_summary_block(out_path, "uMax_filt")
  value_col <- setdiff(names(raw_summary), c("Strain", "RepBiol"))[[1]]
  raw_rep1 <- as.numeric(raw_summary[[value_col]][as.character(raw_summary$RepBiol) == "1"])
  filt_rep1 <- as.numeric(filt_summary[[value_col]][as.character(filt_summary$RepBiol) == "1"])
  expect_length(raw_rep1, 1L)
  expect_length(filt_rep1, 1L)
  expect_false(isTRUE(all.equal(raw_rep1, filt_rep1)))
  expect_equal(filt_rep1, mean(c(10.0, 10.1, 10.2)), tolerance = 1e-10)
})

test_that("large platemap automatic biological outliers create filt tabs and full selection restores data", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("readxl")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")

  fixture_path <- file.path(root, "tests", "testthat", "fixtures", "platemap_BigData_test.xlsx")
  fixture_path <- normalizePath(fixture_path, winslash = "/", mustWork = FALSE)

  old <- setwd(app_dir)
  on.exit(setwd(old), add = TRUE)
  load_app_sources()

  expect_true(file.exists(fixture_path))
  datos <- as.data.frame(readxl::read_excel(fixture_path, sheet = "Datos"), check.names = FALSE)
  cfg <- as.data.frame(readxl::read_excel(fixture_path, sheet = "PlotSettings"), check.names = FALSE)
  params <- intersect(as.character(cfg$Parameter), names(datos))
  expect_gte(nrow(datos), 5000L)
  expect_gte(length(params), 1L)

  datos$Label <- paste(datos$Strain, datos$Media, sep = "-")
  target_group <- unique(datos$Label)[[1]]
  target_idx <- which(datos$Label == target_group)[[1]]
  target_rep <- as.character(datos$BiologicalReplicate[[target_idx]])
  for (param in params) {
    group_values <- suppressWarnings(as.numeric(datos[[param]][datos$Label == target_group]))
    finite_values <- group_values[is.finite(group_values)]
    expect_gte(length(finite_values), 4L)
    span <- diff(range(finite_values))
    if (!is.finite(span) || span <= 0) span <- 1
    datos[[param]][target_idx] <- max(finite_values) + (1000 * span)
  }

  out_reps <- qc_outlier_replicates(
    df = datos,
    params = params,
    group_col = "Label",
    rep_col = "BiologicalReplicate",
    iqr_mult = 1.5
  )
  expect_true(any(
    as.character(out_reps$Group) == target_group &
      as.character(out_reps$Replicate) == target_rep
  ))

  full_map <- lapply(
    split(as.character(datos$BiologicalReplicate), datos$Label),
    function(values) unique(values[!is.na(values) & nzchar(values)])
  )
  selected_map <- full_map
  for (group in names(selected_map)) {
    flagged <- unique(as.character(out_reps$Replicate[as.character(out_reps$Group) == group]))
    selected_map[[group]] <- setdiff(selected_map[[group]], flagged)
  }

  filtered <- build_filtered_param_export_data(
    df = datos,
    params = params,
    reps_group_map = selected_map
  )
  expect_gte(length(filtered), 1L)
  expect_true(any(vapply(filtered, nrow, integer(1)) < nrow(datos)))

  target_param <- names(filtered)[[1]]
  out_path <- tempfile("bioszen_large_outlier_filt_", fileext = ".xlsx")
  on.exit(unlink(out_path), add = TRUE)
  wb <- generate_summary_wb(datos, target_param)
  wb <- generate_summary_wb(
    datos = renumber_replicates_for_export(filtered[[target_param]]),
    params = target_param,
    wb = wb,
    sheet_suffix = "_filt"
  )
  openxlsx::saveWorkbook(wb, out_path, overwrite = TRUE)
  expect_grouped_workbook_sheets(
    out_path,
    required = safe_sheet(target_param),
    expected_filtered = safe_sheet(paste0(target_param, "_filt"))
  )

  restored <- build_filtered_param_export_data(
    df = datos,
    params = params,
    reps_group_map = full_map
  )
  expect_length(restored, 0L)
})

test_that("technical replicate filt sheets use the stored map for each parameter", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("readxl")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")

  old <- setwd(app_dir)
  on.exit(setwd(old), add = TRUE)
  load_app_sources()

  fixture <- build_download_fixture()
  out_path <- tempfile("bioszen_grouped_download_tech_by_param_", fileext = ".xlsx")
  on.exit(unlink(out_path), add = TRUE)

  filtered_by_param <- build_filtered_param_export_data(
    df = fixture$datos,
    params = fixture$params,
    tech_selection_by_param = list(
      uMax = list(
        "S1||M1||R1" = "T1",
        "S1||M2||R1" = "T1"
      ),
      ODmax = list(
        "S2||M1||R3" = "T1",
        "S2||M2||R3" = "T1"
      )
    )
  )

  expect_setequal(names(filtered_by_param), fixture$params)

  expect_no_error({
    wb <- generate_summary_wb(fixture$datos, fixture$params)
    for (param in names(filtered_by_param)) {
      wb <- generate_summary_wb(
        datos = renumber_replicates_for_export(filtered_by_param[[param]]),
        params = param,
        wb = wb,
        sheet_suffix = "_filt"
      )
    }
    openxlsx::saveWorkbook(wb, out_path, overwrite = TRUE)
  })

  expect_grouped_workbook_sheets(
    out_path,
    required = fixture$params,
    expected_filtered = c("uMax_filt", "ODmax_filt")
  )

  umax_s1 <- read_strain_detail_block(out_path, "uMax_filt", "S1")
  umax_s2 <- read_strain_detail_block(out_path, "uMax_filt", "S2")
  odmax_s1 <- read_strain_detail_block(out_path, "ODmax_filt", "S1")
  odmax_s2 <- read_strain_detail_block(out_path, "ODmax_filt", "S2")

  expect_false(any(as.character(umax_s1$RepBiol) == "1" & as.character(umax_s1$RepTec) == "T2"))
  expect_true(any(as.character(umax_s2$RepBiol) == "3" & as.character(umax_s2$RepTec) == "T2"))
  expect_true(any(as.character(odmax_s1$RepBiol) == "1" & as.character(odmax_s1$RepTec) == "T2"))
  expect_false(any(as.character(odmax_s2$RepBiol) == "3" & as.character(odmax_s2$RepTec) == "T2"))
})

test_that("download workbook ignores blank uploaded headers", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("readxl")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")

  old <- setwd(app_dir)
  on.exit(setwd(old), add = TRUE)
  load_app_sources()

  fixture <- build_download_fixture()
  fixture$datos$blank_header <- "unused"
  fixture$datos$na_header <- "unused"
  names(fixture$datos)[ncol(fixture$datos) - 1L] <- ""
  names(fixture$datos)[ncol(fixture$datos)] <- NA_character_

  cfg <- data.frame(
    Parameter = fixture$params,
    Y_Max = c(1, 1),
    Interval = c(0.2, 0.2),
    Y_Title = fixture$params,
    stringsAsFactors = FALSE
  )
  prep <- prepare_platemap(fixture$datos, cfg)

  expect_false(anyNA(names(prep$datos)))
  expect_true(all(nzchar(names(prep$datos))))
  expect_false(any(grepl("^Column", names(prep$datos))))
  expect_setequal(prep$cfg$Parameter, fixture$params)

  out_path <- tempfile("bioszen_grouped_download_blank_headers_", fileext = ".xlsx")
  on.exit(unlink(out_path), add = TRUE)

  expect_no_error({
    wb <- generate_summary_wb(prep$datos, fixture$params)
    openxlsx::saveWorkbook(wb, out_path, overwrite = TRUE)
  })
  expect_true(file.exists(out_path))
  expect_gt(file.info(out_path)$size, 0)
  expect_grouped_workbook_sheets(out_path, required = fixture$params)
})
