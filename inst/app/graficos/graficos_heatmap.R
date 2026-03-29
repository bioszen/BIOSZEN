# Helpers for heatmap plotting, clustering, and cluster export ---------------

heatmap_valid_hclust_method <- function(method) {
  valid <- c("ward.D2", "ward.D", "complete", "average", "single", "mcquitty", "median", "centroid")
  m <- as.character(method %||% "ward.D2")
  if (!length(m) || !m[[1]] %in% valid) return("ward.D2")
  m[[1]]
}

heatmap_scale_mode_normalize <- function(mode) {
  out <- as.character(mode %||% "none")
  if (!length(out) || !out[[1]] %in% c("none", "row", "column")) return("none")
  out[[1]]
}

heatmap_orientation_normalize <- function(orientation) {
  out <- as.character(orientation %||% "params_rows")
  if (!length(out) || !out[[1]] %in% c("params_rows", "params_cols")) return("params_rows")
  out[[1]]
}

heatmap_abort_if_requested <- function(should_abort = NULL) {
  if (!is.function(should_abort)) return(invisible(FALSE))
  abort_now <- tryCatch(isTRUE(should_abort()), error = function(e) FALSE)
  if (isTRUE(abort_now)) {
    stop("Heatmap generation cancelled because the session is closing.", call. = FALSE)
  }
  invisible(FALSE)
}

heatmap_z_scale <- function(v) {
  if (all(!is.finite(v))) return(rep(NA_real_, length(v)))
  s <- stats::sd(v, na.rm = TRUE)
  m <- mean(v, na.rm = TRUE)
  if (!is.finite(s) || s == 0) return(rep(0, length(v)))
  (v - m) / s
}

heatmap_scale_matrix <- function(mat_vals, scale_mode = "none", should_abort = NULL) {
  scale_mode <- heatmap_scale_mode_normalize(scale_mode)
  if (identical(scale_mode, "row")) {
    for (i in seq_len(nrow(mat_vals))) {
      if ((i %% 25L) == 1L) heatmap_abort_if_requested(should_abort)
      mat_vals[i, ] <- heatmap_z_scale(mat_vals[i, ])
    }
  } else if (identical(scale_mode, "column")) {
    for (j in seq_len(ncol(mat_vals))) {
      if ((j %% 25L) == 1L) heatmap_abort_if_requested(should_abort)
      mat_vals[, j] <- heatmap_z_scale(mat_vals[, j])
    }
  }
  mat_vals
}

heatmap_compute_hclust <- function(mat_vals, axis = c("rows", "cols"), method = "ward.D2") {
  axis <- match.arg(axis)
  method <- heatmap_valid_hclust_method(method)
  if (axis == "rows") {
    if (nrow(mat_vals) <= 1) return(NULL)
    corr <- suppressWarnings(stats::cor(t(mat_vals), use = "pairwise.complete.obs"))
  } else {
    if (ncol(mat_vals) <= 1) return(NULL)
    corr <- suppressWarnings(stats::cor(mat_vals, use = "pairwise.complete.obs"))
  }
  dist_mat <- 1 - corr
  dist_mat[!is.finite(dist_mat)] <- 1
  diag(dist_mat) <- 0
  tryCatch(stats::hclust(as.dist(dist_mat), method = method), error = function(e) NULL)
}

heatmap_compute_clusters <- function(hc, k, labels) {
  labels <- as.character(labels %||% character(0))
  if (is.null(hc) || !inherits(hc, "hclust") || !length(labels)) {
    return(list(ids = setNames(rep(NA_integer_, length(labels)), labels), k_used = 0L))
  }
  max_k <- length(hc$order)
  k_num <- suppressWarnings(as.integer(k)[1])
  if (!is.finite(k_num) || k_num < 1) k_num <- 1L
  k_num <- max(1L, min(max_k, k_num))
  ids <- tryCatch(stats::cutree(hc, k = k_num), error = function(e) NULL)
  if (is.null(ids)) {
    return(list(ids = setNames(rep(NA_integer_, length(labels)), labels), k_used = 0L))
  }
  ids <- as.integer(ids[labels])
  list(ids = stats::setNames(ids, labels), k_used = as.integer(dplyr::n_distinct(ids[is.finite(ids)])))
}

heatmap_cluster_boundaries <- function(cluster_ids) {
  ids <- suppressWarnings(as.integer(cluster_ids))
  if (!length(ids) || all(!is.finite(ids))) return(numeric(0))
  ids <- ids[is.finite(ids)]
  if (length(ids) <= 1) return(numeric(0))
  runs <- rle(ids)
  if (length(runs$lengths) <= 1) return(numeric(0))
  cumsum(runs$lengths)[-length(runs$lengths)] + 0.5
}

heatmap_positions_with_cluster_gap <- function(cluster_ids, n_items, gap_size = 0.65) {
  n_items <- suppressWarnings(as.integer(n_items)[1])
  if (!is.finite(n_items) || n_items < 1L) {
    return(list(pos = numeric(0), boundaries = numeric(0), gap_count = 0L))
  }
  gap_size <- suppressWarnings(as.numeric(gap_size)[1])
  if (!is.finite(gap_size) || gap_size < 0) gap_size <- 0

  base_pos <- seq_len(n_items)
  ids <- suppressWarnings(as.integer(cluster_ids))
  if (!length(ids) || length(ids) != n_items || all(!is.finite(ids)) || n_items <= 1L || gap_size <= 0) {
    return(list(pos = base_pos, boundaries = numeric(0), gap_count = 0L))
  }

  run_lengths <- rle(ids)$lengths
  if (length(run_lengths) <= 1L) {
    return(list(pos = base_pos, boundaries = numeric(0), gap_count = 0L))
  }

  run_ends <- cumsum(run_lengths)[-length(run_lengths)]
  if (!length(run_ends)) {
    return(list(pos = base_pos, boundaries = numeric(0), gap_count = 0L))
  }

  offsets <- numeric(n_items)
  for (idx in run_ends) {
    if (idx < n_items) {
      offsets[(idx + 1L):n_items] <- offsets[(idx + 1L):n_items] + gap_size
    }
  }
  pos <- base_pos + offsets
  boundaries <- pos[run_ends] + 0.5 + (gap_size / 2)

  list(
    pos = as.numeric(pos),
    boundaries = as.numeric(boundaries),
    gap_count = as.integer(length(run_ends))
  )
}

heatmap_map_axis_positions <- function(x, leaf_positions) {
  leaf_positions <- suppressWarnings(as.numeric(leaf_positions))
  if (!length(leaf_positions)) return(as.numeric(x))
  x_in <- seq_along(leaf_positions)
  out <- tryCatch(
    stats::approx(
      x = x_in,
      y = leaf_positions,
      xout = as.numeric(x),
      method = "linear",
      rule = 2
    )$y,
    error = function(e) as.numeric(x)
  )
  as.numeric(out)
}

heatmap_cluster_label_prefix <- function(labels, cluster_ids, prefix = "C") {
  labels <- as.character(labels %||% character(0))
  ids <- suppressWarnings(as.integer(cluster_ids))
  if (!length(labels)) return(character(0))
  if (!length(ids) || all(!is.finite(ids))) return(labels)
  ids_chr <- ifelse(is.finite(ids), paste0("[", prefix, ids, "] "), "")
  paste0(ids_chr, labels)
}

heatmap_hclust_segments <- function(hc) {
  if (is.null(hc) || !inherits(hc, "hclust")) return(NULL)
  n_leaves <- length(hc$order)
  if (n_leaves < 2) return(NULL)
  merge <- hc$merge
  heights <- hc$height
  n_merge <- nrow(merge)
  node_x <- numeric(n_merge)
  node_y <- numeric(n_merge)
  leaf_pos <- integer(n_leaves)
  leaf_pos[hc$order] <- seq_len(n_leaves)
  segs <- vector("list", n_merge * 3L)
  k <- 0L

  get_xy <- function(idx) {
    if (idx < 0) return(c(x = leaf_pos[-idx], y = 0))
    c(x = node_x[idx], y = node_y[idx])
  }

  for (i in seq_len(n_merge)) {
    left <- get_xy(merge[i, 1])
    right <- get_xy(merge[i, 2])
    y_top <- heights[i]
    node_x[i] <- mean(c(left["x"], right["x"]))
    node_y[i] <- y_top

    k <- k + 1L
    segs[[k]] <- c(left["x"], left["y"], left["x"], y_top)
    k <- k + 1L
    segs[[k]] <- c(right["x"], right["y"], right["x"], y_top)
    k <- k + 1L
    segs[[k]] <- c(left["x"], y_top, right["x"], y_top)
  }

  out <- as.data.frame(do.call(rbind, segs[seq_len(k)]), stringsAsFactors = FALSE)
  names(out) <- c("x", "y", "xend", "yend")
  out
}

prepare_heatmap_payload <- function(df_h,
                                    scope = "Por Cepa",
                                    label_mode = FALSE,
                                    all_params = character(0),
                                    params_raw = character(0),
                                    high_dim = FALSE,
                                    do_norm = FALSE,
                                    has_ctrl_selected = FALSE,
                                    show_param_labels = TRUE,
                                    orientation = "params_rows",
                                    scale_mode = "none",
                                    cluster_rows = FALSE,
                                    cluster_cols = FALSE,
                                    hclust_method = "ward.D2",
                                    k_rows = 2L,
                                    k_cols = 2L,
                                    should_abort = NULL) {
  heatmap_abort_if_requested(should_abort)
  if (is.null(df_h) || !is.data.frame(df_h) || !nrow(df_h)) {
    stop("No data available for heatmap.", call. = FALSE)
  }

  if (!"Label" %in% names(df_h)) {
    if (identical(scope, "Por Cepa")) {
      df_h <- df_h %>% dplyr::mutate(Label = Media)
    } else {
      df_h <- df_h %>% dplyr::mutate(Label = paste(Strain, Media, sep = "-"))
    }
  }

  group_col <- if (identical(scope, "Por Cepa")) {
    "Media"
  } else if (isTRUE(label_mode)) {
    "Strain"
  } else {
    "Label"
  }
  if (!group_col %in% names(df_h)) {
    stop("No data available for heatmap.", call. = FALSE)
  }

  all_params <- as.character(all_params %||% character(0))
  all_params <- all_params[!is.na(all_params) & nzchar(all_params)]
  params_raw <- as.character(params_raw %||% character(0))
  params_raw <- params_raw[!is.na(params_raw) & nzchar(params_raw)]
  if (!length(params_raw)) {
    params_raw <- all_params
  }
  if (!length(params_raw)) {
    stop("Select at least one parameter.", call. = FALSE)
  }

  resolve_param <- function(p) {
    np <- paste0(p, "_Norm")
    if (isTRUE(do_norm) && isTRUE(has_ctrl_selected) && np %in% names(df_h)) np else p
  }
  params_use <- unique(vapply(params_raw, resolve_param, character(1)))
  params_use <- intersect(params_use, names(df_h))
  if (!length(params_use)) {
    stop("No parameters available for heatmap.", call. = FALSE)
  }

  # Script-style logic for large omics tables:
  # build a parameter x group matrix directly from app-managed data.
  group_vals <- as.character(df_h[[group_col]])
  keep_rows <- !is.na(group_vals) & nzchar(group_vals)
  if (!any(keep_rows)) {
    stop("No data available for heatmap.", call. = FALSE)
  }

  group_vals <- group_vals[keep_rows]
  group_levels <- unique(group_vals)
  value_cols <- lapply(params_use, function(p) suppressWarnings(as.numeric(df_h[[p]][keep_rows])))
  value_mat <- as.matrix(as.data.frame(value_cols, check.names = FALSE, stringsAsFactors = FALSE))
  colnames(value_mat) <- params_use

  mat_vals <- matrix(
    NA_real_,
    nrow = length(params_use),
    ncol = length(group_levels),
    dimnames = list(params_use, group_levels)
  )
  for (j in seq_along(group_levels)) {
    if ((j %% 10L) == 1L) heatmap_abort_if_requested(should_abort)
    idx <- which(group_vals == group_levels[[j]])
    if (!length(idx)) next
    means <- suppressWarnings(colMeans(value_mat[idx, , drop = FALSE], na.rm = TRUE))
    means[!is.finite(means)] <- NA_real_
    mat_vals[, j] <- means
  }

  long_h <- as.data.frame(as.table(mat_vals), stringsAsFactors = FALSE)
  names(long_h) <- c("Parametro", "Group", "Valor")
  long_h$Parametro <- as.character(long_h$Parametro)
  long_h$Group <- as.character(long_h$Group)
  long_h$Valor <- suppressWarnings(as.numeric(long_h$Valor))

  mat_vals[!is.finite(mat_vals)] <- NA_real_
  if (!nrow(mat_vals) || !ncol(mat_vals) || !any(is.finite(mat_vals))) {
    stop("No data available for heatmap.", call. = FALSE)
  }

  scale_mode <- heatmap_scale_mode_normalize(scale_mode)
  mat_vals <- heatmap_scale_matrix(mat_vals, scale_mode = scale_mode, should_abort = should_abort)
  orientation <- heatmap_orientation_normalize(orientation)
  if (identical(orientation, "params_cols")) {
    mat_vals <- t(mat_vals)
  }
  row_entity <- if (identical(orientation, "params_cols")) "group" else "parameter"
  col_entity <- if (identical(orientation, "params_cols")) "parameter" else "group"

  row_order <- rownames(mat_vals)
  col_order <- colnames(mat_vals)
  hclust_method <- heatmap_valid_hclust_method(hclust_method)
  row_hc <- NULL
  col_hc <- NULL

  if (isTRUE(cluster_rows)) {
    heatmap_abort_if_requested(should_abort)
    row_hc <- heatmap_compute_hclust(mat_vals, axis = "rows", method = hclust_method)
    if (!is.null(row_hc) && length(row_hc$order) == nrow(mat_vals)) {
      row_order <- rownames(mat_vals)[row_hc$order]
    }
  }
  if (isTRUE(cluster_cols)) {
    heatmap_abort_if_requested(should_abort)
    col_hc <- heatmap_compute_hclust(mat_vals, axis = "cols", method = hclust_method)
    if (!is.null(col_hc) && length(col_hc$order) == ncol(mat_vals)) {
      col_order <- colnames(mat_vals)[col_hc$order]
    }
  }

  mat_vals <- mat_vals[row_order, col_order, drop = FALSE]

  row_cluster_info <- heatmap_compute_clusters(row_hc, k_rows, labels = row_order)
  col_cluster_info <- heatmap_compute_clusters(col_hc, k_cols, labels = col_order)
  row_cluster_ids_ordered <- as.integer(row_cluster_info$ids[row_order])
  col_cluster_ids_ordered <- as.integer(col_cluster_info$ids[col_order])

  row_levels <- rev(row_order)
  col_levels <- col_order
  row_cluster_ids_display <- rev(row_cluster_ids_ordered)
  col_cluster_ids_display <- col_cluster_ids_ordered

  plot_df <- as.data.frame(as.table(mat_vals), stringsAsFactors = FALSE)
  names(plot_df) <- c("Parametro", "Group", "Valor")
  plot_df$Parametro <- as.character(plot_df$Parametro)
  plot_df$Group <- as.character(plot_df$Group)
  row_gap_info <- heatmap_positions_with_cluster_gap(
    cluster_ids = row_cluster_ids_display,
    n_items = length(row_levels),
    gap_size = 0.65
  )
  col_gap_info <- heatmap_positions_with_cluster_gap(
    cluster_ids = col_cluster_ids_display,
    n_items = length(col_levels),
    gap_size = 0.65
  )
  row_breaks <- as.numeric(row_gap_info$pos %||% numeric(0))
  col_breaks <- as.numeric(col_gap_info$pos %||% numeric(0))
  if (length(row_breaks) != length(row_levels)) row_breaks <- seq_along(row_levels)
  if (length(col_breaks) != length(col_levels)) col_breaks <- seq_along(col_levels)

  plot_df$x <- col_breaks[match(plot_df$Group, col_levels)]
  plot_df$y <- row_breaks[match(plot_df$Parametro, row_levels)]

  can_show_top_dendro <- !is.null(col_hc) && ncol(mat_vals) > 1
  can_show_side_dendro <- !is.null(row_hc) && nrow(mat_vals) > 1
  n_rows <- nrow(mat_vals)
  n_cols <- ncol(mat_vals)

  x_base_min <- 0.5
  x_base_max <- if (length(col_breaks)) max(col_breaks, na.rm = TRUE) + 0.5 else n_cols + 0.5
  y_base_max <- if (length(row_breaks)) max(row_breaks, na.rm = TRUE) + 0.5 else n_rows + 0.5
  top_dend_gap <- 0.35
  side_dend_gap <- 0.06
  top_space <- if (can_show_top_dendro) max(1.2, y_base_max * 0.24) else 0
  side_space <- if (can_show_side_dendro) max(0.25, x_base_max * 0.08) else 0

  top_segs <- NULL
  if (can_show_top_dendro) {
    top_raw <- heatmap_hclust_segments(col_hc)
    if (!is.null(top_raw) && nrow(top_raw) > 0) {
      hmax <- max(c(top_raw$y, top_raw$yend), na.rm = TRUE)
      scale_h <- if (is.finite(hmax) && hmax > 0) top_space / hmax else 0
      y0 <- y_base_max + top_dend_gap
      top_segs <- data.frame(
        x = heatmap_map_axis_positions(top_raw$x, col_breaks),
        y = y0 + top_raw$y * scale_h,
        xend = heatmap_map_axis_positions(top_raw$xend, col_breaks),
        yend = y0 + top_raw$yend * scale_h
      )
    }
  }

  side_segs <- NULL
  if (can_show_side_dendro) {
    side_raw <- heatmap_hclust_segments(row_hc)
    if (!is.null(side_raw) && nrow(side_raw) > 0) {
      hmax <- max(c(side_raw$y, side_raw$yend), na.rm = TRUE)
      scale_h <- if (is.finite(hmax) && hmax > 0) side_space / hmax else 0
      x0 <- x_base_min - side_dend_gap
      row_scale_x <- n_rows - side_raw$x + 1
      row_scale_xend <- n_rows - side_raw$xend + 1
      side_segs <- data.frame(
        x = x0 - side_raw$y * scale_h,
        y = heatmap_map_axis_positions(row_scale_x, row_breaks),
        xend = x0 - side_raw$yend * scale_h,
        yend = heatmap_map_axis_positions(row_scale_xend, row_breaks)
      )
    }
  }

  row_boundaries <- as.numeric(row_gap_info$boundaries %||% numeric(0))
  col_boundaries <- as.numeric(col_gap_info$boundaries %||% numeric(0))

  row_labels <- heatmap_cluster_label_prefix(row_levels, row_cluster_ids_display, prefix = "R")
  col_labels <- heatmap_cluster_label_prefix(col_levels, col_cluster_ids_display, prefix = "C")
  if (!isTRUE(show_param_labels)) {
    if (identical(row_entity, "parameter")) row_labels <- rep("", length(row_labels))
    if (identical(col_entity, "parameter")) col_labels <- rep("", length(col_labels))
  }

  list(
    group_col = group_col,
    params_use = params_use,
    long_h = long_h,
    mat_vals = mat_vals,
    plot_df = plot_df,
    row_order = row_order,
    col_order = col_order,
    row_levels = row_levels,
    col_levels = col_levels,
    row_labels = row_labels,
    col_labels = col_labels,
    row_hc = row_hc,
    col_hc = col_hc,
    top_segs = top_segs,
    side_segs = side_segs,
    can_show_top_dendro = can_show_top_dendro,
    can_show_side_dendro = can_show_side_dendro,
    n_rows = n_rows,
    n_cols = n_cols,
    row_breaks = row_breaks,
    col_breaks = col_breaks,
    x_min = x_base_min,
    x_max = x_base_max,
    y_max = y_base_max,
    x_base_min = x_base_min,
    x_base_max = x_base_max,
    y_base_max = y_base_max,
    top_dend_gap = top_dend_gap,
    side_dend_gap = side_dend_gap,
    top_space = top_space,
    side_space = side_space,
    row_entity = row_entity,
    col_entity = col_entity,
    orientation = orientation,
    scale_mode = scale_mode,
    row_cluster_ids_ordered = row_cluster_ids_ordered,
    col_cluster_ids_ordered = col_cluster_ids_ordered,
    row_cluster_ids_display = row_cluster_ids_display,
    col_cluster_ids_display = col_cluster_ids_display,
    row_cluster_count = as.integer(row_cluster_info$k_used),
    col_cluster_count = as.integer(col_cluster_info$k_used),
    row_boundaries = row_boundaries,
    col_boundaries = col_boundaries
  )
}

write_heatmap_cluster_workbook <- function(payload, file) {
  if (is.null(payload) || !is.list(payload) || is.null(payload$mat_vals)) {
    stop("Heatmap cluster payload is missing.", call. = FALSE)
  }

  mat_vals <- payload$mat_vals
  row_order <- as.character(payload$row_order %||% rownames(mat_vals))
  col_order <- as.character(payload$col_order %||% colnames(mat_vals))
  row_entity <- as.character(payload$row_entity %||% "parameter")
  col_entity <- as.character(payload$col_entity %||% "group")
  row_item_label <- if (identical(row_entity, "group")) "Group" else "Parameter"
  col_item_label <- if (identical(col_entity, "parameter")) "Parameter" else "Group"
  row_cluster <- suppressWarnings(as.integer(payload$row_cluster_ids_ordered))
  col_cluster <- suppressWarnings(as.integer(payload$col_cluster_ids_ordered))
  row_cluster <- if (length(row_cluster) == length(row_order)) row_cluster else rep(NA_integer_, length(row_order))
  col_cluster <- if (length(col_cluster) == length(col_order)) col_cluster else rep(NA_integer_, length(col_order))

  wb <- openxlsx::createWorkbook()

  summary_tbl <- tibble::tibble(
    Metric = c(
      paste0("Rows (", tolower(row_item_label), "s)"),
      paste0("Columns (", tolower(col_item_label), "s)"),
      "Row clusters",
      "Column clusters"
    ),
    Value = c(
      as.character(length(row_order)),
      as.character(length(col_order)),
      as.character(payload$row_cluster_count %||% 0L),
      as.character(payload$col_cluster_count %||% 0L)
    )
  )

  openxlsx::addWorksheet(wb, "Summary")
  openxlsx::writeData(wb, "Summary", summary_tbl)

  matrix_tbl <- data.frame(
    RowItem = row_order,
    as.data.frame(mat_vals[row_order, col_order, drop = FALSE], stringsAsFactors = FALSE, check.names = FALSE),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  names(matrix_tbl)[1] <- row_item_label
  openxlsx::addWorksheet(wb, "HeatmapMatrix")
  openxlsx::writeData(wb, "HeatmapMatrix", matrix_tbl)

  row_tbl <- tibble::tibble(Cluster = row_cluster, RowItem = row_order)
  names(row_tbl)[2] <- row_item_label
  row_tbl <- row_tbl %>%
    dplyr::mutate(
      Cluster = ifelse(is.finite(Cluster), as.integer(Cluster), NA_integer_),
      ClusterLabel = ifelse(is.finite(Cluster), paste0("R", Cluster), "NA")
    ) %>%
    dplyr::arrange(Cluster)

  openxlsx::addWorksheet(wb, "RowClusters")
  openxlsx::writeData(wb, "RowClusters", row_tbl)

  col_tbl <- tibble::tibble(Cluster = col_cluster, ColItem = col_order)
  names(col_tbl)[2] <- col_item_label
  col_tbl <- col_tbl %>%
    dplyr::mutate(
      Cluster = ifelse(is.finite(Cluster), as.integer(Cluster), NA_integer_),
      ClusterLabel = ifelse(is.finite(Cluster), paste0("C", Cluster), "NA")
    ) %>%
    dplyr::arrange(Cluster)

  openxlsx::addWorksheet(wb, "ColumnClusters")
  openxlsx::writeData(wb, "ColumnClusters", col_tbl)

  valid_row_clusters <- sort(unique(row_cluster[is.finite(row_cluster)]))
  if (length(valid_row_clusters)) {
    for (cid in valid_row_clusters) {
      members <- row_order[row_cluster == cid]
      if (!length(members)) next
      sub_tbl <- matrix_tbl[matrix_tbl[[row_item_label]] %in% members, , drop = FALSE]
      sheet <- safe_sheet(paste0("RowCluster_", cid))
      if (sheet %in% openxlsx::sheets(wb)) {
        openxlsx::removeWorksheet(wb, sheet = sheet)
      }
      openxlsx::addWorksheet(wb, sheet)
      openxlsx::writeData(wb, sheet, sub_tbl)
    }
  }

  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
}
