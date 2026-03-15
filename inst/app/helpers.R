# Helpers from the original app -------------------------------------------------

# Generic helpers to convert matrices to tidy tibbles
matrix_to_tibble <- function(mat, colname = "p.adj") {
  tibble::as_tibble(mat, rownames = "grupo1") |>
    tidyr::pivot_longer(-grupo1,
                        names_to  = "grupo2",
                        values_to = colname) |>
    dplyr::filter(!is.na(.data[[colname]]))
}

# Helper: convierte matriz de p-values de PMCMRplus a tibble
pmcmr_to_tibble <- function(obj) {
  mat <- obj$p.value
  tibble::as_tibble(mat, rownames = "grupo1") |>
    tidyr::pivot_longer(-grupo1,
                        names_to  = "grupo2",
                        values_to = "p.adj") |>
    dplyr::filter(!is.na(p.adj))
}

# Estadística -----------------------------------------------------------
split_comparison <- function(x) {
  stringr::str_split_fixed(x, "-", 2)
}

dunnett_to_tibble <- function(obj) {
  mat <- obj[[1]][ , 4, drop = FALSE]
  cmp <- split_comparison(rownames(mat))
  tibble::tibble(
    grupo1 = cmp[, 1],
    grupo2 = cmp[, 2],
    p.adj  = mat[, 1]
  )
}

set_control <- function(df, control_lbl) {
  if (!is.null(control_lbl) && control_lbl %in% df$Label)
    df$Label <- forcats::fct_relevel(df$Label, control_lbl)
  df
}

# Filtra grupos que no alcanzan el mínimo de observaciones requeridas
filter_min_obs <- function(df, min_n = 2) {
  df %>%
    dplyr::group_by(Label) %>%
    dplyr::filter(dplyr::n() >= min_n) %>%
    dplyr::ungroup()
}

safe_pairwise_t <- function(df, method = "none") {
  df <- filter_min_obs(df) |> droplevels()
  grupos <- levels(df$Label)
  if (length(grupos) < 2) return(tibble::tibble())
  combinaciones <- utils::combn(grupos, 2, simplify = FALSE)
  resultados <- lapply(combinaciones, function(g) {
    sub <- dplyr::filter(df, Label %in% g)
    tryCatch(
      rstatix::t_test(sub, Valor ~ Label, paired = can_paired(sub)),
      error = function(e) NULL
    )
  })
  resultados <- resultados[!vapply(resultados, is.null, logical(1))]
  if (length(resultados) == 0) return(tibble::tibble())
  dplyr::bind_rows(resultados) %>%
    rstatix::adjust_pvalue(method = method) %>%
    rstatix::add_significance("p.adj")
}

safe_pairwise_wilcox <- function(df, method = "none") {
  df <- filter_min_obs(df) |> droplevels()
  grupos <- levels(df$Label)
  if (length(grupos) < 2) return(tibble::tibble())
  combinaciones <- utils::combn(grupos, 2, simplify = FALSE)
  resultados <- lapply(combinaciones, function(g) {
    sub <- dplyr::filter(df, Label %in% g)
    tryCatch(
      rstatix::wilcox_test(sub, Valor ~ Label, paired = can_paired(sub)),
      error = function(e) NULL
    )
  })
  resultados <- resultados[!vapply(resultados, is.null, logical(1))]
  if (length(resultados) == 0) return(tibble::tibble())
  dplyr::bind_rows(resultados) %>%
    rstatix::adjust_pvalue(method = method) %>%
    rstatix::add_significance("p.adj")
}

# Determina si puede realizarse una prueba pareada (mismo número de observaciones)
can_paired <- function(df) {
  counts <- table(droplevels(df$Label))
  length(counts) == 2 && counts[1] == counts[2]
}

# Utilidades para nombres de archivos ---------------------------------
safe_file <- function(x) {
  ext  <- tools::file_ext(x)
  name <- tools::file_path_sans_ext(x)
  name <- gsub("[^A-Za-z0-9_\\-]", "_", name)
  paste0(name, ".", ext)
}

safe_sheet <- function(x) {
  out <- as.character(if (is.null(x)) "" else x)
  if (!length(out) || is.na(out[1]) || !nzchar(out[1])) out <- "Sheet"
  out <- gsub("[^A-Za-z0-9_]", "_", out[1])
  out <- gsub("_+", "_", out)
  out <- sub("^_+", "", out)
  out <- sub("_+$", "", out)
  if (!nzchar(out)) out <- "Sheet"
  substr(out, 1, 31)
}

sanitize <- function(x) {
  gsub("[/\\\\:*?\"<>|]", "_", x)
}

# Normaliza columnas de parámetros contra un medio control y tolera faltantes.
normalize_params <- function(df, params = character(0), do_norm = FALSE, ctrl_medium = NULL) {
  if (!isTRUE(do_norm)) return(df)
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(df)
  if (is.null(params)) params <- character(0)
  params <- params[!is.na(params) & nzchar(params)]

  available <- intersect(params, names(df))
  if (!length(available) || !"Media" %in% names(df)) return(df)

  ctrl_medium <- if (is.null(ctrl_medium) || !length(ctrl_medium)) {
    ""
  } else {
    trimws(as.character(ctrl_medium[[1]]))
  }
  if (!nzchar(ctrl_medium) || identical(toupper(ctrl_medium), "NA")) {
    out <- df %>%
      dplyr::mutate(dplyr::across(
        dplyr::all_of(available),
        ~ .x,
        .names = "{.col}_Norm"
      ))
    attr(out, "norm_fallback") <- TRUE
    return(out)
  }

  media_chr <- trimws(as.character(df$Media))
  is_ctrl_row <- !is.na(media_chr) & nzchar(media_chr) & media_chr == ctrl_medium
  has_ctrl <- any(is_ctrl_row, na.rm = TRUE)
  fallback_flag <- !has_ctrl

  out <- df
  if (!has_ctrl) {
    for (p in available) out[[paste0(p, "_Norm")]] <- df[[p]]
    attr(out, "norm_fallback") <- TRUE
    return(out)
  }

  strain_chr <- if ("Strain" %in% names(df)) {
    trimws(as.character(df$Strain))
  } else {
    rep("__ALL__", nrow(df))
  }
  rep_chr <- if ("BiologicalReplicate" %in% names(df)) {
    trimws(as.character(df$BiologicalReplicate))
  } else {
    rep("__ALL__", nrow(df))
  }
  rep_key <- paste(strain_chr, rep_chr, sep = "||")

  for (p in available) {
    vec <- suppressWarnings(as.numeric(df[[p]]))
    ctrl_valid <- is_ctrl_row & is.finite(vec) & vec != 0

    rep_base <- if (any(ctrl_valid)) {
      tapply(vec[ctrl_valid], rep_key[ctrl_valid], function(z) z[1])
    } else numeric(0)
    strain_base <- if (any(ctrl_valid)) {
      tapply(vec[ctrl_valid], strain_chr[ctrl_valid], function(z) mean(z, na.rm = TRUE))
    } else numeric(0)
    global_base <- if (any(ctrl_valid)) {
      suppressWarnings(mean(vec[ctrl_valid], na.rm = TRUE))
    } else {
      NA_real_
    }

    base_vec <- rep(NA_real_, length(vec))

    if (length(rep_base)) {
      idx <- which(rep_key %in% names(rep_base))
      if (length(idx)) base_vec[idx] <- as.numeric(rep_base[rep_key[idx]])
    }

    idx_need <- which(!is.finite(base_vec))
    if (length(idx_need) && length(strain_base)) {
      strain_match <- strain_chr[idx_need]
      ok <- strain_match %in% names(strain_base)
      if (any(ok)) {
        base_vec[idx_need[ok]] <- as.numeric(strain_base[strain_match[ok]])
      }
    }

    if (is.finite(global_base) && global_base != 0) {
      base_vec[!is.finite(base_vec)] <- global_base
    }

    norm_vec <- vec
    can_norm <- is.finite(vec) & is.finite(base_vec) & base_vec != 0
    norm_vec[can_norm] <- vec[can_norm] / base_vec[can_norm]
    if (any(!can_norm & is.finite(vec))) fallback_flag <- TRUE

    out[[paste0(p, "_Norm")]] <- norm_vec
  }

  attr(out, "norm_fallback") <- isTRUE(fallback_flag)
  out
}

# Paleta segura -------------------------------------------------------
# Devuelve un vector de colores usando hue_pal() o un vector vacío si n es 0.
safe_hue <- function(n) {
  base <- c(
    "#3D7FD3", "#B26DDC", "#2CA4B8", "#6DC36D",
    "#F2A950", "#E85C66", "#1F7A8C", "#5D9C91"
  )
  if (n <= 0) {
    character(0)
  } else if (n <= length(base)) {
    base[seq_len(n)]
  } else {
    scales::hue_pal()(n)
  }
}

# Data quality helpers --------------------------------------------------------
qc_long_values <- function(df, params, group_col, rep_col = "BiologicalReplicate") {
  empty <- tibble::tibble(
    Group = character(),
    Replicate = character(),
    Parameter = character(),
    Value = numeric()
  )

  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(empty)
  if (!is.character(params)) params <- as.character(params)
  params <- intersect(params[!is.na(params) & nzchar(params)], names(df))
  if (!length(params)) return(empty)
  if (!group_col %in% names(df) || !rep_col %in% names(df)) return(empty)

  out <- df %>%
    dplyr::transmute(
      Group = as.character(.data[[group_col]]),
      Replicate = as.character(.data[[rep_col]]),
      dplyr::across(dplyr::all_of(params))
    ) %>%
    dplyr::filter(
      !is.na(Group), nzchar(Group),
      !is.na(Replicate), nzchar(Replicate),
      toupper(Replicate) != "NA"
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(params),
      names_to = "Parameter",
      values_to = "Value"
    ) %>%
    dplyr::mutate(Value = suppressWarnings(as.numeric(Value))) %>%
    dplyr::filter(is.finite(Value))

  if (!nrow(out)) return(empty)
  out
}

qc_detect_iqr_outliers <- function(df, params, group_col, rep_col = "BiologicalReplicate", iqr_mult = 1.5) {
  long <- qc_long_values(df, params, group_col, rep_col)
  empty <- tibble::tibble(
    Group = character(),
    Replicate = character(),
    Parameter = character(),
    Value = numeric(),
    q1 = numeric(),
    q3 = numeric(),
    iqr = numeric(),
    lower = numeric(),
    upper = numeric(),
    is_outlier = logical()
  )
  if (!nrow(long)) return(empty)

  k <- suppressWarnings(as.numeric(iqr_mult)[1])
  if (!is.finite(k) || k <= 0) k <- 1.5

  long %>%
    dplyr::group_by(Group, Parameter) %>%
    dplyr::mutate(
      q1 = stats::quantile(Value, 0.25, na.rm = TRUE, names = FALSE),
      q3 = stats::quantile(Value, 0.75, na.rm = TRUE, names = FALSE),
      iqr = q3 - q1,
      lower = q1 - k * iqr,
      upper = q3 + k * iqr,
      is_outlier = Value < lower | Value > upper
    ) %>%
    dplyr::ungroup()
}

qc_summarize_outliers <- function(outlier_long) {
  empty <- tibble::tibble(
    Parameter = character(),
    Group = character(),
    Outliers = integer(),
    MinOutlier = numeric(),
    MaxOutlier = numeric()
  )
  if (is.null(outlier_long) || !is.data.frame(outlier_long) || !nrow(outlier_long)) return(empty)
  if (!"is_outlier" %in% names(outlier_long)) return(empty)

  outlier_long %>%
    dplyr::filter(is_outlier) %>%
    dplyr::group_by(Parameter, Group) %>%
    dplyr::summarise(
      Outliers = dplyr::n(),
      MinOutlier = {
        vals <- Value[is.finite(Value)]
        if (length(vals)) min(vals) else NA_real_
      },
      MaxOutlier = {
        vals <- Value[is.finite(Value)]
        if (length(vals)) max(vals) else NA_real_
      },
      .groups = "drop"
    )
}

qc_outlier_replicates <- function(df, params, group_col, rep_col = "BiologicalReplicate", iqr_mult = 1.5) {
  out <- qc_detect_iqr_outliers(
    df = df,
    params = params,
    group_col = group_col,
    rep_col = rep_col,
    iqr_mult = iqr_mult
  )
  empty <- tibble::tibble(Group = character(), Replicate = character())
  if (!nrow(out)) return(empty)

  out %>%
    dplyr::filter(is_outlier) %>%
    dplyr::distinct(Group, Replicate) %>%
    dplyr::arrange(Group, Replicate)
}

qc_rank_replicates <- function(df, params, group_col, rep_col = "BiologicalReplicate") {
  long <- qc_long_values(df, params, group_col, rep_col)
  empty <- tibble::tibble(Group = character(), Replicate = character(), Score = numeric())
  if (!nrow(long)) return(empty)

  rep_param <- long %>%
    dplyr::group_by(Group, Replicate, Parameter) %>%
    dplyr::summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop")

  centers <- rep_param %>%
    dplyr::group_by(Group, Parameter) %>%
    dplyr::summarise(
      center = stats::median(Value, na.rm = TRUE),
      spread_mad = stats::mad(Value, center = center, constant = 1, na.rm = TRUE),
      spread_iqr = stats::IQR(Value, na.rm = TRUE),
      spread_sd = stats::sd(Value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      spread = dplyr::case_when(
        is.finite(spread_mad) & spread_mad > 0 ~ spread_mad,
        is.finite(spread_iqr) & spread_iqr > 0 ~ spread_iqr / 1.349,
        is.finite(spread_sd) & spread_sd > 0 ~ spread_sd,
        TRUE ~ 1
      )
    ) %>%
    dplyr::select(Group, Parameter, center, spread)

  scored <- rep_param %>%
    dplyr::left_join(centers, by = c("Group", "Parameter")) %>%
    dplyr::mutate(component = abs(Value - center) / spread) %>%
    dplyr::group_by(Group, Replicate) %>%
    dplyr::summarise(
      Score = mean(component[is.finite(component)], na.rm = TRUE),
      ValidComponents = sum(is.finite(component)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      Score = dplyr::if_else(is.nan(Score), Inf, Score)
    ) %>%
    dplyr::filter(is.finite(Score), ValidComponents > 0) %>%
    dplyr::select(Group, Replicate, Score)

  if (!nrow(scored)) return(empty)
  scored
}

qc_pick_top_replicates <- function(score_tbl, keep_n) {
  empty <- tibble::tibble(Group = character(), Replicate = character(), Score = numeric(), Rank = integer())
  if (is.null(score_tbl) || !is.data.frame(score_tbl) || !nrow(score_tbl)) return(empty)
  required <- c("Group", "Replicate", "Score")
  if (!all(required %in% names(score_tbl))) return(empty)

  keep_n <- suppressWarnings(as.integer(keep_n)[1])
  if (!is.finite(keep_n) || keep_n <= 0) return(empty)

  score_tbl %>%
    dplyr::mutate(
      Group = as.character(Group),
      Replicate = as.character(Replicate)
    ) %>%
    dplyr::filter(
      !is.na(Group), nzchar(Group),
      !is.na(Replicate), nzchar(Replicate),
      is.finite(Score)
    ) %>%
    dplyr::group_by(Group) %>%
    dplyr::arrange(Score, Replicate, .by_group = TRUE) %>%
    dplyr::mutate(Rank = dplyr::row_number()) %>%
    dplyr::filter(Rank <= keep_n) %>%
    dplyr::ungroup()
}

normalize_replicate_selection <- function(values) {
  vals <- as.character(values)
  vals <- vals[!is.na(vals) & nzchar(vals) & toupper(vals) != "NA"]
  vals <- unique(vals)
  if (!length(vals)) return(character(0))

  num_direct <- suppressWarnings(as.numeric(vals))
  num_embedded <- suppressWarnings(as.numeric(gsub("[^0-9+\\-\\.]", "", vals)))
  num_key <- ifelse(is.finite(num_direct), num_direct, num_embedded)
  has_num <- is.finite(num_key)

  ord <- order(!has_num, num_key, vals, na.last = TRUE)
  vals[ord]
}

sync_replicate_selection_map <- function(
  current_map,
  groups,
  available_map,
  input_map = list(),
  drop_all = character(0)
) {
  if (is.null(groups)) groups <- character(0)
  groups <- as.character(groups)
  groups <- groups[!is.na(groups) & nzchar(groups)]
  if (!length(groups)) {
    return(list(map = if (is.list(current_map)) current_map else list(), changed = FALSE))
  }

  if (!is.list(current_map)) current_map <- list()
  if (!is.list(available_map)) available_map <- list()
  if (!is.list(input_map)) input_map <- list()
  drop_all <- normalize_replicate_selection(drop_all)

  updated_map <- current_map
  changed <- FALSE

  for (g in groups) {
    available <- normalize_replicate_selection(available_map[[g]])
    prev <- normalize_replicate_selection(current_map[[g]])

    if (!length(available)) {
      next_sel <- character(0)
    } else if (!is.null(input_map[[g]])) {
      next_sel <- normalize_replicate_selection(intersect(input_map[[g]], available))
    } else if (!is.null(current_map[[g]])) {
      next_sel <- normalize_replicate_selection(intersect(current_map[[g]], available))
    } else {
      next_sel <- setdiff(available, drop_all)
    }

    next_sel <- setdiff(next_sel, drop_all)
    if (!identical(prev, next_sel)) changed <- TRUE
    updated_map[[g]] <- next_sel
  }

  list(map = updated_map, changed = changed)
}

filter_export_replicates_for_download <- function(
  df,
  reps_strain_map = list(),
  reps_group_map = list(),
  drop_all = character(0),
  active_strain = NULL
) {
  if (is.null(df) || !is.data.frame(df) || !nrow(df)) {
    return(list(df = df, has_changes = FALSE, dropped_rows = 0L))
  }
  required <- c("Strain", "Media", "BiologicalReplicate")
  if (!all(required %in% names(df))) {
    return(list(df = df, has_changes = FALSE, dropped_rows = 0L))
  }

  if (!is.list(reps_strain_map)) reps_strain_map <- list()
  if (!is.list(reps_group_map)) reps_group_map <- list()
  drop_all <- normalize_replicate_selection(drop_all)
  strain_sel <- if (is.null(active_strain) || !length(active_strain)) {
    ""
  } else {
    as.character(active_strain[[1]])
  }

  rep_chr <- as.character(df$BiologicalReplicate)
  grp_chr <- paste(as.character(df$Strain), as.character(df$Media), sep = "-")
  keep <- rep(TRUE, nrow(df))

  if (length(drop_all)) {
    keep <- keep & !(rep_chr %in% drop_all)
  }

  if (length(reps_group_map)) {
    for (g in names(reps_group_map)) {
      idx <- grp_chr == as.character(g)
      if (!any(idx)) next
      sel <- normalize_replicate_selection(reps_group_map[[g]])
      keep[idx] <- keep[idx] & (rep_chr[idx] %in% sel)
    }
  }

  if (nzchar(strain_sel) && length(reps_strain_map)) {
    media_chr <- as.character(df$Media)
    strain_chr <- as.character(df$Strain)
    for (m in names(reps_strain_map)) {
      idx <- strain_chr == strain_sel & media_chr == as.character(m)
      if (!any(idx)) next
      sel <- normalize_replicate_selection(reps_strain_map[[m]])
      keep[idx] <- keep[idx] & (rep_chr[idx] %in% sel)
    }
  }

  out <- df[keep, , drop = FALSE]
  dropped <- as.integer(sum(!keep))
  list(df = out, has_changes = dropped > 0L, dropped_rows = dropped)
}

detect_filtered_params_for_download <- function(raw_df, filtered_df, params) {
  if (is.null(params)) return(character(0))
  params <- as.character(params)
  if (!is.data.frame(raw_df) || !is.data.frame(filtered_df)) return(character(0))
  params <- intersect(params[!is.na(params) & nzchar(params)], names(raw_df))
  params <- intersect(params, names(filtered_df))
  if (!length(params)) return(character(0))

  affected <- vapply(params, function(p) {
    before <- suppressWarnings(as.numeric(raw_df[[p]]))
    after <- suppressWarnings(as.numeric(filtered_df[[p]]))
    sum(is.finite(after)) < sum(is.finite(before))
  }, logical(1))
  params[affected]
}

renumber_replicates_for_export <- function(df, group_cols = c("Strain", "Media"), rep_col = "BiologicalReplicate") {
  if (is.null(df) || !is.data.frame(df) || !nrow(df)) return(df)
  group_cols <- intersect(as.character(group_cols), names(df))
  if (!length(group_cols) || !rep_col %in% names(df)) return(df)

  out <- df
  rep_chr <- as.character(out[[rep_col]])
  valid_rep <- !is.na(rep_chr) & nzchar(rep_chr) & toupper(rep_chr) != "NA"
  if (!any(valid_rep)) return(out)

  key <- do.call(
    paste,
    c(lapply(group_cols, function(col) as.character(out[[col]])), list(sep = "\r"))
  )
  for (k in unique(key[valid_rep])) {
    idx <- which(valid_rep & key == k)
    if (!length(idx)) next
    reps <- unique(rep_chr[idx])
    rep_num <- suppressWarnings(as.numeric(reps))
    reps <- if (all(is.finite(rep_num))) reps[order(rep_num)] else sort(reps)
    mapping <- stats::setNames(as.character(seq_along(reps)), reps)
    out[[rep_col]][idx] <- unname(mapping[rep_chr[idx]])
  }

  out
}

filter_curve_wide_for_export <- function(curve_wide, meta_df) {
  if (is.null(curve_wide) || !is.data.frame(curve_wide) || !"Time" %in% names(curve_wide)) return(curve_wide)
  if (is.null(meta_df) || !is.data.frame(meta_df) || !"Well" %in% names(meta_df)) return(curve_wide)

  wells <- as.character(meta_df$Well)
  wells <- wells[!is.na(wells) & nzchar(wells)]
  keep_cols <- c("Time", intersect(names(curve_wide), wells))
  curve_wide[, keep_cols, drop = FALSE]
}
