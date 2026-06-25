# Canonical shared helper source ------------------------------------------------
# Keep statistical/file helper implementations here and avoid redefining them
# in global.R to prevent silent overrides during app bootstrap.

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

# Combina diferencias punto-a-punto de curvas con Fisher, manteniendo
# p-values ultra-pequeños en lugar de descartarlos por underflow.
curve_pointwise_fisher <- function(d1, d2) {
  joined <- d1 %>%
    dplyr::select(Time, Avg_1 = Avg, SD_1 = SD, N_1 = N) %>%
    dplyr::inner_join(
      d2 %>% dplyr::select(Time, Avg_2 = Avg, SD_2 = SD, N_2 = N),
      by = "Time"
    ) %>%
    dplyr::mutate(
      var_1 = (SD_1^2) / pmax(N_1, 1),
      var_2 = (SD_2^2) / pmax(N_2, 1),
      se = sqrt(pmax(var_1 + var_2, 0)),
      diff = Avg_1 - Avg_2,
      z_i = ifelse(is.finite(se) & se > 0, diff / se, NA_real_),
      p_i = ifelse(is.finite(z_i), 2 * stats::pnorm(-abs(z_i)), NA_real_),
      p_i = ifelse(is.finite(p_i), pmax(pmin(p_i, 1), .Machine$double.xmin), NA_real_)
    ) %>%
    dplyr::filter(is.finite(diff), is.finite(p_i), p_i <= 1)

  n_points <- nrow(joined)
  if (n_points < 2) {
    return(list(estimate = NA_real_, p_value = NA_real_, n_points = n_points))
  }

  p_i <- pmax(joined$p_i, .Machine$double.xmin)
  fisher_stat <- -2 * sum(log(p_i))
  p_fisher <- stats::pchisq(fisher_stat, df = 2 * length(p_i), lower.tail = FALSE)
  w <- ifelse(is.finite(joined$se) & joined$se > 0, 1 / (joined$se^2), NA_real_)
  est <- if (all(!is.finite(w))) {
    mean(joined$diff, na.rm = TRUE)
  } else {
    weighted.mean(joined$diff, w = w, na.rm = TRUE)
  }

  list(estimate = as.numeric(est), p_value = as.numeric(p_fisher), n_points = n_points)
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

metadata_data_keys <- function() {
  c(
    "scope", "strain", "param",
    "doNorm", "ctrlMedium",
    "stackParams", "orderStack",
    "corr_param_x", "corr_param_y",
    "corr_adv_anchor",
    "corrm_params", "heat_params",
    "curve_stats_methods"
  )
}

metadata_filter_design_only <- function(meta_tbl) {
  if (is.null(meta_tbl) || !is.data.frame(meta_tbl) || !all(c("Campo", "Valor") %in% names(meta_tbl))) {
    return(meta_tbl)
  }
  keep <- !as.character(meta_tbl$Campo) %in% metadata_data_keys()
  meta_tbl[keep, , drop = FALSE]
}

metadata_text_allowed_styles <- function(allowed = NULL) {
  if (is.null(allowed)) {
    allowed <- if (exists("bioszen_plot_text_styles", mode = "function")) {
      bioszen_plot_text_styles()
    } else {
      c("bold", "italic", "underline")
    }
  }
  unique(tolower(as.character(allowed)))
}

metadata_text_style_value <- function(styles, allowed = NULL) {
  vals <- tolower(trimws(as.character(styles %||% character(0))))
  vals <- vals[!is.na(vals) & nzchar(vals)]
  vals <- intersect(vals, metadata_text_allowed_styles(allowed))
  if (!length(vals)) return("normal")
  paste(vals, collapse = ",")
}

metadata_parse_text_style_value <- function(value, allowed = NULL) {
  vals <- as.character(value %||% "")
  if (!length(vals) || is.na(vals[[1]]) || !nzchar(trimws(vals[[1]]))) {
    return(character(0))
  }
  vals <- tolower(trimws(unlist(strsplit(vals[[1]], "[,;]", perl = TRUE), use.names = FALSE)))
  vals <- vals[!is.na(vals) & nzchar(vals)]
  vals <- vals[!vals %in% c("normal", "plain", "regular", "none")]
  intersect(vals, metadata_text_allowed_styles(allowed))
}

metadata_text_target_label <- function(target) {
  labels <- c(
    title = "Plot title",
    composition_title = "Composition title",
    plot_titles = "Plot titles",
    axis_titles = "Axis titles",
    axis_text = "Axis numbers",
    legend = "Legend",
    data_labels = "Data labels",
    significance = "Significance text"
  )
  target <- as.character(target %||% "")
  out <- unname(labels[target])
  if (!length(out) || is.na(out) || !nzchar(out)) target else out
}

metadata_text_style_sheet_rows <- function(style_tbl, font_field, style_prefix) {
  empty <- data.frame(Campo = character(0), Valor = character(0), stringsAsFactors = FALSE)
  if (is.null(style_tbl) || !is.data.frame(style_tbl) || !nrow(style_tbl)) return(empty)

  col_value <- function(names) {
    idx <- match(tolower(names), tolower(colnames(style_tbl)))
    idx <- idx[!is.na(idx)]
    if (!length(idx)) return(rep(NA_character_, nrow(style_tbl)))
    as.character(style_tbl[[idx[[1]]]])
  }

  rows <- empty
  append_row <- function(campo, valor) {
    campo <- as.character(campo %||% "")
    valor <- as.character(valor %||% "")
    if (!length(campo) || is.na(campo[[1]]) || !nzchar(trimws(campo[[1]]))) return(invisible(NULL))
    valor <- if (!length(valor) || is.na(valor[[1]])) "" else valor[[1]]
    rows <<- rbind(
      rows,
      data.frame(Campo = campo[[1]], Valor = valor, stringsAsFactors = FALSE)
    )
    invisible(NULL)
  }

  font <- col_value(c("FontFamily", "Fuente", "Font"))
  font <- font[!is.na(font) & nzchar(trimws(font))]
  if (length(font)) append_row(font_field, font[[1]])

  input_ids <- col_value(c("InputId", "Campo", "StyleInputId"))
  styles <- col_value(c("Style", "Estilo"))
  size_ids <- col_value(c("SizeInputId", "TamanoInputId", "SizeField"))
  sizes <- col_value(c("Size", "Tamano", "Tamaño"))

  for (i in seq_len(nrow(style_tbl))) {
    input_id <- input_ids[[i]]
    if (!is.na(input_id) && nzchar(trimws(input_id)) && startsWith(input_id, style_prefix)) {
      append_row(input_id, metadata_text_style_value(metadata_parse_text_style_value(styles[[i]])))
    }

    size_id <- size_ids[[i]]
    size <- sizes[[i]]
    if (
      !is.na(size_id) && nzchar(trimws(size_id)) &&
      !is.na(size) && nzchar(trimws(size))
    ) {
      append_row(size_id, size)
    }
  }

  rows[!duplicated(rows$Campo, fromLast = TRUE), , drop = FALSE]
}

metadata_merge_rows <- function(meta_tbl, extra_tbl) {
  if (is.null(extra_tbl) || !is.data.frame(extra_tbl) || !nrow(extra_tbl)) return(meta_tbl)
  if (is.null(meta_tbl) || !is.data.frame(meta_tbl) || !all(c("Campo", "Valor") %in% names(meta_tbl))) {
    return(extra_tbl)
  }
  meta_tbl <- meta_tbl[!as.character(meta_tbl$Campo) %in% as.character(extra_tbl$Campo), , drop = FALSE]
  rbind(meta_tbl, extra_tbl)
}

sanitize <- function(x, fallback = "") {
  if (is.null(x) || !length(x)) return(fallback)
  out <- as.character(x)
  out[is.na(out)] <- ""
  out <- gsub("[/\\\\:*?\"<>|]", "_", out)
  out <- trimws(out)
  out <- gsub("_+", "_", out)
  out <- gsub("^_+|_+$", "", out)
  empty <- !nzchar(out)
  if (any(empty)) out[empty] <- fallback
  out
}

# Limit axis intervals to avoid generating excessive tick marks that can
# freeze plotting when users enter very small break values.
axis_interval_limited <- function(max_value,
                                  interval,
                                  max_ticks = 40L,
                                  fallback_ticks = 6L,
                                  min_step = 1e-9) {
  pick_first_num <- function(x, default = NA_real_) {
    vals <- suppressWarnings(as.numeric(x))
    vals <- vals[is.finite(vals)]
    if (!length(vals)) default else vals[[1]]
  }

  max_val <- pick_first_num(max_value, default = NA_real_)
  if (!is.finite(max_val) || max_val <= 0) {
    max_val <- 1
  }

  step <- pick_first_num(interval, default = NA_real_)
  if (!is.finite(step) || step <= 0) {
    step <- max_val / max(2, as.numeric(fallback_ticks))
  }

  max_ticks <- suppressWarnings(as.integer(max_ticks))
  if (!is.finite(max_ticks) || max_ticks < 5L) max_ticks <- 40L
  fallback_ticks <- suppressWarnings(as.integer(fallback_ticks))
  if (!is.finite(fallback_ticks) || fallback_ticks < 2L) fallback_ticks <- 6L
  min_step <- suppressWarnings(as.numeric(min_step))
  if (!is.finite(min_step) || min_step <= 0) min_step <- 1e-9

  min_allowed <- max(max_val / max_ticks, min_step)
  adjusted <- FALSE
  if (!is.finite(step) || step < min_allowed) {
    step <- min_allowed
    adjusted <- TRUE
  }

  tick_count <- floor(max_val / step) + 1L
  if (!is.finite(tick_count) || tick_count > (max_ticks + 1L)) {
    step <- max(max_val / max_ticks, min_step)
    tick_count <- floor(max_val / step) + 1L
    adjusted <- TRUE
  }

  list(
    max = max_val,
    step = step,
    tick_count = tick_count,
    adjusted = adjusted
  )
}

axis_breaks_limited <- function(max_value,
                                interval,
                                max_ticks = 40L,
                                fallback_ticks = 6L,
                                min_step = 1e-9) {
  lim <- axis_interval_limited(
    max_value = max_value,
    interval = interval,
    max_ticks = max_ticks,
    fallback_ticks = fallback_ticks,
    min_step = min_step
  )
  out <- seq(0, lim$max, by = lim$step)
  if (!length(out)) out <- c(0, lim$max)
  if (tail(out, 1) < lim$max) out <- c(out, lim$max)
  unique(round(out, 10))
}

axis_interval_limited_range <- function(min_value,
                                        max_value,
                                        interval,
                                        max_ticks = 40L,
                                        fallback_ticks = 6L,
                                        min_step = 1e-9) {
  pick_first_num <- function(x, default = NA_real_) {
    vals <- suppressWarnings(as.numeric(x))
    vals <- vals[is.finite(vals)]
    if (!length(vals)) default else vals[[1]]
  }

  lo <- pick_first_num(min_value, default = 0)
  hi <- pick_first_num(max_value, default = 1)
  if (!is.finite(lo)) lo <- 0
  if (!is.finite(hi)) hi <- lo + 1
  if (hi <= lo) {
    hi <- lo + 1
  }

  span <- hi - lo
  lim <- axis_interval_limited(
    max_value = span,
    interval = interval,
    max_ticks = max_ticks,
    fallback_ticks = fallback_ticks,
    min_step = min_step
  )
  lim$min <- lo
  lim$max <- hi
  lim$span <- span
  lim
}

axis_breaks_limited_range <- function(min_value,
                                      max_value,
                                      interval,
                                      max_ticks = 40L,
                                      fallback_ticks = 6L,
                                      min_step = 1e-9) {
  lim <- axis_interval_limited_range(
    min_value = min_value,
    max_value = max_value,
    interval = interval,
    max_ticks = max_ticks,
    fallback_ticks = fallback_ticks,
    min_step = min_step
  )
  out <- seq(lim$min, lim$max, by = lim$step)
  if (!length(out)) out <- c(lim$min, lim$max)
  if (tail(out, 1) < lim$max) out <- c(out, lim$max)
  unique(round(out, 10))
}

# Ensure stacked-plot summaries never propagate NaN/Inf into plot layers.
sanitize_stack_summary <- function(df, mean_col = "Mean", sd_col = "SD") {
  if (is.null(df) || !is.data.frame(df) || !nrow(df)) return(df)

  if (mean_col %in% names(df)) {
    mean_vec <- suppressWarnings(as.numeric(df[[mean_col]]))
    mean_vec[!is.finite(mean_vec)] <- 0
    df[[mean_col]] <- mean_vec
  }

  if (sd_col %in% names(df)) {
    sd_vec <- suppressWarnings(as.numeric(df[[sd_col]]))
    sd_vec[!is.finite(sd_vec) | sd_vec < 0] <- 0
    df[[sd_col]] <- sd_vec
  }

  df
}

default_errorbar_stat_for_plot <- function(plot_type = NULL, allow_minmax = TRUE) {
  type_chr <- if (is.null(plot_type) || !length(plot_type)) "" else toupper(trimws(as.character(plot_type[[1]])))
  if (isTRUE(allow_minmax) && identical(type_chr, "BOXPLOT")) "MINMAX" else "SD"
}

normalize_errorbar_stat <- function(value, default = "SD", allow_minmax = TRUE) {
  allowed <- c("SD", "SEM")
  if (isTRUE(allow_minmax)) allowed <- c(allowed, "MINMAX")
  default_chr <- toupper(trimws(as.character(default)))
  if (!default_chr %in% allowed) default_chr <- "SD"
  if (is.null(value) || !length(value)) return(default_chr)

  value_chr <- toupper(trimws(as.character(value[[1]])))
  if (is.na(value_chr) || !nzchar(value_chr)) return(default_chr)
  if (value_chr %in% c(
    "SEM", "SE", "SE_MEAN", "STANDARD ERROR", "STANDARD ERROR OF MEAN",
    "ERROR ESTANDAR", "ERROR ESTÁNDAR"
  )) {
    return("SEM")
  }
  if (value_chr %in% c(
    "SD", "STD", "STDDEV", "STANDARD DEVIATION",
    "DESVIACION ESTANDAR", "DESVIACIÓN ESTÁNDAR"
  )) {
    return("SD")
  }
  if (isTRUE(allow_minmax) &&
      value_chr %in% c(
        "MINMAX", "MIN-MAX", "MIN_MAX", "MIN/MAX", "RANGE",
        "MINIMUM/MAXIMUM", "WHISKERS", "ORIGINAL",
        "MINIMO/MAXIMO", "MÍNIMO/MÁXIMO", "MINIMO Y MAXIMO", "MÍNIMO Y MÁXIMO"
      )) {
    return("MINMAX")
  }
  default_chr
}

calculate_errorbar_height <- function(values, stat = "SD", sd_values = NULL, n_values = NULL) {
  stat <- normalize_errorbar_stat(stat, allow_minmax = FALSE)
  vals <- suppressWarnings(as.numeric(values))
  vals <- vals[is.finite(vals)]

  sd_vec <- if (is.null(sd_values)) numeric(0) else suppressWarnings(as.numeric(sd_values))
  sd_vec <- sd_vec[is.finite(sd_vec) & sd_vec >= 0]
  sd_val <- if (length(sd_vec)) {
    mean(sd_vec, na.rm = TRUE)
  } else if (length(vals) > 1) {
    stats::sd(vals, na.rm = TRUE)
  } else {
    NA_real_
  }

  if (!is.finite(sd_val) || sd_val < 0) return(NA_real_)
  if (identical(stat, "SD")) return(sd_val)

  n_vec <- if (is.null(n_values)) numeric(0) else suppressWarnings(as.numeric(n_values))
  n_vec <- n_vec[is.finite(n_vec) & n_vec > 0]
  n_val <- if (length(n_vec)) {
    mean(n_vec, na.rm = TRUE)
  } else {
    length(vals)
  }
  if (!is.finite(n_val) || n_val <= 0) return(NA_real_)
  sd_val / sqrt(n_val)
}

# Decide if normalized data should be consumed in reactive pipelines.
# We only enable strict normalization after a non-empty control medium exists.
should_use_normalized_data <- function(do_norm = FALSE, ctrl_medium = NULL) {
  if (!isTRUE(do_norm)) return(FALSE)
  if (is.null(ctrl_medium) || !length(ctrl_medium)) return(FALSE)
  ctrl_chr <- trimws(as.character(ctrl_medium[[1]]))
  nzchar(ctrl_chr)
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
  if (!nzchar(ctrl_medium)) {
    out <- df
    for (p in available) out[[paste0(p, "_Norm")]] <- NA_real_
    attr(out, "norm_fallback") <- TRUE
    attr(out, "norm_mode") <- "unavailable"
    attr(out, "norm_raw_fallback_params") <- character(0)
    attr(out, "norm_partial_omitted") <- FALSE
    return(out)
  }

  media_chr <- trimws(as.character(df$Media))
  is_ctrl_row <- !is.na(media_chr) & nzchar(media_chr) &
    tolower(media_chr) == tolower(ctrl_medium)
  has_ctrl <- any(is_ctrl_row, na.rm = TRUE)

  out <- df
  if (!has_ctrl) {
    for (p in available) out[[paste0(p, "_Norm")]] <- NA_real_
    attr(out, "norm_fallback") <- TRUE
    attr(out, "norm_mode") <- "unavailable"
    attr(out, "norm_raw_fallback_params") <- character(0)
    attr(out, "norm_partial_omitted") <- FALSE
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
  strain_chr[is.na(strain_chr) | !nzchar(strain_chr)] <- "__NA_STRAIN__"
  rep_chr[is.na(rep_chr) | !nzchar(rep_chr)] <- "__NA_REP__"
  rep_key <- paste(strain_chr, rep_chr, sep = "||")

  fallback_flag <- FALSE
  partial_omit_flag <- FALSE
  raw_fallback_params <- character(0)

  for (p in available) {
    vec <- suppressWarnings(as.numeric(df[[p]]))
    ctrl_valid <- is_ctrl_row & is.finite(vec) & vec != 0

    rep_base <- if (any(ctrl_valid)) {
      tapply(vec[ctrl_valid], rep_key[ctrl_valid], function(z) {
        z <- z[is.finite(z) & z != 0]
        if (!length(z)) return(NA_real_)
        mean(z, na.rm = TRUE)
      })
    } else numeric(0)
    if (length(rep_base)) {
      rep_base <- rep_base[is.finite(rep_base) & rep_base != 0]
    }

    base_vec <- rep(NA_real_, length(vec))

    if (length(rep_base)) {
      idx <- which(rep_key %in% names(rep_base))
      if (length(idx)) base_vec[idx] <- as.numeric(rep_base[rep_key[idx]])
    }

    norm_vec <- rep(NA_real_, length(vec))
    can_norm <- is.finite(vec) & is.finite(base_vec) & base_vec != 0
    norm_vec[can_norm] <- vec[can_norm] / base_vec[can_norm]

    invalid_finite <- is.finite(vec) & !can_norm
    if (any(invalid_finite, na.rm = TRUE)) {
      fallback_flag <- TRUE
      if (any(is.finite(norm_vec), na.rm = TRUE)) {
        partial_omit_flag <- TRUE
      }
    }

    out[[paste0(p, "_Norm")]] <- norm_vec

    sd_col <- paste0("SD_", p)
    if (sd_col %in% names(df)) {
      sd_vec <- suppressWarnings(as.numeric(df[[sd_col]]))
      sd_norm <- rep(NA_real_, length(sd_vec))
      can_norm_sd <- is.finite(sd_vec) & sd_vec >= 0 &
        is.finite(base_vec) & base_vec != 0
      sd_norm[can_norm_sd] <- sd_vec[can_norm_sd] / abs(base_vec[can_norm_sd])
      out[[paste0(sd_col, "_Norm")]] <- sd_norm
    }

    n_col <- paste0("N_", p)
    if (n_col %in% names(df)) {
      out[[paste0(n_col, "_Norm")]] <- suppressWarnings(as.numeric(df[[n_col]]))
    }
  }

  raw_fallback_params <- unique(raw_fallback_params)
  fallback_mode <- if (!isTRUE(fallback_flag)) {
    "none"
  } else if (isTRUE(partial_omit_flag)) {
    "partial_skip"
  } else {
    "unavailable"
  }

  attr(out, "norm_fallback") <- isTRUE(fallback_flag)
  attr(out, "norm_mode") <- fallback_mode
  attr(out, "norm_raw_fallback_params") <- raw_fallback_params
  attr(out, "norm_partial_omitted") <- isTRUE(partial_omit_flag)
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
qc_long_values <- function(df, params, group_col, rep_col = "BiologicalReplicate", subgroup_col = NULL) {
  subgroup_col <- if (is.null(subgroup_col) || !length(subgroup_col)) {
    NULL
  } else {
    as.character(subgroup_col[[1]])
  }
  has_subgroup <- !is.null(subgroup_col) && nzchar(subgroup_col)
  empty <- if (has_subgroup) {
    tibble::tibble(
      Group = character(),
      Subgroup = character(),
      Replicate = character(),
      Parameter = character(),
      Value = numeric()
    )
  } else {
    tibble::tibble(
      Group = character(),
      Replicate = character(),
      Parameter = character(),
      Value = numeric()
    )
  }

  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(empty)
  if (!is.character(params)) params <- as.character(params)
  params <- intersect(params[!is.na(params) & nzchar(params)], names(df))
  if (!length(params)) return(empty)
  if (!group_col %in% names(df) || !rep_col %in% names(df)) return(empty)
  if (has_subgroup && !subgroup_col %in% names(df)) return(empty)

  out <- if (has_subgroup) {
    df %>%
      dplyr::transmute(
        Group = as.character(.data[[group_col]]),
        Subgroup = as.character(.data[[subgroup_col]]),
        Replicate = as.character(.data[[rep_col]]),
        dplyr::across(dplyr::all_of(params))
      ) %>%
      dplyr::filter(
        !is.na(Group), nzchar(Group),
        !is.na(Subgroup), nzchar(Subgroup),
        !is.na(Replicate), nzchar(Replicate)
      )
  } else {
    df %>%
      dplyr::transmute(
        Group = as.character(.data[[group_col]]),
        Replicate = as.character(.data[[rep_col]]),
        dplyr::across(dplyr::all_of(params))
      ) %>%
      dplyr::filter(
        !is.na(Group), nzchar(Group),
        !is.na(Replicate), nzchar(Replicate)
      )
  }

  out <- out %>%
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

qc_detect_iqr_outliers <- function(df, params, group_col, rep_col = "BiologicalReplicate", subgroup_col = NULL, iqr_mult = 1.5) {
  long <- qc_long_values(df, params, group_col, rep_col, subgroup_col = subgroup_col)
  has_subgroup <- "Subgroup" %in% names(long) || (!is.null(subgroup_col) && length(subgroup_col) && nzchar(as.character(subgroup_col[[1]])))
  empty <- if (has_subgroup) {
    tibble::tibble(
      Group = character(),
      Subgroup = character(),
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
  } else {
    tibble::tibble(
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
  }
  if (!nrow(long)) return(empty)

  k <- suppressWarnings(as.numeric(iqr_mult)[1])
  if (!is.finite(k) || k <= 0) k <- 1.5
  group_vars <- c("Group", if ("Subgroup" %in% names(long)) "Subgroup", "Parameter")

  long %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
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
  has_subgroup <- "Subgroup" %in% names(outlier_long)
  empty <- if (has_subgroup) {
    tibble::tibble(
      Parameter = character(),
      Group = character(),
      Subgroup = character(),
      Outliers = integer(),
      MinOutlier = numeric(),
      MaxOutlier = numeric()
    )
  } else {
    tibble::tibble(
      Parameter = character(),
      Group = character(),
      Outliers = integer(),
      MinOutlier = numeric(),
      MaxOutlier = numeric()
    )
  }
  if (is.null(outlier_long) || !is.data.frame(outlier_long) || !nrow(outlier_long)) return(empty)
  if (!"is_outlier" %in% names(outlier_long)) return(empty)
  group_vars <- c("Parameter", "Group", if (has_subgroup) "Subgroup")

  outlier_long %>%
    dplyr::filter(is_outlier) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
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

qc_outlier_replicates <- function(df, params, group_col, rep_col = "BiologicalReplicate", subgroup_col = NULL, iqr_mult = 1.5) {
  out <- qc_detect_iqr_outliers(
    df = df,
    params = params,
    group_col = group_col,
    rep_col = rep_col,
    subgroup_col = subgroup_col,
    iqr_mult = iqr_mult
  )
  has_subgroup <- "Subgroup" %in% names(out) || (!is.null(subgroup_col) && length(subgroup_col) && nzchar(as.character(subgroup_col[[1]])))
  empty <- if (has_subgroup) {
    tibble::tibble(Group = character(), Subgroup = character(), Replicate = character())
  } else {
    tibble::tibble(Group = character(), Replicate = character())
  }
  if (!nrow(out)) return(empty)

  key_cols <- c("Group", if ("Subgroup" %in% names(out)) "Subgroup", "Replicate")
  out %>%
    dplyr::filter(is_outlier) %>%
    dplyr::select(dplyr::all_of(key_cols)) %>%
    dplyr::distinct() %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(key_cols)))
}

qc_rank_replicates <- function(df, params, group_col, rep_col = "BiologicalReplicate", subgroup_col = NULL) {
  long <- qc_long_values(df, params, group_col, rep_col, subgroup_col = subgroup_col)
  has_subgroup <- "Subgroup" %in% names(long) || (!is.null(subgroup_col) && length(subgroup_col) && nzchar(as.character(subgroup_col[[1]])))
  empty <- if (has_subgroup) {
    tibble::tibble(Group = character(), Subgroup = character(), Replicate = character(), Score = numeric())
  } else {
    tibble::tibble(Group = character(), Replicate = character(), Score = numeric())
  }
  if (!nrow(long)) return(empty)

  rep_group_cols <- c("Group", if ("Subgroup" %in% names(long)) "Subgroup", "Replicate", "Parameter")
  center_group_cols <- c("Group", if ("Subgroup" %in% names(long)) "Subgroup", "Parameter")
  score_group_cols <- c("Group", if ("Subgroup" %in% names(long)) "Subgroup", "Replicate")

  rep_param <- long %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(rep_group_cols))) %>%
    dplyr::summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop")

  centers <- rep_param %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(center_group_cols))) %>%
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
    dplyr::select(dplyr::all_of(center_group_cols), center, spread)

  scored <- rep_param %>%
    dplyr::left_join(centers, by = center_group_cols) %>%
    dplyr::mutate(component = abs(Value - center) / spread) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(score_group_cols))) %>%
    dplyr::summarise(
      Score = mean(component[is.finite(component)], na.rm = TRUE),
      ValidComponents = sum(is.finite(component)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      Score = dplyr::if_else(is.nan(Score), Inf, Score)
    ) %>%
    dplyr::filter(is.finite(Score), ValidComponents > 0) %>%
    dplyr::select(dplyr::all_of(score_group_cols), Score)

  if (!nrow(scored)) return(empty)
  scored
}

qc_pick_top_replicates <- function(score_tbl, keep_n) {
  has_subgroup <- "Subgroup" %in% names(score_tbl)
  empty <- if (has_subgroup) {
    tibble::tibble(Group = character(), Subgroup = character(), Replicate = character(), Score = numeric(), Rank = integer())
  } else {
    tibble::tibble(Group = character(), Replicate = character(), Score = numeric(), Rank = integer())
  }
  if (is.null(score_tbl) || !is.data.frame(score_tbl) || !nrow(score_tbl)) return(empty)
  required <- c("Group", "Replicate", "Score")
  if (!all(required %in% names(score_tbl))) return(empty)

  keep_n <- suppressWarnings(as.integer(keep_n)[1])
  if (!is.finite(keep_n) || keep_n <= 0) return(empty)
  group_cols <- c("Group", if ("Subgroup" %in% names(score_tbl)) "Subgroup")
  out_cols <- c(group_cols, "Replicate", "Score", "Rank")
  score_tbl <- score_tbl %>%
    dplyr::mutate(
      Group = as.character(Group),
      Replicate = as.character(Replicate)
    )
  if (has_subgroup) {
    score_tbl <- score_tbl %>% dplyr::mutate(Subgroup = as.character(Subgroup))
  }

  score_tbl %>%
    dplyr::filter(
      !is.na(Group), nzchar(Group),
      !is.na(Replicate), nzchar(Replicate),
      is.finite(Score)
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::arrange(Score, Replicate, .by_group = TRUE) %>%
    dplyr::mutate(Rank = dplyr::row_number()) %>%
    dplyr::filter(Rank <= keep_n) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(out_cols))
}

normalize_replicate_selection <- function(values) {
  vals <- as.character(values)
  vals <- vals[!is.na(vals) & nzchar(vals)]
  vals <- unique(vals)
  if (!length(vals)) return(character(0))

  num_direct <- suppressWarnings(as.numeric(vals))
  num_embedded <- suppressWarnings(as.numeric(gsub("[^0-9+\\-\\.]", "", vals)))
  num_key <- ifelse(is.finite(num_direct), num_direct, num_embedded)
  has_num <- is.finite(num_key)

  ord <- order(!has_num, num_key, vals, na.last = TRUE)
  vals[ord]
}

qc_build_tech_selection_key <- function(group, biorep, strain = NULL, media = NULL) {
  clean_one <- function(value) {
    if (is.null(value) || !length(value)) return("")
    out <- as.character(value[[1]])
    if (is.na(out)) "" else out
  }

  biorep_chr <- clean_one(biorep)
  if (!nzchar(biorep_chr)) return("")

  strain_chr <- clean_one(strain)
  media_chr <- clean_one(media)
  if (nzchar(strain_chr) && nzchar(media_chr)) {
    return(paste(strain_chr, media_chr, biorep_chr, sep = "||"))
  }

  group_chr <- clean_one(group)
  if (!nzchar(group_chr)) return("")
  paste(group_chr, biorep_chr, sep = "||")
}

qc_filter_by_technical_selection <- function(
  df,
  tech_map,
  group_col = NULL,
  biorep_col = "BiologicalReplicate",
  tech_col = "TechnicalReplicate",
  strain_col = "Strain",
  media_col = "Media"
) {
  if (is.null(df) || !is.data.frame(df) || !nrow(df)) return(df)
  if (!is.list(tech_map) || !length(tech_map)) return(df)
  if (!biorep_col %in% names(df) || !tech_col %in% names(df)) return(df)

  has_group <- !is.null(group_col) && length(group_col) && group_col %in% names(df)
  has_strain_media <- strain_col %in% names(df) && media_col %in% names(df)

  biorep_chr <- as.character(df[[biorep_col]])
  tech_chr <- as.character(df[[tech_col]])
  strain_chr <- if (has_strain_media) as.character(df[[strain_col]]) else rep("", nrow(df))
  media_chr <- if (has_strain_media) as.character(df[[media_col]]) else rep("", nrow(df))
  strain_chr[is.na(strain_chr)] <- ""
  media_chr[is.na(media_chr)] <- ""
  group_chr <- if (has_group) as.character(df[[group_col]]) else rep("", nrow(df))
  group_chr[is.na(group_chr)] <- ""
  derived_group_chr <- if (has_strain_media) paste(strain_chr, media_chr, sep = "-") else rep("", nrow(df))
  group_key_chr <- group_chr
  missing_group <- is.na(group_key_chr) | !nzchar(group_key_chr)
  group_key_chr[missing_group] <- derived_group_chr[missing_group]

  valid_key <- !is.na(biorep_chr) & nzchar(biorep_chr)
  if (has_group) {
    valid_key <- valid_key & !is.na(group_key_chr) & nzchar(group_key_chr)
  }
  if (!any(valid_key)) return(df)

  key_vals <- rep("", nrow(df))
  if (has_strain_media) {
    valid_sm <- valid_key &
      nzchar(strain_chr) &
      nzchar(media_chr)
    if (any(valid_sm)) {
      key_vals[valid_sm] <- mapply(
        qc_build_tech_selection_key,
        group = group_key_chr[valid_sm],
        biorep = biorep_chr[valid_sm],
        strain = strain_chr[valid_sm],
        media = media_chr[valid_sm],
        USE.NAMES = FALSE
      )
    }
  }

  missing_idx <- valid_key & !nzchar(key_vals)
  if (any(missing_idx)) {
    key_vals[missing_idx] <- mapply(
      qc_build_tech_selection_key,
      group = group_key_chr[missing_idx],
      biorep = biorep_chr[missing_idx],
      USE.NAMES = FALSE
    )
  }

  keys <- unique(key_vals[valid_key & nzchar(key_vals)])
  if (!length(keys)) return(df)

  keep <- rep(TRUE, nrow(df))
  for (key in keys) {
    sel <- tech_map[[key]]
    if (is.null(sel)) next
    idx <- key_vals == key
    selectable <- idx & !is.na(tech_chr) & nzchar(tech_chr)
    if (!any(selectable)) next
    allowed <- normalize_replicate_selection(intersect(as.character(sel), tech_chr[selectable]))
    keep[selectable] <- tech_chr[selectable] %in% allowed
  }

  df[keep, , drop = FALSE]
}

qc_build_technical_outlier_selection <- function(
  df,
  out_reps,
  group_col,
  current_map = list(),
  biorep_col = "BiologicalReplicate",
  tech_col = "TechnicalReplicate",
  strain_col = "Strain",
  media_col = "Media"
) {
  empty <- list(map = list(), changed = 0L)
  if (is.null(df) || !is.data.frame(df) || !nrow(df)) return(empty)
  if (is.null(out_reps) || !is.data.frame(out_reps) || !nrow(out_reps)) return(empty)
  if (!all(c("Group", "Replicate") %in% names(out_reps))) return(empty)
  if (!all(c(biorep_col, tech_col) %in% names(df))) return(empty)

  has_group <- !is.null(group_col) && length(group_col) && group_col %in% names(df)
  has_strain_media <- strain_col %in% names(df) && media_col %in% names(df)
  if (!has_group && !has_strain_media) return(empty)
  if (!is.list(current_map)) current_map <- list()

  n <- nrow(df)
  strain_chr <- if (has_strain_media) as.character(df[[strain_col]]) else rep("", n)
  media_chr <- if (has_strain_media) as.character(df[[media_col]]) else rep("", n)
  strain_chr[is.na(strain_chr)] <- ""
  media_chr[is.na(media_chr)] <- ""
  group_chr <- if (has_group) as.character(df[[group_col]]) else rep("", n)
  group_chr[is.na(group_chr)] <- ""
  derived_group_chr <- if (has_strain_media) paste(strain_chr, media_chr, sep = "-") else rep("", n)
  group_key_chr <- group_chr
  missing_group <- is.na(group_key_chr) | !nzchar(group_key_chr)
  group_key_chr[missing_group] <- derived_group_chr[missing_group]

  combos <- data.frame(
    Group = group_key_chr,
    Strain = strain_chr,
    Media = media_chr,
    BiologicalReplicate = as.character(df[[biorep_col]]),
    TechnicalReplicate = as.character(df[[tech_col]]),
    stringsAsFactors = FALSE
  )
  combos <- combos[
    !is.na(combos$Group) & nzchar(combos$Group) &
      !is.na(combos$BiologicalReplicate) & nzchar(combos$BiologicalReplicate) &
      !is.na(combos$TechnicalReplicate) & nzchar(combos$TechnicalReplicate),
    ,
    drop = FALSE
  ]
  if (!nrow(combos)) return(empty)
  combos <- unique(combos)

  out_reps <- out_reps %>%
    dplyr::mutate(
      Group = as.character(.data$Group),
      Replicate = as.character(.data$Replicate)
    )
  if ("Subgroup" %in% names(out_reps)) {
    out_reps <- out_reps %>% dplyr::mutate(Subgroup = as.character(.data$Subgroup))
  }

  split_key <- paste(combos$Group, combos$Strain, combos$Media, combos$BiologicalReplicate, sep = "\r")
  selection_map <- list()
  changed <- 0L

  for (split in unique(split_key)) {
    idx <- split_key == split
    group_name <- combos$Group[idx][[1]]
    strain_name <- combos$Strain[idx][[1]]
    media_name <- combos$Media[idx][[1]]
    biorep_name <- combos$BiologicalReplicate[idx][[1]]
    key <- qc_build_tech_selection_key(
      group = group_name,
      biorep = biorep_name,
      strain = strain_name,
      media = media_name
    )
    if (!nzchar(key)) next

    choices <- normalize_replicate_selection(combos$TechnicalReplicate[idx])
    current <- if (!is.null(current_map[[key]])) {
      normalize_replicate_selection(intersect(as.character(current_map[[key]]), choices))
    } else {
      choices
    }
    flagged <- if ("Subgroup" %in% names(out_reps)) {
      out_reps$Replicate[out_reps$Group == group_name & out_reps$Subgroup == biorep_name]
    } else {
      out_reps$Replicate[out_reps$Group == group_name]
    }
    flagged <- normalize_replicate_selection(intersect(as.character(flagged), choices))
    next_sel <- setdiff(current, flagged)
    selection_map[[key]] <- next_sel
    changed <- changed + length(setdiff(current, next_sel))
  }

  list(map = selection_map, changed = as.integer(changed))
}

build_technical_filtered_detail_table <- function(
  df,
  param,
  tech_map,
  value_col = "Valor"
) {
  if (is.null(df) || !is.data.frame(df) || !nrow(df)) return(tibble::tibble())
  if (!is.list(tech_map) || !length(tech_map)) return(tibble::tibble())

  param_chr <- if (is.null(param)) "" else as.character(param)
  param_chr <- param_chr[!is.na(param_chr) & nzchar(param_chr)]
  if (!length(param_chr)) return(tibble::tibble())
  param_chr <- param_chr[[1]]
  if (!param_chr %in% names(df)) return(tibble::tibble())

  required <- c("Strain", "Media", "BiologicalReplicate", "TechnicalReplicate")
  if (!all(required %in% names(df))) return(tibble::tibble())

  filtered <- qc_filter_by_technical_selection(
    df = df,
    tech_map = tech_map,
    group_col = NULL,
    biorep_col = "BiologicalReplicate",
    tech_col = "TechnicalReplicate",
    strain_col = "Strain",
    media_col = "Media"
  )
  if (!is.data.frame(filtered) || nrow(filtered) >= nrow(df)) return(tibble::tibble())

  ord_col <- if ("Orden" %in% names(filtered)) {
    suppressWarnings(as.numeric(filtered$Orden))
  } else {
    rep(NA_real_, nrow(filtered))
  }
  well_col <- if ("Well" %in% names(filtered)) {
    as.character(filtered$Well)
  } else {
    rep(NA_character_, nrow(filtered))
  }

  out <- tibble::tibble(
    Version = "filt",
    Strain = as.character(filtered$Strain),
    Media = as.character(filtered$Media),
    RepBiol = as.character(filtered$BiologicalReplicate),
    RepTec = as.character(filtered$TechnicalReplicate),
    Well = well_col,
    Parameter = param_chr,
    Valor = suppressWarnings(as.numeric(filtered[[param_chr]])),
    .Orden = ord_col
  )

  bio_levels <- normalize_replicate_selection(out$RepBiol)
  tech_levels <- normalize_replicate_selection(out$RepTec)
  out <- out %>%
    dplyr::mutate(
      RepBiol = factor(.data$RepBiol, levels = bio_levels),
      RepTec = factor(.data$RepTec, levels = tech_levels)
    ) %>%
    dplyr::arrange(.data$Strain, .data$.Orden, .data$Media, .data$RepBiol, .data$RepTec, .data$Well) %>%
    dplyr::mutate(
      RepBiol = as.character(.data$RepBiol),
      RepTec = as.character(.data$RepTec)
    ) %>%
    dplyr::select(-dplyr::all_of(".Orden"))

  if (!identical(value_col, "Valor") && nzchar(as.character(value_col))) {
    names(out)[names(out) == "Valor"] <- as.character(value_col)
  }

  out
}

replicate_selection_group_id <- function(strain, media) {
  strain_chr <- if (is.null(strain) || !length(strain)) "" else as.character(strain[[1]])
  media_chr <- if (is.null(media) || !length(media)) "" else as.character(media[[1]])
  if (!nzchar(strain_chr) || !nzchar(media_chr)) return("")
  paste(strain_chr, media_chr, sep = "-")
}

replicate_selection_get_strain_media <- function(reps_strain_map, strain, media) {
  if (!is.list(reps_strain_map)) return(NULL)
  strain_chr <- if (is.null(strain) || !length(strain)) "" else as.character(strain[[1]])
  media_chr <- if (is.null(media) || !length(media)) "" else as.character(media[[1]])
  if (!nzchar(strain_chr) || !nzchar(media_chr)) return(NULL)
  sub_map <- reps_strain_map[[strain_chr]]
  if (!is.list(sub_map)) return(NULL)
  sel <- sub_map[[media_chr]]
  if (is.null(sel)) NULL else normalize_replicate_selection(sel)
}

replicate_selection_set_strain_media <- function(reps_strain_map, strain, media, selected) {
  if (!is.list(reps_strain_map)) reps_strain_map <- list()
  strain_chr <- if (is.null(strain) || !length(strain)) "" else as.character(strain[[1]])
  media_chr <- if (is.null(media) || !length(media)) "" else as.character(media[[1]])
  if (!nzchar(strain_chr) || !nzchar(media_chr)) return(reps_strain_map)
  sub_map <- reps_strain_map[[strain_chr]]
  if (!is.list(sub_map)) sub_map <- list()
  sub_map[[media_chr]] <- normalize_replicate_selection(selected)
  reps_strain_map[[strain_chr]] <- sub_map
  reps_strain_map
}

replicate_selection_get_group <- function(reps_group_map, strain, media) {
  if (!is.list(reps_group_map)) return(NULL)
  gid <- replicate_selection_group_id(strain, media)
  if (!nzchar(gid)) return(NULL)
  sel <- reps_group_map[[gid]]
  if (is.null(sel)) NULL else normalize_replicate_selection(sel)
}

replicate_selection_set_group <- function(reps_group_map, strain, media, selected) {
  if (!is.list(reps_group_map)) reps_group_map <- list()
  gid <- replicate_selection_group_id(strain, media)
  if (!nzchar(gid)) return(reps_group_map)
  reps_group_map[[gid]] <- normalize_replicate_selection(selected)
  reps_group_map
}

replicate_selection_get_synced <- function(reps_strain_map, reps_group_map, strain, media) {
  sel <- replicate_selection_get_strain_media(reps_strain_map, strain, media)
  if (is.null(sel)) sel <- replicate_selection_get_group(reps_group_map, strain, media)
  sel
}

replicate_selection_set_synced <- function(reps_strain_map, reps_group_map, strain, media, selected) {
  list(
    reps_strain_map = replicate_selection_set_strain_media(
      reps_strain_map, strain, media, selected
    ),
    reps_group_map = replicate_selection_set_group(
      reps_group_map, strain, media, selected
    )
  )
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
  active_strain = NULL,
  tech_selection_map = list()
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

  if (length(reps_strain_map)) {
    media_chr <- as.character(df$Media)
    strain_chr <- as.character(df$Strain)
    is_nested_map <- all(vapply(reps_strain_map, is.list, logical(1)))

    if (is_nested_map) {
      for (strain_name in names(reps_strain_map)) {
        strain_submap <- reps_strain_map[[strain_name]]
        if (!is.list(strain_submap) || !length(strain_submap)) next
        for (m in names(strain_submap)) {
          idx <- strain_chr == as.character(strain_name) & media_chr == as.character(m)
          if (!any(idx)) next
          sel <- normalize_replicate_selection(strain_submap[[m]])
          keep[idx] <- keep[idx] & (rep_chr[idx] %in% sel)
        }
      }
    } else if (nzchar(strain_sel)) {
      for (m in names(reps_strain_map)) {
        idx <- strain_chr == strain_sel & media_chr == as.character(m)
        if (!any(idx)) next
        sel <- normalize_replicate_selection(reps_strain_map[[m]])
        keep[idx] <- keep[idx] & (rep_chr[idx] %in% sel)
      }
    }
  }

  out <- df[keep, , drop = FALSE]
  if (is.list(tech_selection_map) && length(tech_selection_map)) {
    out <- qc_filter_by_technical_selection(
      df = out,
      tech_map = tech_selection_map,
      group_col = NULL,
      biorep_col = "BiologicalReplicate",
      tech_col = "TechnicalReplicate",
      strain_col = "Strain",
      media_col = "Media"
    )
  }
  dropped <- as.integer(nrow(df) - nrow(out))
  list(df = out, has_changes = dropped > 0L, dropped_rows = dropped)
}

build_filtered_param_export_data <- function(
  df,
  params,
  reps_strain_map = list(),
  reps_group_map = list(),
  drop_all = character(0),
  active_strain = NULL,
  tech_selection_map = list(),
  tech_selection_by_param = list(),
  active_tech_param = NULL
) {
  out <- list()
  if (is.null(df) || !is.data.frame(df) || !nrow(df)) return(out)
  params <- if (is.null(params)) character(0) else as.character(params)
  params <- intersect(params, names(df))
  params <- params[!is.na(params) & nzchar(params)]
  if (!length(params)) return(out)
  if (!is.list(tech_selection_by_param)) tech_selection_by_param <- list()
  active_tech_param <- as.character(active_tech_param %||% "")
  active_tech_param <- active_tech_param[!is.na(active_tech_param) & nzchar(active_tech_param)]
  active_tech_param <- if (length(active_tech_param)) active_tech_param[[1]] else ""

  for (param in params) {
    param_tech_map <- tech_selection_map
    has_param_store <- !is.null(tech_selection_by_param[[param]]) &&
      is.list(tech_selection_by_param[[param]])
    if (has_param_store && length(tech_selection_by_param[[param]])) {
      param_tech_map <- tech_selection_by_param[[param]]
    } else if (has_param_store && !identical(param, active_tech_param)) {
      param_tech_map <- list()
    }

    filtered <- filter_export_replicates_for_download(
      df = df,
      reps_strain_map = reps_strain_map,
      reps_group_map = reps_group_map,
      drop_all = drop_all,
      active_strain = active_strain,
      tech_selection_map = param_tech_map
    )
    if (!isTRUE(filtered$has_changes) ||
        !is.data.frame(filtered$df) ||
        !nrow(filtered$df)) {
      next
    }

    affected <- detect_filtered_params_for_download(
      raw_df = df,
      filtered_df = filtered$df,
      params = param
    )
    if (length(affected)) {
      out[[param]] <- filtered$df
    }
  }

  out
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
  valid_rep <- !is.na(rep_chr) & nzchar(rep_chr)
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
