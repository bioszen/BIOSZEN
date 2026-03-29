.bioszen_map_wells <- function(keys, fn, should_abort = NULL, progress_callback = NULL) {
  .bioszen_abort_if_requested(should_abort)
  out <- vector("list", length(keys))
  n_keys <- length(keys)
  for (i in seq_along(keys)) {
    .bioszen_abort_if_requested(should_abort)
    out[[i]] <- fn(keys[[i]])
    if (is.function(progress_callback)) {
      try(progress_callback(done = i, total = n_keys, well = keys[[i]]), silent = TRUE)
    }
  }
  .bioszen_abort_if_requested(should_abort)
  out
}

.bioszen_cancel_condition <- function(message = "Growth processing cancelled.") {
  structure(list(message = message), class = c("bioszen_growth_cancelled", "error", "condition"))
}

.bioszen_abort_if_requested <- function(should_abort = NULL) {
  if (!is.function(should_abort)) return(invisible(FALSE))
  abort_now <- tryCatch(isTRUE(should_abort()), error = function(e) FALSE)
  if (abort_now) stop(.bioszen_cancel_condition())
  invisible(FALSE)
}

# ============================================================
# 1) Fase exponencial - ROBUSTA (sin cambios de lógica)
# ============================================================
identify_exponential_phase_robust <- function(df, time_col, measure_col,
                                              umax_lower_bound = 0.05,
                                              umax_upper_bound = 0.25,
                                              max_iterations = 10,
                                              initial_r_squared_threshold = 0.95,
                                              should_abort = NULL) {
  
  best_model <- NULL
  best_r2    <- -Inf
  best_start <- best_end <- NULL
  
  min_pts      <- 10
  r2_threshold <- initial_r_squared_threshold
  df <- df[!is.na(df[[time_col]]) & !is.na(df[[measure_col]]), ]
  if (nrow(df) < (min_pts + 1)) {
    return(list(start = best_start, end = best_end, model = best_model))
  }
  q05 <- suppressWarnings(stats::quantile(df[[measure_col]], 0.05, na.rm = TRUE))
  q95 <- suppressWarnings(stats::quantile(df[[measure_col]], 0.95, na.rm = TRUE))
  if (!is.finite(q05) || !is.finite(q95)) {
    return(list(start = best_start, end = best_end, model = best_model))
  }
  df <- dplyr::filter(
    df,
    dplyr::between(df[[measure_col]], q05, q95)
  )
  if (nrow(df) < (min_pts + 1)) {
    return(list(start = best_start, end = best_end, model = best_model))
  }
  
  for (i in seq_len(max_iterations)) {
    .bioszen_abort_if_requested(should_abort)
    for (start in seq_len(nrow(df) - min_pts)) {
      .bioszen_abort_if_requested(should_abort)
      for (end in seq(start + min_pts, nrow(df))) {
        .bioszen_abort_if_requested(should_abort)
        if ((end - start + 1) < min_pts) next
        model <- tryCatch(
          suppressWarnings(lm(log(df[[measure_col]][start:end]) ~ df[[time_col]][start:end])),
          error = function(e) NULL
        )
        if (is.null(model)) next
        
        r2   <- suppressWarnings(summary(model)$r.squared)
        umax <- coef(model)[2]
        
        if (!is.na(r2) &&
            umax > umax_lower_bound &&
            umax < umax_upper_bound &&
            r2   > r2_threshold &&
            r2   > best_r2) {
          best_r2    <- r2
          best_model <- model
          best_start <- start
          best_end   <- end
        }
      }
    }
    
    if (!is.null(best_model)) {
      umax <- coef(best_model)[2]
      if (umax < umax_lower_bound) {
        min_pts          <- max(min_pts - 1, 5)
        umax_lower_bound <- umax_lower_bound - 0.01
        r2_threshold     <- max(r2_threshold - 0.01, 0.90)
      } else if (umax > umax_upper_bound) {
        min_pts          <- min(min_pts + 1, nrow(df) - 5)
        umax_upper_bound <- umax_upper_bound + 0.01
        r2_threshold     <- min(r2_threshold + 0.01, 0.99)
      } else {
        break
      }
    }
  }
  
  list(start = best_start, end = best_end, model = best_model)
}

# ============================================================
# 2) Fase exponencial - PERMISIVA (sin cambios de lógica)
# ============================================================
identify_exponential_phase_permissive <- function(df, time_col, measure_col,
                                                  umax_lower_bound = 0.01,
                                                  umax_upper_bound = 0.50,
                                                  max_iterations   = 10,
                                                  should_abort = NULL) {
  
  best_model <- NULL
  best_r2    <- -Inf
  best_start <- best_end <- NULL
  
  min_pts <- 10
  df <- df[!is.na(df[[time_col]]) & !is.na(df[[measure_col]]), ]
  if (nrow(df) < (min_pts + 1)) {
    return(list(start = best_start, end = best_end, model = best_model))
  }
  q05 <- suppressWarnings(stats::quantile(df[[measure_col]], 0.05, na.rm = TRUE))
  q95 <- suppressWarnings(stats::quantile(df[[measure_col]], 0.95, na.rm = TRUE))
  if (!is.finite(q05) || !is.finite(q95)) {
    return(list(start = best_start, end = best_end, model = best_model))
  }
  df <- dplyr::filter(
    df,
    dplyr::between(df[[measure_col]], q05, q95)
  )
  if (nrow(df) < (min_pts + 1)) {
    return(list(start = best_start, end = best_end, model = best_model))
  }
  for (i in seq_len(max_iterations)) {
    .bioszen_abort_if_requested(should_abort)
    for (start in seq_len(nrow(df) - min_pts)) {
      .bioszen_abort_if_requested(should_abort)
      for (end in seq(start + min_pts, nrow(df))) {
        .bioszen_abort_if_requested(should_abort)
        if ((end - start + 1) < min_pts) next
        model <- tryCatch(
          suppressWarnings(lm(log(df[[measure_col]][start:end]) ~ df[[time_col]][start:end])),
          error = function(e) NULL
        )
        if (is.null(model)) next
        r2 <- suppressWarnings(summary(model)$r.squared)
        if (!is.na(r2) && r2 > best_r2) {
          best_r2    <- r2
          best_model <- model
          best_start <- start
          best_end   <- end
        }
      }
    }
    break
  }
  
  list(start = best_start, end = best_end, model = best_model)
}

# ============================================================
# 3) Calcular parámetros – ROBUSTO (paralelo + orden estable)
# ============================================================
calculate_growth_rates_robust <- function(df, should_abort = NULL, progress_callback = NULL) {
  .bioszen_abort_if_requested(should_abort)
  # claves en el MISMO orden que aparecen en el df
  well_order <- unique(df$Well)
  
  do_one <- function(w) {
    .bioszen_abort_if_requested(should_abort)
    d <- df[df$Well == w, , drop = FALSE]
    phase <- identify_exponential_phase_robust(
      d, time_col = "Time", measure_col = "Measurements", should_abort = should_abort
    )
    model <- phase$model
    start <- phase$start
    end   <- phase$end
    
    if (!is.null(model)) {
      lag_time <- d$Time[which.max(d$Time[1:start])]
      dplyr::tibble(
        Well            = d$Well[1],
        µMax            = coef(model)[2],
        max_percap_time = mean(d$Time[start:end]),
        doub_time       = log(2) / coef(model)[2],
        lag_time        = lag_time,
        ODmax           = max(d$Measurements),
        max_time        = d$Time[which.max(d$Measurements)],
        AUC             = gcplyr::auc(x = d$Time, y = d$Measurements)
      )
    } else {
      dplyr::tibble(
        Well            = d$Well[1],
        µMax            = NA_real_,
        max_percap_time = NA_real_,
        doub_time       = NA_real_,
        lag_time        = NA_real_,
        ODmax           = max(d$Measurements),
        max_time        = NA_real_,
        AUC             = NA_real_
      )
    }
  }
  
  res_list <- .bioszen_map_wells(
    well_order,
    do_one,
    should_abort = should_abort,
    progress_callback = progress_callback
  )
  .bioszen_abort_if_requested(should_abort)
  out <- dplyr::bind_rows(res_list)
  
  # asegurar MISMO orden que en df de entrada
  out$Well <- factor(out$Well, levels = well_order)
  out <- dplyr::arrange(out, Well)
  out
}

# ============================================================
# 4) Calcular parámetros – PERMISIVO (paralelo + orden estable)
# ============================================================
calculate_growth_rates_permissive <- function(df, should_abort = NULL, progress_callback = NULL) {
  .bioszen_abort_if_requested(should_abort)
  well_order <- unique(df$Well)
  
  do_one <- function(w) {
    .bioszen_abort_if_requested(should_abort)
    d <- df[df$Well == w, , drop = FALSE]
    phase <- identify_exponential_phase_permissive(
      d, time_col = "Time", measure_col = "Measurements", should_abort = should_abort
    )
    model <- phase$model
    start <- phase$start
    end   <- phase$end
    
    if (!is.null(model)) {
      lag_time <- d$Time[which.max(d$Time[1:start])]
      dplyr::tibble(
        Well            = d$Well[1],
        µMax            = coef(model)[2],
        max_percap_time = mean(d$Time[start:end]),
        doub_time       = log(2) / coef(model)[2],
        lag_time        = lag_time,
        ODmax           = max(d$Measurements),
        max_time        = d$Time[which.max(d$Measurements)],
        AUC             = gcplyr::auc(x = d$Time, y = d$Measurements)
      )
    } else {
      dplyr::tibble(
        Well            = d$Well[1],
        µMax            = NA_real_,
        max_percap_time = NA_real_,
        doub_time       = NA_real_,
        lag_time        = NA_real_,
        ODmax           = max(d$Measurements),
        max_time        = NA_real_,
        AUC             = NA_real_
      )
    }
  }
  
  res_list <- .bioszen_map_wells(
    well_order,
    do_one,
    should_abort = should_abort,
    progress_callback = progress_callback
  )
  .bioszen_abort_if_requested(should_abort)
  out <- dplyr::bind_rows(res_list)
  
  # asegurar MISMO orden que en df de entrada
  out$Well <- factor(out$Well, levels = well_order)
  out <- dplyr::arrange(out, Well)
  out
}

# ============================================================
# 5) Combinar resultados robustos y permisivos rellenando huecos
# ============================================================
combine_growth_results <- function(robust_df, permissive_df) {
  if (is.null(robust_df) || !nrow(robust_df)) return(permissive_df)
  if (is.null(permissive_df) || !nrow(permissive_df)) return(robust_df)

  out <- robust_df
  common <- intersect(names(permissive_df), names(robust_df))

  for (col in setdiff(common, "Well")) {
    missing <- is_empty_value(out[[col]])
    out[[col]][missing] <- permissive_df[[col]][missing]
  }
  out[setdiff(common, "Well")] <- lapply(out[setdiff(common, "Well")], unname)

  if ("Well" %in% common) {
    lvl <- levels(out$Well)
    if (is.null(lvl) || !length(lvl)) lvl <- levels(permissive_df$Well)
    if (is.null(lvl) || !length(lvl)) lvl <- unique(as.character(permissive_df$Well))
    if (!is.null(lvl) && length(lvl)) {
      out$Well <- factor(out$Well, levels = lvl)
      out <- dplyr::arrange(out, Well)
    }
  }

  out
}

# ============================================================
# 6) Detectar valores vacíos (sin cambios)
# ============================================================
is_empty_value <- function(x) {
  is.na(x) | x == "" | is.nan(x)
}
