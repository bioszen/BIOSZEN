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
  structure(
    list(message = message),
    class = c("bioszen_growth_cancelled", "error", "condition")
  )
}

.bioszen_abort_if_requested <- function(should_abort = NULL) {
  if (!is.function(should_abort)) return(invisible(FALSE))
  abort_now <- tryCatch(isTRUE(should_abort()), error = function(e) FALSE)
  if (abort_now) stop(.bioszen_cancel_condition())
  invisible(FALSE)
}

.bioszen_identify_exponential_phase_robust <- function(
    df,
    time_col,
    measure_col,
    umax_lower_bound = 0.05,
    umax_upper_bound = 0.25,
    max_iterations = 10,
    initial_r_squared_threshold = 0.95,
    should_abort = NULL) {
  best_model <- NULL
  best_r2 <- -Inf
  best_start <- best_end <- NULL

  min_pts <- 10
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
          suppressWarnings(stats::lm(
            log(df[[measure_col]][start:end]) ~ df[[time_col]][start:end]
          )),
          error = function(e) NULL
        )
        if (is.null(model)) next

        r2 <- suppressWarnings(summary(model)$r.squared)
        umax <- stats::coef(model)[2]

        if (!is.na(r2) &&
            umax > umax_lower_bound &&
            umax < umax_upper_bound &&
            r2 > r2_threshold &&
            r2 > best_r2) {
          best_r2 <- r2
          best_model <- model
          best_start <- start
          best_end <- end
        }
      }
    }

    if (!is.null(best_model)) {
      umax <- stats::coef(best_model)[2]
      if (umax < umax_lower_bound) {
        min_pts <- max(min_pts - 1, 5)
        umax_lower_bound <- umax_lower_bound - 0.01
        r2_threshold <- max(r2_threshold - 0.01, 0.90)
      } else if (umax > umax_upper_bound) {
        min_pts <- min(min_pts + 1, nrow(df) - 5)
        umax_upper_bound <- umax_upper_bound + 0.01
        r2_threshold <- min(r2_threshold + 0.01, 0.99)
      } else {
        break
      }
    }
  }

  list(start = best_start, end = best_end, model = best_model)
}

.bioszen_identify_exponential_phase_permissive <- function(
    df,
    time_col,
    measure_col,
    umax_lower_bound = 0.01,
    umax_upper_bound = 0.50,
    max_iterations = 10,
    should_abort = NULL) {
  best_model <- NULL
  best_r2 <- -Inf
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
          suppressWarnings(stats::lm(
            log(df[[measure_col]][start:end]) ~ df[[time_col]][start:end]
          )),
          error = function(e) NULL
        )
        if (is.null(model)) next
        r2 <- suppressWarnings(summary(model)$r.squared)
        if (!is.na(r2) && r2 > best_r2) {
          best_r2 <- r2
          best_model <- model
          best_start <- start
          best_end <- end
        }
      }
    }
    break
  }

  list(start = best_start, end = best_end, model = best_model)
}

.bioszen_growth_initial_od <- function(df) {
  if (is.null(df) || !nrow(df) || !"Measurements" %in% names(df)) {
    return(NA_real_)
  }
  measurements <- suppressWarnings(as.numeric(df$Measurements))
  if ("Time" %in% names(df)) {
    time <- suppressWarnings(as.numeric(df$Time))
    ord <- order(time, seq_along(time), na.last = TRUE)
    measurements <- measurements[ord]
  }
  measurements <- measurements[is.finite(measurements)]
  if (!length(measurements)) NA_real_ else measurements[[1]]
}

.bioszen_calculate_growth_rates_robust <- function(
    df,
    should_abort = NULL,
    progress_callback = NULL) {
  .bioszen_abort_if_requested(should_abort)
  well_order <- unique(df$Well)

  do_one <- function(w) {
    .bioszen_abort_if_requested(should_abort)
    d <- df[df$Well == w, , drop = FALSE]
    phase <- .bioszen_identify_exponential_phase_robust(
      d,
      time_col = "Time",
      measure_col = "Measurements",
      should_abort = should_abort
    )
    model <- phase$model
    start <- phase$start
    end <- phase$end

    if (!is.null(model)) {
      lag_time <- d$Time[which.max(d$Time[1:start])]
      dplyr::tibble(
        Well = d$Well[1],
        uMax = stats::coef(model)[2],
        max_percap_time = mean(d$Time[start:end]),
        doub_time = log(2) / stats::coef(model)[2],
        lag_time = lag_time,
        ODmax = max(d$Measurements),
        max_time = d$Time[which.max(d$Measurements)],
        AUC = gcplyr::auc(x = d$Time, y = d$Measurements),
        OD0 = .bioszen_growth_initial_od(d)
      )
    } else {
      dplyr::tibble(
        Well = d$Well[1],
        uMax = NA_real_,
        max_percap_time = NA_real_,
        doub_time = NA_real_,
        lag_time = NA_real_,
        ODmax = max(d$Measurements),
        max_time = NA_real_,
        AUC = NA_real_,
        OD0 = .bioszen_growth_initial_od(d)
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
  names(out)[names(out) == "uMax"] <- "\u00B5Max"
  out$Well <- factor(out$Well, levels = well_order)
  out[order(out$Well), , drop = FALSE]
}

.bioszen_calculate_growth_rates_permissive <- function(
    df,
    should_abort = NULL,
    progress_callback = NULL) {
  .bioszen_abort_if_requested(should_abort)
  well_order <- unique(df$Well)

  do_one <- function(w) {
    .bioszen_abort_if_requested(should_abort)
    d <- df[df$Well == w, , drop = FALSE]
    phase <- .bioszen_identify_exponential_phase_permissive(
      d,
      time_col = "Time",
      measure_col = "Measurements",
      should_abort = should_abort
    )
    model <- phase$model
    start <- phase$start
    end <- phase$end

    if (!is.null(model)) {
      lag_time <- d$Time[which.max(d$Time[1:start])]
      dplyr::tibble(
        Well = d$Well[1],
        uMax = stats::coef(model)[2],
        max_percap_time = mean(d$Time[start:end]),
        doub_time = log(2) / stats::coef(model)[2],
        lag_time = lag_time,
        ODmax = max(d$Measurements),
        max_time = d$Time[which.max(d$Measurements)],
        AUC = gcplyr::auc(x = d$Time, y = d$Measurements),
        OD0 = .bioszen_growth_initial_od(d)
      )
    } else {
      dplyr::tibble(
        Well = d$Well[1],
        uMax = NA_real_,
        max_percap_time = NA_real_,
        doub_time = NA_real_,
        lag_time = NA_real_,
        ODmax = max(d$Measurements),
        max_time = NA_real_,
        AUC = NA_real_,
        OD0 = .bioszen_growth_initial_od(d)
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
  names(out)[names(out) == "uMax"] <- "\u00B5Max"
  out$Well <- factor(out$Well, levels = well_order)
  out[order(out$Well), , drop = FALSE]
}

.bioszen_is_empty_growth_value <- function(x) {
  is.na(x) | x == "" | is.nan(x)
}

.bioszen_combine_growth_results <- function(robust_df, permissive_df) {
  if (is.null(robust_df) || !nrow(robust_df)) return(permissive_df)
  if (is.null(permissive_df) || !nrow(permissive_df)) return(robust_df)

  out <- robust_df
  common <- intersect(names(permissive_df), names(robust_df))
  for (col in setdiff(common, "Well")) {
    missing <- .bioszen_is_empty_growth_value(out[[col]])
    out[[col]][missing] <- permissive_df[[col]][missing]
  }
  out[setdiff(common, "Well")] <- lapply(out[setdiff(common, "Well")], unname)

  if ("Well" %in% common) {
    lvl <- levels(out$Well)
    if (is.null(lvl) || !length(lvl)) lvl <- levels(permissive_df$Well)
    if (is.null(lvl) || !length(lvl)) lvl <- unique(as.character(permissive_df$Well))
    if (!is.null(lvl) && length(lvl)) {
      out$Well <- factor(out$Well, levels = lvl)
      out <- out[order(out$Well), , drop = FALSE]
    }
  }
  out
}

.bioszen_growth_result_columns <- c(
  "Well", "\u00B5Max", "ODmax", "AUC", "lag_time",
  "max_percap_time", "doub_time", "max_time", "OD0"
)

.bioszen_empty_growth_results <- function() {
  out <- as.data.frame(stats::setNames(
    replicate(length(.bioszen_growth_result_columns), logical(0), simplify = FALSE),
    .bioszen_growth_result_columns
  ))
  out$Well <- character(0)
  for (col in setdiff(.bioszen_growth_result_columns, "Well")) out[[col]] <- numeric(0)
  out
}

.bioszen_growth_od0_lookup <- function(tidy_df) {
  if (is.null(tidy_df) || !is.data.frame(tidy_df) ||
      !all(c("Well", "Time", "Measurements") %in% names(tidy_df))) {
    return(stats::setNames(numeric(0), character(0)))
  }
  split_df <- split(tidy_df, as.character(tidy_df$Well), drop = TRUE)
  vapply(split_df, function(d) {
    measurements <- suppressWarnings(as.numeric(d$Measurements))
    time <- suppressWarnings(as.numeric(d$Time))
    ord <- order(time, seq_along(time), na.last = TRUE)
    measurements <- measurements[ord]
    measurements <- measurements[is.finite(measurements)]
    if (!length(measurements)) NA_real_ else measurements[[1]]
  }, numeric(1), USE.NAMES = TRUE)
}

.bioszen_fill_restored_od0 <- function(results, tidy_df) {
  if (is.null(results) || !is.data.frame(results) || !nrow(results)) return(results)
  if (!"OD0" %in% names(results)) results$OD0 <- NA_real_
  lookup <- .bioszen_growth_od0_lookup(tidy_df)
  if (!length(lookup)) return(results)
  idx <- match(as.character(results$Well), names(lookup))
  needs <- is.na(results$OD0) & !is.na(idx)
  results$OD0[needs] <- unname(lookup[idx[needs]])
  results
}

.bioszen_compute_growth_results_batch_core <- function(
    tidy_df,
    should_abort = NULL,
    progress_callback = NULL) {
  .bioszen_abort_if_requested(should_abort)
  wells <- unique(tidy_df$Well)
  tidy_df$Well <- factor(tidy_df$Well, levels = wells)
  tidy_df$Time <- as.numeric(tidy_df$Time)

  robust <- .bioszen_calculate_growth_rates_robust(
    tidy_df,
    should_abort = should_abort,
    progress_callback = function(done, total, well) {
      if (is.function(progress_callback)) {
        progress_callback(stage = "robust", done = done, total = total, well = well)
      }
    }
  )
  .bioszen_abort_if_requested(should_abort)

  fill_cols <- setdiff(names(robust), "Well")
  permissive <- robust
  for (col in fill_cols) permissive[[col]] <- rep(NA_real_, nrow(permissive))

  need_permissive <- rep(FALSE, nrow(robust))
  for (col in fill_cols) {
    need_permissive <- need_permissive | .bioszen_is_empty_growth_value(robust[[col]])
  }

  if (any(need_permissive)) {
    wells_needed <- as.character(robust$Well[need_permissive])
    subset_df <- tidy_df[as.character(tidy_df$Well) %in% wells_needed, , drop = FALSE]
    subset_df$Well <- factor(subset_df$Well, levels = wells)
    permissive_subset <- .bioszen_calculate_growth_rates_permissive(
      subset_df,
      should_abort = should_abort,
      progress_callback = function(done, total, well) {
        if (is.function(progress_callback)) {
          progress_callback(stage = "permissive", done = done, total = total, well = well)
        }
      }
    )
    idx <- match(as.character(permissive$Well), as.character(permissive_subset$Well))
    matched <- !is.na(idx)
    for (col in fill_cols) {
      permissive[[col]][matched] <- permissive_subset[[col]][idx[matched]]
    }
    if (is.function(progress_callback)) {
      progress_callback(
        stage = "permissive_done",
        done = sum(need_permissive),
        total = sum(need_permissive),
        well = NA_character_
      )
    }
  } else if (is.function(progress_callback)) {
    progress_callback(
      stage = "permissive_skipped",
      done = 0L,
      total = 0L,
      well = NA_character_
    )
  }

  .bioszen_abort_if_requested(should_abort)
  out <- .bioszen_combine_growth_results(robust, permissive)
  out$Well <- factor(out$Well, levels = wells)
  out <- out[order(out$Well), , drop = FALSE]
  out[, .bioszen_growth_result_columns, drop = FALSE]
}
