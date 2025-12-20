# ============================================================
# Utilidades de paralelización (sin dependencias duras)
# ============================================================
.bioszen_init_parallel <- function() {
  # Forzar BLAS a 1 hilo por worker (evita ruido numérico y over-subscription)
  Sys.setenv(
    OMP_NUM_THREADS       = "1",
    OPENBLAS_NUM_THREADS  = "1",
    MKL_NUM_THREADS       = "1",
    BLIS_NUM_THREADS      = "1",
    VECLIB_MAXIMUM_THREADS= "1"
  )
  if (requireNamespace("RhpcBLASctl", quietly = TRUE)) {
    try(RhpcBLASctl::blas_set_num_threads(1), silent = TRUE)
    try(RhpcBLASctl::omp_set_num_threads(1),  silent = TRUE)
  }
  
  workers <- suppressWarnings(as.integer(Sys.getenv("BIOSZEN_WORKERS", "0")))
  if (is.na(workers) || workers <= 0) {
    # Usa todos los vCPU disponibles (ajusta si quieres reservar alguno)
    workers <- max(1L, parallel::detectCores(logical = TRUE))
  }
  workers
}

.bioszen_map_wells <- function(keys, fn) {
  # Si hay future.apply, paralelizamos; si no, lapply normal.
  if (requireNamespace("future.apply", quietly = TRUE) &&
      requireNamespace("future", quietly = TRUE)) {
    workers <- .bioszen_init_parallel()
    old_plan <- future::plan()
    on.exit(try(future::plan(old_plan), silent = TRUE), add = TRUE)
    future::plan(future::multisession, workers = workers)
    future.apply::future_lapply(keys, fn, future.seed = TRUE)
  } else {
    lapply(keys, fn)
  }
}

# ============================================================
# 1) Fase exponencial - ROBUSTA (sin cambios de lógica)
# ============================================================
identify_exponential_phase_robust <- function(df, time_col, measure_col,
                                              umax_lower_bound = 0.05,
                                              umax_upper_bound = 0.25,
                                              max_iterations = 10,
                                              initial_r_squared_threshold = 0.95) {
  
  best_model <- NULL
  best_r2    <- -Inf
  best_start <- best_end <- NULL
  
  df <- df[!is.na(df[[time_col]]) & !is.na(df[[measure_col]]), ]
  df <- dplyr::filter(
    df,
    dplyr::between(
      df[[measure_col]],
      stats::quantile(df[[measure_col]], 0.05),
      stats::quantile(df[[measure_col]], 0.95)
    )
  )
  
  min_pts      <- 10
  r2_threshold <- initial_r_squared_threshold
  
  for (i in seq_len(max_iterations)) {
    for (start in seq_len(nrow(df) - min_pts)) {
      for (end in seq(start + min_pts, nrow(df))) {
        if ((end - start + 1) < min_pts) next
        model <- tryCatch(
          lm(log(df[[measure_col]][start:end]) ~ df[[time_col]][start:end]),
          error = function(e) NULL
        )
        if (is.null(model)) next
        
        r2   <- summary(model)$r.squared
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
                                                  max_iterations   = 10) {
  
  best_model <- NULL
  best_r2    <- -Inf
  best_start <- best_end <- NULL
  
  df <- df[!is.na(df[[time_col]]) & !is.na(df[[measure_col]]), ]
  df <- dplyr::filter(
    df,
    dplyr::between(
      df[[measure_col]],
      stats::quantile(df[[measure_col]], 0.05),
      stats::quantile(df[[measure_col]], 0.95)
    )
  )
  
  min_pts <- 10
  for (i in seq_len(max_iterations)) {
    for (start in seq_len(nrow(df) - min_pts)) {
      for (end in seq(start + min_pts, nrow(df))) {
        if ((end - start + 1) < min_pts) next
        model <- tryCatch(
          lm(log(df[[measure_col]][start:end]) ~ df[[time_col]][start:end]),
          error = function(e) NULL
        )
        if (is.null(model)) next
        r2 <- summary(model)$r.squared
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
calculate_growth_rates_robust <- function(df) {
  # claves en el MISMO orden que aparecen en el df
  well_order <- unique(df$Well)
  
  do_one <- function(w) {
    d <- df[df$Well == w, , drop = FALSE]
    phase <- identify_exponential_phase_robust(
      d, time_col = "Time", measure_col = "Measurements"
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
  
  res_list <- .bioszen_map_wells(well_order, do_one)
  out <- dplyr::bind_rows(res_list)
  
  # asegurar MISMO orden que en df de entrada
  out$Well <- factor(out$Well, levels = well_order)
  out <- dplyr::arrange(out, Well)
  out
}

# ============================================================
# 4) Calcular parámetros – PERMISIVO (paralelo + orden estable)
# ============================================================
calculate_growth_rates_permissive <- function(df) {
  well_order <- unique(df$Well)
  
  do_one <- function(w) {
    d <- df[df$Well == w, , drop = FALSE]
    phase <- identify_exponential_phase_permissive(
      d, time_col = "Time", measure_col = "Measurements"
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
  
  res_list <- .bioszen_map_wells(well_order, do_one)
  out <- dplyr::bind_rows(res_list)
  
  # asegurar MISMO orden que en df de entrada
  out$Well <- factor(out$Well, levels = well_order)
  out <- dplyr::arrange(out, Well)
  out
}

# ============================================================
# 5) Detectar valores vacíos (sin cambios)
# ============================================================
is_empty_value <- function(x) {
  is.na(x) | x == "" | is.nan(x)
}
