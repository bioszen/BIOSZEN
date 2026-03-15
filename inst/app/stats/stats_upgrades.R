# Advanced statistical helpers -------------------------------------------------

safe_p_column <- function(df, candidates = NULL) {
  if (is.null(candidates)) {
    candidates <- c("p", "p.value", "p.adj", "adj.p.value", "p_val", "p.value.adj")
  }
  if (is.null(df) || !is.data.frame(df)) return(character(0))
  hit <- intersect(candidates, names(df))
  if (!length(hit)) character(0) else hit[1]
}

cohen_d_value <- function(x, y) {
  x <- suppressWarnings(as.numeric(x))
  y <- suppressWarnings(as.numeric(y))
  x <- x[is.finite(x)]
  y <- y[is.finite(y)]
  if (length(x) < 2 || length(y) < 2) return(NA_real_)
  sx <- stats::sd(x)
  sy <- stats::sd(y)
  if (!is.finite(sx) || !is.finite(sy)) return(NA_real_)
  pooled <- sqrt(((length(x) - 1) * sx^2 + (length(y) - 1) * sy^2) /
                   (length(x) + length(y) - 2))
  if (!is.finite(pooled) || pooled == 0) return(NA_real_)
  (mean(y) - mean(x)) / pooled
}

cliffs_delta_value <- function(x, y) {
  x <- suppressWarnings(as.numeric(x))
  y <- suppressWarnings(as.numeric(y))
  x <- x[is.finite(x)]
  y <- y[is.finite(y)]
  if (!length(x) || !length(y)) return(NA_real_)
  diffs <- outer(y, x, FUN = "-")
  gt <- sum(diffs > 0, na.rm = TRUE)
  lt <- sum(diffs < 0, na.rm = TRUE)
  (gt - lt) / (length(x) * length(y))
}

fold_change_value <- function(x, y) {
  x <- suppressWarnings(as.numeric(x))
  y <- suppressWarnings(as.numeric(y))
  x <- x[is.finite(x)]
  y <- y[is.finite(y)]
  if (!length(x) || !length(y)) return(NA_real_)
  mx <- mean(x)
  my <- mean(y)
  if (!is.finite(mx) || !is.finite(my) || mx == 0) return(NA_real_)
  my / mx
}

bootstrap_stat_ci <- function(x, y, stat_fun, n_boot = 2000, conf_level = 0.95) {
  x <- suppressWarnings(as.numeric(x))
  y <- suppressWarnings(as.numeric(y))
  x <- x[is.finite(x)]
  y <- y[is.finite(y)]
  if (!length(x) || !length(y)) {
    return(list(
      estimate = NA_real_,
      ci_low = NA_real_,
      ci_high = NA_real_,
      p_value = NA_real_,
      dist = numeric(0)
    ))
  }
  n_boot <- suppressWarnings(as.integer(n_boot))
  if (!is.finite(n_boot) || n_boot < 50) n_boot <- 50L
  est <- suppressWarnings(stat_fun(x, y))
  boot_vals <- vapply(seq_len(n_boot), function(i) {
    sx <- sample(x, size = length(x), replace = TRUE)
    sy <- sample(y, size = length(y), replace = TRUE)
    suppressWarnings(stat_fun(sx, sy))
  }, numeric(1))
  boot_vals <- boot_vals[is.finite(boot_vals)]
  if (!length(boot_vals)) {
    return(list(
      estimate = est,
      ci_low = NA_real_,
      ci_high = NA_real_,
      p_value = NA_real_,
      dist = numeric(0)
    ))
  }
  alpha <- (1 - conf_level) / 2
  ci <- stats::quantile(boot_vals, probs = c(alpha, 1 - alpha), na.rm = TRUE, names = FALSE, type = 6)
  p_two <- 2 * min(mean(boot_vals <= 0), mean(boot_vals >= 0))
  p_two <- min(max(p_two, 0), 1)
  list(
    estimate = est,
    ci_low = as.numeric(ci[1]),
    ci_high = as.numeric(ci[2]),
    p_value = p_two,
    dist = boot_vals
  )
}

permutation_mean_diff <- function(x, y, n_perm = 5000) {
  x <- suppressWarnings(as.numeric(x))
  y <- suppressWarnings(as.numeric(y))
  x <- x[is.finite(x)]
  y <- y[is.finite(y)]
  if (length(x) < 2 || length(y) < 2) {
    return(list(estimate = NA_real_, p_value = NA_real_, dist = numeric(0)))
  }
  n_perm <- suppressWarnings(as.integer(n_perm))
  if (!is.finite(n_perm) || n_perm < 100) n_perm <- 100L
  obs <- mean(y) - mean(x)
  pooled <- c(x, y)
  nx <- length(x)
  perm_vals <- vapply(seq_len(n_perm), function(i) {
    idx <- sample.int(length(pooled), size = nx, replace = FALSE)
    g1 <- pooled[idx]
    g2 <- pooled[-idx]
    mean(g2) - mean(g1)
  }, numeric(1))
  perm_vals <- perm_vals[is.finite(perm_vals)]
  if (!length(perm_vals)) {
    return(list(estimate = obs, p_value = NA_real_, dist = numeric(0)))
  }
  p_two <- mean(abs(perm_vals) >= abs(obs))
  list(estimate = obs, p_value = min(max(p_two, 0), 1), dist = perm_vals)
}

apply_multitest_preset <- function(df, p_col = NULL, method = "holm", out_col = "p.multitest") {
  if (is.null(df) || !is.data.frame(df) || !nrow(df)) return(df)
  if (is.null(p_col) || !nzchar(p_col)) {
    p_col <- safe_p_column(df)
  }
  if (!length(p_col) || !p_col %in% names(df)) return(df)
  pvals <- suppressWarnings(as.numeric(df[[p_col]]))
  if (identical(method, "none")) {
    df[[out_col]] <- pvals
    return(df)
  }
  ok <- is.finite(pvals)
  adj <- rep(NA_real_, length(pvals))
  if (any(ok)) {
    adj[ok] <- stats::p.adjust(pvals[ok], method = method)
  }
  df[[out_col]] <- adj
  df
}

pairwise_effect_sizes <- function(df,
                                  label_col = "Label",
                                  value_col = "Valor",
                                  conf_level = 0.95,
                                  n_boot = 2000,
                                  adjust_method = "holm") {
  if (is.null(df) || !is.data.frame(df)) return(tibble::tibble())
  if (!all(c(label_col, value_col) %in% names(df))) return(tibble::tibble())
  dat <- df %>%
    dplyr::mutate(
      .label = as.character(.data[[label_col]]),
      .value = suppressWarnings(as.numeric(.data[[value_col]]))
    ) %>%
    dplyr::filter(!is.na(.label), nzchar(.label), is.finite(.value))

  groups <- unique(dat$.label)
  if (length(groups) < 2) return(tibble::tibble())
  pairs <- utils::combn(groups, 2, simplify = FALSE)

  out <- lapply(pairs, function(pp) {
    g1 <- pp[1]
    g2 <- pp[2]
    x <- dat$.value[dat$.label == g1]
    y <- dat$.value[dat$.label == g2]
    if (length(x) < 2 || length(y) < 2) return(NULL)

    mean_diff_ci <- bootstrap_stat_ci(
      x, y,
      stat_fun = function(a, b) mean(b) - mean(a),
      n_boot = n_boot,
      conf_level = conf_level
    )
    d_ci <- bootstrap_stat_ci(
      x, y,
      stat_fun = cohen_d_value,
      n_boot = n_boot,
      conf_level = conf_level
    )
    cliff_ci <- bootstrap_stat_ci(
      x, y,
      stat_fun = cliffs_delta_value,
      n_boot = n_boot,
      conf_level = conf_level
    )
    fc_ci <- bootstrap_stat_ci(
      x, y,
      stat_fun = fold_change_value,
      n_boot = n_boot,
      conf_level = conf_level
    )

    tibble::tibble(
      group1 = g1,
      group2 = g2,
      n1 = length(x),
      n2 = length(y),
      mean1 = mean(x),
      mean2 = mean(y),
      mean_diff = mean_diff_ci$estimate,
      mean_diff_ci_low = mean_diff_ci$ci_low,
      mean_diff_ci_high = mean_diff_ci$ci_high,
      cohen_d = d_ci$estimate,
      cohen_d_ci_low = d_ci$ci_low,
      cohen_d_ci_high = d_ci$ci_high,
      cliffs_delta = cliff_ci$estimate,
      cliffs_delta_ci_low = cliff_ci$ci_low,
      cliffs_delta_ci_high = cliff_ci$ci_high,
      fold_change = fc_ci$estimate,
      fold_change_ci_low = fc_ci$ci_low,
      fold_change_ci_high = fc_ci$ci_high,
      p.bootstrap = mean_diff_ci$p_value
    )
  })
  out <- out[!vapply(out, is.null, logical(1))]
  if (!length(out)) return(tibble::tibble())
  res <- dplyr::bind_rows(out)
  apply_multitest_preset(res, p_col = "p.bootstrap", method = adjust_method, out_col = "p.bootstrap.adj")
}

pairwise_resampling_tests <- function(df,
                                      label_col = "Label",
                                      value_col = "Valor",
                                      method = "bootstrap",
                                      n_iter = 5000,
                                      conf_level = 0.95,
                                      adjust_method = "holm") {
  if (is.null(df) || !is.data.frame(df)) return(tibble::tibble())
  if (!all(c(label_col, value_col) %in% names(df))) return(tibble::tibble())
  dat <- df %>%
    dplyr::mutate(
      .label = as.character(.data[[label_col]]),
      .value = suppressWarnings(as.numeric(.data[[value_col]]))
    ) %>%
    dplyr::filter(!is.na(.label), nzchar(.label), is.finite(.value))

  groups <- unique(dat$.label)
  if (length(groups) < 2) return(tibble::tibble())
  pairs <- utils::combn(groups, 2, simplify = FALSE)

  out <- lapply(pairs, function(pp) {
    g1 <- pp[1]
    g2 <- pp[2]
    x <- dat$.value[dat$.label == g1]
    y <- dat$.value[dat$.label == g2]
    if (length(x) < 2 || length(y) < 2) return(NULL)

    if (identical(method, "permutation")) {
      tst <- permutation_mean_diff(x, y, n_perm = n_iter)
      ci <- stats::quantile(tst$dist, probs = c((1 - conf_level) / 2, 1 - (1 - conf_level) / 2),
                            names = FALSE, na.rm = TRUE, type = 6)
      ci_low <- if (length(ci) >= 1 && is.finite(ci[1])) as.numeric(ci[1]) else NA_real_
      ci_high <- if (length(ci) >= 2 && is.finite(ci[2])) as.numeric(ci[2]) else NA_real_
      p_val <- tst$p_value
      estimate <- tst$estimate
    } else {
      tst <- bootstrap_stat_ci(
        x, y,
        stat_fun = function(a, b) mean(b) - mean(a),
        n_boot = n_iter,
        conf_level = conf_level
      )
      ci_low <- tst$ci_low
      ci_high <- tst$ci_high
      p_val <- tst$p_value
      estimate <- tst$estimate
    }

    tibble::tibble(
      group1 = g1,
      group2 = g2,
      mean_diff = estimate,
      ci_low = ci_low,
      ci_high = ci_high,
      p.value = p_val,
      resample_method = method
    )
  })
  out <- out[!vapply(out, is.null, logical(1))]
  if (!length(out)) return(tibble::tibble())
  res <- dplyr::bind_rows(out)
  apply_multitest_preset(res, p_col = "p.value", method = adjust_method, out_col = "p.adj")
}

mixed_model_summary <- function(df,
                                label_col = "Label",
                                value_col = "Valor",
                                replicate_col = "BiologicalReplicate") {
  if (is.null(df) || !is.data.frame(df)) return(tibble::tibble())
  need_cols <- c(label_col, value_col)
  if (!all(need_cols %in% names(df))) {
    return(tibble::tibble(
      model = "none",
      term = label_col,
      statistic = NA_real_,
      p.value = NA_real_,
      AIC = NA_real_,
      BIC = NA_real_,
      n = 0L,
      note = "missing_required_columns"
    ))
  }

  dat <- df %>%
    dplyr::mutate(
      .label = as.factor(as.character(.data[[label_col]])),
      .value = suppressWarnings(as.numeric(.data[[value_col]])),
      .rep = if (replicate_col %in% names(df)) as.factor(as.character(.data[[replicate_col]])) else NA
    ) %>%
    dplyr::filter(is.finite(.value), !is.na(.label))

  if (nrow(dat) < 4 || dplyr::n_distinct(dat$.label) < 2) {
    return(tibble::tibble(
      model = "none",
      term = label_col,
      statistic = NA_real_,
      p.value = NA_real_,
      AIC = NA_real_,
      BIC = NA_real_,
      n = nrow(dat),
      note = "insufficient_data"
    ))
  }

  has_rep <- replicate_col %in% names(df) && dplyr::n_distinct(dat$.rep, na.rm = TRUE) > 1

  if (has_rep && requireNamespace("lme4", quietly = TRUE)) {
    fit_mixed <- tryCatch(
      suppressWarnings(lme4::lmer(.value ~ .label + (1 | .rep), data = dat, REML = FALSE)),
      error = function(e) NULL
    )
    if (!is.null(fit_mixed)) {
      p_val <- NA_real_
      stat_val <- NA_real_
      if (requireNamespace("lmerTest", quietly = TRUE)) {
        fit_t <- tryCatch(lmerTest::as_lmerModLmerTest(fit_mixed), error = function(e) NULL)
        if (!is.null(fit_t)) {
          an <- tryCatch(as.data.frame(stats::anova(fit_t)), error = function(e) NULL)
          if (!is.null(an) && ".label" %in% rownames(an)) {
            p_val <- suppressWarnings(as.numeric(an[".label", "Pr(>F)"]))
            stat_val <- suppressWarnings(as.numeric(an[".label", "F value"]))
          }
        }
      }
      if (!is.finite(p_val)) {
        d1 <- tryCatch(stats::drop1(fit_mixed, test = "Chisq"), error = function(e) NULL)
        if (!is.null(d1) && ".label" %in% rownames(d1)) {
          p_val <- suppressWarnings(as.numeric(d1[".label", "Pr(Chi)"]))
          stat_val <- suppressWarnings(as.numeric(d1[".label", "LRT"]))
        }
      }
      return(tibble::tibble(
        model = "mixed_lmer",
        term = label_col,
        statistic = stat_val,
        p.value = p_val,
        AIC = stats::AIC(fit_mixed),
        BIC = stats::BIC(fit_mixed),
        n = nrow(dat),
        note = "ok"
      ))
    }
  }

  lm_formula <- if (has_rep) {
    stats::as.formula(".value ~ .label + .rep")
  } else {
    stats::as.formula(".value ~ .label")
  }
  fit_lm <- tryCatch(stats::lm(lm_formula, data = dat), error = function(e) NULL)
  if (is.null(fit_lm)) {
    return(tibble::tibble(
      model = "none",
      term = label_col,
      statistic = NA_real_,
      p.value = NA_real_,
      AIC = NA_real_,
      BIC = NA_real_,
      n = nrow(dat),
      note = "fit_failed"
    ))
  }
  an <- tryCatch(as.data.frame(stats::anova(fit_lm)), error = function(e) NULL)
  p_val <- NA_real_
  stat_val <- NA_real_
  if (!is.null(an) && ".label" %in% rownames(an)) {
    p_val <- suppressWarnings(as.numeric(an[".label", "Pr(>F)"]))
    stat_val <- suppressWarnings(as.numeric(an[".label", "F value"]))
  }
  tibble::tibble(
    model = if (has_rep) "lm_with_replicate_fixed_effect" else "lm_label_only",
    term = label_col,
    statistic = stat_val,
    p.value = p_val,
    AIC = stats::AIC(fit_lm),
    BIC = stats::BIC(fit_lm),
    n = nrow(dat),
    note = "fallback_lm"
  )
}

correlation_matrix_with_p <- function(df,
                                      params,
                                      method = "spearman",
                                      adjust_method = "holm") {
  if (is.null(df) || !is.data.frame(df)) {
    return(list(cor = matrix(numeric(0), 0, 0), p = matrix(numeric(0), 0, 0), tidy = tibble::tibble()))
  }
  params <- intersect(as.character(params), names(df))
  if (length(params) < 2) {
    return(list(cor = matrix(numeric(0), 0, 0), p = matrix(numeric(0), 0, 0), tidy = tibble::tibble()))
  }

  num_df <- df %>%
    dplyr::transmute(dplyr::across(dplyr::all_of(params), ~ suppressWarnings(as.numeric(.x))))

  n <- length(params)
  cmat <- matrix(NA_real_, n, n, dimnames = list(params, params))
  pmat <- matrix(NA_real_, n, n, dimnames = list(params, params))

  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (i == j) {
        cmat[i, j] <- 1
        pmat[i, j] <- 0
      } else if (j > i) {
        x <- num_df[[params[i]]]
        y <- num_df[[params[j]]]
        ok <- is.finite(x) & is.finite(y)
        if (sum(ok) >= 3) {
          cor_args <- list(x = x[ok], y = y[ok], method = method)
          if (method %in% c("spearman", "kendall")) cor_args$exact <- FALSE
          tst <- tryCatch(
            suppressWarnings(do.call(stats::cor.test, cor_args)),
            error = function(e) NULL
          )
          if (!is.null(tst)) {
            cmat[i, j] <- unname(as.numeric(tst$estimate))
            cmat[j, i] <- cmat[i, j]
            pmat[i, j] <- as.numeric(tst$p.value)
            pmat[j, i] <- pmat[i, j]
          }
        }
      }
    }
  }

  upper_idx <- upper.tri(pmat, diag = FALSE)
  upper_p <- pmat[upper_idx]
  adj_vals <- rep(NA_real_, length(upper_p))
  ok <- is.finite(upper_p)
  if (any(ok)) {
    adj_vals[ok] <- if (identical(adjust_method, "none")) upper_p[ok] else stats::p.adjust(upper_p[ok], method = adjust_method)
  }
  p_adj <- pmat
  p_adj[upper_idx] <- adj_vals
  p_adj[lower.tri(p_adj)] <- t(p_adj)[lower.tri(p_adj)]
  diag(p_adj) <- 0

  tidy <- expand.grid(
    param_x = params,
    param_y = params,
    stringsAsFactors = FALSE
  ) %>%
    dplyr::mutate(
      r = cmat[cbind(match(param_x, params), match(param_y, params))],
      p.value = pmat[cbind(match(param_x, params), match(param_y, params))],
      p.adj = p_adj[cbind(match(param_x, params), match(param_y, params))]
    )

  list(cor = cmat, p = p_adj, tidy = tidy)
}
