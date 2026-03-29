# Helpers for correlation matrix plotting -------------------------------------

corrm_abort_if_requested <- function(should_abort = NULL) {
  if (!is.function(should_abort)) return(invisible(FALSE))
  abort_now <- tryCatch(isTRUE(should_abort()), error = function(e) FALSE)
  if (isTRUE(abort_now)) {
    stop("Correlation matrix generation cancelled because the session is closing.", call. = FALSE)
  }
  invisible(FALSE)
}

prepare_corr_matrix_payload <- function(df_m,
                                        all_params = character(0),
                                        params_raw = character(0),
                                        high_dim = FALSE,
                                        do_norm = FALSE,
                                        has_ctrl_selected = FALSE,
                                        corr_method = "spearman",
                                        adjust_method = "none",
                                        order_profile = FALSE,
                                        show_sig_only = TRUE,
                                        should_abort = NULL) {
  corrm_abort_if_requested(should_abort)
  if (is.null(df_m) || !is.data.frame(df_m) || !nrow(df_m)) {
    stop("No data available for correlation matrix.", call. = FALSE)
  }

  all_params <- as.character(all_params %||% character(0))
  all_params <- all_params[!is.na(all_params) & nzchar(all_params)]
  params_raw <- as.character(params_raw %||% character(0))
  params_raw <- params_raw[!is.na(params_raw) & nzchar(params_raw)]
  if (!length(params_raw)) {
    params_raw <- if (isTRUE(high_dim)) character(0) else head(all_params, 6)
  }
  if (length(params_raw) < 2) {
    stop("Select at least two parameters.", call. = FALSE)
  }

  resolve_param <- function(p) {
    np <- paste0(p, "_Norm")
    if (isTRUE(do_norm) && isTRUE(has_ctrl_selected) && np %in% names(df_m)) np else p
  }
  params_use <- unique(vapply(params_raw, resolve_param, character(1)))
  params_use <- intersect(params_use, names(df_m))
  if (length(params_use) < 2) {
    stop("Select at least two parameters.", call. = FALSE)
  }

  corr_obj <- correlation_matrix_with_p(
    df_m,
    params = params_use,
    method = corr_method %||% "spearman",
    adjust_method = adjust_method %||% "none",
    should_abort = should_abort
  )
  corrm_abort_if_requested(should_abort)

  params_plot <- params_use
  if (isTRUE(order_profile)) {
    corr_for_order <- corr_obj$cor[params_use, params_use, drop = FALSE]
    if (is.matrix(corr_for_order) && nrow(corr_for_order) >= 2) {
      corr_for_order[!is.finite(corr_for_order)] <- 0
      hc <- tryCatch(
        stats::hclust(stats::dist(corr_for_order), method = "average"),
        error = function(e) NULL
      )
      if (!is.null(hc) && length(hc$order) == nrow(corr_for_order)) {
        params_plot <- rownames(corr_for_order)[hc$order]
      }
    }
  }

  mat_df <- corr_obj$tidy %||% tibble::tibble()
  mat_df <- mat_df %>% dplyr::filter(is.finite(r))
  if (!nrow(mat_df)) {
    stop("No data available for correlation matrix.", call. = FALSE)
  }
  corrm_abort_if_requested(should_abort)

  mat_df <- mat_df %>%
    dplyr::mutate(
      param_x = factor(param_x, levels = params_plot),
      param_y = factor(param_y, levels = rev(params_plot)),
      sig = !is.na(p.adj) & p.adj < 0.05,
      stars = dplyr::case_when(
        p.adj < 0.001 ~ "***",
        p.adj < 0.01  ~ "**",
        p.adj < 0.05  ~ "*",
        TRUE ~ ""
      )
    )

  mat_df <- if (isTRUE(show_sig_only)) {
    mat_df %>% dplyr::mutate(label_txt = dplyr::if_else(sig, sprintf("%.2f%s", r, stars), ""))
  } else {
    mat_df %>% dplyr::mutate(label_txt = sprintf("%.2f", r))
  }

  list(
    corr_obj = corr_obj,
    mat_df = mat_df,
    params_plot = params_plot,
    params_use = params_use
  )
}
