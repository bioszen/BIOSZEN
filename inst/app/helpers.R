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

safe_pairwise_t <- function(df, method = "sidak") {
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

safe_pairwise_wilcox <- function(df, method = "holm") {
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
  gsub("[^A-Za-z0-9_]", "_", x)
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

  has_ctrl <- !is.null(ctrl_medium) && !is.na(ctrl_medium) &&
    nzchar(ctrl_medium) && ctrl_medium %in% df$Media
  fallback_flag <- !has_ctrl

  if (!has_ctrl) {
    out <- df %>%
      dplyr::mutate(dplyr::across(
        dplyr::all_of(available),
        ~ .x,
        .names = "{.col}_Norm"
      ))
    attr(out, "norm_fallback") <- TRUE
    return(out)
  }

  out <- df %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c("Strain", "BiologicalReplicate")))) %>%
    dplyr::mutate(dplyr::across(
      dplyr::all_of(available),
      ~ {
        base <- .x[Media == ctrl_medium][1]
        if (is.na(base) || base == 0) {
          fallback_flag <<- TRUE
          .x
        } else {
          .x / base
        }
      },
      .names = "{.col}_Norm"
    )) %>%
    dplyr::ungroup()

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
