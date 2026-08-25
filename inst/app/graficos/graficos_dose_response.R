# Concentration-response analysis -------------------------------------------

bioszen_dose_concentration_pattern <- function() {
  paste0(
    "(?i)",
    "([0-9]+(?:[\\.,][0-9]+)?(?:e[+-]?[0-9]+)?)",
    "\\s*",
    "(pM|nM|uM|µM|μM|mM|M|pg\\s*/?\\s*mL|ng\\s*/?\\s*mL|",
    "ug\\s*/?\\s*mL|µg\\s*/?\\s*mL|μg\\s*/?\\s*mL|",
    "mg\\s*/?\\s*mL|g\\s*/?\\s*L)"
  )
}

bioszen_dose_normalize_text <- function(x) {
  out <- trimws(as.character(x %||% ""))
  ascii <- suppressWarnings(iconv(out, from = "", to = "ASCII//TRANSLIT"))
  ascii[is.na(ascii)] <- out[is.na(ascii)]
  tolower(trimws(ascii))
}

bioszen_dose_control_aliases <- function() {
  c(
    "control", "ctrl", "vehicle", "vehiculo", "vehículo", "dmso",
    "untreated", "no treatment", "sin tratamiento", "mock"
  )
}

bioszen_dose_unit_info <- function(unit) {
  unit_chr <- as.character(unit %||% "")
  unit_chr <- gsub("µ", "u", unit_chr, fixed = TRUE)
  unit_chr <- gsub("μ", "u", unit_chr, fixed = TRUE)
  key <- tolower(gsub("\\s+", "", unit_chr))
  key <- gsub("\\\\", "/", key)
  compact_mass_units <- c(
    pgml = "pg/ml", ngml = "ng/ml", ugml = "ug/ml",
    mgml = "mg/ml", gl = "g/l"
  )
  replace_compact <- key %in% names(compact_mass_units)
  key[replace_compact] <- unname(compact_mass_units[key[replace_compact]])

  family <- rep(NA_character_, length(key))
  factor <- rep(NA_real_, length(key))
  base_unit <- rep(NA_character_, length(key))

  molar_factors <- c(pm = 1e-6, nm = 1e-3, um = 1, mm = 1e3, m = 1e6)
  mass_factors <- c("pg/ml" = 1e-6, "ng/ml" = 1e-3, "ug/ml" = 1,
                    "mg/ml" = 1e3, "g/l" = 1e3)

  molar_idx <- key %in% names(molar_factors)
  mass_idx <- key %in% names(mass_factors)
  family[molar_idx] <- "molar"
  family[mass_idx] <- "mass"
  factor[molar_idx] <- unname(molar_factors[key[molar_idx]])
  factor[mass_idx] <- unname(mass_factors[key[mass_idx]])
  base_unit[molar_idx] <- "µM"
  base_unit[mass_idx] <- "µg/mL"

  data.frame(
    UnitKey = key,
    UnitFamily = family,
    UnitFactor = factor,
    ConcentrationUnit = base_unit,
    stringsAsFactors = FALSE
  )
}

bioszen_dose_unit_choices <- function() {
  c("pM", "nM", "µM", "mM", "M", "pg/mL", "ng/mL", "µg/mL", "mg/mL", "g/L")
}

bioszen_dose_canonical_unit <- function(unit) {
  unit_chr <- as.character(unit %||% "")
  unit_chr <- gsub("µ", "u", unit_chr, fixed = TRUE)
  unit_chr <- gsub("μ", "u", unit_chr, fixed = TRUE)
  key <- tolower(gsub("\\s+", "", unit_chr))
  compact_mass_units <- c(
    pgml = "pg/ml", ngml = "ng/ml", ugml = "ug/ml",
    mgml = "mg/ml", gl = "g/l"
  )
  replace_compact <- key %in% names(compact_mass_units)
  key[replace_compact] <- unname(compact_mass_units[key[replace_compact]])
  canonical <- c(
    pm = "pM", nm = "nM", um = "µM", mm = "mM", m = "M",
    "pg/ml" = "pg/mL", "ng/ml" = "ng/mL", "ug/ml" = "µg/mL",
    "mg/ml" = "mg/mL", "g/l" = "g/L"
  )
  out <- unname(canonical[key])
  out[is.na(out)] <- ""
  out
}

bioszen_dose_normalize_manual_unit <- function(unit) {
  unit_chr <- as.character(unit %||% "")
  unit_chr[is.na(unit_chr)] <- ""
  unit_chr <- trimws(gsub("\\s+", " ", unit_chr))
  canonical <- bioszen_dose_canonical_unit(unit_chr)
  recognized <- nzchar(canonical)
  unit_chr[recognized] <- canonical[recognized]
  unit_chr
}

bioszen_dose_apply_unit_all <- function(mapping, unit) {
  if (!is.data.frame(mapping) || !nrow(mapping)) return(mapping)
  unit_value <- bioszen_dose_normalize_manual_unit(unit)
  unit_value <- if (length(unit_value)) unit_value[[1]] else ""
  if (!nzchar(unit_value)) return(mapping)
  if (!"UnitInput" %in% names(mapping)) mapping$UnitInput <- ""
  mapping$UnitInput <- unit_value
  mapping
}

bioszen_dose_mapping_input_id <- function(media, field = c("dose", "unit")) {
  field <- match.arg(field)
  key <- as.character(media %||% "")
  paste0(
    "dose_mapping_", field, "_",
    substr(digest::digest(key, algo = "md5", serialize = FALSE), 1L, 16L)
  )
}

bioszen_dose_resolve_strain_selection <- function(current, choices, initialized = FALSE) {
  choices <- unique(trimws(as.character(choices %||% character(0))))
  choices <- choices[!is.na(choices) & nzchar(choices)]
  current_missing <- is.null(current)
  current_chr <- trimws(as.character(current %||% character(0)))
  current_chr <- current_chr[!is.na(current_chr) & nzchar(current_chr)]
  selected <- intersect(current_chr, choices)
  stale_selection <- length(current_chr) && !length(selected)
  if (!isTRUE(initialized) || current_missing || stale_selection) selected <- choices
  list(
    selected = selected,
    invalid = any(!current_chr %in% choices),
    missing = current_missing
  )
}

bioszen_parse_concentration_labels <- function(labels, control_labels = character(0)) {
  labels <- as.character(labels %||% character(0))
  if (!length(labels)) {
    return(data.frame(
      Media = character(0), DoseOriginal = numeric(0), UnitOriginal = character(0),
      Dose = numeric(0), UnitFamily = character(0), ConcentrationUnit = character(0),
      Compound = character(0), SeriesKey = character(0), IsControl = logical(0),
      Parsed = logical(0), stringsAsFactors = FALSE
    ))
  }
  labels[is.na(labels)] <- ""

  pattern <- bioszen_dose_concentration_pattern()
  match <- stringr::str_match(labels, stringr::regex(pattern))
  has_dose <- !is.na(match[, 1])
  dose_original <- suppressWarnings(as.numeric(sub(",", ".", match[, 2], fixed = TRUE)))
  unit_original <- match[, 3]
  unit_info <- bioszen_dose_unit_info(unit_original)

  normalized <- bioszen_dose_normalize_text(labels)
  aliases <- unique(bioszen_dose_normalize_text(c(
    bioszen_dose_control_aliases(), control_labels
  )))
  aliases <- aliases[!is.na(aliases) & nzchar(aliases)]
  explicit_controls <- bioszen_dose_normalize_text(control_labels)
  explicit_controls <- explicit_controls[!is.na(explicit_controls) & nzchar(explicit_controls)]

  is_control <- normalized %in% aliases
  if (length(explicit_controls)) {
    is_control <- is_control | normalized %in% explicit_controls
  }

  compound <- labels
  compound[has_dose] <- stringr::str_replace(
    compound[has_dose],
    stringr::regex(pattern),
    " "
  )
  compound <- gsub("[_(),;\\[\\]]+", " ", compound)
  compound <- gsub("\\s+", " ", compound)
  compound <- gsub("^[[:space:]:-]+|[[:space:]:-]+$", "", compound)
  compound[!nzchar(compound)] <- "Treatment"
  compound[is_control] <- ""

  dose <- dose_original * unit_info$UnitFactor
  dose[is_control] <- 0
  parsed <- (has_dose & is.finite(dose) & !is.na(unit_info$UnitFamily)) | is_control

  compound_key <- bioszen_dose_normalize_text(compound)
  series_key <- ifelse(
    has_dose & parsed,
    paste(compound_key, unit_info$UnitFamily, sep = "||"),
    ""
  )

  data.frame(
    Media = labels,
    DoseOriginal = dose_original,
    UnitOriginal = unit_original,
    Dose = dose,
    UnitFamily = unit_info$UnitFamily,
    ConcentrationUnit = unit_info$ConcentrationUnit,
    Compound = compound,
    SeriesKey = series_key,
    IsControl = is_control,
    Parsed = parsed,
    stringsAsFactors = FALSE
  )
}

bioszen_dose_series_table <- function(media, control_labels = character(0)) {
  parsed <- bioszen_parse_concentration_labels(unique(as.character(media)), control_labels)
  parsed <- parsed[
    parsed$Parsed & !parsed$IsControl & nzchar(parsed$SeriesKey),
    c("SeriesKey", "Compound", "UnitFamily", "ConcentrationUnit"),
    drop = FALSE
  ]
  if (!nrow(parsed)) return(parsed)
  parsed <- parsed[!duplicated(parsed$SeriesKey), , drop = FALSE]
  parsed$Display <- paste0(parsed$Compound, " (", parsed$ConcentrationUnit, ")")
  parsed
}

bioszen_dose_mapping_defaults <- function(media,
                                           series_key,
                                           control_labels = character(0),
                                           manual_key = "__MANUAL__") {
  media <- unique(trimws(as.character(media %||% character(0))))
  media <- media[!is.na(media) & nzchar(media)]
  if (!length(media)) return(data.frame())

  parsed <- bioszen_parse_concentration_labels(media, control_labels)
  series_key <- trimws(as.character(series_key %||% ""))
  series_key <- if (length(series_key)) series_key[[1]] else ""

  if (identical(series_key, manual_key)) {
    recognized_units <- unique(parsed$ConcentrationUnit[
      parsed$Parsed & !parsed$IsControl & nzchar(parsed$ConcentrationUnit)
    ])
    control_unit <- if (length(recognized_units) == 1L) recognized_units[[1]] else ""
    dose_default <- ifelse(parsed$Parsed, parsed$Dose, NA_real_)
    unit_default <- ifelse(
      parsed$Parsed & !parsed$IsControl,
      parsed$ConcentrationUnit,
      ifelse(parsed$IsControl, control_unit, "")
    )
    compounds <- unique(parsed$Compound[
      parsed$Parsed & !parsed$IsControl & nzchar(parsed$Compound)
    ])
    compound <- if (length(compounds) == 1L) compounds[[1]] else "Treatment"
    return(data.frame(
      Media = media,
      DoseDefault = dose_default,
      UnitDefault = unit_default,
      IsControl = parsed$IsControl,
      Compound = compound,
      stringsAsFactors = FALSE
    ))
  }

  series <- bioszen_dose_series_table(media, control_labels)
  series <- series[series$SeriesKey == series_key, , drop = FALSE]
  if (!nrow(series)) return(data.frame())

  target_compound <- as.character(series$Compound[[1]])
  target_unit <- as.character(series$ConcentrationUnit[[1]])
  target_normalized <- bioszen_dose_normalize_text(target_compound)
  media_normalized <- bioszen_dose_normalize_text(media)
  unresolved_target <- !parsed$Parsed & nzchar(target_normalized) &
    grepl(target_normalized, media_normalized, fixed = TRUE)
  keep <- parsed$SeriesKey == series_key | parsed$IsControl | unresolved_target

  data.frame(
    Media = media[keep],
    DoseDefault = ifelse(
      parsed$IsControl[keep],
      0,
      ifelse(parsed$SeriesKey[keep] == series_key, parsed$Dose[keep], NA_real_)
    ),
    UnitDefault = rep(target_unit, sum(keep)),
    IsControl = parsed$IsControl[keep],
    Compound = target_compound,
    stringsAsFactors = FALSE
  )
}

bioszen_validate_dose_mapping <- function(mapping) {
  empty_errors <- data.frame(Media = character(0), Code = character(0), stringsAsFactors = FALSE)
  if (is.null(mapping) || !is.data.frame(mapping) || !nrow(mapping) ||
      !all(c("Media", "DoseInput", "UnitInput") %in% names(mapping))) {
    return(list(
      valid = FALSE, mapping = data.frame(), errors = empty_errors,
      mixed_units = FALSE, code = "no_rows"
    ))
  }

  out <- mapping[, c("Media", "DoseInput", "UnitInput"), drop = FALSE]
  out$Media <- trimws(as.character(out$Media))
  out$DoseInput <- trimws(as.character(out$DoseInput))
  out$UnitInput <- trimws(as.character(out$UnitInput))
  out$Dose <- suppressWarnings(as.numeric(sub(",", ".", out$DoseInput, fixed = TRUE)))
  out$Unit <- bioszen_dose_normalize_manual_unit(out$UnitInput)

  errors <- empty_errors
  add_errors <- function(index, code) {
    if (!any(index)) return(invisible(NULL))
    errors <<- rbind(
      errors,
      data.frame(Media = out$Media[index], Code = code, stringsAsFactors = FALSE)
    )
    invisible(NULL)
  }
  add_errors(!nzchar(out$DoseInput), "missing_concentration")
  add_errors(nzchar(out$DoseInput) & (!is.finite(out$Dose) | out$Dose < 0), "invalid_concentration")
  add_errors(!nzchar(out$UnitInput), "missing_unit")

  valid_units <- unique(out$Unit[nzchar(out$Unit)])
  mixed_units <- length(valid_units) > 1L
  valid <- !nrow(errors) && !mixed_units
  list(
    valid = valid,
    mapping = out,
    errors = errors,
    mixed_units = mixed_units,
    code = if (nrow(errors)) "row_errors" else if (mixed_units) "mixed_units" else "ok"
  )
}

bioszen_prepare_dose_response_data <- function(df,
                                                response_col,
                                                series_key,
                                                selected_strains = character(0),
                                                control_labels = character(0),
                                                normalized = FALSE,
                                                concentration_map = NULL,
                                                compound_label = NULL) {
  if (is.null(df) || !is.data.frame(df) || !nrow(df)) return(data.frame())
  required <- c("Strain", "Media", "BiologicalReplicate", response_col)
  if (!all(required %in% names(df))) return(data.frame())

  mapping_status <- bioszen_validate_dose_mapping(concentration_map)
  if (!is.null(concentration_map)) {
    if (!isTRUE(mapping_status$valid)) return(data.frame())
    mapping <- mapping_status$mapping
    map_index <- match(trimws(as.character(df$Media)), mapping$Media)
    keep <- !is.na(map_index)
    out <- df[keep, , drop = FALSE]
    map_index <- map_index[keep]
    if (!nrow(out)) return(data.frame())

    mapped_units <- mapping$Unit[map_index]
    unit_info <- bioszen_dose_unit_info(mapped_units)
    recognized_units <- !is.na(unit_info$UnitFamily) & is.finite(unit_info$UnitFactor)
    unit_factor <- ifelse(recognized_units, unit_info$UnitFactor, 1)
    out$Dose <- mapping$Dose[map_index] * unit_factor
    out$UnitFamily <- ifelse(recognized_units, unit_info$UnitFamily, "custom")
    out$ConcentrationUnit <- ifelse(
      recognized_units,
      unit_info$ConcentrationUnit,
      mapped_units
    )
    target_compound <- trimws(as.character(compound_label %||% "Treatment"))
    if (!length(target_compound) || !nzchar(target_compound[[1]])) target_compound <- "Treatment"
    out$Compound <- target_compound[[1]]
    out$IsControl <- out$Dose == 0
  } else {
    parsed <- bioszen_parse_concentration_labels(df$Media, control_labels)
    series_info <- bioszen_dose_series_table(df$Media, control_labels)
    series_info <- series_info[series_info$SeriesKey == series_key, , drop = FALSE]
    if (!nrow(series_info)) return(data.frame())

    target_family <- series_info$UnitFamily[[1]]
    target_unit <- series_info$ConcentrationUnit[[1]]
    target_compound <- series_info$Compound[[1]]
    keep <- (parsed$SeriesKey == series_key) | parsed$IsControl

    out <- df[keep, , drop = FALSE]
    parsed <- parsed[keep, , drop = FALSE]
    if (!nrow(out)) return(data.frame())

    out$Dose <- parsed$Dose
    out$UnitFamily <- ifelse(parsed$IsControl, target_family, parsed$UnitFamily)
    out$ConcentrationUnit <- target_unit
    out$Compound <- target_compound
    out$IsControl <- parsed$IsControl
  }
  out$Response <- suppressWarnings(as.numeric(out[[response_col]]))
  out$Strain <- trimws(as.character(out$Strain))
  out$Media <- trimws(as.character(out$Media))
  out$BiologicalReplicate <- trimws(as.character(out$BiologicalReplicate))

  selected_strains <- trimws(as.character(selected_strains %||% character(0)))
  selected_strains <- selected_strains[nzchar(selected_strains)]
  if (length(selected_strains)) {
    out <- out[out$Strain %in% selected_strains, , drop = FALSE]
  }

  out <- out[
    is.finite(out$Dose) & out$Dose >= 0 & is.finite(out$Response) &
      !is.na(out$Strain) & nzchar(out$Strain) &
      !is.na(out$BiologicalReplicate) & nzchar(out$BiologicalReplicate),
    , drop = FALSE
  ]
  if (!nrow(out)) return(data.frame())

  out$Response <- if (isTRUE(normalized)) out$Response * 100 else out$Response
  out <- out |>
    dplyr::group_by(
      Strain, Compound, UnitFamily, ConcentrationUnit, Dose, BiologicalReplicate
    ) |>
    dplyr::summarise(
      Response = mean(Response, na.rm = TRUE),
      Media = paste(sort(unique(Media)), collapse = "; "),
      IsControl = any(IsControl),
      .groups = "drop"
    ) |>
    dplyr::filter(is.finite(Response), is.finite(Dose), Dose >= 0)

  out
}

bioszen_dose_ed_values <- function(fit, level = 0.95) {
  ed <- tryCatch(
    suppressMessages(suppressWarnings(
      drc::ED(
        fit,
        respLev = 50,
        interval = "delta",
        level = level,
        type = "relative",
        reference = "upper",
        display = FALSE
      )
    )),
    error = function(e) NULL
  )
  if (is.null(ed)) {
    return(c(estimate = NA_real_, se = NA_real_, lower = NA_real_, upper = NA_real_))
  }
  mat <- suppressWarnings(as.matrix(ed))
  if (!length(mat) || !nrow(mat)) {
    return(c(estimate = NA_real_, se = NA_real_, lower = NA_real_, upper = NA_real_))
  }
  values <- suppressWarnings(as.numeric(mat[1, ]))
  cols <- gsub("[^a-z]", "", tolower(colnames(mat) %||% rep("", length(values))))
  pick <- function(candidates, fallback) {
    idx <- which(cols %in% candidates)
    if (length(idx)) values[[idx[[1]]]] else if (length(values) >= fallback) values[[fallback]] else NA_real_
  }
  c(
    estimate = pick(c("estimate", "estimated"), 1L),
    se = pick(c("stderror", "standarderror", "se"), 2L),
    lower = pick(c("lower", "lowerlimit"), 3L),
    upper = pick(c("upper", "upperlimit"), 4L)
  )
}

bioszen_fit_dose_response_strain <- function(df,
                                              ci_level = 0.95,
                                              include_linear_slope = FALSE,
                                              fit_function = NULL) {
  empty <- list(
    fit = NULL,
    prediction = data.frame(),
    ic50 = NA_real_, ic50_se = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_,
    hill_slope = NA_real_, lower_asymptote = NA_real_, upper_asymptote = NA_real_,
    response_range = NA_real_, inflection_point = NA_real_,
    maximum_slope = NA_real_, maximum_slope_magnitude = NA_real_,
    n_observations = NA_integer_, residual_df = NA_real_, rss = NA_real_,
    rmse = NA_real_, r_squared = NA_real_, adjusted_r_squared = NA_real_,
    aic = NA_real_, bic = NA_real_, log_likelihood = NA_real_,
    linear_slope = NA_real_, linear_slope_se = NA_real_,
    linear_ci_lower = NA_real_, linear_ci_upper = NA_real_,
    linear_p_value = NA_real_, linear_r_squared = NA_real_,
    status = "fit_failed", comparable = FALSE
  )
  if (!requireNamespace("drc", quietly = TRUE)) {
    empty$status <- "dependency_missing"
    return(empty)
  }
  model_df <- data.frame(
    Dose = suppressWarnings(as.numeric(df$Dose)),
    Response = suppressWarnings(as.numeric(df$Response))
  )
  model_df <- model_df[
    is.finite(model_df$Dose) & model_df$Dose >= 0 & is.finite(model_df$Response),
    , drop = FALSE
  ]
  empty$n_observations <- nrow(model_df)
  empty$residual_df <- nrow(model_df) - 4L
  dose_levels <- sort(unique(model_df$Dose))
  positive_levels <- dose_levels[dose_levels > 0]
  if (nrow(model_df) < 6L || length(dose_levels) < 4L || length(positive_levels) < 3L) {
    empty$status <- "insufficient_doses"
    return(empty)
  }
  if (diff(range(model_df$Response, na.rm = TRUE)) <= sqrt(.Machine$double.eps)) {
    empty$status <- "flat_response"
    return(empty)
  }

  if (is.null(fit_function)) {
    fit_function <- function(data) {
      drc::drm(Response ~ Dose, data = data, fct = drc::LL.4())
    }
  }
  fit <- tryCatch(
    suppressMessages(suppressWarnings(fit_function(model_df))),
    error = function(e) NULL
  )
  if (is.null(fit)) return(empty)

  coefficients <- suppressWarnings(as.numeric(stats::coef(fit)))
  if (length(coefficients) < 4L || any(!is.finite(coefficients[seq_len(4L)]))) {
    return(empty)
  }
  ed <- bioszen_dose_ed_values(fit, level = ci_level)
  hill <- coefficients[[1]]
  lower <- coefficients[[2]]
  upper <- coefficients[[3]]
  inflection <- coefficients[[4]]
  ic50 <- ed[["estimate"]]
  if (!is.finite(ic50)) ic50 <- inflection

  response_range <- upper - lower
  maximum_slope <- if (is.finite(inflection) && inflection > 0) {
    -(response_range * hill) / (4 * inflection)
  } else {
    NA_real_
  }

  fitted_values <- tryCatch(
    suppressWarnings(as.numeric(stats::predict(fit, newdata = model_df))),
    error = function(e) rep(NA_real_, nrow(model_df))
  )
  residual_values <- model_df$Response - fitted_values
  valid_residuals <- is.finite(residual_values)
  rss <- if (any(valid_residuals)) sum(residual_values[valid_residuals]^2) else NA_real_
  rmse <- if (is.finite(rss)) sqrt(rss / sum(valid_residuals)) else NA_real_
  total_ss <- sum((model_df$Response - mean(model_df$Response))^2)
  r_squared <- if (is.finite(rss) && is.finite(total_ss) && total_ss > 0) {
    1 - rss / total_ss
  } else {
    NA_real_
  }
  n_observations <- nrow(model_df)
  parameter_count <- 4L
  residual_df <- n_observations - parameter_count
  adjusted_r_squared <- if (is.finite(r_squared) && residual_df > 0L) {
    1 - (1 - r_squared) * (n_observations - 1) / residual_df
  } else {
    NA_real_
  }
  safe_model_stat <- function(expr) {
    tryCatch(suppressWarnings(as.numeric(expr)), error = function(e) NA_real_)
  }
  aic <- safe_model_stat(stats::AIC(fit))
  bic <- safe_model_stat(stats::BIC(fit))
  log_likelihood <- safe_model_stat(stats::logLik(fit))

  linear <- list(
    slope = NA_real_, se = NA_real_, lower = NA_real_, upper = NA_real_,
    p_value = NA_real_, r_squared = NA_real_
  )
  if (isTRUE(include_linear_slope)) {
    linear_fit <- tryCatch(stats::lm(Response ~ Dose, data = model_df), error = function(e) NULL)
    if (!is.null(linear_fit)) {
      linear_summary <- summary(linear_fit)
      linear_coef <- linear_summary$coefficients
      linear_ci <- tryCatch(
        suppressWarnings(stats::confint(linear_fit, "Dose", level = ci_level)),
        error = function(e) c(NA_real_, NA_real_)
      )
      if (is.matrix(linear_ci)) linear_ci <- linear_ci[1, ]
      linear <- list(
        slope = suppressWarnings(as.numeric(linear_coef["Dose", "Estimate"])),
        se = suppressWarnings(as.numeric(linear_coef["Dose", "Std. Error"])),
        lower = suppressWarnings(as.numeric(linear_ci[[1]])),
        upper = suppressWarnings(as.numeric(linear_ci[[2]])),
        p_value = suppressWarnings(as.numeric(linear_coef["Dose", "Pr(>|t|)"])),
        r_squared = suppressWarnings(as.numeric(linear_summary$r.squared))
      )
    }
  }

  inhibitory <- is.finite(hill) && hill > 0 && is.finite(lower) &&
    is.finite(upper) && upper > lower
  min_positive <- min(positive_levels)
  max_tested <- max(positive_levels)
  status <- "ok"
  comparable <- inhibitory && is.finite(ic50) && ic50 > 0 &&
    ic50 >= min_positive && ic50 <= max_tested
  if (!inhibitory) {
    status <- "non_inhibitory"
  } else if (!is.finite(ic50) || ic50 <= 0) {
    status <- "not_estimable"
  } else if (ic50 > max_tested) {
    status <- "not_reached"
  } else if (ic50 < min_positive) {
    status <- "below_range"
  }

  grid_positive <- unique(exp(seq(
    log(min_positive), log(max_tested), length.out = 220L
  )))
  grid <- unique(c(if (any(dose_levels == 0)) 0 else numeric(0), grid_positive))
  pred_raw <- tryCatch(
    suppressWarnings(stats::predict(
      fit,
      newdata = data.frame(Dose = grid),
      interval = "confidence",
      level = ci_level
    )),
    error = function(e) NULL
  )
  prediction <- data.frame()
  if (!is.null(pred_raw)) {
    if (is.matrix(pred_raw) || is.data.frame(pred_raw)) {
      pred_mat <- as.matrix(pred_raw)
      pred_cols <- gsub("[^a-z]", "", tolower(colnames(pred_mat) %||% rep("", ncol(pred_mat))))
      col_value <- which(pred_cols %in% c("prediction", "fit", "estimate"))
      col_lower <- which(pred_cols %in% c("lower", "lowerlimit"))
      col_upper <- which(pred_cols %in% c("upper", "upperlimit"))
      if (!length(col_value)) col_value <- 1L
      prediction <- data.frame(
        Dose = grid,
        Fit = suppressWarnings(as.numeric(pred_mat[, col_value[[1]]])),
        Lower = if (length(col_lower)) suppressWarnings(as.numeric(pred_mat[, col_lower[[1]]])) else NA_real_,
        Upper = if (length(col_upper)) suppressWarnings(as.numeric(pred_mat[, col_upper[[1]]])) else NA_real_
      )
    } else {
      prediction <- data.frame(
        Dose = grid,
        Fit = suppressWarnings(as.numeric(pred_raw)),
        Lower = NA_real_,
        Upper = NA_real_
      )
    }
    prediction <- prediction[is.finite(prediction$Dose) & is.finite(prediction$Fit), , drop = FALSE]
  }

  list(
    fit = fit,
    prediction = prediction,
    ic50 = ic50,
    ic50_se = ed[["se"]],
    ci_lower = ed[["lower"]],
    ci_upper = ed[["upper"]],
    hill_slope = hill,
    lower_asymptote = lower,
    upper_asymptote = upper,
    response_range = response_range,
    inflection_point = inflection,
    maximum_slope = maximum_slope,
    maximum_slope_magnitude = abs(maximum_slope),
    n_observations = n_observations,
    residual_df = residual_df,
    rss = rss,
    rmse = rmse,
    r_squared = r_squared,
    adjusted_r_squared = adjusted_r_squared,
    aic = aic,
    bic = bic,
    log_likelihood = log_likelihood,
    linear_slope = linear$slope,
    linear_slope_se = linear$se,
    linear_ci_lower = linear$lower,
    linear_ci_upper = linear$upper,
    linear_p_value = linear$p_value,
    linear_r_squared = linear$r_squared,
    status = status,
    comparable = comparable
  )
}

bioszen_dose_pairwise_comparisons <- function(parameters, conf_level = 0.95) {
  if (!is.data.frame(parameters) || !nrow(parameters) ||
      !"Comparable" %in% names(parameters)) {
    return(data.frame())
  }
  comparable <- !is.na(parameters$Comparable) & parameters$Comparable
  valid <- parameters[comparable, , drop = FALSE]
  valid <- valid[
    is.finite(valid$IC50) & valid$IC50 > 0 &
      is.finite(valid$IC50_SE) & valid$IC50_SE > 0,
    , drop = FALSE
  ]
  if (nrow(valid) < 2L) return(data.frame())

  pairs <- utils::combn(seq_len(nrow(valid)), 2L, simplify = FALSE)
  z_crit <- stats::qnorm(1 - (1 - conf_level) / 2)
  rows <- lapply(pairs, function(pair) {
    a <- valid[pair[[1]], , drop = FALSE]
    b <- valid[pair[[2]], , drop = FALSE]
    log_ratio <- log(a$IC50[[1]] / b$IC50[[1]])
    se_log <- sqrt(
      (a$IC50_SE[[1]] / a$IC50[[1]])^2 +
        (b$IC50_SE[[1]] / b$IC50[[1]])^2
    )
    z_value <- if (is.finite(se_log) && se_log > 0) log_ratio / se_log else NA_real_
    p_value <- if (is.finite(z_value)) 2 * stats::pnorm(-abs(z_value)) else NA_real_
    data.frame(
      StrainA = as.character(a$Strain[[1]]),
      StrainB = as.character(b$Strain[[1]]),
      IC50_Ratio_A_over_B = exp(log_ratio),
      Ratio_CI_Lower = if (is.finite(se_log)) exp(log_ratio - z_crit * se_log) else NA_real_,
      Ratio_CI_Upper = if (is.finite(se_log)) exp(log_ratio + z_crit * se_log) else NA_real_,
      P_Value = p_value,
      LowerIC50Strain = if (a$IC50[[1]] <= b$IC50[[1]]) as.character(a$Strain[[1]]) else as.character(b$Strain[[1]]),
      stringsAsFactors = FALSE
    )
  })
  out <- dplyr::bind_rows(rows)
  out$P_Adjusted <- stats::p.adjust(out$P_Value, method = "holm")
  out$ConclusionCode <- ifelse(
    is.finite(out$P_Adjusted) & out$P_Adjusted < 0.05,
    "different",
    "not_significant"
  )
  out
}

bioszen_analyze_dose_response <- function(df,
                                          response_col,
                                          parameter_label,
                                          series_key,
                                          selected_strains = character(0),
                                          control_labels = character(0),
                                          normalized = FALSE,
                                          ci_level = 0.95,
                                          concentration_map = NULL,
                                          compound_label = NULL,
                                          include_linear_slope = FALSE,
                                          display_mode = "individual",
                                          error_stat = "SD") {
  observations <- bioszen_prepare_dose_response_data(
    df = df,
    response_col = response_col,
    series_key = series_key,
    selected_strains = selected_strains,
    control_labels = control_labels,
    normalized = normalized,
    concentration_map = concentration_map,
    compound_label = compound_label
  )
  if (!nrow(observations)) {
    return(list(
      observations = data.frame(), predictions = data.frame(), parameters = data.frame(),
      comparisons = data.frame(), diagnostics = data.frame(), replicate_values = data.frame(),
      settings = data.frame(), fits = list(), message = "no_data"
    ))
  }

  strain_levels <- unique(as.character(observations$Strain))
  fits <- setNames(vector("list", length(strain_levels)), strain_levels)
  parameter_rows <- vector("list", length(strain_levels))
  diagnostic_rows <- vector("list", length(strain_levels))
  prediction_rows <- vector("list", length(strain_levels))

  for (index in seq_along(strain_levels)) {
    strain <- strain_levels[[index]]
    strain_df <- observations[observations$Strain == strain, , drop = FALSE]
    fit_info <- bioszen_fit_dose_response_strain(
      strain_df,
      ci_level = ci_level,
      include_linear_slope = include_linear_slope
    )
    fits[[strain]] <- fit_info$fit

    positive <- strain_df$Dose[strain_df$Dose > 0]
    min_tested <- if (length(positive)) min(positive) else NA_real_
    max_tested <- if (length(positive)) max(positive) else NA_real_
    parameter_rows[[index]] <- data.frame(
      Strain = strain,
      Parameter = parameter_label,
      Compound = as.character(strain_df$Compound[[1]]),
      ConcentrationUnit = as.character(strain_df$ConcentrationUnit[[1]]),
      ResultBasis = paste0("IC50 based on ", parameter_label),
      IC50 = fit_info$ic50,
      ED50 = fit_info$ic50,
      EC50 = NA_real_,
      IC50_SE = fit_info$ic50_se,
      CI_Lower = fit_info$ci_lower,
      CI_Upper = fit_info$ci_upper,
      HillSlope = fit_info$hill_slope,
      LowerAsymptote = fit_info$lower_asymptote,
      UpperAsymptote = fit_info$upper_asymptote,
      ResponseRange = fit_info$response_range,
      InflectionPoint = fit_info$inflection_point,
      MaximumSlope = fit_info$maximum_slope,
      MaximumSlopeMagnitude = fit_info$maximum_slope_magnitude,
      MinTested = min_tested,
      MaxTested = max_tested,
      DoseLevels = dplyr::n_distinct(strain_df$Dose),
      BiologicalReplicates = dplyr::n_distinct(strain_df$BiologicalReplicate),
      Status = fit_info$status,
      Comparable = isTRUE(fit_info$comparable),
      stringsAsFactors = FALSE
    )
    diagnostic_rows[[index]] <- data.frame(
      Strain = strain,
      Parameter = parameter_label,
      Model = "Four-parameter log-logistic (LL.4)",
      Observations = fit_info$n_observations,
      ResidualDF = fit_info$residual_df,
      RSS = fit_info$rss,
      RMSE = fit_info$rmse,
      R_Squared = fit_info$r_squared,
      Adjusted_R_Squared = fit_info$adjusted_r_squared,
      AIC = fit_info$aic,
      BIC = fit_info$bic,
      LogLikelihood = fit_info$log_likelihood,
      LinearSlope = fit_info$linear_slope,
      LinearSlopeSE = fit_info$linear_slope_se,
      LinearSlopeCI_Lower = fit_info$linear_ci_lower,
      LinearSlopeCI_Upper = fit_info$linear_ci_upper,
      LinearSlopeP_Value = fit_info$linear_p_value,
      Linear_R_Squared = fit_info$linear_r_squared,
      Status = fit_info$status,
      Converged = !is.null(fit_info$fit),
      stringsAsFactors = FALSE
    )
    if (nrow(fit_info$prediction)) {
      pred <- fit_info$prediction
      pred$Strain <- strain
      prediction_rows[[index]] <- pred
    }
  }

  parameters <- dplyr::bind_rows(parameter_rows)
  diagnostics <- dplyr::bind_rows(diagnostic_rows)
  predictions <- dplyr::bind_rows(prediction_rows)
  comparable <- parameters$Comparable & is.finite(parameters$IC50)
  parameters$SusceptibilityRank <- NA_integer_
  parameters$RelativeToLowestIC50 <- NA_real_
  if (any(comparable)) {
    min_ic50 <- min(parameters$IC50[comparable])
    parameters$SusceptibilityRank[comparable] <- rank(
      parameters$IC50[comparable], ties.method = "min"
    )
    parameters$RelativeToLowestIC50[comparable] <- parameters$IC50[comparable] / min_ic50
  }

  comparisons <- bioszen_dose_pairwise_comparisons(parameters, conf_level = ci_level)
  display_mode <- if (identical(display_mode, "mean_error")) "Mean +/- error" else "Individual biological replicates"
  error_stat <- toupper(as.character(error_stat %||% "SD"))
  if (!error_stat %in% c("SD", "SEM")) error_stat <- "SD"
  settings <- data.frame(
    Setting = c(
      "Response parameter", "Response mode", "Curve model", "Confidence level",
      "Displayed points", "Displayed error bars", "Linear trend slope",
      "Technical replicate handling", "Biological replicate handling",
      "Selected strains", "Series key"
    ),
    Value = c(
      parameter_label,
      if (isTRUE(normalized)) "Normalized (% of control)" else "Raw",
      "Independent four-parameter log-logistic (LL.4) model per strain",
      format(ci_level, trim = TRUE),
      display_mode,
      if (identical(display_mode, "Mean +/- error")) error_stat else "Not applicable",
      if (isTRUE(include_linear_slope)) "Included as a secondary descriptive analysis" else "Not requested",
      "Selected technical replicates are averaged within each biological replicate before nonlinear modelling",
      "Each selected biological replicate is an independent model observation",
      paste(selected_strains, collapse = ", "),
      as.character(series_key %||% "")
    ),
    stringsAsFactors = FALSE
  )
  mapping_status <- bioszen_validate_dose_mapping(concentration_map)
  if (isTRUE(mapping_status$valid) && nrow(mapping_status$mapping)) {
    settings <- dplyr::bind_rows(
      settings,
      data.frame(
        Setting = paste0("Concentration mapping: ", mapping_status$mapping$Media),
        Value = paste(mapping_status$mapping$Dose, mapping_status$mapping$Unit),
        stringsAsFactors = FALSE
      )
    )
  }
  list(
    observations = observations,
    predictions = predictions,
    parameters = parameters,
    comparisons = comparisons,
    diagnostics = diagnostics,
    replicate_values = data.frame(),
    settings = settings,
    fits = fits,
    strain_levels = strain_levels,
    normalized = isTRUE(normalized),
    parameter = parameter_label,
    message = "ok"
  )
}

bioszen_dose_display_data <- function(observations,
                                       mode = "individual",
                                       error_stat = "SD") {
  if (!is.data.frame(observations) || !nrow(observations)) return(data.frame())
  mode <- if (identical(mode, "mean_error")) "mean_error" else "individual"
  error_stat <- toupper(as.character(error_stat %||% "SD"))
  if (!error_stat %in% c("SD", "SEM")) error_stat <- "SD"

  if (identical(mode, "individual")) {
    out <- observations
    out$N <- 1L
    out$SD <- NA_real_
    out$SEM <- NA_real_
    out$Error <- NA_real_
    out$DisplayMode <- "individual"
    return(out)
  }

  observations |>
    dplyr::group_by(Strain, Compound, ConcentrationUnit, Dose) |>
    dplyr::summarise(
      SD = stats::sd(Response, na.rm = TRUE),
      N = sum(is.finite(Response)),
      Response = mean(Response, na.rm = TRUE),
      Media = paste(sort(unique(Media)), collapse = "; "),
      IsControl = any(IsControl),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      SEM = dplyr::if_else(N > 0L, SD / sqrt(N), NA_real_),
      Error = if (identical(error_stat, "SEM")) SEM else SD,
      BiologicalReplicate = paste0("n=", N),
      DisplayMode = "mean_error"
    )
}

bioszen_prepare_dose_replicate_values <- function(df,
                                                   response_col,
                                                   parameter_label,
                                                   concentration_map,
                                                   selected_strains = character(0),
                                                   normalized_df = NULL,
                                                   normalized_col = NULL) {
  required <- c("Strain", "Media", "BiologicalReplicate", response_col)
  if (!is.data.frame(df) || !nrow(df) || !all(required %in% names(df))) return(data.frame())
  mapping_status <- bioszen_validate_dose_mapping(concentration_map)
  if (!isTRUE(mapping_status$valid) || !nrow(mapping_status$mapping)) return(data.frame())

  mapping <- mapping_status$mapping
  map_index <- match(trimws(as.character(df$Media)), mapping$Media)
  keep <- !is.na(map_index)
  out <- df[keep, , drop = FALSE]
  map_index <- map_index[keep]
  if (!nrow(out)) return(data.frame())

  selected_strains <- trimws(as.character(selected_strains %||% character(0)))
  selected_strains <- selected_strains[nzchar(selected_strains)]
  if (length(selected_strains)) out <- out[as.character(out$Strain) %in% selected_strains, , drop = FALSE]
  if (!nrow(out)) return(data.frame())

  map_index <- match(trimws(as.character(out$Media)), mapping$Media)
  mapped_units <- mapping$Unit[map_index]
  unit_info <- bioszen_dose_unit_info(mapped_units)
  recognized <- !is.na(unit_info$UnitFamily) & is.finite(unit_info$UnitFactor)
  unit_factor <- ifelse(recognized, unit_info$UnitFactor, 1)
  out$Concentration <- mapping$Dose[map_index] * unit_factor
  out$ConcentrationUnit <- ifelse(recognized, unit_info$ConcentrationUnit, mapped_units)
  out$RawValue <- suppressWarnings(as.numeric(out[[response_col]]))
  out$Strain <- trimws(as.character(out$Strain))
  out$Condition <- trimws(as.character(out$Media))
  out$BiologicalReplicate <- trimws(as.character(out$BiologicalReplicate))
  if (!"TechnicalReplicate" %in% names(out)) out$TechnicalReplicate <- NA_character_
  out$TechnicalReplicate <- trimws(as.character(out$TechnicalReplicate))
  out$Parameter <- as.character(parameter_label %||% response_col)
  out$NormalizedValue <- NA_real_

  if (is.data.frame(normalized_df) && nrow(normalized_df) &&
      !is.null(normalized_col) && normalized_col %in% names(normalized_df)) {
    normalized_keys <- c("Strain", "Media", "BiologicalReplicate")
    if (all(normalized_keys %in% names(normalized_df))) {
      normalized_lookup <- normalized_df |>
        dplyr::transmute(
          Strain = trimws(as.character(Strain)),
          Media = trimws(as.character(Media)),
          BiologicalReplicate = trimws(as.character(BiologicalReplicate)),
          NormalizedValue = suppressWarnings(as.numeric(.data[[normalized_col]])) * 100
        ) |>
        dplyr::group_by(Strain, Media, BiologicalReplicate) |>
        dplyr::summarise(NormalizedValue = mean(NormalizedValue, na.rm = TRUE), .groups = "drop")
      out <- out |>
        dplyr::select(-NormalizedValue) |>
        dplyr::left_join(normalized_lookup, by = c("Strain", "Media", "BiologicalReplicate"))
    }
  }

  model_source <- if (is.data.frame(normalized_df) && nrow(normalized_df) &&
                      !is.null(normalized_col) && normalized_col %in% names(normalized_df)) {
    bioszen_prepare_dose_response_data(
      df = normalized_df,
      response_col = normalized_col,
      series_key = "__MAPPED__",
      selected_strains = selected_strains,
      normalized = TRUE,
      concentration_map = concentration_map,
      compound_label = "Treatment"
    )
  } else {
    bioszen_prepare_dose_response_data(
      df = df,
      response_col = response_col,
      series_key = "__MAPPED__",
      selected_strains = selected_strains,
      normalized = FALSE,
      concentration_map = concentration_map,
      compound_label = "Treatment"
    )
  }
  out$ModelValue <- NA_real_
  if (is.data.frame(model_source) && nrow(model_source)) {
    model_lookup <- model_source |>
      dplyr::transmute(
        Strain = as.character(Strain),
        Concentration = Dose,
        ConcentrationUnit,
        BiologicalReplicate = as.character(BiologicalReplicate),
        ModelValue = Response
      )
    out <- out |>
      dplyr::select(-ModelValue) |>
      dplyr::left_join(
        model_lookup,
        by = c("Strain", "Concentration", "ConcentrationUnit", "BiologicalReplicate")
      )
  }

  out |>
    dplyr::filter(
      is.finite(Concentration), Concentration >= 0,
      is.finite(RawValue), nzchar(Strain), nzchar(Condition), nzchar(BiologicalReplicate)
    ) |>
    dplyr::select(
      Strain, Condition, Concentration, ConcentrationUnit,
      BiologicalReplicate, TechnicalReplicate, Parameter,
      RawValue, NormalizedValue, ModelValue
    ) |>
    dplyr::arrange(Strain, Concentration, BiologicalReplicate, TechnicalReplicate)
}

bioszen_dose_plot_x <- function(dose, min_positive, log_scale = FALSE) {
  dose <- suppressWarnings(as.numeric(dose))
  if (!isTRUE(log_scale)) return(dose)
  zero_position <- min_positive / 10
  if (!is.finite(zero_position) || zero_position <= 0) zero_position <- 1e-6
  ifelse(dose == 0, zero_position, dose)
}

bioszen_dose_format_label <- function(template, value) {
  template <- as.character(template %||% "")
  value <- as.character(value %||% "")
  template <- if (length(template) && !is.na(template[[1]])) template[[1]] else ""
  value <- if (length(value) && !is.na(value[[1]])) value[[1]] else ""
  marker <- regexpr("%s", template, fixed = TRUE)[[1]]
  if (marker < 1L) return(template)

  prefix <- if (marker > 1L) substr(template, 1L, marker - 1L) else ""
  suffix_start <- marker + 2L
  suffix <- if (suffix_start <= nchar(template)) {
    substr(template, suffix_start, nchar(template))
  } else {
    ""
  }
  paste0(prefix, value, suffix)
}

bioszen_dose_axis_breaks <- function(limits = NULL,
                                      interval = 0,
                                      n = 7,
                                      automatic_when_unbounded = FALSE) {
  interval <- suppressWarnings(as.numeric(interval %||% 0))
  interval <- if (length(interval) && is.finite(interval[[1]])) interval[[1]] else 0
  break_fun <- if (interval > 0) {
    scales::breaks_width(interval)
  } else {
    scales::breaks_pretty(n = n)
  }
  if (is.null(limits)) {
    if (isTRUE(automatic_when_unbounded) && interval <= 0) return(ggplot2::waiver())
    return(break_fun)
  }

  limits <- suppressWarnings(as.numeric(limits))
  if (length(limits) != 2L || any(!is.finite(limits)) || limits[[2]] <= limits[[1]]) {
    return(break_fun)
  }
  values <- suppressWarnings(as.numeric(break_fun(limits)))
  tolerance <- max(1, abs(limits)) * sqrt(.Machine$double.eps)
  values <- values[
    is.finite(values) &
      values >= limits[[1]] - tolerance &
      values <= limits[[2]] + tolerance
  ]
  sort(unique(signif(c(values, limits), 14)))
}

build_dose_response_plot_impl <- function(analysis,
                                          input,
                                          lang,
                                          tr_text,
                                          palette_for_levels,
                                          margin_adj,
                                          fs_title,
                                          fs_axis,
                                          fs_legend,
                                          axis_size) {
  placeholder <- function(key) {
    ggplot2::ggplot() +
      ggplot2::theme_void() +
      ggplot2::annotate("text", 0, 0, label = tr_text(key, lang))
  }
  if (is.null(analysis) || !is.list(analysis) || !nrow(analysis$observations %||% data.frame())) {
    return(placeholder("dose_no_valid_data"))
  }

  obs <- analysis$observations
  pred <- analysis$predictions %||% data.frame()
  strains <- analysis$strain_levels %||% unique(as.character(obs$Strain))
  strains <- as.character(strains)
  display_mode <- as.character(input$dose_point_display %||% "individual")
  if (!display_mode %in% c("individual", "mean_error", "curve_only")) {
    display_mode <- "individual"
  }
  show_points <- !identical(display_mode, "curve_only")
  error_stat <- as.character(input$errbar_stat %||% "SD")
  display <- bioszen_dose_display_data(obs, display_mode, error_stat)
  obs$Strain <- factor(as.character(obs$Strain), levels = strains)
  display$Strain <- factor(as.character(display$Strain), levels = strains)
  if (nrow(pred)) pred$Strain <- factor(as.character(pred$Strain), levels = strains)

  positive <- obs$Dose[is.finite(obs$Dose) & obs$Dose > 0]
  min_positive <- if (length(positive)) min(positive) else 1
  log_scale <- isTRUE(input$dose_log_x)
  zero_position <- min_positive / 10
  if (!is.finite(zero_position) || zero_position <= 0) zero_position <- 1e-6
  obs$DosePlot <- bioszen_dose_plot_x(obs$Dose, min_positive, log_scale)
  display$DosePlot <- bioszen_dose_plot_x(display$Dose, min_positive, log_scale)
  if (nrow(pred)) pred$DosePlot <- bioszen_dose_plot_x(pred$Dose, min_positive, log_scale)

  numeric_setting <- function(value, default) {
    value <- suppressWarnings(as.numeric(value %||% default))
    if (!length(value) || !is.finite(value[[1]])) default else value[[1]]
  }
  text_setting <- function(value) {
    value <- trimws(as.character(value %||% ""))
    if (!length(value)) "" else value[[1]]
  }
  requested_limits <- function(minimum, maximum, log_axis = FALSE) {
    minimum <- numeric_setting(minimum, 0)
    maximum <- numeric_setting(maximum, 0)
    if (!is.finite(maximum) || maximum <= minimum) return(NULL)
    if (isTRUE(log_axis)) {
      minimum <- if (minimum <= 0) zero_position else minimum
      if (maximum <= minimum) return(NULL)
    }
    c(minimum, maximum)
  }
  x_limits <- requested_limits(input$dose_xmin, input$dose_xmax, log_scale)
  y_limits <- requested_limits(input$dose_ymin, input$dose_ymax, FALSE)
  x_interval <- numeric_setting(input$dose_xbreak, 0)
  y_interval <- numeric_setting(input$dose_ybreak, 0)
  line_width <- max(0.1, numeric_setting(input$dose_line_width, 1.05))
  point_size <- max(0.1, numeric_setting(input$dose_point_size, 2.8))
  point_stroke <- max(0.1, numeric_setting(input$dose_point_stroke, 0.65))
  ci_alpha <- min(1, max(0.01, numeric_setting(input$dose_ci_alpha, 0.14)))

  colors <- palette_for_levels(strains)
  p <- ggplot2::ggplot()

  if (nrow(pred) && isTRUE(input$dose_show_ci)) {
    pred_ci <- pred[is.finite(pred$Lower) & is.finite(pred$Upper), , drop = FALSE]
    if (nrow(pred_ci)) {
      p <- p + ggplot2::geom_ribbon(
        data = pred_ci,
        inherit.aes = FALSE,
        ggplot2::aes(x = DosePlot, ymin = Lower, ymax = Upper, fill = Strain),
        alpha = ci_alpha,
        colour = NA,
        show.legend = FALSE
      )
    }
  }
  if (nrow(pred)) {
    p <- p + ggplot2::geom_line(
      data = pred,
      inherit.aes = FALSE,
      ggplot2::aes(x = DosePlot, y = Fit, colour = Strain),
      linewidth = line_width,
      na.rm = TRUE,
      show.legend = TRUE
    )
  }
  if (identical(display_mode, "mean_error")) {
    error_rows <- display[is.finite(display$Error), , drop = FALSE]
    if (nrow(error_rows)) {
      p <- p + ggplot2::geom_errorbar(
        data = error_rows,
        inherit.aes = FALSE,
        ggplot2::aes(
          x = DosePlot,
          ymin = Response - Error,
          ymax = Response + Error,
          colour = Strain
        ),
        width = 0,
        linewidth = 0.6,
        na.rm = TRUE,
        show.legend = FALSE
      )
    }
  }
  if (isTRUE(show_points)) {
    p <- p + ggplot2::geom_point(
      data = display,
      inherit.aes = FALSE,
      ggplot2::aes(x = DosePlot, y = Response, fill = Strain),
      shape = 21,
      colour = "black",
      stroke = point_stroke,
      size = point_size,
      alpha = 0.8,
      na.rm = TRUE,
      show.legend = FALSE
    )
  }

  automatic_x_label <- paste0(
    tr_text("dose_x_label", lang), " (", as.character(obs$ConcentrationUnit[[1]]), ")"
  )
  custom_x_label <- text_setting(input$dose_xlab)
  custom_y_label <- text_setting(input$dose_ylab)
  shared_y_label <- text_setting(input$yLab)
  x_label <- if (nzchar(custom_x_label)) custom_x_label else automatic_x_label
  y_label <- if (nzchar(custom_y_label)) {
    custom_y_label
  } else if (nzchar(shared_y_label)) {
    shared_y_label
  } else if (isTRUE(analysis$normalized)) {
    bioszen_dose_format_label(
      tr_text("dose_y_normalized", lang),
      analysis$parameter
    )
  } else {
    as.character(analysis$parameter)
  }

  p <- p +
    ggplot2::scale_colour_manual(
      name = NULL, values = colors, limits = strains, drop = FALSE
    ) +
    ggplot2::scale_fill_manual(
      name = NULL, values = colors, limits = strains, drop = FALSE,
      guide = "none"
    ) +
    ggplot2::guides(
      colour = ggplot2::guide_legend(
        override.aes = list(alpha = 1, linewidth = line_width)
      )
    ) +
    ggplot2::labs(
      title = input$plotTitle,
      x = x_label,
      y = y_label
    ) +
    ggplot2::theme_minimal(base_size = input$base_size, base_family = "Helvetica") +
    ggplot2::theme(
      plot.margin = margin_adj(20, 45, 15, 15),
      plot.title = ggplot2::element_text(size = fs_title, face = "bold"),
      axis.title = ggplot2::element_text(size = fs_axis, face = "bold", colour = "black"),
      axis.text = ggplot2::element_text(size = fs_axis, colour = "black"),
      axis.line = ggplot2::element_line(linewidth = axis_size, colour = "black"),
      axis.ticks = ggplot2::element_line(linewidth = axis_size, colour = "black"),
      panel.grid = ggplot2::element_blank(),
      legend.position = "right",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = fs_legend, hjust = 0)
    )

  if (isTRUE(log_scale)) {
    zero_break <- if (any(obs$Dose == 0)) zero_position else numeric(0)
    log_range <- if (!is.null(x_limits)) x_limits else range(c(zero_position, positive))
    breaks <- sort(unique(c(zero_break, scales::log_breaks()(log_range))))
    if (!is.null(x_limits)) breaks <- c(breaks, x_limits)
    breaks <- sort(unique(signif(breaks, 14)))
    breaks <- breaks[
      is.finite(breaks) & breaks > 0 &
        breaks >= log_range[[1]] & breaks <= log_range[[2]]
    ]
    labels <- scales::label_number()(breaks)
    if (length(zero_break)) {
      labels[abs(breaks - zero_position) < .Machine$double.eps^0.5] <- "0"
    }
    p <- p + ggplot2::scale_x_log10(
      limits = x_limits,
      breaks = breaks,
      labels = labels,
      expand = ggplot2::expansion(mult = 0)
    )
  } else {
    p <- p + ggplot2::scale_x_continuous(
      limits = x_limits,
      breaks = bioszen_dose_axis_breaks(x_limits, x_interval, n = 7),
      labels = scales::label_number(),
      expand = ggplot2::expansion(mult = 0)
    )
  }
  p <- p + ggplot2::scale_y_continuous(
    limits = y_limits,
    breaks = bioszen_dose_axis_breaks(
      y_limits, y_interval, n = 5, automatic_when_unbounded = TRUE
    ),
    labels = if (is.finite(y_interval) && y_interval > 0) {
      scales::label_number()
    } else {
      ggplot2::waiver()
    },
    expand = ggplot2::expansion(mult = 0)
  )

  p
}

bioszen_dose_workbook_tables <- function(analysis) {
  if (is.null(analysis) || !is.list(analysis) || !nrow(analysis$parameters %||% data.frame())) {
    stop("No concentration-response results are available.", call. = FALSE)
  }
  nonempty_or_message <- function(x, message) {
    if (is.data.frame(x) && nrow(x)) x else data.frame(Message = message, stringsAsFactors = FALSE)
  }

  parameters <- analysis$parameters
  curve_parameters <- parameters |>
    dplyr::select(dplyr::any_of(c(
      "Strain", "Parameter", "Compound", "ConcentrationUnit", "ResultBasis",
      "HillSlope", "MaximumSlope", "MaximumSlopeMagnitude", "LowerAsymptote",
      "UpperAsymptote", "ResponseRange", "InflectionPoint", "ED50", "EC50",
      "CI_Lower", "CI_Upper", "MinTested", "MaxTested", "DoseLevels",
      "BiologicalReplicates", "Status"
    )))
  ic50_results <- parameters |>
    dplyr::select(dplyr::any_of(c(
      "Strain", "Parameter", "Compound", "ConcentrationUnit", "ResultBasis",
      "IC50", "IC50_SE", "CI_Lower", "CI_Upper", "Comparable", "Status",
      "SusceptibilityRank", "RelativeToLowestIC50"
    )))
  replicate_values <- analysis$replicate_values %||% data.frame()
  if (is.data.frame(replicate_values) && !isTRUE(analysis$normalized) &&
      "NormalizedValue" %in% names(replicate_values)) {
    replicate_values$NormalizedValue <- NULL
  }

  list(
    "Replicate values" = nonempty_or_message(
      replicate_values,
      "No replicate-level values are available."
    ),
    "Curve parameters" = nonempty_or_message(curve_parameters, "No curve parameters are available."),
    "IC50 results" = nonempty_or_message(ic50_results, "No IC50 results are available."),
    "Strain comparisons" = nonempty_or_message(
      analysis$comparisons %||% data.frame(),
      "No estimable pairwise IC50 comparisons are available."
    ),
    "Model diagnostics" = nonempty_or_message(
      analysis$diagnostics %||% data.frame(),
      "No model diagnostics are available."
    ),
    "Analysis settings" = nonempty_or_message(
      analysis$settings %||% data.frame(),
      "No analysis settings are available."
    )
  )
}

bioszen_write_dose_response_workbook <- function(analysis, file) {
  tables <- bioszen_dose_workbook_tables(analysis)
  workbook <- openxlsx::createWorkbook()
  for (sheet in names(tables)) {
    openxlsx::addWorksheet(workbook, sheet)
    openxlsx::writeData(workbook, sheet, tables[[sheet]], withFilter = nrow(tables[[sheet]]) > 1L)
    openxlsx::freezePane(workbook, sheet, firstRow = TRUE)
    openxlsx::setColWidths(workbook, sheet, cols = seq_len(ncol(tables[[sheet]])), widths = "auto")
  }
  openxlsx::saveWorkbook(workbook, file, overwrite = TRUE)
  invisible(file)
}
