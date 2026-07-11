# Shared rendering constants ---------------------------------------------------

# Export DPI is user-editable within this supported range. CSS_DPI describes
# browser pixel geometry only and must not be treated as an export default.
BIOSZEN_DEFAULT_DPI <- 300
BIOSZEN_MIN_DPI <- 72
BIOSZEN_MAX_DPI <- 600
BIOSZEN_CSS_DPI <- 96

bioszen_validate_dpi <- function(value,
                                 default = BIOSZEN_DEFAULT_DPI,
                                 minimum = BIOSZEN_MIN_DPI,
                                 maximum = BIOSZEN_MAX_DPI) {
  raw <- value
  if (is.null(raw) || !length(raw)) {
    return(list(value = as.numeric(default), valid = FALSE, reason = "missing"))
  }

  first <- raw[[1]]
  if (is.null(first) || length(first) == 0 || is.na(first) ||
      (is.character(first) && !nzchar(trimws(first)))) {
    return(list(value = as.numeric(default), valid = FALSE, reason = "missing"))
  }

  parsed <- suppressWarnings(as.numeric(first))
  if (!length(parsed) || !is.finite(parsed[[1]])) {
    return(list(value = as.numeric(default), valid = FALSE, reason = "non_numeric"))
  }

  parsed <- parsed[[1]]
  if (parsed < minimum || parsed > maximum) {
    return(list(value = as.numeric(default), valid = FALSE, reason = "unsupported"))
  }

  list(value = parsed, valid = TRUE, reason = "valid")
}

bioszen_effective_dpi <- function(value, ...) {
  bioszen_validate_dpi(value, ...)$value
}
