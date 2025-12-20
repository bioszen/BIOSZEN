#!/usr/bin/env Rscript
# Create a single-file App.R with an embedded package archive (base64 in comments).

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3) {
  stop("Usage: Rscript utils/make_embedded_launcher.R <launcher> <payload> <output>")
}

launcher <- args[[1]]
payload_arg <- args[[2]]
output <- args[[3]]

if (!file.exists(launcher)) stop("Launcher not found: ", launcher)

skip_payload <- tolower(payload_arg) %in% c("skip", "none", "omit", "")
if (!skip_payload && !file.exists(payload_arg)) stop("Payload not found: ", payload_arg)

base64_encode <- function(raw) {
  if (!length(raw)) return("")
  alphabet <- c(LETTERS, letters, 0:9, "+", "/")
  pad <- (3 - (length(raw) %% 3)) %% 3
  if (pad) raw <- c(raw, as.raw(rep(0, pad)))

  m <- matrix(as.integer(raw), ncol = 3, byrow = TRUE)
  i1 <- bitwShiftR(m[, 1], 2)
  i2 <- bitwOr(bitwShiftL(bitwAnd(m[, 1], 3), 4), bitwShiftR(m[, 2], 4))
  i3 <- bitwOr(bitwShiftL(bitwAnd(m[, 2], 15), 2), bitwShiftR(m[, 3], 6))
  i4 <- bitwAnd(m[, 3], 63)

  vals <- c(rbind(i1, i2, i3, i4))
  out <- alphabet[vals + 1L]
  if (pad) {
    out[(length(out) - pad + 1L):length(out)] <- "="
  }
  paste(out, collapse = "")
}

lines <- readLines(launcher, warn = FALSE)

if (skip_payload) {
  out_lines <- lines
} else {
  payload_raw <- readBin(payload_arg, "raw", n = file.info(payload_arg)$size)
  payload_b64 <- base64_encode(payload_raw)

  marker <- paste0("##==BIOSZEN_PAYLOAD:", basename(payload_arg), "==##")
  chunks <- if (nzchar(payload_b64)) {
    starts <- seq(1, nchar(payload_b64), by = 76)
    ends <- pmin(starts + 75, nchar(payload_b64))
    substring(payload_b64, starts, ends)
  } else {
    character(0)
  }

  out_lines <- c(lines, marker, paste0("# ", chunks))
}

writeLines(out_lines, output, useBytes = TRUE)
