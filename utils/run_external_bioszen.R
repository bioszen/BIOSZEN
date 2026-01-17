#!/usr/bin/env Rscript
# Minimal launcher to run BIOSZEN-v1.0.0-win-App.R without opening it.

target_default <- "BIOSZEN-v1.0.0-win-App.R"

args_all <- commandArgs(trailingOnly = FALSE)
trailing <- commandArgs(trailingOnly = TRUE)

file_arg <- "--file="
this_file <- sub(file_arg, "", args_all[grepl(file_arg, args_all)])
wrapper_dir <- if (length(this_file) && nzchar(this_file)) dirname(normalizePath(this_file)) else getwd()

target_arg <- if (length(trailing)) trailing[[1]] else target_default
target_full <- if (file.exists(target_arg)) {
  normalizePath(target_arg)
} else {
  normalizePath(file.path(wrapper_dir, target_arg), mustWork = FALSE)
}

if (!file.exists(target_full)) {
  stop("No se encontró el script: ", target_arg, "\nBuscado en: ", wrapper_dir)
}

# Forward any extra arguments after the target name to the big script.
extra_args <- if (length(trailing) > 1) trailing[-1] else character(0)

rscript_bin <- Sys.which("Rscript")
if (!nzchar(rscript_bin)) {
  candidate <- file.path(R.home("bin"), "Rscript.exe")
  if (file.exists(candidate)) rscript_bin <- candidate
}
if (!nzchar(rscript_bin)) stop("No se encontró Rscript en el PATH ni en R.home().")

old_wd <- setwd(dirname(target_full))
on.exit(setwd(old_wd), add = TRUE)

cat("Lanzando BIOSZEN con: ", target_full, "\n", sep = "")
status <- system2(rscript_bin, c("--vanilla", shQuote(target_full), extra_args), stdout = "", stderr = "", wait = TRUE)

quit(status = if (is.null(status)) 0L else status)
