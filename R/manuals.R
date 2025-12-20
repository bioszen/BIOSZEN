# Helper to convert DOCX manuals to PDF during development/build.
convert_manuals_to_pdf <- function(force = FALSE) {
  # Try to locate the source www dir within the project
  root_www <- NULL
  if (dir.exists(file.path("inst", "app", "www"))) {
    root_www <- normalizePath(file.path("inst", "app", "www"), winslash = "/", mustWork = TRUE)
  } else {
    pkg_www <- system.file("app/www", package = "BIOSZEN")
    if (nzchar(pkg_www)) root_www <- pkg_www
  }
  if (is.null(root_www) || !dir.exists(root_www)) {
    message("No se encontró la carpeta 'inst/app/www'.")
    return(invisible(FALSE))
  }

  docx_files <- c(file.path(root_www, "MANUAL_ES.docx"), file.path(root_www, "MANUAL_EN.docx"))
  docx_files <- docx_files[file.exists(docx_files)]
  if (!length(docx_files)) {
    message("No se encontraron archivos DOCX de manuales en ", root_www)
    return(invisible(FALSE))
  }

  ok_any <- FALSE
  for (docx in docx_files) {
    pdf <- sub("\\.docx$", ".pdf", docx, ignore.case = TRUE)
    if (file.exists(pdf) && !isTRUE(force)) {
      message(basename(pdf), " ya existe; usar force = TRUE para regenerar.")
      ok_any <- TRUE
      next
    }

    ok <- FALSE
    # Prefer doconv if available (LibreOffice/Word backend)
    if (!ok && requireNamespace("doconv", quietly = TRUE)) {
      ok <- tryCatch({
        doconv::to_pdf(docx, output = pdf)
        file.exists(pdf)
      }, error = function(e) FALSE)
    }

    # Fallback: pandoc + available PDF engine
    if (!ok && requireNamespace("rmarkdown", quietly = TRUE)) {
      engines <- c("xelatex","lualatex","pdflatex","wkhtmltopdf")
      have <- engines[nzchar(Sys.which(engines))]
      if (length(have) > 0) {
        eng <- have[[1]]
        ok <- tryCatch({
          rmarkdown::pandoc_convert(
            docx, to = "pdf", output = pdf,
            options = sprintf("--pdf-engine=%s", eng)
          )
          file.exists(pdf)
        }, error = function(e) FALSE)
      }
    }

    if (!ok) {
      message("No fue posible convertir ", basename(docx), 
              ". Asegúrate de tener LibreOffice/MS Word o LaTeX/wkhtmltopdf instalado.")
    } else {
      message("Generado: ", basename(pdf))
      ok_any <- TRUE
    }
  }

  invisible(ok_any)
}

