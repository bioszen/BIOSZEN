.bioszen_normalize_rrid <- function(value) {
  if (is.null(value)) value <- ""
  value <- trimws(as.character(value))
  if (!length(value) || is.na(value[[1]]) || !nzchar(value[[1]])) return("")
  value <- value[[1]]
  if (grepl("^RRID:", value, ignore.case = TRUE)) {
    return(paste0("RRID:", sub("^RRID:", "", value, ignore.case = TRUE)))
  }
  paste0("RRID:", value)
}

.bioszen_metadata <- function() {
  candidates <- unique(c(
    file.path(getwd(), "DESCRIPTION"),
    file.path(getwd(), "..", "DESCRIPTION"),
    file.path(getwd(), "..", "..", "DESCRIPTION")
  ))
  desc <- NULL
  for (path in candidates[file.exists(candidates)]) {
    raw <- tryCatch(read.dcf(path), error = function(e) NULL)
    if (!is.null(raw) && "Package" %in% colnames(raw) &&
        identical(as.character(raw[1, "Package"]), "BIOSZEN")) {
      desc <- as.list(raw[1, , drop = TRUE])
      break
    }
  }
  if (is.null(desc)) {
    desc <- suppressWarnings(
      tryCatch(utils::packageDescription("BIOSZEN"), error = function(e) NULL)
    )
  }

  field <- function(name, fallback = "") {
    if (is.null(desc) || !name %in% names(desc)) return(fallback)
    value <- as.character(desc[[name]])
    if (!length(value) || is.na(value[[1]]) || !nzchar(value[[1]])) fallback else value[[1]]
  }

  list(
    title = "BIOSZEN",
    version = field("Version", "unknown"),
    year = field("Config/BIOSZEN/CitationYear", "2026"),
    author = "Szenfeld, B.",
    concept_doi = field("Config/BIOSZEN/ConceptDOI", "10.5281/zenodo.18217210"),
    latest_archived_version = field("Config/BIOSZEN/LatestArchivedVersion", field("Version", "unknown")),
    latest_archived_doi = field("Config/BIOSZEN/LatestArchivedDOI", field("Config/BIOSZEN/ConceptDOI", "10.5281/zenodo.18217210")),
    rrid = .bioszen_normalize_rrid(field("Config/BIOSZEN/RRID", "")),
    rrid_resolver = field("Config/BIOSZEN/RRIDResolver", ""),
    repository = field("URL", "https://github.com/bioszen/BIOSZEN")
  )
}

.bioszen_citation_text <- function() {
  metadata <- .bioszen_metadata()
  sprintf(
    "%s (%s). %s. Zenodo. https://doi.org/%s",
    metadata$author,
    metadata$year,
    metadata$title,
    metadata$concept_doi
  )
}

.bioszen_citation_bibentry <- function() {
  metadata <- .bioszen_metadata()
  utils::bibentry(
    bibtype = "Manual",
    title = metadata$title,
    author = utils::person(
      given = "Benjam\u00EDn",
      family = "Szenfeld",
      comment = c(ORCID = "0009-0003-4702-4149")
    ),
    year = metadata$year,
    doi = metadata$concept_doi,
    url = paste0("https://doi.org/", metadata$concept_doi),
    note = "R package and Shiny application"
  )
}

.bioszen_citation_methods <- function() {
  metadata <- .bioszen_metadata()
  sprintf(
    "Data analysis and visualization were performed with %s v%s (%s; Zenodo DOI: %s).",
    metadata$title,
    metadata$latest_archived_version,
    metadata$rrid,
    metadata$latest_archived_doi
  )
}

.bioszen_startup_citation_text <- function() {
  metadata <- .bioszen_metadata()
  paste(
    "##",
    "## BIOSZEN",
    sprintf("## See %s for additional documentation and source code.", metadata$repository),
    "## Please cite software as:",
    paste0("##   ", .bioszen_citation_text()),
    paste0("## Research Resource Identifier: ", metadata$rrid),
    "##",
    sep = "\n"
  )
}

.bioszen_startup_citation <- function(force = FALSE) {
  if (!isTRUE(getOption("BIOSZEN.show_startup_citation", TRUE))) {
    return(invisible(FALSE))
  }
  if (!isTRUE(force) && isTRUE(getOption("BIOSZEN.startup_citation_shown", FALSE))) {
    return(invisible(FALSE))
  }

  options(BIOSZEN.startup_citation_shown = TRUE)
  packageStartupMessage(.bioszen_startup_citation_text())
  invisible(TRUE)
}

#' Cite BIOSZEN
#'
#' Returns the official BIOSZEN citation from the package's centralized
#' metadata. The concept DOI always resolves to the current Zenodo record.
#'
#' @param format Citation format: plain `"text"`, a `"bibentry"`, BibTeX, the
#'   DOI alone, the RRID, or a Methods-ready statement.
#'
#' @return A character value, except for `format = "bibentry"`, which returns
#'   a [utils::bibentry()] object.
#' @export
bioszen_citation <- function(format = c("text", "bibentry", "bibtex", "doi", "rrid", "methods")) {
  format <- match.arg(format)
  if (identical(format, "text")) return(.bioszen_citation_text())
  if (identical(format, "doi")) return(.bioszen_metadata()$concept_doi)
  if (identical(format, "rrid")) return(.bioszen_metadata()$rrid)
  if (identical(format, "methods")) return(.bioszen_citation_methods())

  entry <- .bioszen_citation_bibentry()
  if (identical(format, "bibentry")) return(entry)
  paste(format(entry, style = "Bibtex"), collapse = "\n")
}
