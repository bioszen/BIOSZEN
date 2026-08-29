\name{bioszen_citation}
\alias{bioszen_citation}
\title{Cite BIOSZEN}
\usage{
bioszen_citation(format = c("text", "bibentry", "bibtex", "doi", "rrid", "methods"))
}
\arguments{
\item{format}{Citation format: plain \code{"text"}, a \code{"bibentry"}, BibTeX, the DOI alone, the RRID, or a Methods-ready statement.}
}
\value{A character value, except for \code{format = "bibentry"}, which returns a \code{bibentry} object.}
\description{
Returns the official BIOSZEN citation from the package's centralized metadata.
The concept DOI always resolves to the current Zenodo record.
}
