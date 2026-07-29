\name{bioszen_update_available}
\alias{bioszen_update_available}
\title{Check for a BIOSZEN Update}
\usage{
bioszen_update_available(
  repos = .bioszen_default_repositories(),
  quiet = FALSE
)
}
\arguments{
\item{repos}{Package repositories to query. The BIOSZEN R-universe and CRAN are used by default.}
\item{quiet}{Whether connection or repository errors should be returned silently as \code{NA}.}
}
\value{
A logical scalar. Attributes contain the installed and available versions.
\code{NA} means that the check could not be completed.
}
\description{Checks the BIOSZEN R-universe repository without installing anything.}
