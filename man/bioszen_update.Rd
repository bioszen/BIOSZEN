\name{bioszen_update}
\alias{bioszen_update}
\title{Update BIOSZEN}
\usage{
bioszen_update(
  ask = TRUE,
  repos = .bioszen_default_repositories()
)
}
\arguments{
\item{ask}{Whether to ask for interactive confirmation before installation. Set to \code{FALSE} only when prior user consent has already been obtained.}
\item{repos}{Package repositories used for the check and installation.}
}
\value{\code{TRUE} when an update was installed and \code{FALSE} when no installation was needed or the user declined.}
\description{
Checks R-universe for a newer BIOSZEN version, requests consent, and installs it
into the library containing the current package. BIOSZEN never updates while
its Shiny application is running.
}
