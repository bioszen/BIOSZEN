\name{BIOSZEN}
\alias{BIOSZEN}
\alias{run_app}
\title{Launch BIOSZEN}
\usage{
BIOSZEN(
  host = getOption("shiny.host", "127.0.0.1"),
  port = getOption("shiny.port", 4321),
  launch.browser = getOption("shiny.launch.browser", TRUE)
)

run_app(
  host = getOption("shiny.host", "127.0.0.1"),
  port = getOption("shiny.port", 4321),
  launch.browser = getOption("shiny.launch.browser", TRUE)
)
}
\arguments{
\item{host}{Host interface for the local Shiny server.}
\item{port}{Port for the local Shiny server.}
\item{launch.browser}{Browser launcher passed to \code{shiny::runApp()}.}
}
\value{The value returned by \code{shiny::runApp()}, invisibly when appropriate.}
\description{
Starts the interactive BIOSZEN Shiny application. \code{BIOSZEN()} is the
concise public launcher; \code{run_app()} remains available for backward
compatibility.
}
