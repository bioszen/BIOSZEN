\name{run_app}
\alias{run_app}
\title{Launch the BIOSZEN Shiny App}
\description{
Launches the interactive BIOSZEN application bundled with the package.
}
\usage{
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
\value{
Starts the BIOSZEN Shiny app.
}
\examples{
if (interactive()) {
  run_app()
}
}
