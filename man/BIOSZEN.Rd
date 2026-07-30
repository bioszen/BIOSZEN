\name{BIOSZEN}
\alias{BIOSZEN}
\alias{run_app}
\title{Launch BIOSZEN}
\usage{
BIOSZEN(
  host = getOption("shiny.host", "127.0.0.1"),
  port = getOption("shiny.port", 4321),
  launch.browser = TRUE,
  app_window = FALSE
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
\item{launch.browser}{Browser launcher passed to \code{shiny::runApp()}. The
default, \code{TRUE}, opens the app automatically. Supply \code{FALSE} to
disable automatic opening or a function to use a custom launcher.}

\item{app_window}{Logical. When \code{FALSE} (the default), \code{BIOSZEN()}
opens the operating system's configured default browser. Set it to \code{TRUE}
to prefer a dedicated Chromium app window and fall back to the default browser.
This argument is used by \code{BIOSZEN()}; \code{run_app()} retains its
original browser-launch interface.}
}
\value{The value returned by \code{shiny::runApp()}, invisibly when appropriate.}
\description{
Starts the interactive BIOSZEN Shiny application. \code{BIOSZEN()} is the
concise public launcher; \code{run_app()} remains available for backward
compatibility.
}
