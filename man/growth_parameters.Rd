\name{growth_parameters}
\alias{growth_parameters}
\title{Calculate BIOSZEN Growth Parameters}
\usage{
growth_parameters(
  input,
  output_dir = NULL,
  sheet = NULL,
  overwrite = FALSE,
  max_time = 48,
  time_interval = 0.5
)
}
\arguments{
\item{input}{A data frame, a supported growth file (\code{.xlsx}, \code{.xls}, or \code{.csv}), a vector of supported files, or a directory containing supported files. Data frames may be wide (\code{Time} plus well columns) or tidy (\code{Well}, \code{Time}, and \code{Measurements}).}
\item{output_dir}{Optional directory for generated parameter workbooks. No files are written when this is \code{NULL}. A missing directory is created.}
\item{sheet}{Optional Excel sheet name or index. The first sheet is used by default. Ignored for data frames and CSV files.}
\item{overwrite}{Whether existing result workbooks may be replaced.}
\item{max_time}{Maximum time used when converting the raw reader-export format supported by the Shiny growth module.}
\item{time_interval}{Time interval used when converting the raw reader-export format supported by the Shiny growth module.}
}
\value{
For one input, the same parameter table produced by the Shiny growth workflow,
with class \code{bioszen_growth_parameters}. For multiple files, a named list
with class \code{bioszen_growth_parameters_list}. Generated file paths are
stored in the \code{output_file} attribute of each result.
}
\description{
Calculates microbial growth parameters with the same combined method used by
the BIOSZEN Shiny application. The robust exponential-phase detector is run
first; the permissive detector fills only values that the robust method could
not calculate.
}
\examples{
time <- seq(0, 12, by = 0.5)
curves <- data.frame(
  Time = time,
  A1 = 0.05 + 0.9 / (1 + exp(-0.8 * (time - 5)))
)
result <- growth_parameters(curves)
}
