\name{growth_parameters_irregular}
\alias{growth_parameters_irregular}
\title{Calculate BIOSZEN Growth Parameters from Irregular Time Points}
\usage{
growth_parameters_irregular(
  input,
  output_dir = NULL,
  sheet = NULL,
  overwrite = FALSE,
  time_column = NULL
)
}
\arguments{
\item{input}{A data frame, a supported growth file (\code{.xlsx},
\code{.xls}, or \code{.csv}), a vector of supported files, or a directory
containing supported files. Data frames may be wide or tidy.}

\item{output_dir}{Optional directory for generated parameter workbooks. No
files are written when this is \code{NULL}.}

\item{sheet}{Optional Excel sheet name or index. The first sheet is used by
default. Ignored for data frames and CSV files.}

\item{overwrite}{Whether existing result workbooks may be replaced.}

\item{time_column}{Optional time-column name. When \code{NULL} or blank,
common English and Spanish names such as \code{Time}, \code{Tiempo},
\code{Hour}, and \code{Hora} are detected automatically.}
}
\value{
The same result type and parameter columns as \code{growth_parameters()}.
}
\description{
A convenience variant of \code{growth_parameters()} for curve tables whose
recorded time points are uneven or discontinuous. Numeric time values are read
directly from the selected column and must be finite, unique, and strictly
increasing within each well.
}
\examples{
irregular <- data.frame(
  Time = c(0, 0.08, 0.17, 0.5, 1, 2),
  A1 = c(0.05, 0.051, 0.053, 0.06, 0.08, 0.16)
)
result <- growth_parameters_irregular(irregular)
}
\seealso{
\code{\link{growth_parameters}()}
}
