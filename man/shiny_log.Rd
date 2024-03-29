% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shiny_ast.R
\name{shiny_log}
\alias{shiny_log}
\title{Format specific UPAS log file data frame columns
to be more user friendly for the Shiny app}
\usage{
shiny_log(df)
}
\arguments{
\item{df}{Pass a UPAS v2 or v2+ log data frame from 'read_ast_log' function.}
}
\value{
A modified data frame with extraneous variables removed and
formatted SampleTime for shiny app plots.
}
\description{
Format specific UPAS log file data frame columns
to be more user friendly for the Shiny app
}
\examples{
upasv2x_log_shiny <- shiny_log(upasv2x_log)
}
