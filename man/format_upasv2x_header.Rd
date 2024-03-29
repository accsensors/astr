% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/upasv2x.R
\name{format_upasv2x_header}
\alias{format_upasv2x_header}
\title{Read the header data from an Access Sensor Technologies (AST) air sampler
log file}
\usage{
format_upasv2x_header(df_h)
}
\arguments{
\item{df_h}{Pass a upasv2x dataframe from read_ast_header function.}
}
\value{
A modified data frame with header data in wide format.
}
\description{
Read the header data from an Access Sensor Technologies (AST) air sampler
log file
}
\examples{
upasv2x_header <- format_upasv2x_header(upasv2x_header_raw)
}
