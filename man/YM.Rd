% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/YM.R
\name{YM}
\alias{YM}
\title{YM}
\usage{
YM(df)
}
\value{
Adds column to existing df
}
\description{
Creates a Year-Month column
}
\examples{
dta <- fmxdat::BRICSTRI \%>\% rename(date = Date) \%>\% fmxdat::YM()
}
