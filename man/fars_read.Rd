% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_read}
\alias{fars_read}
\title{Read file}
\usage{
fars_read(filename)
}
\arguments{
\item{filename}{A character string with the name of the file to read}
}
\value{
A data frame with data readed from the csv file, or an error if the
  file does not exists.
}
\description{
This function reads data from .csv file from the US
National Highway Traffic Safety Administration's Fatality Analysis
Reporting System (FARS), which is a nationwide census, providing the
American public yearly data, regarding fatal injuries suffered in motor
vehicle traffic crashes.
}
\details{
For more information, see:
  \item{\url{https://www.nhtsa.gov/Data/Fatality-Analysis-Reporting-System-(FARS)}}
}
\examples{
library(dplyr)
library(readr)
yr <- 2015
data <- yr \%>\% fars_read
head(data)

}
