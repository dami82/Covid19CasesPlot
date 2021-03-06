% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scr_01.R
\name{get_available_geo_areas}
\alias{get_available_geo_areas}
\title{Get All Available Geo Areas.}
\usage{
get_available_geo_areas(covid19.data)
}
\arguments{
\item{covid19.data, }{list returned by fetch_latest_covid19_data(). 
It includes worldwide or US COVID19 infection data.}
}
\value{
a character vector including all valid geographical areas.
}
\description{
Retrieve a character vector including all valid Geographical 
Areas (Countries or US States).
}
\examples{
library(Covid19CasesPlot)
x <- fetch_latest_covid19_data()
head(get_available_geo_areas(x))

}
