% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/brick_calc_monthly_mean.r
\name{brick_calc_monthly_mean}
\alias{brick_calc_monthly_mean}
\title{Calculate the monthly means of a raster brick}
\usage{
brick_calc_monthly_mean(input_brick, input_brick_time_as_posixlt)
}
\arguments{
\item{input_brick}{raster brick object that has many layers that represent an series of timesteps.}

\item{input_brick_time_as_posixlt}{associated POSIXlt class vector describing the time value for each layer.}
}
\value{
Returns a raster brick object with 12 layers, one for each month that has been calulated.  (NB: The function may fail if there are not 12 months in the input brick.)
}
\description{
This function assumes the input is a raster brick object that has many layers that represent an ordered series of different timesteps. It selects the layers for each month and calulates the mean for that month. Then returns a brick of 12 layers, representing the January-December means.
}
\examples{
#I need to add an example...
}
