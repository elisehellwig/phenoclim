% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/diurnal_temperature.R
\name{diTemp}
\alias{diTemp}
\title{Calculates hourly temperatures}
\usage{
diTemp(lat, date, tmin, tmax)
}
\arguments{
\item{lat}{The latitude of the location of the temperature measurements.}

\item{date}{The date the temperature measurements were taken.}

\item{tmin}{The minimum daily temperature}

\item{tmax}{The maximum daily temperature}
}
\value{
A vector of 24 hourly temperatures.
}
\description{
This function calculates hourly temperatures for a given place based on its
    latitude, the day of the year and the minimum and maximum daily
    temperatures.
}
\examples{
diTemp(35, "2013-02-18", 8, 17)
}
