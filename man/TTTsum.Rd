% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/thermalsum.R
\name{TTTsum}
\alias{TTTsum}
\title{Calculates thermal time sums for TTT model}
\usage{
TTTsum(pars, yrloc, tdat, form, startDate, thresh, varying, mclass)
}
\arguments{
\item{pars}{Cardinal temperatures}

\item{yrloc}{data.frame, the year/locations we have phenology data for.}

\item{tdat}{list containing the temperature information}

\item{form}{the functional form of the thermal time accumulation}

\item{startDate}{POSIXct, the date to start accumulating time or thermal
time towards the model threshold.}

\item{thresh}{numeric, the length of thermal time accumulation (in either
days or thermal time units).}

\item{varying}{character, c('start', 'threshold') should either of these
pars vary from year to year.}

\item{mclass}{character, class of model to be estimating, options are
'PlantModel' or 'FlowerModel'. If you have negative day values, you
probably want flower model.}
}
\value{
The thermal sums for a given series of years.
}
\description{
Calculates thermal time sums for a given amount of thermal time after
flowering for a series of years.
}
