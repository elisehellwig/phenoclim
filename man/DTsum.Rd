% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/thermalsum.R
\name{DTsum}
\alias{DTsum}
\title{Calculates thermal time sums for DT model}
\usage{
DTsum(ctemps, yrloc, tdat, form, startDate, threshDate, varying, mclass)
}
\arguments{
\item{ctemps}{Cardinal temperatures}

\item{yrloc}{data.frame, the year/locations we have phenology data for.}

\item{tdat}{data.frame containing the temperature information}

\item{form}{the functional form of the thermal time accumulation}

\item{startDate}{POSIXct, the date to start accumulating time or thermal
time towards the model threshold.}

\item{threshDate}{POSIXct, the date to stop accumulating thermal
time (threshold does not vary).}

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
Calculates thermal time sums for a given length of days after flowering for
a series of years.
}
