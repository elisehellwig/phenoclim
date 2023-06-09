% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/minrmse.R
\name{minrmseTTT}
\alias{minrmseTTT}
\title{Calculates TTT model RMSE}
\usage{
minrmseTTT(pars, fdat, tdat, form, start, thresh, stage, varying, modclass,
  startingevent = NA)
}
\arguments{
\item{pars}{Cardinal temperatures}

\item{fdat}{the data.frame containing the phenological information}

\item{tdat}{list containing the temperature information}

\item{form}{the functional form of the thermal time accumulation}

\item{start}{numeric, the day to start accumulating time or thermal time
towards the model threshold.}

\item{thresh}{the threshold in thermal time that days are counted for.}

\item{stage}{the number of the stage of the phenological model}

\item{varying}{character, c('start', 'threshold') should either of these pars
vary from year to year.}

\item{modclass}{character, type of model to be estimating, options are
'PlantModel' or 'FlowerModel'. If you have negative day values, you
probably want flower model.}

\item{startingevent}{numeric, days first event happened each year.}
}
\value{
The RMSE value for a given set of cardinal temperatures and thermal
    time accumulation length.
}
\description{
Calculates RMSE for the combined model of thermal time accumulation given a
    certain set of cardinal temperatures and thermal time accumulation
    length. Counts the number of days necessary to reach the Thermal time
    threshold and use that number to predict the event. TTT1-3
}
