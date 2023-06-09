% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plantmodelclass.R, R/plantmodelmethods.R,
%   R/flowermodelmethods.R
\docType{methods}
\name{olm}
\alias{olm}
\alias{olm,PlantModel-method}
\alias{olm,FlowerModel-method}
\title{Returns the lm object phenological models}
\usage{
olm(object)

\S4method{olm}{PlantModel}(object)

\S4method{olm}{FlowerModel}(object)
}
\arguments{
\item{object}{An object of class PlantModel}
}
\value{
An lm object containing the ordinary linear model used for stage
    length prediction.
}
\description{
Returns the lm object phenological models

Accesses the linear models of a PlantModel object

Accesses the linear models of a FlowerModel object
}
