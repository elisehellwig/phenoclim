% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parameterlistclass.R, R/parameterlistmethods.R,
%   R/plantmodelmethods.R, R/flowermodelmethods.R
\docType{methods}
\name{startday}
\alias{startday}
\alias{startday,ParameterList-method}
\alias{startday,list-method}
\alias{startday,PlantModel-method}
\alias{startday,FlowerModel-method}
\title{Returns  a vector of start days}
\usage{
startday(object)

\S4method{startday}{ParameterList}(object)

\S4method{startday}{list}(object)

\S4method{startday}{PlantModel}(object)

\S4method{startday}{PlantModel}(object)

\S4method{startday}{FlowerModel}(object)
}
\arguments{
\item{object}{An object of class ParameterList}
}
\value{
A vector of the start days or NAs for starting at bloom for
    PlantModels and harvest for FlowerModels
}
\description{
Returns  a vector of start days

Accesses the start day of the ParameterList object

Accesses the start day for a list of ParameterLists

Accesses the start days of a plant object

Accesses the start days of a plant object

Accesses the start days of a FlowerModel object
}
