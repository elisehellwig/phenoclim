% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parameterlistclass.R, R/parameterlistmethods.R
\docType{methods}
\name{simplified<-}
\alias{simplified<-}
\alias{simplified<-,ParameterList-method}
\alias{simplified<-,list-method}
\title{Setting whether model is simplified}
\usage{
simplified(object) <- value

\S4method{simplified}{ParameterList}(object) <- value

\S4method{simplified}{list}(object) <- value
}
\arguments{
\item{object}{An object of class ParameterList}

\item{value}{A logical vector specifying whether the model is simplified.}
}
\description{
Used to change whether or not the model is simplified without recreating the
    object.
}
