#' @include parameterlistmethods.R
NULL

#############################################################
#PlantModel class
#############################################################
#' An S4 class a fitted model of plant development
#'
#' @slot parameters A ParameterList object that contains the fit parameters.
#' @slot error A vector of RMSE values for each of the models (not cross-validated).
#' @slot phenology A data.frame that contains the phenological data.
#' @slot temperature A list that contains temperature data for all of
#'     the years for which there is phenological data.
#' @slot olm A list of linear models relating the thermal time or day
#'     accumulation to the stage length
#' @export
setClass('PlantModel',
         slots=list(parameters='ParameterList',
                    error='numeric',
                    phenology='data.frame',
                    temperature='list',
                    olm='list'), contains = c('ParameterList', 'lm'))

#############################################################
#Generics

#' Returns the parameters for a phenological model
#'
#' @param object An object of class PlantModel
#' @return The ParameterList of the phenological model
#' @export
setGeneric('parameters', function(object) standardGeneric('parameters'))


#' Returns the RMSE for a phenological model
#'
#' @param object An object of class PlantModel
#' @return The root mean squared error (RMSE) of the model
#' @export
setGeneric('error', function(object) standardGeneric('error'))

#' Returns the phenology data.frame
#'
#' @param object An object of class PlantModel
#' @return A data.frame with the phenology data
#' @export
setGeneric('phenology', function(object) standardGeneric('phenology'))

#' Returns the temperature list
#'
#' @param object An object of class PlantModel
#' @return A list with the temperature data
#' @export
setGeneric('temperature', function(object) standardGeneric('temperature'))


#' Returns the lm object phenological model
#'
#' @param object An object of class PlantModel
#' @return An lm object containing the ordinary linear model used for stage
#'     length prediction.
#' @export
setGeneric('olm', function(object) standardGeneric('olm'))


