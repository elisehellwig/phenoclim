#############################################################
#GrowthModel class
#############################################################
#' An S4 class a fitted model of plant development
#'
#' @slot phenology A data.frame that contains the phenological data.
#' @slot temperature A list that contains temperature data for all of
#'     the years for which there is phenological data.
#' @slot modeltype The type of structure the model will have. Options are
#'     'partial', 'full', 'combined', or 'time'.
#' @slot form The functional form of the thermal time accumulation.
#' @slot stages An integer with the number of stages in the phenological model
#' @slot parameters A ParameterList object that contains the fit parameters.
#' @export
setClass('PlantModel',
         slots=list(parameters='ParameterList',
                    error='numeric',
                    form='character',
                    modeltype='character',
                    simplified='logical',
                    stages='integer',
                    data='list'))

#############################################################
#Generics

#' Returns the RMSE for a phenological model
#'
#' @param object An object of class PlantModel
#' @return The root mean squared error (RMSE) of the model
#' @export
setGeneric('error', function(object) standardGeneric('error'))


#' Is the PlantModel fit with the simplified model?
#'
#' @param object An object of class PlantModel
#' @return logical, is the PlantModel the simplified version of the model?
#' @export
setGeneric('simplified', function(object) standardGeneric('simplified'))

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

#' Displays the model type
#'
#' @param object An object of class PlantModel
#' @return Returns the model type (character)
#' @export
setGeneric('modeltype', function(object) standardGeneric('modeltype'))

#' Displays the number of stages in the phenological model
#'
#' @param object An object of class PlantModel
#' @return The number of stages in the phenological model (numeric)
#' @export
setGeneric('stages', function(object) standardGeneric('stages'))



