#This document defines the Plant class around which this package is developed

#' An S4 class to represent the development of a plant
#'
#' @slot phenology A data.frame that contains the phenological data.
#' @slot temperature A data.frame that contains temperature data for all of
#'     the years for which there is phenological data.
#' @slot modeltype The type of structure the model will have. Options are
#'     'partial', 'full', 'combined', or 'time'.
#' @slot form The functional form of the thermal time accumulation.
#' @slot stages An integer with the number of stages in the phenological model
#' @slot
setClass('Plant',
         slots=list(phenology = "data.frame",
                    temperature = "data.frame",
                    modeltype = "character",
                    form = "function",
                    stages = "numeric"))


#############################################################


#' Returns the phenology data.frame
#'
#' @param object An object of class Plant
#' @return A data.frame with the phenology data
#' @export
setGeneric('phenology', function(object) standardGeneric('phenology'))

#' Returns the temperature data.frame
#'
#' @param object An object of class Plant
#' @return A data.frame with the temperature data
#' @export
setGeneric('temperature', function(object) standardGeneric('temperature'))

#' Displays the model type
#'
#' @param object An object of class Plant
#' @return Returns the model type (character)
#' @export
setGeneric('modeltype', function(object) standardGeneric('modeltype'))

#' Returns the functional form of the model
#'
#' @param object An object of class Plant
#' @return Returns the functional form of the thermal time accumulation model.
#' @export
setGeneric('form', function(object) standardGeneric('form'))

#' Displays the number of stages in the phenological model
#'
#' @param object An object of class Plant
#' @return The number of stages in the phenological model (numeric)
#' @export
setGeneric('stages', function(object) standardGeneric('stages'))



#############################################################




