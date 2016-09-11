#This document defines the Plant class around which this package is developed

#' An S4 class to represent the development of a plant
#'
#' @slot phenology A data.frame that contains the phenological data.
#' @slot temperature A list that contains temperature data for all of
#'     the years for which there is phenological data.
#' @slot modeltype The type of structure the model will have. Options are
#'     'partial', 'full', 'combined', or 'time'.
#' @slot form The functional form of the thermal time accumulation.
#' @slot stages An integer with the number of stages in the phenological model
#' @slot
setClass('Plant',
         slots=list(phenology = "data.frame",
                    temperature = "list",
                    modeltype = "character",
                    form = "function",
                    stages = "numeric"))


#############################################################
###Accessor generics

#' Returns the phenology data.frame
#'
#' @param object An object of class Plant
#' @return A data.frame with the phenology data
#' @export
setGeneric('phenology', function(object) standardGeneric('phenology'))

#' Returns the temperature list
#'
#' @param object An object of class Plant
#' @return A list with the temperature data
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
#replacement generics

#' Setting the phenology data.frame
#'
#'  Used to change the phenology data.frame without recreating the object.
#' @param object a Plant object
#' @param value a data.frame with the relevant phenology data. Given N stages,
#'     the data.frame should have columns year, event1 through eventN+1, and
#'     length1 through lengthN.
#' @export
setGeneric('phenology<-',
           function(object, value) standardGeneric('phenology<-'))


#' Setting the temperature list
#'
#'  Used to change the temperature list without recreating the object.
#' @param object a Plant object
#' @param value a data.frame with the relevant temperature data. If the form
#'     of the model is 'gdd' or 'gddsimple' the data.frame should have the
#'     variables year, tmin and tmax. Otherwise it should have the variables
#'     year and tmax
#' @export
setGeneric('temperature<-',
           function(object, value) standardGeneric('temperature<-'))


