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
#' @slot crossvalidated logical, is the error crossvalidated?
#' @export
setClass('PlantModel',
         slots=list(parameters='ParameterList',
                    error='numeric',
                    phenology='data.frame',
                    temperature='data.frame',
                    olm='list',
                    crossvalidated='logical'))

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


#' Returns the lm object phenological models
#'
#' @param object An object of class PlantModel
#' @return An lm object containing the ordinary linear model used for stage
#'     length prediction.
#' @export
setGeneric('olm', function(object) standardGeneric('olm'))


#' Returns the crossvalidated logical vecter
#'
#' @param object An object of class PlantModel
#' @return A logical vector answering the question 'Is the model error
#'     crossvalidated?'
#' @export
setGeneric('crossvalidated', function(object) standardGeneric('crossvalidated'))

###################################
#sets stuff

#' Setting whether the error is crossvalidated
#'
#'  Used to change whether or not the error is crossvalidated without recreating
#'       the object.
#'
#' @param object An object of class PlantModel
#' @param value logical, is the error crossvalidated?
setGeneric('crossvalidated<-', function(object, value) {
    standardGeneric('crossvalidated<-')} )


#' Setting the error
#'
#'  Used to change the error without recreating the object.
#'
#' @param object An object of class PlantModel
#' @param value numeric, the model error
setGeneric('error<-', function(object, value) {
    standardGeneric('error<-')} )




