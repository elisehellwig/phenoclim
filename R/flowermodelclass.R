#' @include parameterlistmethods.R plantmodelclass.R
NULL

#############################################################
#PlantModel class
#############################################################
#' An S4 class a fitted model of plant development
#'
#' @slot parameters A list of ParameterList objects that contains the fit
#'     parameters.
#' @slot error A vector of RMSE values for each of the models (not cross-
#'     validated).
#' @slot phenology A data.frame that contains the phenological data and thermal
#'     time data.
#' @slot olm A list of linear models relating the thermal time or day
#'     accumulation to the stage length
#' @slot crossvalidated logical, is the error crossvalidated?
#' @export
setClass('FlowerModel',
         slots=list(parameters='list',
                    error='ANY',
                    phenology='data.frame',
                    olm='list',
                    crossvalidated='logical'))

#############################################################


#############################################################
