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
setClass('GrowthModel',
         slots=list(phenology='data.frame',
                    temperature='list',
                    modeltype='character',
                    form='character',
                    stages='integer',
                    parameters='ParameterList'))
