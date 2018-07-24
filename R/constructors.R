#' @include parameterlistmethods.R thermal.R general.R
NULL

#This script has constructor functions for both ParameterList and Plant classes

#' Creates ParameterList object
#'
#' This function creates an object of the class ParameterList. Plant objects
#'     require objects of the class ParameterList for their parameters slot.
#'
#' @param n integer, the number of stages in the model.
#' @param mt character, the model type. Can be either 'DT' or 'TTT'.
#' @param simple logical, is the model the simplified version?
#' @param ff character, the functional form of the thermal time calculations.
#'     Options are 'gdd', 'gddsimple', 'linear', 'flat', 'triangle', and
#'     'anderson'. For more information see ______.
#' @param ct A list, data.frame, or matrix of cardinal temperatures. In the
#'     case of a data.frame or matrix, each row should contain the cardinal
#'     temperatures for a given stage of the model. If ct is a list, each
#'     element of the list should be a vector of cardinal temperatures for a
#'     given stage of the model.
#' @param modelthreshold numeric vector of model thresholds (one for each
#'     stage).
#' @param start numeric vector of start days (one for each stage).
#' @param varyingparameters character vector, which parameters will have the
#'     of the year vary from year to year. Can contain,'start' and 'threshold',
#'     or be NA.
#' @param optimized character, Determines what parameters are optimized in the
#'     model. Can contain "cardinaltemps", "threshold", "start" or any
#'     combination of all three, but it must contain at least one.
#' @param ModelClass character, the class to be fit. Options are
#'     'PlantModel' or 'FlowerModel'.
#' @return An object of the class ParameterList.
#' @export
parameterlist <- function(n, mt, simple, ff, ct, modelthreshold, start,
    varyingparameters, optimized=c('cardinaltemps','threshold','start'),
    ModelClass='PlantModel') {


    if (class(ct)=='list') {#if cardinal temps are in a list
        ctlist <- ct        #don't need to do anything to them


    } else if  (class(ct) %in% c('data.frame', 'matrix')) {#if ct not in list
        ctlist <- lapply(1:dim(ct)[1], function(i) ct[i,]) #convert ct to list

    } else {
        stop('ct must be of the type list, data.frame, or matrix.')
    }


    if (mt=='TTT' & 'threshold' %in% varyingpars) {
        stop('Threshold day cannot vary year to year because it is measured in
             thermal time.')

    }


    newobject <- new('ParameterList', stages=n, modeltype=mt,
                    simplified=simple, form=ff, cardinaltemps=ctlist,
                    threshold=modelthreshold, startday=start,
                    varyingpars=varyingparameters, parsOptimized=optimized,
                    mclass=ModelClass)

    return(newobject)
}



