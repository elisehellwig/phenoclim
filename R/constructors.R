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
#' @param length numeric vector of model lengths.
#' @param lims list of start and stop pairs. Start is always the day close to
#'     flowering. This means that it will be the earliest day in a PlantModel,
#'     but the latest day in a FlowerModel. If length is not NA, one of the
#'     limits has to be NA.
#' @param optimized character, Determines what parameters are optimized in the
#'     model.Can contain "cardinaltemps", "modlength" or both, but it must
#'     contain at least one of the two.
#' @param ModelClass character, the class to be fit. Options are
#'     'PlantModel' or 'FlowerModel'.
#' @return An object of the class ParameterList.
#' @export
parameterlist <- function(n, mt, simple, ff, ct, length, lims,
    optimized=c('cardinaltemps','modlength'), ModelClass='PlantModel') {

    if (class(ct)=='list') {#if cardinal temps are in a list
                            #don't need to do anything to them

        #creat ParameterList class object
        newobject <- new('ParameterList', stages=n, modeltype=mt,
                         simplified=simple, form=ff, cardinaltemps=ct,
                         modlength=length, parsOptimized=optimized,
                         mclass=ModelClass)

    } else if (class(ct) %in% c('data.frame', 'matrix') ) { #if ct not in list
        ctlist <- lapply(1:dim(ct)[1], function(i) ct[i,]) #convert ct to list

        #then create ParameterList class object
        newobject <- new('ParameterList', stages=n, modeltype=mt,
                         simplified=simple, form=ff, cardinaltemps=ctlist,
                         modlength=length, parsOptimized=optimized,
                         mclass=ModelClass)
    } else {
        stop('ct must be of the type list, data.frame, or matrix.')
    }

    return(newobject)
}



