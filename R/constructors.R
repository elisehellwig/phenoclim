#' @include plantmethodsbasic.R parameterlistmethods.R

#This script defines functions to create objects of the Plant and ParameterList
# classes

#' Creates a ParameterList object
#'
#' @param ct A list, data.frame, or matrix of cardinal temperatures. In the
#'     case of a data.frame or matrix, each row should contain the cardinal
#'     temperatures for a given stage of the model. If ct is a list, each
#'     element of the list should be a vector of cardinal temperatures for a
#'     given stage of the model.
#' @param length A vector of model lengths
#' @return An object of the class ParameterList.
#' @export
parameterlist <- function(ct, length) {

    if (class(ct)=='list') {
        newobject <- new('ParameterList', cardinaltemps=ct, modlength=length)

    } else if (class(ct) %in% c('data.frame', 'matrix') ) {
        ctlist <- lapply(1:dim(ct)[1], function(i) ct[i,])

        newobject <- new('Parameterlist', cardinaltemps=ctlist,
                         modlength=length)
    } else {
        stop('ct must be of the type list, data.frame, or matrix.')
    }

    return(newobject)
}







#' Creates ParameterList object
#'
#' @param ct A list of cardinal temperatures
#' @param length A numeric vector with the lengths of thermal time/day
#'     accumulation. There should either be one entry for each stage or one
#'     entry for all the stages.
#'
