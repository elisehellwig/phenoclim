#' @include minrmse.R
NULL

#' Generates the Objective function
#'
#' This function creates a function to be passed to the DEoptim function that
#' optimizes the cardinal temperatures and the length of thermal time or day
#' accumulation for a PlantModel object.
#'
#' @param parlist ParameterList, contains all of the parlist needed to
#'     create the phenology model/PlantModel object.
#' @param phenology data.frame, contains all of the phenology data necessary for
#'     running the phenology model.
#' @param templist list, contains all the temperature data for estimating
#'     thermal time.
#' @param stage numeric, the stage of the model.
#' @param CT Should the cardinal temperatures be optimized? If yes CT is TRUE,
#'     if not CT should be a vector of the cardinal temperatures.
#' @param L Should accumulation length be optimized? If yes L is TRUE,
#'     if not L should be the accumulation length to be used.
#' @param simple logical, is the simplified version of the model being run?
#' @param listindex numeric, the index of the ParameterList from parlist that
#'     you are at
#' @param startday logical, is the day to start counting being optimized? (ie.
#'     not using bloom as startday)
#' @param stgtyp character, type of model to be estimating, options are
#'     'PlantModel' or 'FlowerModel'. If you have negative day values, you
#'     probably want flower model.
#' @return the function that is passed to DEoptim to optimize.
objective <- function(parlist, phenology, templist, stage, CT, L,
                      simple, listindex, startday, stgtype) {

    if (stgtype=='FlowerModel') {
        PL <- parlist
    } else {
        PL <- parlist[[listindex]]
    }

    #extract parameters from ParameterList object
    if (stgtype=='FlowerModel') {
        pars <- cardinaltemps(PL)[[1]]
    } else {
        pars <- cardinaltemps(PL)[[stage]]
    }


    #extract model length/threshold from parameterlist object
    ml <- modlength(PL)

    #create vector of names of events (columns)
    if (stgtype=='FlowerModel') {
        events <- c('event1','event0')
        fnames <- c('year', events)
    } else {
        events <- paste0('event', stage:(stage+1))
        fnames <- c('year', events, paste0('length', stage))

    }


    #create data.frame with only the columns necessary
    fdat <- phenology[, fnames]


    #which parameters are geting estimated
    if (CT[listindex]) ct <- TRUE else ct <- pars
    if (L[listindex]) l <- TRUE else l <- ml


    #create a function that evaluates returns the rmse of the model that can be
        #minimized using the function DEoptim()
    fun <- function(x) {
        return(minrmse(x, fdat, templist, modeltype(PL), form(PL)[stage], stage,
                       ct, l, simple[listindex], startday, stgtype=stgtype))
    }

    return(fun)

}
