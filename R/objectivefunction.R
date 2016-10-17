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
objective <- function(parlist, phenology, templist, stage, CT, L,
                      simple) {

    pars <- cardinaltemps(parlist)[[stage]]
    ml <- modlength(parlist)[stage]
    events <- paste0('event', stage:(stage+1))

    fdat <- phenology[, c('year', events, paste0('length', stage))]


    if (CT) ct <- TRUE else ct <- pars
    if (L) l <- TRUE else l <- pars


    fun <- function(x) {
        return(minrmse(x, fdat, templist, modeltype(parlist), form(parlist),
                       stage, ct, l, simple))
    }

    return(fun)

}
