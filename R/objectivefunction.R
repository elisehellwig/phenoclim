#' @include minrmse.R
NULL

#' Generates the Objective function
#'
#' This function creates a function to be passed to the DEoptim function that
#' optimizes the cardinal temperatures and the length of thermal time or day
#' accumulation for a PlantModel object.
#'
#' @param parameters ParameterList, contains all of the parameters needed to
#'     create the phenology model/PlantModel object.
#' @param phenology data.frame, contains all of the phenology data necessary for
#'     running the phenology model.
#' @param temperature list, contains all the temperature data for estimating
#'     thermal time.
#' @param stage numeric, the stage of the model.
#' @param CT Should the cardinal temperatures be optimized? If yes CT is TRUE,
#'     if not CT should be a vector of the cardinal temperatures.
#' @param L Should accumulation length be optimized? If yes L is TRUE,
#'     if not L should be the accumulation length to be used.
objective <- function(parameters, phenology, temperature, stage, CT, L) {

    pars <- cardinaltemps(parameters)[[stage]]
    ml <- modlength(parameters)[stage]
    events <- paste0('event', stage:(stage+1))

    fdat <- phenology[, c('year', events, paste0('length', stage))]

    tlist <- extracttemp(temperature, fdat$year, 1, 365)

    if (CT) ct <- TRUE else ct <- pars
    if (L) l <- TRUE else l <- pars


    fun <- function(x) {
        return(minrmse(x, fdat, tlist, modeltype(parameters), form(parameters),
                       stage, ct, l))
    }

    return(fun)

}
