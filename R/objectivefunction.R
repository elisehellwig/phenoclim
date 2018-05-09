#' @include minrmse.R
NULL

#' Generates the Objective function
#'
#' This function creates a function to be passed to the DEoptim function that
#' optimizes the cardinal temperatures and the length of thermal time or day
#' accumulation for a PlantModel or FlowerModel object.
#'
#' @param parlist ParameterList, contains all of the parlist needed to
#'     create the phenology model.
#' @param phenology data.frame, contains all of the phenology data necessary for
#'     running the phenology model.
#' @param templist list, contains all the temperature data for estimating
#'     thermal time.
#' @param stage numeric, the stage of the model.
#' @param CT Should the cardinal temperatures be optimized?
#' @param Start logical, Should model start day be optimized? If yes,
#'     Start is TRUE.
#' @param Threshold logical, Should model threshold be optimized? If yes, it is
#'     TRUE
#' @param simple logical, is the simplified version of the model being run?
#' @param listindex numeric, the index of the ParameterList from parlist that
#'     you are at
#' @param varying character, c('start', 'threshold') should either of these pars
#'     vary from year to year.
#' @param mclass character, type of model to be estimating, options are
#'     'PlantModel' or 'FlowerModel'. If you have negative day values, you
#'     probably want flower model.
#' @return the function that is passed to DEoptim to optimize.
objective <- function(parlist, phenology, templist, stage, CT, Start,
                      Threshold, listindex, mclass) {

    #extract parameters from ParameterList object
    if (mclass=='FlowerModel') {
        PL <- parlist
        events <- paste0('event', 0:1)
        fnames <- c('year', events)

    } else {
        PL <- parlist[[listindex]]

        events <- paste0('event', stage:(stage+1))
        fnames <- c('year', events, paste0('length', stage))

    }

    #what is the type of model (DT or TTT)
    modtype <- modeltype(PL)

    #is the threshold estimated
    if (!Threshold) th <- threshold(PL)[stage] else th <- Threshold

    if (is.na(th)) {
        stop('the variable th cannot be NA at this point. Something went wrong')
    }

    #are the cardinal temps estimated
    if (!CT) ct <- cardinaltemps(PL)[[stage]] else ct <- CT

    #is the start day estimated
    if (!Start) s <- startday(PL)[stage] else s <- Start


    #create data.frame with only the columns necessary
    fdat <- phenology[, fnames]

    #create a function that evaluates returns the rmse of the model that can be
    #minimized using the function DEoptim()
    fun <- function(x) {
        return(minrmse(x, fdat, templist, modeltype(PL), form(PL)[stage], stage,
                       ct, start, thresh, simple[listindex], stgtype=modtype,
                       varying=varyingpars(PL), modclass = mclass(PL)))
    }

    return(fun)
}



