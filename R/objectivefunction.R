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
#' @param temp data.frame, contains all the temperature data for estimating
#'     thermal time and has a POSIXct date-time column for indexing
#' @param stage numeric, the stage of the model.
#' @param CT Should the cardinal temperatures be optimized?
#' @param Start logical, Should model start day be optimized? If yes,
#'     Start is TRUE.
#' @param Threshold logical, Should model threshold be optimized? If yes, it is
#'     TRUE
#' @param listindex numeric, the index of the ParameterList from parlist that
#'     you are at
#' @param mclass character, type of model to be estimating, options are
#'     'PlantModel' or 'FlowerModel'. If you have negative day values, you
#'     probably want flower model.
#' @return the function that is passed to DEoptim to optimize.
#' @export
objective <- function(parlist, phenology, temp, stage, CT, Start,
                      Threshold, listindex, mclass) {

    #print('objective')
    #extract parameters from ParameterList object
    if (mclass=='FlowerModel') {
        PL <- parlist[[listindex]]
        events <- paste0('event', 0:1)
        fnames <- c('year', events)

    } else if (mclass=='PlantModel') {
        PL <- parlist[[listindex]]

        events <- paste0('event', stage:(stage+1))
        fnames <- c('year', events, paste0('length', stage))

    } else {
        stop('Model class must be either FlowerModel or PlantModel.')
    }

    #what is the type of model (DT, TTT, or dual)
    modtype <- modeltype(PL)
    simple <- simplified(PL)

    #is the threshold estimated
    if (!Threshold[listindex]) { #Mclass=Flowermodel, DT2,4-5; TTT1-3
        th <- threshold(PL)[stage]

    } else {#Mclass=Flowermodel, DT1, 3-4
        th <- Threshold[listindex]
    }

    if (is.na(th)) {
        stop('the variable th cannot be NA at this point. Something went wrong')
    }


    #is the start day estimated
    if (!Start[listindex]) { #FlowerModel: DT5, TTT3
        s <- startday(PL)[stage]

    } else {
        s <- Start[listindex]
    }


    #are the cardinal temps estimated
    if (!CT[listindex]) {
        ct <- cardinaltemps(PL)[[stage]]
    } else {
        ct <- CT[listindex]
    }

    #create data.frame with only the columns necessary
    fdat <- phenology[, fnames]

    #create a function that evaluates returns the rmse of the model that can be
    #minimized using the function DEoptim()
    fun <- function(x) {
        return(minrmse(x, fdat=fdat, tdat=temp, modtype=modeltype(PL),
                       form=form(PL), stage=stage,
                       CT=ct, S=s, TH=th, simple=simple,
                       varying=varyingpars(PL),
                       modclass = mclass(PL), firstevent=events[1]))
    }

    return(fun)
}



