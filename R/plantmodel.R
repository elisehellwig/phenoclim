#' @include constructors.R minrmse.R

# This document contains the function that fits phenological models for Plant
#     objects.


#' Fits phenological models
#'
#' `plantmodel` fits phenological models of two general types: thermal time accumulation and day accumulation (for more information see ____).
#'
#' @param phenology data.frame, contains the phenological data.
#'     Required columns are `year`, and `event1` through `eventN` where `N`
#'     is the number of phenological events in the model.
#' @param temperature list, contains the temperature data. Each element of
#'     the list should be either a vector of hourly temperatures or a
#'     data.frame of minimum and maximum daily temperatures. Each element
#'     of the list should be named with the corresponding year.
#' @param model character, specifies the type of model. Options are 'tta'
#'     (thermal time accumulation) or 'da' (day accumulation).
#' @param parameters ParameterList, contains the parameter values and
#'     functional form of the thermal time calculations.
plantmodel <- function(phenology, temperature, model, parameters, lbounds,
                       ubounds, stages=1, cores=1L, estimateCT=TRUE,
                       estimatelength=TRUE, full=FALSE) {

    n <- stages+1
    events <- paste0('event', 1:n)
    lengthcols <- paste0('length',1:stages)

    pdat <- phenology[, c('year', events)]

    ldat <- ldply(1:stages, function(i) eventi(pdat,i+1) - eventi(pdat, i))
    names(ldat) <- lengthcols

    d <- data.frame(pdat, ldat)


    if (model=='thermal' & FULL) {

    }

    if (!estimateCT & !estimatelength) {
        stop('You at least estimate either the cardinal temperatures or the model length, though you can estimate both.')
    }

    if (lbounds != ubounds) {
        stop('The lower and upper bound vectors must have the same number of parameters')
    }

    ttform <- form(p)

    if (boundlength(ttform, estimateCT, estimatelength)!=lbounds) {
        stop(paste0('The bounds have the wrong number of parameter values. ',
                   'Your bounds are of length', length(lbounds),
                   ', and they should be of length',
                   boundlength(ttform, estimateCT, estimatelength), '.'))
    }

    n <- stages(p)

    functionlist <- lapply(1:n, function(i) {
        objective(p, i, estimateCT, estimatelength)
    })

    if (n > 1 & cores > 1) {

        optimlist <- mclapply(1:length(functionlist), function(i) {
            DEoptim(functionlist[[i]], lower=lbounds,upper=ubounds)$optim
        }, mc.cores=cores)

    } else {
        optimlist <- lapply(1:length(functionlist), function(i) {
            DEoptim(functionlist[[i]], lower=lbounds, upper=ubounds)$optim
        })
    }

    return(optimlist)



    pm <- new('PlantModel',
              parameters=,
              error=,
              form=,
              modeltype=,
              )


}