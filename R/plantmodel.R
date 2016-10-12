#' @include constructors.R minrmse.R plantmodelcheck.R

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
#' @param lbounds numeric, a vector of lower bounds for the parameters in the
#'     model
#' @param ubounds numeric, a vector of the upper bounds for the parameters in
#'     the model.
#' @param estimateCT logical, should the cardinal temperatures be estimated?
#' @param estimatelength logical, should the accumulation length or threshold be
#'     estimated?
#' @param simplified logical, should the simplified version of the model be run.
plantmodel <- function(phenology, temperature, model, parameters, lbounds,
                       ubounds, cores=1L, estimateCT=TRUE,
                       estimatelength=TRUE, simplified=FALSE) {

    stages <- stages(parameters)
    n <- stages+1
    events <- paste0('event', 1:n)
    lengthcols <- paste0('length',1:stages)



    pdat <- phenology[, c('year', events)]

    ldat <- ldply(1:stages, function(i) eventi(pdat,i+1) - eventi(pdat, i))
    names(ldat) <- lengthcols

    d <- data.frame(pdat, ldat)


    if (model=='thermal' & simplified) {

        lmlist <- lapply(1:stages, function(i) {
            fmla <- paste0(lengthcols[i]," ~ 1")
            lm(fmla, data=d)
        })

        errorvec <- sapply(1:stages, function(i) {
            rmsd(fitted(lmlist[[i]]), d[,lengthcols[i]])



            break
        })

    }

    ttform <- form(parameters)

    if (boundlength(ttform, estimateCT, estimatelength)!=lbounds) {
        stop(paste0('The bounds have the wrong number of parameter values. ',
                   'Your bounds are of length', length(lbounds),
                   ', and they should be of length',
                   boundlength(ttform, estimateCT, estimatelength), '.'))
    }


    functionlist <- lapply(1:stages, function(i) {
        objective(p, i, estimateCT, estimatelength)
    })

    if (stages > 1 & cores > 1) {

        optimlist <- mclapply(1:length(functionlist), function(i) {
            DEoptim(functionlist[[i]], lower=lbounds,upper=ubounds)$optim
        }, mc.cores=cores)

    } else {
        optimlist <- lapply(1:length(functionlist), function(i) {
            DEoptim(functionlist[[i]], lower=lbounds, upper=ubounds)$optim
        })
    }

    return(optimlist)

    if (estimateCT & estimatelength) {
        newlength <-
    }

    plist <- parameterlist()


    pm <- new('PlantModel',
              parameters=,
              error=,
              form=,
              modeltype=,
              )


}
