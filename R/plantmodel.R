#' @include constructors.R minrmse.R plantmodelcheck.R
NULL

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
#' @param cores integer, if using parallel processing, how many cores should R
#'     use to fit the model.
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

        fits <- ldply(lmlist, function(mod) {
            fitted(mod)
        })

    } else {
        ttform <- form(parameters)

        if (boundlength(ttform, estimateCT, estimatelength)!=lbounds) {
            stop(paste0('The bounds have the wrong number of parameter values. ',
                        'Your bounds are of length', length(lbounds),
                        ', and they should be of length',
                        boundlength(ttform, estimateCT, estimatelength), '.'))
        }


        functionlist <- lapply(1:stages, function(i) {
            objective(parameters, i, estimateCT, estimatelength)
        })


        #optimizing the parameters
        if (stages > 1 & cores > 1) {

            optimlist <- mclapply(1:length(functionlist), function(i) {
                DEoptim(functionlist[[i]], lower=lbounds,upper=ubounds)$optim
            }, mc.cores=cores)

        } else {
            optimlist <- lapply(1:length(functionlist), function(i) {
                DEoptim(functionlist[[i]], lower=lbounds, upper=ubounds)$optim
            })
        }

        #extracting those parameters
        if (estimateCT & estimatelength) {
            newlength <- sapply(optimlist, function(ol) unname(ol[['bestmem']])[1])
            newct <- lapply(optimlist, function(ol) unname(ol[['bestmem']])[-1] )

        } else if (estimateCT & !estimatelength) {
            newlength <- modlength(parameters)
            newct <- lapply(optimlist, function(ol) unname(ol[['bestmem']]))

        } else {
            newlength <- sapply(optimlist, function(ol) unname(ol[['bestmem']])[1])
            newct <- cardinaltemps(parameters)
        }

        #creating predictors for stage length based on the parameters
        predictornames <- paste0(modeltype, 1:stages)
        predictors <- ldply(1:stages, function(i) {
            thermalsum(newct, phenology, temperature, modeltype, ttform,
                       newlength, i)
        })

        names(predictors) <- predictornames
        d2 <- cbind(d, predictors)

    }

    if (!simplified) {

        lmlist <- lapply(1:stages, function(i) {
            f <- formula(paste(paste0('length',i), ' ~ ', predictornames[i] ))
            lm(f, data=d2)
        })

        fits <- ldply(lmlist, function(mod) {
            fitted(mod)
        })

    } else if (simplified & modeltype=='da') {
        lmlist <- list(NA)
        fits <- predictors
    }

    names(fits) <- paste0('fitstage', 1:stages)
    d3 <- cbind(d2, fits)

    rmse <- sapply(1:stages, function(i) {
       fit <- paste0('fitstage', i)
        observed <- paste0('length',i)
        rmsd(d3[,fit], d3[,observed])
    })

    DEparameters <- parameters
    modlength(DEparameters) <- newlength
    cardinaltemps(DEparameters) <- newct

    pm <- new('PlantModel',
              parameters=DEparameters,
              error=rmse,
              phenology=d3,
              temperature=temperature,
              olm=lmlist)

    return(pm)

}
