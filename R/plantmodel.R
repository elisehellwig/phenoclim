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
#' @param temps data.frame, contains the temperature data. There should be a
#'     column for year, day of the year, hourly temperature (named temp) or min
#'     max daily temperatures (named tmin and tmax), and hour if the
#'     temperatures are hourly.
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
#' @return A PlantModel object.
#' @export
plantmodel <- function(phenology, temp, parlist, lbounds,
                       ubounds, cores=1L, estimateCT=TRUE,
                       estimatelength=TRUE, simple=FALSE) {

    stages <- stages(parlist)
    n <- stages+1
    events <- paste0('event', 1:n)
    lengthcols <- paste0('length',1:stages)



    pdat <- phenology[, c('year', events)]

    ldat <- as.data.frame(sapply(1:stages, function(i) {
        eventi(pdat,i+1) - eventi(pdat, i)
        }))

    names(ldat) <- lengthcols

    d <- data.frame(pdat, ldat)


    if (model=='thermal' & simple) {

        lmlist <- lapply(1:stages, function(i) {
            fmla <- paste0(lengthcols[i]," ~ 1")
            lm(fmla, data=d)
        })

        fits <- as.data.frame(sapply(lmlist, function(mod) {
            fitted(mod)
        }))

    } else {
        ttform <- form(parlist)

        if (boundlength(ttform, estimateCT, estimatelength)!=length(lbounds)) {
            stop(paste0('The bounds have the wrong number of parameter values. ',
                        'Your bounds are of length ', length(lbounds),
                        ', and they should be of length ',
                        boundlength(ttform, estimateCT, estimatelength), '.'))
        }


        functionlist <- lapply(1:stages, function(i) {
            objective(parameters, d, temps, i, estimateCT,
                      estimatelength, simple)
        })


        #optimizing the parameters
        if (cores > 1) {

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
            newlength <- modlength(parlist)
            newct <- lapply(optimlist, function(ol) unname(ol[['bestmem']]))

        } else {
            newlength <- sapply(optimlist, function(ol) unname(ol[['bestmem']])[1])
            newct <- cardinaltemps(parlist)
        }

        #creating predictors for stage length based on the parameters
        predictornames <- paste0(modeltype(parlist), 1:stages)
        tempslist <- extracttemp(temps, d$year, 1, 331)
        predictors <- as.data.frame(sapply(1:stages, function(i) {
            thermalsum(newct, d, tempslist, modeltype(parlist),
                       ttform, newlength, i)
        }))

        names(predictors) <- predictornames
        d2 <- cbind(d, predictors)

    }

    if (!simple) {

        lmlist <- lapply(1:stages, function(i) {
            f <- formula(paste(paste0('length',i), ' ~ ', predictornames[i] ))
            lm(f, data=d2)
        })

        fits <- as.data.frame(sapply(lmlist, function(mod) {
            fitted(mod)
        }))

    } else if (simple & modeltype=='da') {
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

    DEparameters <- parlist
    modlength(DEparameters) <- newlength
    cardinaltemps(DEparameters) <- newct

    pm <- new('PlantModel',
              parameters=DEparameters,
              error=rmse,
              phenology=d3,
              temperature=temps,
              olm=lmlist)

    return(pm)

}
