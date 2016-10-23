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
#' @param parlist list of ParameterLists, contains the parameter values and
#'     functional form of the thermal time calculations. Note as of right now.
#'     All ParameterLists must have the same number of stages.
#' @param lbounds numeric, a vector of lower bounds for the parameters in the
#'     model
#' @param ubounds numeric, a vector of the upper bounds for the parameters in
#'     the model.
#' @param cores integer, if using parallel processing, how many cores should R
#'     use to fit the model.
#' @param iterations numeric, the number of iterations used in the differential
#'     evolution optimization of the phenological parameters.
#' @param small logical, if small empty dataframes will be returned.
#' @return A PlantModel object.
#' @export
plantmodel <- function(phenology, temps, parlist, lbounds, ubounds,
                       cores=1L, iterations=200, small=FALSE) {

    hforms <- c('linear','flat','anderson','triangle','asymcur')

    stages <- stages(parlist[[1]])
    n <- stages+1
    events <- paste0('event', 1:n)
    lengthcols <- paste0('length',1:stages)
    simple <- sapply(parlist, function(pl) simplified(pl))
    ttforms <- lapply(parlist, function(pl) form(pl))
    m <- length(parlist)

    for (i in 1:m) {
        for (j in 1:stages) {
            if (ttforms[[i]][j]=='anderson') {
                suppressWarnings(cardinaltemps(parlist[[i]])[[j]] <- c(4,25,36))
        }

        }
    }




    estimateCT <- sapply(1:m, function(i) {
        if ('cardinaltemps' %in% parsOptimized(parlist[[i]])) {

            if ('anderson' %in% ttforms[[i]]) {
               FALSE
            } else {
               TRUE
            }

        } else {
            FALSE
        }
    })



    estimatelength <- sapply(parlist, function(pl) {
        if ('modlength' %in% parsOptimized(pl)) TRUE else FALSE
    })


    pdat <- phenology[, c('year', events)]

    ldat <- as.data.frame(sapply(1:stages, function(i) {
        eventi(pdat,i+1) - eventi(pdat, i)
        }))

    names(ldat) <- lengthcols

    d <- data.frame(pdat, ldat)


    if (modeltype(parlist[[1]])=='thermal' & simple[[1]]) {

        lmlist <- lapply(1:stages, function(i) {
            fmla <- paste0(lengthcols[i]," ~ 1")
            lm(fmla, data=d)
        })

        fits <- as.data.frame(sapply(lmlist, function(mod) {
            fitted(mod)
        }))

    } else {

        if (boundlength(ttforms, estimateCT, estimatelength)!=length(lbounds)) {
            stop(paste0('The bounds have the wrong number of parameter values. ',
                        'Your bounds are of length ', length(lbounds),
                        ', and they should be of length ',
                        boundlength(ttforms, estimateCT, estimatelength), '.'))
        }

        if ('gdd' %in% unlist(ttforms) | 'gddsimple' %in% unlist(ttforms)) {
            daytemps <- unique(temps[,c('year','day','tmin','tmax')])
            daytemplist <- extracttemp(daytemps, d$year, 1, 365)
        }

        if (ifelse(any(unlist(ttforms) %in% hforms), TRUE, FALSE)) {
             hourtempslist <- extracttemp(temps, d$year, 1, 365)
        }


        functionlist <- lapply(1:m, function(j) {
                lapply(1:stages, function(i) {
                    if (ttforms[[j]][i] %in% c('gdd','gddsimple')){
                        tl <- daytemplist
                    } else {
                        tl <- hourtemplist
                    }

                    objective(parlist, d, tl, i, estimateCT,
                          estimatelength, simple, j)
            })
        })

        lboundlist <- lapply(1:m, function(i) {
            lapply(1:length(ttforms[[i]]), function(j) {
                bndlen <- boundlength(ttforms[[i]][j], estimateCT[i],
                                      estimatelength[i])

                lbounds[1:bndlen]
            })
        })

        uboundlist <- lapply(1:m, function(i) {
            lapply(1:length(ttforms[[i]]), function(j) {
                bndlen <- boundlength(ttforms[[i]][j], estimateCT[i],
                                      estimatelength[i])

                ubounds[1:bndlen]
            })
        })

        #optimizing the parameters
        if (cores > 1) {

            optimlist <- mclapply(1:m, function(i) {
                lapply(1:stages, function(j) {
                   DEoptim(functionlist[[i]][[j]], lower=lbounds[[i]][[j]],
                           upper=ubounds[[i]][[j]],
                           control=DEoptim.control(itermax=iterations,
                                                   trace=FALSE))$optim
                })
            }, mc.cores=cores)

        } else {

            optimlist <- lapply(1:m, function(i) {
                lapply(1:stages, function(j) {
                    DEoptim(functionlist[[i]][[j]], lower=lbounds[[i]][[j]],
                            upper=ubounds[[i]][[j]],
                            control=DEoptim.control(itermax=iterations,
                                                    trace=FALSE))$optim
                })
            })
        }

        #extracting those parameters
        newlength <- lapply(1:m, function(i) {
            if (estimatelength[i]) {
                sapply(optimlist[[i]], function(ol) {
                    unname(ol[["bestmem"]][1])
                })
            } else {
                modlength(parlist[[i]])
            }
        })



        newct <- lapply(1:m, function(i) {
            if (estimatelength[i] & estimateCT[i]) {

            }
        })
        if (estimateCT & estimatelength) {
            newct <- lapply(optimlist, function(ol) {
                unname(ol[['bestmem']])[-1]
                })

        } else if (estimateCT & !estimatelength) {
            newlength <- modlength(parlist)
            newct <- lapply(optimlist, function(ol) {
                unname(ol[['bestmem']])
                       })

        } else {
            newlength <- sapply(optimlist, function(ol) {
                unname(ol[['bestmem']])[1]
                })
            newct <- cardinaltemps(parlist)

        }


        #creating predictors for stage length based on the parameters
        predictornames <- lapply(1:length(parlist), function(i) {
            sapply(1:stages, function(j) {
                paste0(modeltype(parlist[[i]]), ttform[[i]][j], j)
            })
        })

        ij <- expand.grid(1:length(parlist), 1:stages)

        predictors <- as.data.frame(sapply(1:nrow(ij), function(i) {
            s <- ij[i, 2]
            fm <- ij[i, 1]
            thermalsum(newct[[fm]][[s]])
        }))
        predictors <- as.data.frame(sapply(1:stages, function(i) {
            thermalsum(newct[[i]], d, tempslist, modeltype(parlist),
                       ttform, newlength[i], i)
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

    } else if (simple & modeltype(parlist)=='day') {
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

    if (small) {
        temps <- temps[1:5,]
    }

    pm <- new('PlantModel',
              parameters=DEparameters,
              error=rmse,
              phenology=d3,
              temperature=temps,
              olm=lmlist,
              crossvalidated=FALSE)

    return(pm)

}
