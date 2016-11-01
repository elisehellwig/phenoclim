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
#'     All ParameterLists must have the same number of stages, and the same
#'     model type.
#' @param lbounds numeric, a vector of lower bounds for the parameters in the
#'     model
#' @param ubounds numeric, a vector of the upper bounds for the parameters in
#'     the model.
#' @param cores integer, if using parallel processing, how many cores should R
#'     use to fit the model.
#' @param iterations numeric, the number of iterations used in the differential
#'     evolution optimization of the phenological parameters.
#' @return A PlantModel object.
#' @export
plantmodel <- function(phenology, temps, parlist, lbounds, ubounds,
                       cores=1L, iterations=200) {


    stages <- stages(parlist[[1]])
    n <- stages+1
    events <- paste0('event', 1:n)
    lengthcols <- paste0('length',1:stages)
    simple <- sapply(parlist, function(pl) simplified(pl))
    ttforms <- lapply(parlist, function(pl) form(pl))
    m <- length(parlist)

    checktemps(temps, phenology, ttforms)

    for (i in 1:m) {
        for (j in 1:stages) {
            if (ttforms[[i]][j]=='anderson') {
                cardinaltemps(parlist[[i]])[[j]] <- c(4,25,36)
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


   # print(1)
    estimatelength <- sapply(parlist, function(pl) {
        if ('modlength' %in% parsOptimized(pl)) TRUE else FALSE
    })


    pdat <- phenology[, c('year', events)]

    ldat <- as.data.frame(sapply(1:stages, function(i) {
        eventi(pdat,i+1) - eventi(pdat, i)
        }))

    names(ldat) <- lengthcols

    d <- data.frame(pdat, ldat)


    if (modeltype(parlist[[1]])=='thermal' & simple[1]) {

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


        extractedtemps <- extracttemplist(temps, pdat$year, ttforms)
        daytemplist <- extractedtemps[[1]]
        hourtemplist <- extractedtemps[[2]]


        functionlist <- lapply(1:m, function(j) {
                lapply(1:stages, function(i) {
                    objective(parlist, d, whichtemp(ttforms[[j]][i],daytemplist,
                                                    hourtemplist),
                              i, estimateCT,estimatelength, simple, j)
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
                   DEoptim(functionlist[[i]][[j]], lower=lboundlist[[i]][[j]],
                           upper=uboundlist[[i]][[j]],
                           control=DEoptim.control(itermax=iterations,
                                                   trace=FALSE))$optim
                })
            }, mc.cores=cores)

        } else {

            optimlist <- lapply(1:m, function(i) {
                lapply(1:stages, function(j) {
                    DEoptim(functionlist[[i]][[j]], lower=lboundlist[[i]][[j]],
                            upper=uboundlist[[i]][[j]],
                            control=DEoptim.control(itermax=iterations,
                                                    trace=FALSE))$optim
                })
            })
        }

        #print(2)

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
                lapply(optimlist[[i]], function(ol) {
                    unname(ol[['bestmem']])[-1]
                })
            } else if (estimateCT[i] & !estimatelength[i]) {
                lapply(optimlist[[i]], function(ol) {
                    unname(ol[['bestmem']])
                })
            } else {
                newct <- cardinaltemps(parlist[[i]])
            }
        })

        #creating predictors for stage length based on the parameters

       # print(3)
        predictornames <- lapply(1:m, function(i) {
            sapply(1:stages, function(j) {
                paste0(modeltype(parlist[[i]]), ttforms[[i]][j], j)
            })
        })

        ij <- expand.grid(1:stages, 1:m)
        names(ij) <- c('stage','pl')
        ij$form <- sapply(1:nrow(ij), function(i) {
            ttforms[[ij[i,2]]][ij[i,1]]
        })


        predictors <- as.data.frame(sapply(1:nrow(ij), function(i) {
            s <- ij[i, 1]
            fm <- ij[i, 2]
            thermalsum(newct[[fm]][[s]], d,
                       whichtemp(ij[i,'form'], daytemplist, hourtemplist),
                       modeltype(parlist[[fm]]),
                       ij[i,'form'],
                       newlength[[fm]][s],
                       s)
        }))

        names(predictors) <- unlist(predictornames)
        d2 <- cbind(d, predictors)

    }

    #print(4)

    if (!simple[1]) {

       # print(4.1)
        lmlist <- lapply(1:m, function(j) {
            lapply(1:stages, function(i) {
                f <- formula(paste(paste0('length',i), ' ~ ',
                                   predictornames[[j]][i] ))
                lm(f, data=d2)
            })
        })

       # print(4.2)
        fits <- as.data.frame(sapply(unlist(lmlist, recursive=FALSE),
                                     function(mod) fitted(mod)))

       # print(4.3)
    } else if (simple[1] & modeltype(parlist[[1]])=='day') {
        lmlist <- list(NA)
        fits <- predictors
    }

   # print(5)
    names(fits) <- sapply(1:nrow(ij), function(i) {
        paste0('fit', modeltype(parlist[[ij[i,'pl']]]), ij[i,'form'],
               ij[i,'stage'])
    })
    d3 <- cbind(d2, fits)


    #print(6)
    rmse <- sapply(1:m, function(i) {
        sapply(1:stages, function(j) {
            fit <- paste0('fit', modeltype(parlist[[i]]), ttforms[[i]][j],j)
            observed <- paste0('length',j)
            rmsd(d3[,fit], d3[,observed])
        })
    })


  #  print(7)
    DEparameters <- parlist

    for (i in 1:m) {
        modlength(DEparameters[[i]]) <- newlength[[i]]
        cardinaltemps(DEparameters[[i]]) <- newct[[i]]
    }

    #print(8)
    pm <- new('PlantModel',
              parameters=DEparameters,
              error=rmse,
              phenology=d3,
              olm=lmlist,
              crossvalidated=FALSE)

    return(pm)

}
