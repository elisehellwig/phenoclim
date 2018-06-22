#' @include constructors.R modelcheck.R objectivefunction.R flowermodelmethods.R
NULL

# This document contains the function that fits phenological models for Plant
#     objects.


#' Fits phenological models
#'
#' `flowermodel` fits phenological models of two general types: thermal time accumulation and day accumulation (for more information see ____).
#'
#' @param phenology data.frame, contains the phenological data.
#'     Required columns are `year`, `event1` and `event0` which represents the
#'     harvest date from the previous year.
#' @param temps data.frame, contains the temperature data. There should be a
#'     column for year, day of the year, hourly temperature (named temp) or min
#'     max daily temperatures (named tmin and tmax), and hour if the
#'     temperatures are hourly. Must have data from one year before you have
#'     phenology data
#' @param parlist ParameterList, contains the parameter values and
#'     functional form of the thermal time calculations.
#' @param lbounds numeric, a vector of lower bounds for the parameters in the
#'     model
#' @param ubounds numeric, a vector of the upper bounds for the parameters in
#'     the model.
#' @param cores integer, if using parallel processing, how many cores should R
#'     use to fit the model.
#' @param iterations numeric, the number of iterations used in the differential
#'     evolution optimization of the phenological parameters.
#' @param ensemble logical, should an ensemble prediction be used? NOT
#'     currently implemented.
#' @return A PlantModel object.
#' @export
flowermodel <- function(phenology, temps, parlist, lbounds, ubounds,
                       cores=1L, iterations=200, ensemble=FALSE) {


    parlist <- parlist[[1]]
    stages <- 1 #Number of stages in FlowerModel
    n <- stages+1 #number of events
    events <- paste0('event', 0:1) #event column names

    start <- startday(parlist) #start vector
    thresh <- threshold(parlist) #model threshold
    vp <- varyingpars(parlist) #which pars will vary

    if (is.na(thresh)) { #is the model simplified
        simple <- TRUE
    }

    mtype <- modeltype(parlist) #model type TTT or DT
    ttform <- form(parlist) #functional form

    #extracting the appropriate form names

    #Checking to make sure all of the right variables and etc are present
    checktemps(temps, phenology, ttform)

    if (ttform=='anderson') {
        cardinaltemps(parlist) <-list(c(4,25,36))
    }



    #detects if you are estimating the cardinal temperatures

    #are you estimating cardinal temperatures?
    if ('cardinaltemps' %in% parsOptimized(parlist)) {

        if ('anderson' %in% ttform) { #is the form anderson?
            estimateCT <- FALSE #if the form is anderson, you don't estimate cardinal temps
        } else {
            estimateCT <- TRUE #otherwise estimateCT is true
        }

    } else {
        estimateCT <- FALSE
    }



   #for which models are you estimating /threshold
    estimatethresh<-ifelse('threshold' %in% parsOptimized(parlist), TRUE, FALSE)

    #is the end day being estimated?
    estimatestart <- ifelse('start' %in% parsOptimized(parlist), TRUE, FALSE)

    if (estimatestart & start==0) {
        stop('If you want to opimize the start day you cannot set the start day to be the harvest readiness date from the previous year.')
    }

    if (estimatethresh & is.na(Threshold)) {
        stop('If you want to opimize the threshold you cannot run the base model (threshold=NA).')
    }


    #phenology data with only the year and event data.
    d <- phenology[, c('year', events)]

    #average flowering day model
    if (modeltype(parlist)=='DT' & simple) {

        #average flowering day lm
        DTsimp <- lm(event1 ~ 1, data=d)

        #extracting fitted data
        fits <- round(unname(fitted(DTsimp)))

        #getting the new average bloom dates of the fits
        newbloom <- round(unname(coef(DTsimp)[1]))

    } else {

        blen <- boundlength(ttform, estimateCT, estimatestart, estimatethresh)


        #note this won't work for more than one stage
        if (blen!=length(lbounds)) {
            stop(paste0('The bounds have the wrong number of parameter values. ',
                        'Your bounds are of length ', length(lbounds),
                        ', and they should be of length ', blen, '.'))
        }

        #ive decided to wrap all of the temp stuff into the extract temps thing

        tempyears <- c(min(d$year)-1, d$year)
        extractedtemps <- extracttemplist(temps, tempyears, ttform, d$event0,
                                          'FlowerModel')
        daytemplist <- extractedtemps[[1]]
        hourtemplist <- extractedtemps[[2]]
        relevanttemplist <- whichtemp(ttform, daytemplist, hourtemplist)

###########################################################################
        #working in here
        #current task is working on updating TTTsum to work with the new
        #temperature data structure and indexing
        objfun <- objective(parlist, d, relevanttemplist, 1, estimateCT,
                            estimatestart, estimatethresh, simple, 1,
                            'FlowerModel')
#######Things are changed up to hear for FlowerModel and startday#######

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
    } else if (simple[1] & modeltype(parlist[[1]])=='TTT') {

        lmlist <- lapply(1:m, function(j) {
            lapply(1:stages, function(i) {
                dat <- data.frame(y=1:10, x=1:10)
                names(dat) <- c(paste0('length',i), predictornames[[j]][i])
                f <- formula(paste(paste0('length',i), ' ~ ',
                                   predictornames[[j]][i] ))
                lm(f, data=dat)
            })
        })

        fits <- predictors

    }

   # print(5)

    names(fits) <- sapply(1:nrow(ij), function(i) {
        paste0('fit', modeltype(parlist[[ij[i,'pl']]]), ij[i,'form'],
               ij[i,'stage'])
    })

    if (ensemble) {
        fits$fitensemble <- apply(fits, 1, mean)
    }



    if (exists('d2')) {
        d3 <- cbind(d2, fits)
    } else {
        d3 <- cbind(d, fits)
    }

    #print(fits)

    if (ensemble) {
        fitnames <- c(sapply(1:m, function(i) paste0('fit',
                                                     modeltype(parlist[[i]]),
                                                     ttforms[[i]][1],1)),
                      'fitensemble')

        rmse <- sapply(1:(m+1), function(i) {
            sapply(1:stages, function(j) {
                observed <- paste0('length',j)
                rmsd(d3[,fitnames[i]], d3[,observed])
            })
        })


    } else {
        fitnames <- sapply(1:m, function(i) paste0('fit',
                                                     modeltype(parlist[[i]]),
                                                     ttforms[[i]][1],1))

        rmse <- sapply(1:m, function(i) {
            sapply(1:stages, function(j) {
                observed <- paste0('length',j)
                rmsd(d3[,fitnames[i]], d3[,observed])
            })
        })
    }


    #print(6)



  #  print(7)
    DEparameters <- parlist


    for (i in 1:m) {
        modlength(DEparameters[[i]]) <- newlength[[i]]
        if ((!simple[1]) | (modeltype(parlist[[1]])=='TTT')) {
            cardinaltemps(DEparameters[[i]]) <- newct[[i]]
        }
    }

    if (ensemble) {
        DEparameters[[m+1]] <- parameterlist(n=stages,
                                             mt=modeltype(parlist[[1]]),
                                             simple=simple[1],
                                             ff='ensemble',
                                             ct=list(c(0,0,0)),
                                             length=0)
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
