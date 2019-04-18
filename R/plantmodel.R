#' @include constructors.R modelcheck.R objectivefunction.R plantmodelmethods.R
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



# Part 1: Assembling model inputs -----------------------------------------


    stages <- stages(parlist[[1]]) #extract # of stages from the Parameterlist
    n <- stages+1 #number of events
    events <- paste0('event', 1:n) #event column names
    lengthcols <- paste0('length',1:stages) #length column names

    #start vector
    start <- sapply(parlist, function(pl) startday(pl))

    #model threshold vector
    thresh <- sapply(parlist, function(pl) threshold(pl))

    #which pars will vary
    vp <- sapply(parlist, function(pl) varyingpars(pl))

    #are the models simplified?
    simple <- sapply(parlist, function(pl) simplified(pl))

    #model type TTT or DT
    mtype <- modeltype(parlist[[1]])

    ttforms <- sapply(parlist, function(pl) form(pl)) #list of functional forms
    m <- length(parlist) #number of functional forms


    ij <- expand.grid(1:stages, 1:m) #unique combos of form numbers and stages
    names(ij) <- c('stage','pl') #giving columns names

    #extracting the appropriate form names
    ij$form <- sapply(1:nrow(ij), function(i) {
        ttforms[ij$pl[i]]
    })

    #Checking to make sure all of the right variables and etc are present
    checktemps(temps, phenology, ttforms, 'PlantModel')

    for (i in 1:m) {
        for (j in 1:stages) {
            if (ttforms[i]=='anderson') {
                cardinaltemps(parlist[[i]])[[j]] <- c(4,25,36)
        }

        }
    }



    #detects if you are estimating the cardinal temperatures
    estimateCT <- sapply(1:m, function(i) {

        #are you estimating cardinal temperatures?
        if ('cardinaltemps' %in% parsOptimized(parlist[[i]])) {

            if ('anderson' %in% ttforms[i]) { #is the form anderson?
               FALSE #if the form is anderson, you don't estimate cardinal temps
            } else {
               TRUE #otherwise estimateCT is true
            }

        } else {
            FALSE
        }
    })

    #for which models are you estimating the start day?
    estimatestart <- sapply(parlist, function(pl) {
        if ('start' %in% parsOptimized(pl)) TRUE else FALSE
    })

   #for which models are you estimating model length/threshold
    estimatethresh <- sapply(parlist, function(pl) {
        if ('threshold' %in% parsOptimized(pl)) TRUE else FALSE
    })


    #phenology data with only the year and event data.
    pdat <- phenology[, c('year', events)]

    #data frame with the length of the stages
    ldat <- as.data.frame(sapply(1:stages, function(i) {
        eventi(pdat, i+1) - eventi(pdat, i)
        }))

    #naming the stage length columns
    names(ldat) <- lengthcols

    #joining the event and length data frames
    d <- data.frame(pdat, ldat)

    #checkign to to make sure that thresh is not both set and also optimized
    badthresh <- ifelse(estimatethresh & is.na(thresh), TRUE, FALSE)

    if (any(badthresh)) {
        stop('If you want to opimize the threshold you cannot run the base model (threshold=NA).')
    }



# Part 2: Running model optimization --------------------------------------


    if (modeltype(parlist[[1]])=='DT' & simple[1]) { #average stage length model

        lmlist <- lapply(1:stages, function(i) { #average stage length lm
            fmla <- paste0(lengthcols[i]," ~ 1")
            lm(fmla, data=d)
        })

        #extracting fitted data
        fits <- lapply(1:length(lmlist), function(i) {
            as.data.frame(sapply(1:length(ttforms), function(j) {
                fitted(lmlist[[i]])
            }))
        })[[1]]

        newlength <- lapply(1:m, function(i) { #getting the new average
            fits[1,1]                           #season length of the fits
        })

    } else {

        blen <- boundlength(ttforms, estimateCT, estimatestart, estimatethresh)

        if (blen!=length(lbounds)) {
            stop(paste0('The bounds have the wrong number of parameter values. ',
                        'Your bounds are of length ', length(lbounds),
                        ', and they should be of length ', blen, '.'))
        }


        functionlist <- lapply(1:m, function(j) {
                lapply(1:stages, function(i) {
                    objective(parlist, d, temps, i, estimateCT,
                              estimatestart, estimatethresh, j,
                              'PlantModel')
            })
        })

        lboundlist <- lapply(1:m, function(i) {
            bndlen <- boundlength(ttforms[i], estimateCT[i], estimatestart[i],
                                  estimatethresh[i])

            lbounds[1:bndlen]
        })

        uboundlist <- lapply(1:m, function(i) {
            bndlen <- boundlength(ttforms[i], estimateCT[i], estimatestart[i],
                                  estimatethresh[i])

            ubounds[1:bndlen]
        })


        #optimizing the parameters
        if (cores > 1) {

            optimlist <- mclapply(1:m, function(i) {
                lapply(1:stages, function(j) {
                   DEoptim(functionlist[[i]][[j]], lower=lboundlist[[i]],
                           upper=uboundlist[[i]],
                           control=DEoptim.control(itermax=iterations,
                                                   trace=FALSE))$optim
                })
            }, mc.cores=cores)

        } else {

            optimlist <- lapply(1:m, function(i) {
                lapply(1:stages, function(j) {
                    DEoptim(functionlist[[i]][[j]], lower=lboundlist[[i]],
                            upper=uboundlist[[i]],
                            control=DEoptim.control(itermax=iterations,
                                                    trace=FALSE))$optim
                })
            })
        }

        #print(2)

# Part 3: Extracting Optimized Parameters ---------------------------------

        #Stages are the rows and functional forms are the columns of the matrix:
        #
        #        linear  GDD
        #stage 1    ?     ?
        #stage 2    ?     ?

        estimatelist <- list(estimatestart, estimatethresh, estimateCT)

        newstart <- extractParameters(estimatelist, 'start', parlist,
                                      optimlist)

        newthresh <- extractParameters(estimatelist, 'threshold', parlist,
                                       optimlist)

        newct <- extractParameters(estimatelist, 'cardinaltemps', parlist,
                                   optimlist)

        #creating predictors for stage length based on the parameters

       # print(3)
        predictornames <- lapply(1:m, function(i) {
            sapply(1:stages, function(j) {
                paste0(modeltype(parlist[[i]]), ttforms[i], j)
            })
        })



        predictors <- as.data.frame(sapply(1:nrow(ij), function(i) {
            s <- ij[i, 'stage']
            fm <- ij[i, 'pl']
            thermalsum(ctemps=newct[[fm]][[s]], yrs=d$year,
                       tdat=temps, modtype=modeltype(parlist[[fm]]),
                       form=ij[i,'form'], start=newstart[s, fm],
                       thresh=newthresh[s, fm], mclass='PlantModel',
                       startingevent=d$event1,
                       varying=varyingpars(parlist[[fm]]))
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


    if (exists('d2')) {
        d3 <- cbind(d2, fits)
    } else {
        d3 <- cbind(d, fits)
    }

    #print(fits)

    fitnames <- sapply(1:m, function(i) {
        sapply(1:stages, function(j){
            paste0('fit', modeltype(parlist[[i]]),
                   ttforms[i],j)
        })
    })

    fitnames <- convertToMatrix(fitnames, stages, m)


    rmsd <- sapply(1:m, function(i) {
        sapply(1:stages, function(j) {
            observed <- paste0('length',j)
            rmse(d3[,fitnames[j,i]], d3[,observed])
        })
    })


    #print(6)



    #print(7)
    DEparameters <- parlist

    #print(DEparameters)

    for (i in 1:m) {
        startday(DEparameters[[i]]) <- newstart[,i]
        threshold(DEparameters[[i]]) <- newthresh[,i]

        if ((!simple[i]) | (modeltype(parlist[[i]])=='TTT')) {
            #print(newct[[i]])
            for (j in 1:stages) {
                if (!is.list(newct[[i]][[j]])) {
                    newCTlist <- list(newct[[i]][[j]])
                } else {
                    newCTlist <- newct[[i]][[j]]
                }
                cardinaltemps(DEparameters[[i]]) <- newCTlist # issue
            }

         }
    }


    #print(8)
    pm <- new('PlantModel',
              parameters=DEparameters,
              error=rmsd,
              phenology=d3,
              olm=lmlist,
              crossvalidated=FALSE)

    return(pm)

}
