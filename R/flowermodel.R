#' @include constructors.R modelcheck.R objectivefunction.R flowermodelmethods.R extraction.R
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
#' @return A FlowerModel object.
#' @export
flowermodel <- function(phenology, temps, parlist, lbounds, ubounds,
                       cores=1L, iterations=200) {


# Part 1: Assembling Model Inputs -----------------------------------------


   stages <- 1 #Number of stages in FlowerModel
    n <- stages+1 #number of events
    m <- length(parlist)
    events <- paste0('event', 0:1) #event column names

    #start vector
    start <- sapply(parlist, function(pl) startday(pl))

    #model threshold vector
    thresh <- sapply(parlist, function(pl) threshold(pl))

    #which pars will vary
    vp <- sapply(parlist, function(pl) varyingpars(pl))

    #are the models simplified
    simple <- simplified(parlist[[1]])

    mtype <- modeltype(parlist[[1]]) #model type TTT or DT
    ttform <- sapply(parlist, function(pl) form(pl)) #functional form

    #extracting the appropriate form names

    #Checking to make sure all of the right variables and etc are present
    checktemps(temps, phenology, ttform, 'FlowerModel')

    #checking to make sure there are no NAs in the flowering dates
    if (any(is.na(phenology[,'event1']))) {
        stop('There are NAs in the flowering event column (event1).')
    }

    for (i in 1:m) {
        if (ttform[i]=='anderson') {
            cardinaltemps(parlist[[i]]) <-list(c(4,25,36))
        }
    }

    #detects if you are estimating the cardinal temperatures
    estimateCT <- sapply(parlist, function(pl) {
        if (!('cardinaltemps' %in% parsOptimized(pl))) {
            FALSE
        } else if (form(pl)=='anderson') FALSE else TRUE
    })


   #for which models are you estimating /threshold
    estimatethresh <- sapply(parlist, function(pl) {
        if ('threshold' %in% parsOptimized(pl)) TRUE else FALSE
    })

    #is the end day being estimated?
    estimatestart <- sapply(parlist, function(pl) {
        if ('start' %in% parsOptimized(pl)) TRUE else FALSE
    })


   badstart <- ifelse(estimatestart & start==0, TRUE, FALSE)

    if (any(badstart)) {
        stop('If you want to opimize the start day you cannot set the start day to be the harvest readiness date from the previous year.')}

    badthresh <- ifelse(estimatethresh & is.na(thresh), TRUE, FALSE)

    if (any(badthresh)) {
        stop('If you want to opimize the threshold you cannot run the base model (threshold=NA).')
    }


    #phenology data with only the year and event data.
    d <- phenology[, c('year', events)]



# Part 2: Running model optimization --------------------------------------


    #average flowering day model
    if (mtype=='DT' & simple) {

        if ('start' %in% vp[1]) {
            leap <- ifelse(is.leapyear(d$year), 366, 365)

            d$length0 <- ifelse(d$event0>d$event1,
                                leap-d$event0 + d$event1,
                                d$event1 - d$event0)

            modlist <- lm(length0 ~ 1, data=d)

            for (i in 1:m) {
                varyingpars(parlist[[i]]) <- c('start','threshold')
            }


        } else {
            #average flowering day lm
            modlist <- lm(event1 ~ 1, data=d)

            for (i in 1:m) {
                varyingpars(parlist[[i]]) <- NA
            }


        }



        #extracting fitted data
        fits <- round(unname(fitted(modlist)))

        if (start %in% vp[1]) {
            newstart <- rep(0, m)
        } else {
            newstart <- rep(1, m)
        }


        #getting the new average bloom dates of the fits
        newthresh <- rep(round(unname(coef(modlist)[1])), m)

    } else { #everything but the DT simple model

        blen <- boundlength(ttform, estimateCT, estimatestart, estimatethresh)

        #do your bounds have enough parameters?
        if (blen!=length(lbounds)) {
          stop(paste0('The bounds have the wrong number of parameter values. ',
                        'Your bounds are of length ', length(lbounds),
                        ', and they should be of length ', blen, '.'))
        }



        functionlist <- lapply(1:m, function(i) {
            objective(parlist, d, temps, 1, estimateCT,
                      estimatestart, estimatethresh, i,
                      'FlowerModel')
        })


        #creating the appropriate length bounds for each thermal time form
        lboundlist <- lapply(1:m, function(i) {
            bndlen <- boundlength(ttform[i], estimateCT[i], estimatestart[i],
                                  estimatethresh[i])

            lbounds[1:bndlen]
        })


        uboundlist <- lapply(1:m, function(i) {
            bndlen <- boundlength(ttform[i], estimateCT[i], estimatestart[i],
                                  estimatethresh[i])

            ubounds[1:bndlen]
        })

        #optimizing the parameters
        #parameter order: Start Threshold Cardinaltemps



        #optimizing the parameters
        if (cores > 1) {

            optimlist <- mclapply(1:m, function(i) {
                DEoptim(functionlist[[i]], lower=lboundlist[[i]],
                    upper=uboundlist[[i]],
                    control=DEoptim.control(itermax=iterations,
                                            trace=FALSE))$optim
            }, mc.cores=cores)

        } else {

            optimlist <- lapply(1:m, function(i) {
                DEoptim(functionlist[[i]], lower=lboundlist[[i]],
                        upper=uboundlist[[i]],
                        control=DEoptim.control(itermax=iterations,
                                                trace=FALSE))$optim
                })
        }


        #print(2)

# Part 3: extract optimized parameters ------------------------------------
        #print(estimatestart)
        #print(optimlist)

        #extracting those parameters
        estimatelist <- list(estimatestart, estimatethresh, estimateCT)

        newstart <- extractParameters(estimatelist, 'start', parlist,
                                      optimlist)

        newthresh <- extractParameters(estimatelist, 'threshold', parlist,
                                       optimlist)

        newct <- extractParameters(estimatelist, 'cardinaltemps', parlist,
                                   optimlist)

        #creating predictors for stage length based on the parameters

        #print(3)
        predictornames <- lapply(1:m, function(i) {
            paste0(mtype, ttform[i])
        })

        predictors <- as.data.frame(sapply(1:m, function(i) {
            thermalsum(newct[[i]], d$year, temps, mtype, ttform[i],
                       newstart[i], newthresh[i], vp, 'FlowerModel', d$event0)
        }))


        d2 <- cbind(d, predictors)
        nc2 <- ncol(d2)

        names(d2)[(nc2-m+1):nc2] <- predictornames

    } #this closes everything but the DT simple model


# Part 3: Run Model -------------------------------------------------------


    #print(4)

    if (!simple) {
        #print(d2[,predictornames])
       # print(4.1)
        #predicting event one based on
        modlist <- lapply(1:m, function(i) {
            f <- formula(paste('event1', ' ~ ', predictornames[i] ))
            lm(f, data=d2)
        })

       # print(4.2)
        fits <- as.data.frame(sapply(modlist, fitted))

       # print(4.3)
    } else if (simple & mtype=='TTT') {

        #creates dummy data to force the creation of a linear model with
        #beta=1 and alpha=0

        modlist <- lapply(predictornames, function(pname) {
            dat <- data.frame(y=1:10, x=1:10)
            names(dat) <- c('event1', pname)
            f <- formula(paste('event1', ' ~ ', pname))
            lm(f, data=dat)
        })


        fits <- predictors

    }

    #print(5)

    #adding the fitted data to the dataframe
    if (exists('d2')) {
        d3 <- cbind(d2, fits)
    } else {
        d3 <- cbind(d, fits)
    }


    #giving the fitted data a name

    if (mtype=='DT' & simple) {
        fitname <- 'DTsimple'
        names(d3)[ncol(d3)] <- fitname

    } else {
        fitname <- sapply(1:m, function(i) {
            paste0('fit', mtype, ttform[i])
        })

        nc3 <- ncol(d3)
        names(d3)[(nc3-m+1):nc3] <- fitname

    }



    #print(fits)


    rmsd <- sapply(fitname, function(fname) {
        rmse(d3[,fname], d3[,'event1'])
    })

    #print(7)
    DEparameters <- parlist

    #print(7.1)

    for (i in 1:m) {
        if ((!simple) | (mtype=='TTT')) {
            # print(7.2)
            if (!is.list(newct[[i]])) {
                newct[[i]] <- list(newct[[i]])
            }

            cardinaltemps(DEparameters[[i]]) <- newct[[i]]
        }

        #print(7.3)
        startday(DEparameters[[i]]) <- newstart[i]
        threshold(DEparameters[[i]]) <- newthresh[i]

    }

    if (!(class(modlist)=='list')) {
        modlist <- list(modlist)
    }

    if (length(modlist)==1) {
        modlist <- replicate(m, modlist[[1]], simplify = FALSE)
    }

    #print(8)
    fm <- new('FlowerModel',
              parameters=DEparameters,
              error=rmsd,
              phenology=d3,
              olm=modlist,
              crossvalidated=FALSE)

    return(fm)

}
