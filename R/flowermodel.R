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
#' @return A FlowerModel object.
#' @export
flowermodel <- function(phenology, temps, parlist, lbounds, ubounds,
                       cores=1L, iterations=200) {


    stages <- 1 #Number of stages in FlowerModel
    n <- stages+1 #number of events
    events <- paste0('event', 0:1) #event column names

    start <- startday(parlist) #start vector
    thresh <- threshold(parlist) #model threshold
    vp <- varyingpars(parlist) #which pars will vary

    simple <- simplified(parlist)

    mtype <- modeltype(parlist) #model type TTT or DT
    ttform <- form(parlist) #functional form

    #extracting the appropriate form names

    #Checking to make sure all of the right variables and etc are present
    checktemps(temps, phenology, ttform, 'FlowerModel')

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

    if (estimatethresh & is.na(thresh)) {
        stop('If you want to opimize the threshold you cannot run the base model (threshold=NA).')
    }


    #phenology data with only the year and event data.
    d <- phenology[, c('year', events)]

    #average flowering day model
    if (modeltype(parlist)=='DT' & simple) {

        #average flowering day lm
        mod <- lm(event1 ~ 1, data=d)

        #extracting fitted data
        fits <- round(unname(fitted(mod)))

        #getting the new average bloom dates of the fits
        newthreshold <- round(unname(coef(mod)[1]))

    } else { #everything but the DT simple model

        blen <- boundlength(ttform, estimateCT, estimatestart, estimatethresh)


        #note this won't work for more than one stage
        if (blen!=length(lbounds)) {
            stop(paste0('The bounds have the wrong number of parameter values. ',
                        'Your bounds are of length ', length(lbounds),
                        ', and they should be of length ', blen, '.'))
        }



        objfun <- objective(parlist, d, temps, 1, estimateCT,
                            estimatestart, estimatethresh, 1,
                            'FlowerModel')

        #optimizing the parameters
        #parameter order: Start Threshold Cardinaltemps
        optimmod <- DEoptim(objfun,
                            lower=lbounds,
                            upper=ubounds,
                            control=DEoptim.control(itermax=iterations,
                                                    trace=FALSE)
                            )$optim

        #print(2)

# Part 2: extract optimized parameters ------------------------------------

        #extracting those parameters

        if (estimatestart) {
            newstart <- unname(optimmod[["bestmem"]][1])

            if (estimatethresh) {
                newthreshold <- unname(optimmod[["bestmem"]][2])
            } else {
                newthreshold <- threshold(parlist)
            }

        } else {
            newstart <- startday(parlist)

            if (estimatethresh) {
                newthreshold <- unname(optimmod[["bestmem"]][1])
            } else {
                newthreshold <- threshold(parlist)
            }

        }

        npars <- sum(c(estimatestart, estimatethresh))

        if (estimateCT) {
            if (npars>0) {
                newct <- unname(optimmod[["bestmem"]][-(1:npars)])

            } else {
                newct <- unname(optimmod[["bestmem"]])
            }
        } else {
            newct <- cardinaltemps(parlist)
        }


        #creating predictors for stage length based on the parameters

        print(3)
        predictornames <- paste0(modeltype(parlist), ttform)

        predictors <- thermalsum(newct, d$year, temps, mtype, ttform, newstart,
                                 newthreshold, vp, 'FlowerModel', d$event0)

        d2 <- cbind(d, predictors)
        names(d2)[ncol(d2)] <- predictornames

    } #this closes everything but the DT simple model


# Part 3: Run Model -------------------------------------------------------


    print(4)

    if (!simple) {
        #print(d2[,predictornames])
       # print(4.1)
        #predicting event one based on
        f <- formula(paste0('event1', ' ~ ',  predictornames))
        mod <- lm(f, data=d2)

       # print(4.2)
        fits <- as.data.frame(fitted(mod))

       # print(4.3)
    } else if (simple & modeltype(parlist)=='TTT') {

        #creates dummy data to force the creation of a linear model with
        #beta=1 and alpha=0
        dat <- data.frame(y=1:10, x=1:10)
        names(dat) <- c('event1', predictornames)
        f <- formula(paste0('event1 ~ ',predictornames))

        #dummy model
        mod <- lm(f, data=dat)


        fits <- predictors

    }

    print(5)

    #adding the fitted data to the dataframe
    if (exists('d2')) {
        d3 <- cbind(d2, fits)
    } else {
        d3 <- cbind(d, fits)
    }

    #giving the fitted data a name

    if (modeltype(parlist)=='DT' & simple) {
        fitname <- 'DTsimple'
    } else {
        fitname <- paste0('fit', modeltype(parlist), ttform, 1)
    }


    names(d3)[ncol(d3)] <- fitname
    #print(fits)

    rmsd <- rmse(d3[, fitname], d3[,'event1'])

    print(7)
    DEparameters <- parlist


    if ((!simple) | (modeltype(parlist)=='TTT')) {
        cardinaltemps(DEparameters) <- list(newct)
    }

    startday(DEparameters) <- newstart
    threshold(DEparameters) <- newthreshold


    print(8)
    fm <- new('FlowerModel',
              parameters=list(DEparameters),
              error=rmsd,
              phenology=d3,
              olm=list(mod),
              crossvalidated=FALSE)

    return(fm)

}
