

#' Picks correct ordinal indicator for a number
#'
#' This function returns the correct ordinal indicator for the number provided,
#'     i.e. the two letter suffix used to denote that a number is a rank
#'     (ex. 1st).
#'
#' @param n numeric or integer, a number that needs an ordinal indicator.
#' @return character, the ordinal indicator of the number provided.
pickEnding <- function(n) {
    digits <- as.numeric(strsplit(as.character(n), '')[[1]])

    ones <- digits[length(digits)]

    if (length(digits)==1) {
        tens <- 0
    } else {
        tens <- digits[(length(digits)-1)]
    }

    if (ones==1 & tens!=1) {
        ending <- 'st'
    } else if (ones==2 & tens!=1) {
        ending <- 'nd'
    } else if (ones==3 & tens!=1) {
        ending <- 'rd'
    } else {
        ending <- 'th'
    }

    return(ending)
}




##############################################

#' Checks to see if parameters are in the correct order
#'
#' @param pars a vector of cardinal temperatures, should be organized least to
#'      greatest
#' @param start numeric, the day the model starts running
#' @param end numeric, the day of the event to be predicted
#' @param mclass character, the model class, "FlowerModel" or "PlantModel"
#' @param thresh numeric, the threshold day for a DT model.
#' @param form character, the functional form used to calculate thermal time.
#' @return logical, TRUE if the cardinal temperatures are organized least to greatest, and start and thresh comes before predicted event. FALSE otherwise
checkpars <- function(pars, start, end, mclass, thresh=NA, form=NA) {

    #print('checkpars')
    #print(start)
    #print(thresh)
    #print(pars)
    #print(end)
    passcheck <- TRUE


     #check to see if there are more parameters than any of the models use
    if (length(pars)>4) {
        stop('There are no models with more than four parameters')
    }

    #print(pars[1])

    if (is.na(pars[1]) & form %in% c('utah','utahalt')) {
        parsort <- NA
    } else {
        parsort <-sort(pars)#put parameters in ascending order
    }


    #print(pars)
    #print(parsort)
    if (!identical(pars, parsort)) { #are parameters already in ascending
          passcheck <- FALSE
          #print('issue with cts')
    }



    if (mclass=='PlantModel') {
        tooLate <- any(ifelse(start>end, TRUE, FALSE))


    } else if (mclass=='FlowerModel') {

        if (is.na(thresh[1])) {
            tooLate <- any(ifelse(start>=end, TRUE, FALSE))

        } else {
            tooLate <- any(ifelse(start>=end | thresh>=end | thresh<=start,
                                  TRUE, FALSE))
        }

    } else {
        stop('mclass must be PlantModel or FlowerModel')
    }


    if (tooLate) {
        passcheck <- FALSE
    }

    #print(passcheck)
    return(passcheck)

}

#######################################

#' Functional form parameter number
#'
#' This function returns the number of parameters a supplied thermal time
#'     functional form (form) requires.
#'
#' @param form character, the name of the functional form in question.
#'     Options are 'gdd', 'gddsimple', 'linear', 'flat', 'asymcur, 'anderson'
#'     and 'triangle'.
#' @return The number of parameters required for the thermal time functional
#'     form supplied.
parnum <- function(form) {

    #identify how many parameters a model should have based on its form

    if (form %in% c('gdd', 'gddsimple','linear','chillbasic',
                           'utah','utahalt')) {
        n <- 1
    } else if (form=='flat') {
        n <- 2
    } else if (form %in% c('anderson', 'asymcur', 'triangle', 'ensemble')) {
        n <- 3
    } else if (form=='trapezoid') {
        n <- 4
    } else {
        stop('form must be either gdd, gddsimple, linear, flat, asymcur, anderson, triangle, or trapezoid, or ensemble.')
    }

    return(n)
}


##############################################

#' Length of the bounds variables
#'
#' This function returns the length that the lbounds and ubounds variables
#'     should be based on the functinoal form of the thermal time
#'     accumulation as well as whether or not the cardinal temperatures and
#'     the length of accumulation need to be estimated.
#'
#' @param form character, the thermal time functional form.
#' @param CT logical, do the cardinal temperatures need to be estimated?
#' @param Start logical, does the start day need to be estimated?
#' @param Thresh logical, does the model threshold need to be
#'     estimated?
#' @return The length the bounds vectors need to be.
#' @export
boundlength <- function(form, CT, Start, Thresh) {

    #what is the maximum number of parameters any form requires?
    pn <- max(sapply(form, function(fm) parnum(fm)))

    #lining up whether we are estimating the threshold, start, and end values
        #for each stage, and checking to make sure we are not estimating all 3
        #in one of the stages


    CT <- any(CT) #do any models need to estimate cardinal temperatures
    anyThresh <- any(Thresh) #do any models need to estimate threshold?
    anyStart <- any(Start) #do any models need to estimate startday?

    # vector of whether we are estimating each parameter
    logipars <- c(CT, anyStart, anyThresh)

    if (!any(logipars)) {
        stop('You must at least estimate one of the following: cardinal temperatures, start day, model threshold.')

    } else {

        #this may cause problems later

        #what is the max number of length related pars estimated in a stage
        STmax <- max(apply(cbind(Start, Thresh), 1, sum))

        #total max number of parameters we will have for any model
        parslength <- pn*CT + STmax


    }

    return(parslength)
}


#####################################

#' Creates a list to pass to do.call in the DT/TT family of functions
#'
#' @param temps The vector of temperatures used to calculate thermal time.
#' @param pars A vector of parameters
#' @param sum logical, should the thermal times be summed
#' @return A list of parameters that can be passed to do.call
parslist <- function(temps, pars, sum=FALSE) {

    if (is.na(pars[1])) {
        pl <- list(temps, sum)

    } else if (length(pars)==1) {
        pl <- list(temps, pars, sum)

    } else if (length(pars)==2) {
        pl <- list(temps, pars[1], pars[2], sum)

    } else if (length(pars)==3) {
        pl <- list(temps, pars[1], pars[2], pars[3], sum)

    } else if (length(pars)==4) {
        pl <- list(temps, pars[1], pars[2], pars[3], pars[4], sum)

    } else if (length(pars)==5) {
        pl <- list(temps, pars[1], pars[2], pars[3], pars[4], pars[5], sum)


    } else if (length(pars)==6) {
        pl <- list(temps, pars[1], pars[2], pars[3], pars[4], pars[5], pars[6],
                   sum)


    } else {
        stop('There are no models with more than 6 parameters')
    }

    return(pl)
}

##############################################

#' Calculates length of model
#'
#' @param ml numeric, the lengths of each of the stages, or NA
#' @param lims list, the start/stop pairs for each of the stages.
#' @return A vector with the lengths of each of the stages.
calclength <- function(ml, lims) {
    mllog <- ifelse(is.na(ml), FALSE, TRUE)

    mlength <- sapply(seq_along(ml), function(i) {
        if (mllog[i]) {
            ml[i]
        } else {
            lims[[i]][2]-lims[[i]][1]
        }
    })

    return(mlength)
}

##############################################

#' Legwork behind the ParameterList show method
#'
#' @param object Parameterlist object
#' @return A data.frame with the columns type, form, length, lims, stage, Base,
#'     Optimal, and Critical
showparlist <- function(object) {
    #extracting the information from the parlist object
    #print(1)
    n <- object@stages #number of stages in a model
    vp <- object@varyingpars #varying parameters
    mtype <- object@modeltype #model type
    #print(1.3)
    thresh <- round(object@threshold) #model thresholds
    start <- round(object@startday) #model start days
    modclass <- object@mclass #model class.

    #print(1.5)
    if (length(object@form)==n) { #extracting forms from parameterlist
        forms <- object@form
    } else { #replicating name if there is only on form
        forms <- rep(object@form, n)
    }

    #print(2)
    ctlist <- lapply(object@cardinaltemps, function(ct){ #reformating cardinal
        ct <- round(ct, 2)
        if (length(ct)==3) {                             #temperatures
            ct
        } else {
            c(ct, rep(' ', 3-length(ct)))
        }
    })

    pars <- as.data.frame(do.call(rbind, ctlist)) #converting ctlist list to df
    #pars <- round(pars)
    names(pars) <- c('Base','Opt.','Crit.') #giving the columns names

    #print(3)

    #creating vector of when each stage starts
    if (!('start' %in% vp)) {
        startvec <- paste('day', start)
    } else if (modclass=='PlantModel') {
        startvec <- paste(paste0('event', 1:n), '+', start)
    } else {
        startvec <- paste('event0' + start)
    }

    #creating a vector of the thresholds for each stage
    if (modclass=="PlantModel") {
        if (mtype=='TTT') {
            threshvec <- paste(thresh, 'TTU')
        } else if ('threshold' %in% vp) {
            totalthresh <- start+thresh
            threshvec <- paste(paste0('event', 1:n), '+', totalthresh)
        } else {
            threshvec <- paste('day', thresh)
        }

    } else {
        if (mtype=='TTT' | mtype=='Dual') {
            threshvec <- paste(thresh, 'TTU', collapse = ', ')

        } else if ('threshold' %in% vp) {
            totalthresh <- start+thresh
            threshvec <- paste(paste0('event', 1:n), '+', totalthresh)

        } else {
            threshvec <- paste(paste('day', thresh), collapse=', ')
        }
    }

    #print(4)
    #adding information from different stages
   modspecs <- data.frame(stage=1:n,
                          type=rep(mtype, n),
                          form=forms,
                          simplified=object@simplified,
                          class=rep(modclass, n),
                          start=startvec,
                          thresh=threshvec)

    lengthpars <- cbind(modspecs, pars) #putting stage length and parameter
                                            #information together

    cat('TTU = Thermal Time Units. All other numbers are in days.\n')
    return(lengthpars)
}


