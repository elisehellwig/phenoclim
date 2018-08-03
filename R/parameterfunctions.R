##############################################

#' Checks to see if parameters are in the correct order
#'
#' @param pars a vector of cardinal temperatures, should be organized least to
#'      greatest
#' @param start numeric, the day the model starts running
#' @param end numeric, the day of the event to be predicted
#' @param mclass character, the model class, "FlowerModel" or "PlantModel"
#' @param thresh numeric, model threshold if the model is a time threshold model
#' @return logical, TRUE if the cardinal temperatures are organized least to greatest, and start and thresh comes before predicted event. FALSE otherwise
checkpars <- function(pars, start, end, mclass, thresh=NA) {

    #print(start)
    #print(end)
    passcheck <- TRUE

     #check to see if there are more parameters than any of the models use
    if (length(pars)>4) {
        stop('There are no models with more than four parameters')
    }

    parsort <-sort(pars)#put parameters in ascending order

    if (!identical(pars, parsort)) { #are parameters already in ascending
          passcheck <- FALSE
    }




    if (is.na(thresh)) {
        tooLate <- any(ifelse(start>end, TRUE, FALSE))

    } else {
        threshDate <- start + days(thresh)
        tooLate <- any(ifelse(start>end | threshDate>end, TRUE, FALSE))
    }


    if (tooLate) {
        passcheck <- FALSE
    }

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
    if (form %in% c('gdd', 'gddsimple','linear')) {
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
        stop('You must at least estimate one of the following: cardinal temperatures, the model threshold, the model limits (start/stop days).')

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

    if (length(pars)==1) {
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
    n <- object@stages #number of stages in a model
    vp <- object@varyingpars #varying parameters
    mtype <- object@modeltype #model type

    if (length(object@form)==n) { #extracting forms from parameterlist
        forms <- object@form
    } else { #replicating name if there is only on form
        forms <- rep(object@form, stages)
    }

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

    thresh <- round(object@threshold)
    start <- round(object@startday)
    modclass <- object@mclass

    if (modclass=='PlantModel') {
       fromday <- ifelse(is.na(start), 'bloom', start)
       tothresh <- ifelse(is.na(thresh), 'harvest', thresh)
    } else {
        fromday <- ifelse(is.na(start), 'harvest', start)
        tothresh <- ifelse(is.na(thresh), 'bloom', thresh)
    }



    #ml <- paste('from', from, 'to', to)

    if ('start' %in% vp) {
        s1 <- 'Model starts '
        s2 <- ' days after harvest,'
    } else {
        s1 <- 'Model starts on the '
        s2 <- ' day of the year,'
    }

    if (('threshold' %in% vp)| mtype=='TTT') {
        s3 <- 'and runs for '

        if (mtype=='DT') s4 <- ' days.\n' else s4 <- ' thermal time units.\n'

    } else  {
        t3 <- 'and runs until '
        t4 <- ' day.\n'
    }

    #adding information from different stages
   modspecs <- data.frame(stage=1:n,
                          type=rep(mtype, n),
                          form=forms,
                          simplified=object@simplified,
                          class=rep(modclass, n))

    lengthpars <- cbind(modspecs, pars) #putting stage length and parameter
                                            #information together

    cat(paste0(s1, fromday, s2, s3, tothresh, s4))
    return(lengthpars)
}


