##############################################

#' Checks to see if parameters are in the correct order
#'
#' @param pars a vector of cardinal temperatures, should be organized least to greatest
#' @return logical, TRUE if the cardinal temperatures are organized least to greatest, FALSE otherwise
checkpars <- function(pars) {

     #check to see if there are more parameters than any of the models use
    if (length(pars)>4) {
        stop('There are no models with more than four parameters')

    } else {

        parsort <-sort(pars)#put parameters in ascending order

        if (identical(pars, parsort)) { #are parameters already in ascending
            return(TRUE)                #order?

        } else {
            return(FALSE)
        }

    }

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
#' @param L logical, does the thermal time accumulation length need to be
#'     estimated?
#' @param start logical, is the start day estimated?
#' @return The length the bounds vectors need to be.
boundlength <- function(form, CT, L, start=FALSE) {

    #what is the maximum number of parameters any form requires?
    pn <- max(sapply(form, function(fm) parnum(fm)))

    CT <- any(CT) #do any models need to estimate cardinal temperatures
    L <- any(L) #do any models need to estimate threshold?

    if (start) {
        lengthnum <- 2
    } else {
        lengthnum <- 1
    }

    if ((!CT) & (!L)) {
        stop('You must estimate the cardinal temperatures, the model length, or both.')

    } else if (L & (!CT)) { #if you are only estimating length
            parslength <- lengthnum  #you only need to estimate one or two
                                      #parameters

    } else if (L & CT) {    # if you are estimating length and Cardinal temps
        parslength <- pn + lengthnum #you need to estimate 1+pn parameters

    } else {
        parslength <- pn #if you are only estimating cardinal times you need to
    }                       #estimate pn parameters



    return(parslength)
}


#####################################

#' Creates a list to pass to do.call in the DT/TT family of functions
#'
#' @param temps The vector of temperatures used to calculate thermal time.
#' @param pars A vector of parameters
#' @param sum logical, should the thermal times be summed
#' @param full logical, is the model type full?
#' @return A list of parameters that can be passed to do.call
parslist <- function(temps, pars, sum=FALSE, full=FALSE) {

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


    } else {
        stop('There are no models with more than 5 parameters')
    }

    return(pl)
}


##############################################

#' Legwork behind the ParameterList show method
#'
#' @param object Parameterlist object
#' @return A data.frame with the columns type, form, length, stage, Base,
#'     Optimal, and Critical
showparlist <- function(object) {
    n <- object@stages #number of stages in a model


    if (length(object@form)==n) { #extracting forms from PlantModel
        forms <- object@form
    } else { #replicating name if there is only on form
        forms <- rep(object@form, stages)
    }

    ctlist <- lapply(object@cardinaltemps, function(ct){ #reformating cardinal
        if (length(ct)==3) {                             #temperatures
            ct
        } else {
            c(ct, rep(' ', 3-length(ct)))
        }
    })
    pars <- as.data.frame(do.call(rbind, ctlist)) #converting ctlist list to df
    pars <- round(pars)
    names(pars) <- c('Base','Opt.','Crit.') #giving the columns names

    mlen <- object@modlength
    stgtyp <- object@stagetype

    ml <- sapply(mlen, function(l) {
        if (length(day) >1 ) {
            abs(l[2]-l[1])
        } else {
            l
        }

    })


    #adding information from different stages
    stagelength <- data.frame(stage=1:n,
                              type=rep(object@modeltype, n),
                              form=forms,
                              length=round(ml),
                              stagetype=rep(stgtyp, n))

    lengthpars <- cbind(stagelength, pars) #putting stage length and parameter
                                            #information together
    return(lengthpars)
}


