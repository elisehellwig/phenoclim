##############################################

#' Checks to see if parameters are in the correct order
#'
#' @param pars a vector of cardinal temperatures, should be organized least to greatest
#' @return logical, TRUE if the cardinal temperatures are organized least to greatest, FALSE otherwise
checkpars <- function(pars) {


    if (length(pars)>4) {
        stop('There are no models with more than four parameters')

    } else {

        parsort <-sort(pars)

        if (identical(pars, parsort)) {
            return(TRUE)

        } else {
            return(FALSE)
        }

    }

}

#######################################



#' Functional form parameter number
#'
#' This function returns the number of parameters a supplied thermal time functional form (form) requires.
#'
#' @param form character, the name of the functional form in question.
#'     Options are 'gdd', 'gddsimple', 'linear', 'flat', 'anderson' and
#'     'triangle'.
#' @return The number of parameters required for the thermal time functional
#'     form supplied.
parnum <- function(form) {

    if (form %in% c('gdd', 'gddsimple','linear')) {
        n <- 1
    } else if (form=='flat') {
        n <- 2
    } else if (form %in% c('anderson', 'triangle')) {
        n <- 3
    } else if (form=='trapezoid') {
        n <- 4
    } else {
        stop('form must be either gdd, gddsimple, linear, flat, anderson, triangle, or trapezoid.')
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
#' @return The length the bounds vectors need to be.
boundlength <- function(form, CT, L) {

    if ((!CT) & (!L)) {
        stop('You must estimate the cardinal temperatures, the model length, or both.')

    } else if (L & (!CT)) {
        parslength <- 1

    } else if (L & CT) {
        parslength <- parnum(form) + 1

    } else {
        parslength <- parnum(form)
    }

    return(parlength)
}


#####################################

#' Creates a list to pass to do.call in the yearsums family of functions
#'
#' @param temps The vector of temperatures used to calculate thermal time.
#' @param pars A vector of parameters
#' @param sum logical, should the thermal times be summed
#' @param full logical, is the model type full?
#' @return A list of parameters that can be passed to do.call
parlist <- function(temps, pars, sum=FALSE, full=FALSE) {

    if (length(pars)==1) {
        pl <- list(temps, pars, sum)

    } else if (length(pars)==2) {
        pl <- list(temps, pars[1], pars[2], sum)

    } else if (length(pars)==3) {
        pl <- list(temps, pars[1], pars[2], pars[3], sum)

    } else if (length(pars)==4) {
        pl <- list(temps, pars[1], pars[2], pars[3], pars[4], sum)

    } else {
        stop('There are no models with more than 4 parameters')
    }

    return(pl)
}
