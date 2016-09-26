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
checkparlength <- function(pars, form, CT, L) {

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
