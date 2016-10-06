

#This script contains functions to check various things to make sure the
#plant model is correctly formed.

phenologycheck <- function(n, df) {

    pnames <- c('year', paste0('event', 1:n))
    missingcols <- setdiff(pnames, names(df))

    if (length0(missingcols)) {
        return(TRUE)
    } else {
        msg <- c(FALSE, paste("You are missing the following variables in your phenology data.frame:", missingcols))
        return(msg)
    }
}


modeltypecheck <-  function(mt) {

    if (mt %in% c('thermal', 'day')) {
        return(TRUE)

    } else {
        msg <- c(FALSE, "modeltype must be either 'thermal' or 'day'.")
        return(msg)

    }
}

temptypecheck <- function(frm, temp) {

    valid <- TRUE
    msg <- NULL

    if (frm %in% c('gdd','gddsimple')) {

        if (is.list(temp[[1]])) {
            valid <- FALSE
            if (!is.data.frame(temp[[1]][[1]]) | dim(temp[[1]][[1]])[2] != 2) {
                msg <- c(msg,
                         'Min and max temperatures are required for gdd and gddsimple models.')
            }
        } else {

            if (!is.data.frame(temp[[1]]) | dim(temp[[1]])[2] != 2) {
                valid <- FALSE
                msg <- c(msg,
                         'Min and max temperatures are required for gdd and gddsimple models.')
            }
        }

    } else {

        if ( (!is.numeric(temp[[1]])) |  (!is.numeric(temp[[1]][[1]])) ) {
            valid <- FALSE
            msg <- c(msg, 'Numeric vectors of temperatures are required for the hourly functional forms.')
        }
    }

    msg <- c(valid, msg)

    return(msg)
}

