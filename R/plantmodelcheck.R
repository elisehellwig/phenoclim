#This script contains functions to check various things to make sure the
#plant model is correctly formed.

#' Checks phenology data.frame
#'
#' This function checks to see if the phenology data.frame has all of the
#'     correct columns.
#'
#' @param n integer, number of stages in the model
#' @param df data.frame, the data.frame containing the phenology data.
#' @return This function returns TRUE if all the variables necessary to fit a
#'     PlantModel are present. If not it returns a vector with the logical FALSE
#'     and an error message with a list of the missing variables.
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

#' Checks model type
#'
#' @param mt character, the type of model to be fit using plantmodel()
#' @return The function returns true if the model type is one of the allowed
#'     types (thermal or day), and false with a message if it is not.
modeltypecheck <-  function(mt) {

    if (mt %in% c('thermal', 'day')) {
        return(TRUE)

    } else {
        msg <- c(FALSE, "modeltype must be either 'thermal' or 'day'.")
        return(msg)

    }
}





