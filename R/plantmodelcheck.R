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

#' Checks temperature class
#'
#' This function checks to make sure the temperature data is in the right format
#'     given the functional form used to calculate thermal time with it.
#'
#' @param frm character, the functional form of the thermal time model.
#' @param temp list, the list of temperatures used to calculate thermal time
#'     accumulation.
#' @return This function returns True if the class of the temperature is
#'     correct. Otherwise it returns a vector with FALSE and an error message.
tempclasscheck <- function(frm, temp) {

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


#' Checks a number of problems with the PlantModel
#'
#' @param phenology data.frame, the data.frame containing the phenology data.
#' @param temperature list, the list of temperatures used to calculate thermal time
#'     accumulation.
#' @param form character, the functional form of the thermal time model.
#' @param modtype character, the type of model to be fit using plantmodel()
#' @param stages integer, number of phenological stages in the model.
plantmodelcheck <- function(phenology, temperature, form, modtype, stages) {

    #checking phenology data frame
    pc <- phenologycheck(phenology)
    if (!pc[1]) stop(pc[2])

    #checking temperature list class
    tcc <- tempclasscheck(form, temperature)
    if (!tcc[1]) stop(tcc[2])

    #checking to make sure all the years are there
    tyc <-checktempyears(phenology, temperature)
    if (!tyc[[1]]) stop(paste('Missing temperature data from years:', tyc[[2]]))

    #checking the model type
    mtc <- modeltypecheck(modtype)
    if (!mtc[1]) stop(mtc[2])

}




