#' @include general.R
NULL


#This script contains functions to check various things to make sure the
#plant model is correctly formed.

#' Checks phenology data.frame
#'
#' This function checks to see if the phenology data.frame has all of the
#'     correct columns.
#'
#' @param n integer, number of stages in the model
#' @param df data.frame, the data.frame containing the phenology data.
#' @param flower logical, is the model a FlowerModel?
#' @return This function returns TRUE if all the variables necessary to fit a
#'     PlantModel are present. If not it returns a vector with the logical FALSE
#'     and an error message with a list of the missing variables.
phenologycheck <- function(n, df, flower) {




    if (flower) {
        pnames <- c('year', paste0('event', 0:1))
    } else {
        pnames <- c('year', paste0('event', 1:n)) #column names
    }

    missingcols <- setdiff(pnames, names(df)) #check if there are missing
    #columns in the data.frame

    if (length0(missingcols)) {
        return(TRUE)
    } else {
        msg <- c(FALSE, paste("You are missing the following variables in your phenology data.frame:", missingcols))
        return(msg) #tells you which columns are missing
    }
}

#' Checks model type
#'
#' @param mt list, the types of model to be fit using plantmodel()
#' @return The function returns true if the model type is one of the allowed
#'     types (thermal or day), and false with a message if it is not.
modeltypecheck <-  function(mt) {

    mt <- unlist(mt) #unlist the model types

    if (all(ifelse(mt %in% c('DT', 'TTT'), TRUE, FALSE))) {
        return(TRUE)

    } else {
        msg <- c(FALSE, "modeltype must be either 'DT' or 'TTT'.")
        return(msg)

    }
}


#' Checks temperature class
#'
#' This function checks to make sure the temperature data is in the right format
#'     given the functional form used to calculate thermal time with it.
#'
#' @param frm character, the functional form of the thermal time model.
#' @param temp data.frame, the data.frame with temperatures used to calculate
#'     thermal time accumulation.
#' @return This function returns True if the class of the temperature is
#'     correct. Otherwise it returns a vector with FALSE and an error message.
tempclasscheck <- function(frm, temp) {

    hrforms <- c('linear','flat','triangle','asymcur','anderson')
    tempvars <- c('year','day')


    if (!is.data.frame(temp)) {
        stop('Temperature data must be a data frame.')
    }

    frmv <- unlist(frm) #unlist forms from arguments


    if (any(ifelse(frmv %in% c('gdd','gddsimple'), TRUE, FALSE))) {
        tempvars <- c(tempvars, 'tmin','tmax') #variables for GDD models

    }

    if (any(ifelse(frmv %in% hrforms, TRUE, FALSE))) {
        tempvars <- c(tempvars, 'hour','temp') #variables for GDH models
    }

    #check to see if any of the variables are missing from the temperature
    #data.frame
    if (any(ifelse(tempvars %in% names(temp), FALSE, TRUE))) {
        stop( paste('Temperature data must contain the following variables:',
                     paste0(tempvars, collapse=', '))) #spits out an error with
        #the names of the variables that are missing
    }


}


#' Checks temp data.frame is ship shape
#'
#' @param temp data.frame, contains all the temperature data
#' @param pheno data.frame, contains all the phenological data
#' @param forms list, contains all the functional forms to be used to calculate
#'     the thermal time in the model.
#' @return logical, TRUE if everything goes well, an error if not.
checktemps <- function(temp, pheno, forms) {

    if (!checktempyears(pheno, temp)[[1]]) {
        stop(paste('You are missing temperature data for the following years',
                   paste(checktempyears(pheno, temp)[[2]], sep=", ")))
    }

    tempclasscheck(forms, temp)
}




