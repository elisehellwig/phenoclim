#This document defines the Plant class around which this package is developed

#' An S4 class to represent the development of a plant
#'
#' @slot phenology A data.frame that contains the phenological data.
#' @slot temperature A data.frame that contains temperature data for all of
#'     the years for which there is phenological data.
#' @slot modeltype The type of structure the model will have. Options are
#'     'partial', 'full', 'combined', or 'time'.
#' @slot form The functional form of the thermal time accumulation.
#' @slot stages An integer with the number of stages in the phenological model
#' @slot
setClass('Plant',
         slots=list(phenology = "data.frame",
                    temperature = "data.frame",
                    modeltype = "character",
                    form = "function",
                    stages = "numeric"))


#############################################################











#############################################################



setValidity("Plant", function(object) {
    msg <- NULL
    valid <- TRUE

    if (!(modeltype %in% c('partial','full','combined','time'))) {
        valid <- FALSE
        msg <- c(msg,
                 "modeltype must be either partial, full combined or time")
    }

    if (length(names()))


})


