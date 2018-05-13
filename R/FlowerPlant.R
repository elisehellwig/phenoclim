#this script will contain functions that help do things differently for
#flowermodels and plantmodels

#' Returns response variable name
#'
#' This function takes in the type of model (PlantModel or FlowerModel) and
#'     the stage and based on that decides what the name of the response
#'     variable to be predicted in the linear model.
#'
#' @param ModelClass character, the type of model used, either PlantModel or
#'     FlowerModel
#' @param stage numeric, what stage is the is the model predicting for.
#' @return The name of the response variable as a character.
responseVar <- function(ModelClass, stage) {

     if (ModelClass=='PlantModel') {#set name of response variable
        respVar <- paste0('length', stage)

    } else if (ModelClass=='FlowerModel') {
        respVar <- paste0('event', stage)
    } else {
        stop('Model must be PlantModel or FlowerModel.')
    }


    return(respVar)
}

#' Calculates number of days in a year
#'
#' This function determines whether or not a given year is a leap year anc then
#'     if it is returns 366, and if not returns 365.
#'
#' @param y numeric a vector of years
#' @return A numeric vector of the lengths of the years inputted.
yearlength <- function(y) {

    yl <- ifelse(leap_year(y), 366, 365)

    return(yl)
}
