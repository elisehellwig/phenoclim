#this script will contain functions that help do things differently for
#flowermodels and plantmodels

#' Returns response variable name
#'
#' This function takes in the type of model (PlantModel or FlowerModel) and
#'     the stage and based on that decides what the name of the response
#'     variable to be predicted in the linear model.
#'
#' @param Model character, the type of model used, either PlantModel or
#'     FlowerModel
#' @param stage numeric, what stage is the is the model predicting for.
#' @return The name of the response variable as a character.
responseVar <- function(Model, stage) {

     if (Model=='PlantModel') {#set name of response variable
        respVar <- paste0('length', stage)

    } else if (Model=='FlowerModel') {
        respVar <- paste0('event', stage)
    } else {
        stop('Model must be PlantModel or FlowerModel.')
    }


    return(respVar)
}


yearlength <- function(y) {

    wl <- ifelse(leapyear(y), 366, 365)

    return(yl)
}
