#' @include minrmse.R
NULL

#' Generates the Objective function
#'
#' This function creates a function to be passed to the DEoptim function that
#' optimizes the cardinal temperatures and the length of thermal time or day
#' accumulation for a PlantModel or FlowerModel object.
#'
#' @param parlist ParameterList, contains all of the parlist needed to
#'     create the phenology model.
#' @param phenology data.frame, contains all of the phenology data necessary for
#'     running the phenology model.
#' @param templist list, contains all the temperature data for estimating
#'     thermal time.
#' @param stage numeric, the stage of the model.
#' @param CT Should the cardinal temperatures be optimized? If yes CT is TRUE,
#'     if not CT should be a vector of the cardinal temperatures.
#' @param Threshold logical, Should model threshold be optimized? If yes,
#'     Threshold is TRUE, if not Threshold should be the model threshold to be
#'     used.
#' @param simple logical, is the simplified version of the model being run?
#' @param listindex numeric, the index of the ParameterList from parlist that
#'     you are at
#' @param startday logical, is the day to start counting being optimized? (ie.
#'     not using bloom as startday)
#' @param mclass character, type of model to be estimating, options are
#'     'PlantModel' or 'FlowerModel'. If you have negative day values, you
#'     probably want flower model.
#' @return the function that is passed to DEoptim to optimize.
objective <- function(parlist, phenology, templist, stage, CT, Threshold,
                      simple, listindex, startday, mclass) {

    if (stgtype=='FlowerModel') {
        #extract parameters from parlist object

        fun <- objectiveFlower(parlist, phenology, templist, stage, CT,
                               Threshold, simple, listindex, startday)

    } else {
        #extract parameters from ParameterList object
        fun <- objectivePlant(parlist, phenology, templist, stage, CT,
                              Threshold, simple, listindex, startday)
    }

    return(fun)

}

#' Generates the Objective function for a Flower Model
#'
#' This function creates a function to be passed to the DEoptim function that
#' optimizes the cardinal temperatures and the length of thermal time or day
#' accumulation for a FlowerModel object.
#'
#' @param parlist ParameterList, contains all of the parlist needed to
#'     create the phenology model/PlantModel object.
#' @param phenology data.frame, contains all of the phenology data necessary for
#'     running the phenology model.
#' @param templist list, contains all the temperature data for estimating
#'     thermal time.
#' @param stage numeric, the stage of the model.
#' @param CT Should the cardinal temperatures be optimized? If yes CT is TRUE,
#'     if not CT should be a vector of the cardinal temperatures.
#' @param Threshold logical, Should model threshold be optimized? If yes,
#'     Threshold is TRUE, if not Threshold should be the model threshold to be
#'     used.
#' @param simple logical, is the simplified version of the model being run?
#' @param listindex numeric, the index of the ParameterList from parlist that
#'     you are at
#' @param startday logical, is the day to start counting being optimized? (ie.
#'     not using bloom as startday)
#' @return the function that is passed to DEoptim to optimize.
objectiveFlower <- function(parlist, phenology, templist, stage, CT, Threshold,
                            simple, listindex, startday) {

    #extract parameters from parlist object
    CTpars <- cardinaltemps(parlist)[[1]]


    #extract threshold from parameterlist object
    th <- threshold(parlist)[listindex]

    #create vector of names of events (columns)
    events <- c('event1','event0')
    fnames <- c('year', events)


    #create data.frame with only the columns necessary
    fdat <- phenology[, fnames]


    #which parameters are geting estimated
    if (CT) ct <- TRUE else ct <- CTpars
    if (Threshold) thresh <- TRUE else thresh <- th


    #create a function that evaluates returns the rmse of the model that can be
    #minimized using the function DEoptim()
    fun <- function(x) {
        return(minrmse(x, fdat, templist, modeltype(PL), form(PL)[stage],
                       stage, ct, thresh, simple[listindex], startday,
                       stgtype='FlowerModel'))
    }

    return(fun)

}



#' Generates the Objective function for a PlantModel
#'
#' This function creates a function to be passed to the DEoptim function that
#' optimizes the cardinal temperatures and the length of thermal time or day
#' accumulation for a PlantModel object.
#'
#' @param parlist ParameterList, contains all of the parlist needed to
#'     create the phenology model/PlantModel object.
#' @param phenology data.frame, contains all of the phenology data necessary for
#'     running the phenology model.
#' @param templist list, contains all the temperature data for estimating
#'     thermal time.
#' @param stage numeric, the stage of the model.
#' @param CT Should the cardinal temperatures be optimized? If yes CT is TRUE,
#'     if not CT should be a vector of the cardinal temperatures.
#' @param Threshold logical, Should model threshold be optimized? If yes,
#'     Threshold is TRUE, if not Threshold should be the model threshold to be
#'     used.
#' @param simple logical, is the simplified version of the model being run?
#' @param listindex numeric, the index of the ParameterList from parlist that
#'     you are at
#' @param startday logical, is the day to start counting being optimized? (ie.
#'     not using bloom as startday)
#' @return the function that is passed to DEoptim to optimize.
objectivePlant <- function(parlist, phenology, templist, stage, CT, Threshold,
                           simple, listindex, startday) {

    #extract parameters from ParameterList object
    PL <- parlist[[listindex]]
    CTpars <- cardinaltemps(PL)[[stage]]

    #extract threshold from parameterlist object
    th <- threshold(PL)


    events <- paste0('event', stage:(stage+1))
    fnames <- c('year', events, paste0('length', stage))


    #create data.frame with only the columns necessary
    fdat <- phenology[, fnames]


    #which parameters are geting estimated
    if (CT[listindex]) ct <- TRUE else ct <- CTpars
    if (Threshold[listindex]) thresh <- TRUE else thresh <- th


    #create a function that evaluates returns the rmse of the model that can be
    #minimized using the function DEoptim()
    fun <- function(x) {
        return(minrmse(x, fdat, templist, modeltype(PL), form(PL)[stage], stage,
                       ct, thresh, simple[listindex], startday, stgtype=stgtype))
    }

    return(fun)

}





