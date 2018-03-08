#' @include thermalsum.R parameterfunctions.R FlowerPlant.R
NULL

# This document contains functions that are minimized using the DEoptim
#    function to find the optimum cardinal temperature and length of thermal
#    accumulation

#' Calculates DT model RMSE
#'
#' Calculates RMSE for the thermal model given a certain set of cardinal
#'     temperatures and thermal time accumulation length.
#'
#' @param pars Cardinal temperatures
#' @param fdat the data.frame containing the phenological information
#' @param tdat list containing the temperature information
#' @param form the functional form of the thermal time accumulation
#' @param length the length of thermal time accumulation (in days). It can be
#'     either a set length of time (one number) or the total length of the
#'     stage (one length for each entry in fdat).
#' @param stage the number of the stage of the phenological model
#' @param stgtype character, type of model to be estimating, options are
#'     'PlantModel' or 'FlowerModel'. If you have negative day values, you
#'     probably want flower model.
#' @return The RMSE value for a given set of cardinal temperatures and thermal
#'     time accumulation length.
minrmseDT <- function(pars, fdat, tdat, form, length, stage, stgtype) {

    responsename <- responseVar(stgtype, stage)

    if (checkpars(pars)) { #if parameters are in ascending order

        #calculate the thermal sum for each year using the cardinal temps and
            #day threshold
        tsums <- thermalsum(pars, fdat, tdat, 'DT', form, length, stage,
                            stgtype)

        #use that data as a predictor in a model to predict stage length
        mod <- lm(fdat[,responsename] ~ tsums)
        fit <- fitted(mod) #extract fitted values

        rmse <- rmsd(fit, fdat[,responsename]) #calculate RMSE of fitted values

    } else {
        rmse <- Inf #if cardinal temperatures are not in ascending order the
                        #rmse is infinite.
    }

    return(rmse)

}



#' Calculates simplified TTT model RMSE
#'
#' Calculates RMSE for the simplified model of day accumulation given a
#'     certain set of cardinal temperatures and day accumulation
#'     length.
#'
#' @param pars Cardinal temperatures
#' @param fdat the data.frame containing the phenological information
#' @param tdat list containing the temperature information
#' @param form the functional form of the thermal time accumulation
#' @param length the length of thermal time accumulation (in days). It can be
#'     either a set length of time (one number) or the total length of the
#'     stage (one length for each entry in fdat).
#' @param stage the number of the stage of the phenological model
#' @param stgtype character, type of model to be estimating, options are
#'     'PlantModel' or 'FlowerModel'. If you have negative day values, you
#'     probably want flower model.
#' @return The RMSE value for a given set of cardinal temperatures and thermal
#'     time accumulation length.
minrmseTTTsimplified <- function(pars, fdat, tdat, form, length, stage,
                                 stgtype) {

    responsename <- responseVar(stgtype, stage) #name of response variable

    if (checkpars(pars)) { #are parameters in ascending order

        #calculate day thermal time threshold is met
        daymet <- thermalsum(pars, fdat, tdat, 'TTT', form, length,
                                      stage, stgtype)

        if (any(is.infinite(daymet))){ #were any of the thermal sums
            #that did not resolve
            rmse <- Inf
        } else {
            rmse <- rmsd(daymet, fdat[,responsename]) #calculate rmse
        }

    } else {
        rmse <- Inf #if cardinal temps not in ascending order rmse is infinite
    }


    return(rmse)

}


#' Calculates TTT model RMSE
#'
#' Calculates RMSE for the combined model of thermal time accumulation given a
#'     certain set of cardinal temperatures and thermal time accumulation
#'     length.
#'
#' @param pars Cardinal temperatures
#' @param fdat the data.frame containing the phenological information
#' @param tdat list containing the temperature information
#' @param form the functional form of the thermal time accumulation
#' @param length the length of thermal time accumulation (in days). It can be
#'     either a set length of time (one number) or the total length of the
#'     stage (one length for each entry in fdat).
#' @param stage the number of the stage of the phenological model
#' @param stgtype character, type of model to be estimating, options are
#'     'PlantModel' or 'FlowerModel'. If you have negative day values, you
#'     probably want flower model.
#' @return The RMSE value for a given set of cardinal temperatures and thermal
#'     time accumulation length.
minrmseTTT <- function(pars, fdat, tdat, form, length, stage, stgtype) {

    responsename <- responseVar(stgtype, stage) #name of response variable

    if (!checkpars(pars)) { #are cardinal temperatures in ascending order
       rmse <- Inf #if not, rmse is infinite


    }  else {

        #calculate day thermal time threshold is met
        daymet <- thermalsum(pars, fdat, tdat, 'TTT', form, length, stage,
                             stgtype)
        #print(daymet)

        if (any(is.infinite(daymet))) {
            rmse <- Inf #if any year, the threshold is not met, rmse is inf


        } else {

            #is the day met before the end of the stage in questions?
            positive <- ifelse(daymet > eventi(fdat, stage+1), FALSE, TRUE)

            if (all(positive)) {  #if so create model to use day met to predict
                                    #stage length
                mod <- lm(fdat[,responsename] ~ daymet)
                fit <- fitted(mod) #extract fitted data
                rmse <- rmsd(fit, fdat[,responsename]) #calculate rmse

            } else {
                rmse <- Inf #if not, rmse is infinite
            }

        }
    }

    return(rmse)

}


#' Calculates model RMSE
#'
#' Calculates RMSE for a Day Threshold (DT) or Thermal Time Threshold (TTT)
#' model of thermal time accumulation given a certain set of cardinal
#' temperatures and a day/thermal time threshold.
#'
#' @param pars vector with threshold value and cardinal temperatures
#' @param fdat the data.frame containing the phenological information
#' @param modtype character, the type of model. The model can be 'DT' (day
#'     threshold) or 'TTT' (thermal time threshold)
#' @param tdat list containing the temperature information
#' @param form the functional form of the thermal time accumulation
#' @param stage the number of the stage of the phenological model
#' @param CT logical, should the cardinal temperatures be optimized. If not CT
#'     is the vector of cardinal temperatures
#' @param L logical, should the model accumulation length be optimized. If not L
#'     is the (numeric) model accumulation length.
#' @param simple logical, should the simplified version of the model be run?
#' @param startday logical, is the day to start counting being optimized? (ie.
#'     not using bloom as startday)
#' @param stgtype logical, Do count forward from the starting event (as
#'     opposed to backward)? If you have negative values in your event days
#'     you forward should probably be FALSE. Bloom model forward=false, harvest
#'     model forward=true.
#' @return The RMSE value for a given set of cardinal temperatures and thermal
#'     time accumulation length.
#' @export
minrmse <- function(pars, fdat, tdat, modtype, form, stage, CT, L, simple,
                    startday, stgtype) {

    ctlen <- parnum(form) #what is the number of parameter values for the form

    #assigning parameter values to variables based on what parameters are
        #in the model
    if (isTRUE(L) & isTRUE(CT)) {

        if (!startday) {
            length <- pars[1]
            ct <- pars[2:(ctlen+1)]

        } else {
            mlen <- pars[1:2]
            ct <- pars[3:(ctlen+2)]
        }


    } else if (!isTRUE(L) & isTRUE(CT)) {
        mlen <- L
        ct <- pars[1:ctlen]

    } else {
        if (!startday) {
            mlen <- pars[1]
        } else {
            mlen <- pars[1:2]
        }

        ct <- CT[1:ctlen]

    }

    #print(simple)


    if (modtype=='DT' & !simple) {
        rmse <- minrmseDT(ct, fdat, tdat, form, mlen, stage, stgtype=stgtype)

    } else if (modtype == 'TTT' & simple) {
        rmse <- minrmseTTTsimplified(ct, fdat, tdat, form, mlen, stage,
                                     stgtype=stgtype)

    } else if (modtype == 'TTT') {
        rmse <- minrmseTTT(ct, fdat, tdat, form, mlen, stage,
                           stgtype=stgtype)
    } else {
        stop('Only options for modeltypes are TTT and DT.')
    }

    return(rmse)

}

