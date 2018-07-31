#' @include thermalsum.R parameterfunctions.R FlowerPlant.R
NULL

# This document contains functions that are minimized using the DEoptim
#    function to find the optimum cardinal temperature and length of thermal
#    accumulation

#' Calculates DT model RMSE
#'
#' Calculates RMSE for the thermal model given a certain set of cardinal
#'     temperatures and the Time/Day threshold. Counts the amount of thermal
#'     time required to reach the day threshold. #These are the DT1,3 models
#'
#' @param pars Cardinal temperatures
#' @param fdat the data.frame containing the phenological information
#' @param tdat list containing the temperature information
#' @param form the functional form of the thermal time accumulation
#' @param start numeric, the day to start accumulating time or thermal time
#'     towards the model threshold.
#' @param thresh the threshold in days that thermal time is accumulated for.
#' @param stage the number of the stage of the phenological model
#' @param varying character, c('start', 'threshold') should either of these pars
#'     vary from year to year.
#' @param modclass character, type of model to be estimating, options are
#'     'PlantModel' or 'FlowerModel'. If you have negative day values, you
#'     probably want flower model.
#' @param startingevent numeric, days first event happened each year.
#' @return The RMSE value for a given set of cardinal temperatures and thermal
#'     time accumulation length.
minrmseDT <- function(pars, fdat, tdat, form, start, thresh, stage, varying,
                      modclass, startingevent=NA) {

    #print('minrmseDT')

    responsename <- responseVar(modclass, stage)

    if (checkpars(pars)) { #if parameters are in ascending order

        #calculate the thermal sum for each year using the cardinal temps and
            #day threshold
        tsums <- thermalsum(pars, fdat$year, tdat, 'DT', form, start, thresh,
                            varying, modclass, startingevent)

        #use that data as a predictor in a model to predict stage length
        mod <- lm(fdat[,responsename] ~ tsums)
        fit <- fitted(mod) #extract fitted values

        rmsd <- rmse(fit, fdat[,responsename]) #calculate RMSE of fitted values

    } else {
        rmsd <- Inf #if cardinal temperatures are not in ascending order the
                        #rmse is infinite.
    }

    return(rmsd)

}



#' Calculates simplified TTT model RMSE
#'
#' Calculates RMSE for the simplified model of day accumulation given a
#'     certain set of cardinal temperatures and day accumulation
#'     length. Counts the amount of thermal time required to reach the season
#'     length. This corresponds to model TTT1-3
#'
#' @param pars Cardinal temperatures
#' @param fdat the data.frame containing the phenological information
#' @param tdat list containing the temperature information
#' @param form the functional form of the thermal time accumulation.
#' @param start numeric, the day to start accumulating time or thermal time
#'     towards the model threshold.
#' @param thresh the threshold in thermal time that days are counted for.
#' @param stage the number of the stage of the phenological model
#' @param varying character, c('start', 'threshold') should either of these pars
#'     vary from year to year.
#' @param modclass character, type of model to be estimating, options are
#'     'PlantModel' or 'FlowerModel'. If you have negative day values, you
#'     probably want flower model.
#' @param startingevent numeric, days first event happened each year.
#' @return The RMSE value for a given set of cardinal temperatures and thermal
#'     time accumulation length.
minrmseTTTsimplified <- function(pars, fdat, tdat, form, start, thresh, stage,
                                 varying, modclass, startingevent=NA) {
   # print('minrmseTTTsimp')

    responsename <- responseVar(modclass, stage) #name of response variable

    if (checkpars(pars)) { #are parameters in ascending order

        #calculate day thermal time threshold is met
        daymet <- thermalsum(pars, fdat$year, tdat, 'TTT', form, start, thresh,
                                      varying, modclass, startingevent)

        if (any(is.infinite(daymet))){ #were any of the thermal sums
            #that did not resolve
            rmsd <- Inf
        } else {
            rmsd <- rmse(daymet, fdat[,responsename]) #calculate rmse
        }

    } else {
        rmsd <- Inf #if cardinal temps not in ascending order rmse is infinite
    }


    return(rmsd)

}


#' Calculates TTT model RMSE
#'
#' Calculates RMSE for the combined model of thermal time accumulation given a
#'     certain set of cardinal temperatures and thermal time accumulation
#'     length. Counts the number of days necessary to reach the Thermal time
#'     threshold and use that number to predict the event. TTT1-3
#'
#' @param pars Cardinal temperatures
#' @param fdat the data.frame containing the phenological information
#' @param tdat list containing the temperature information
#' @param form the functional form of the thermal time accumulation
#' @param start numeric, the day to start accumulating time or thermal time
#'     towards the model threshold.
#' @param thresh the threshold in thermal time that days are counted for.
#' @param stage the number of the stage of the phenological model
#' @param varying character, c('start', 'threshold') should either of these pars
#'     vary from year to year.
#' @param modclass character, type of model to be estimating, options are
#'     'PlantModel' or 'FlowerModel'. If you have negative day values, you
#'     probably want flower model.
#' @param startingevent numeric, days first event happened each year.
#' @return The RMSE value for a given set of cardinal temperatures and thermal
#'     time accumulation length.
minrmseTTT <- function(pars, fdat, tdat, form, start, thresh, stage,
                       varying, modclass, startingevent=NA) {

   # print('minrmseTTT')

    responsename <- responseVar(modclass, stage) #name of response variable

    if (!checkpars(pars)) { #are cardinal temperatures in ascending order
       rmsd <- Inf #if not, rmse is infinite


    }  else {

        #calculate day thermal time threshold is met
        daymet <- thermalsum(pars, fdat$year, tdat, 'TTT', form, start, thresh,
                             varying, modclass, startingevent)
        #print(daymet)

        if (any(is.infinite(daymet))) {
            rmsd <- Inf #if any year, the threshold is not met, rmse is inf


        } else {

            #is the day met before the end of the stage in questions?
            positive <- ifelse(daymet > eventi(fdat, modclass, stage),
                               FALSE, TRUE)

            if (all(positive)) {  #if so create model to use day met to predict
                                    #stage length
                mod <- lm(fdat[,responsename] ~ daymet)
                fit <- fitted(mod) #extract fitted data
                rmsd <- rmse(fit, fdat[,responsename]) #calculate rmse

            } else {
                rmsd <- Inf #if not, rmse is infinite
            }

        }
    }

    return(rmsd)

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
#'     is the vector of cardinal temperatures.
#' @param S logical/numeric, is the day to start counting being optimized? If
#'     not, the value associated with the start day should be provided.
#' @param TH logical/numeric, should the model threshold be optimized. If
#'     not L is the (numeric) model threshold.
#' @param simple logical, should the simplified version of the model be run?
#' @param varying character, c('start', 'threshold') should either of these pars
#'     vary from year to year.
#' @param modclass character, type of model to be estimating, options are
#'     'PlantModel' or 'FlowerModel'. If you have negative day values, you
#'     probably want flower model.
#' @param firstevent character, the name of the column that contains the
#'     starting phenological event for the model. For FlowerModels this is the
#'     harvest date.
#' @param startingevent numeric, days first event happened each year. Only
#'     necessary if you are running minrmse on its own (not within the
#'     flowermodel or plantmodel functions).
#' @return The RMSE value for a given set of cardinal temperatures start day
#'     model threshold.
#' @export
minrmse <- function(pars, fdat, tdat, modtype, form, stage, CT, S, TH, simple,
                    varying, modclass, firstevent, startingevent=NA) {

    ctlen <- parnum(form) #what is the number of parameter values for the form
    plen <- length(pars)

    #assigning parameter values to variables based on what parameters are
        #in the model

   # print('minrmse')

    ##########partitioning out the parameters####################
    if (isTRUE(CT)) ct <- pars[(plen-ctlen+1):plen] else ct <- CT[1:ctlen]


    startthresh <- convertParameters(pars, modtype, S, TH, varying,
                                     fdat[,firstevent], fdat$year, modclass)

    s <- startthresh[[1]]
    th <- startthresh[[2]]

    #print(simple)

    ##########sending pars to their respective functions####################


    if (modtype=='DT' & !simple) {
        # print(class(s))
        # print(class(th))
        rmse <- minrmseDT(ct, fdat, tdat, form, s, th, stage, varying,
                          modclass, startingevent)

    } else if (modtype == 'TTT' & simple) {
        rmse <- minrmseTTTsimplified(ct, fdat, tdat, form, s, th, stage,
                                     varying, modclass, startingevent)

    } else if (modtype == 'TTT') {
        rmse <- minrmseTTT(ct, fdat, tdat, form, s, th, stage, varying,
                           modclass, startingevent)
    } else {
        stop('Only options for modeltypes are TTT and DT.')
    }

    return(rmse)

}

