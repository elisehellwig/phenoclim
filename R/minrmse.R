#' @include thermalsum.R

# This document contains functions that are minimized using the DEoptim
#    function to find the optimum cardinal temperature and length of thermal
#    accumulation

#' Calculates partial model RMSE
#'
#' Calculates RMSE for the partial model of thermal time accumulation given a
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
#' @return The RMSE value for a given set of cardinal temperatures and thermal
#'     time accumulation length.
minrmsepartial <- function(pars, fdat, tdat, form, length, stage) {

    if (checkpars(pars)) {
        tsums <- thermalsum(pars, fdat, tdat, 'partial', form, length, stage)
        events <- paste0('event', stage:(stage+1))

        stagelength <- fdat[,events[2]] - fdat[,events[1]]
        mod <- lm(stagelength ~ tsums)
        fit <- fitted(mod)
        rmse <- rmsd(fit, stagelength)

    } else {
        rmse <- Inf
    }

    return(rmse)

}



#' Calculates full model RMSE
#'
#' Calculates RMSE for the full model of thermal time accumulation given a
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
#' @return The RMSE value for a given set of cardinal temperatures and thermal
#'     time accumulation length.
minrmsefull <- function(pars, fdat, tdat, form, length, stage) {

    if (checkpars(pars)) {

        predictedlength <- thermalsum(pars, fdat, tdat, 'full', form, length,
                                      stage)
        observedlength <- eventi(fdat, stage+1) - eventi(fdat, stage)
        rmse <- rmsd(predictedlength, observedlength)

    } else {
        rmse <- Inf
    }


    return(rmse)

}


#' Calculates combined model RMSE
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
#' @return The RMSE value for a given set of cardinal temperatures and thermal
#'     time accumulation length.
minrmsecomb <- function(pars, fdat, tdat, form, length, stage) {

    if (checkpars(pars)) {
        #print(2)
       rmse <- Inf

    }  else {

        #print(pars)

        daymet <- thermalsum(pars, fdat, tdat, 'combined', form, length, stage)
        #print(daymet)

        if (any(is.infinite(daymet))) {
            rmse <- Inf


        } else {

            stagelength <- eventi(fdat, stage+1) - eventi(fdat, stage)
            positive <- ifelse(daymet > eventi(fdat, stage+1), FALSE, TRUE)

            if (all(positive)) {
                mod <- lm(stagelength ~ daymet)
                fit <- fitted(mod)
                rmse <- rmsd(fit, stagelength)

            } else {
                rmse <- Inf
            }

        }
    }

    return(rmse)

}


#' Calculates model RMSE
#'
#' Calculates RMSE for a partial, full or combined model of thermal time
#'     accumulation given a certain set of cardinal temperatures and thermal
#'     time accumulation length.
#'
#' @param pars Cardinal temperatures
#' @param fdat the data.frame containing the phenological information
#' @param modtype character, the type of model. Can be 'partial', 'full' or
#'     'combined'.
#' @param tdat list containing the temperature information
#' @param form the functional form of the thermal time accumulation
#' @param length the length of thermal time accumulation (in days). It can be
#'     either a set length of time (one number) or the total length of the
#'     stage (one length for each entry in fdat).
#' @param stage the number of the stage of the phenological model
#' @return The RMSE value for a given set of cardinal temperatures and thermal
#'     time accumulation length.
#' @export
minrmse <- function(pars, fdat, tdat, modtype, form, length, stage) {

    if (modtype=='partial') {
        rmse <- minrmsepartial(pars, fdat, tdat, form, length, stage)

    } else if (modtype == 'full') {
        rmse <- minrmsefull(pars, fdat, tdat, form, length, stage)

    } else if (modtype == 'combined') {
        minrmsecombined(pars, fdat, tdat, form, length, stage)
    } else {
        stop('Only options for model types are partial, full, and combined.')
    }

    return(rmse)

}

