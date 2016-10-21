#' @include thermalsum.R parameterfunctions.R
NULL

# This document contains functions that are minimized using the DEoptim
#    function to find the optimum cardinal temperature and length of thermal
#    accumulation

#' Calculates TTA model RMSE
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
#' @return The RMSE value for a given set of cardinal temperatures and thermal
#'     time accumulation length.
minrmsethermal <- function(pars, fdat, tdat, form, length, stage) {

    responsename <- paste0('length',stage)

    if (checkpars(pars)) {
        tsums <- thermalsum(pars, fdat, tdat, 'thermal', form, length, stage)
        #print(tsums)

        mod <- lm(fdat[,responsename] ~ tsums)
        fit <- fitted(mod)
        #print(fit)
        #print(fdat[,responsename])
        rmse <- rmsd(fit, fdat[,responsename])

    } else {
        rmse <- Inf
    }

    return(rmse)

}



#' Calculates simplified DA model RMSE
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
#' @return The RMSE value for a given set of cardinal temperatures and thermal
#'     time accumulation length.
minrmsedaysimplified <- function(pars, fdat, tdat, form, length, stage) {

    responsename <- paste0('length',stage)

    if (checkpars(pars)) {
        #print('simple')

        predictedlength <- thermalsum(pars, fdat, tdat, 'day', form, length,
                                      stage)

        if (any(is.infinite(predictedlength))){
            rmse <- Inf
        } else {
            rmse <- rmsd(predictedlength, fdat[,responsename])
        }

    } else {
        rmse <- Inf
    }


    return(rmse)

}


#' Calculates DA model RMSE
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
minrmseday <- function(pars, fdat, tdat, form, length, stage) {

    responsename <- paste0('length',stage)

    if (!checkpars(pars)) {
        #print(2)
       rmse <- Inf


    }  else {

        #print(pars)

        daymet <- thermalsum(pars, fdat, tdat, 'day', form, length, stage)
        #print(daymet)

        if (any(is.infinite(daymet))) {
            rmse <- Inf


        } else {

            positive <- ifelse(daymet > eventi(fdat, stage+1), FALSE, TRUE)

            if (all(positive)) {
                mod <- lm(fdat[,responsename] ~ daymet)
                fit <- fitted(mod)
                rmse <- rmsd(fit, fdat[,responsename])

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
#' @param stage the number of the stage of the phenological model
#' @param CT logical, should the cardinal temperatures be optimized. If not CT
#'     is the vector of cardinal temperatures
#' @param L logical, should the model accumulation length be optimized. If not L
#'     is the (numeric) model accumulation length.
#' @param simple logical, should the simplified version of the model be run?
#' @return The RMSE value for a given set of cardinal temperatures and thermal
#'     time accumulation length.
#' @export
minrmse <- function(pars, fdat, tdat, modtype, form, stage, CT, L, simple) {

    ctlen <- parnum(form)

    if (isTRUE(L) & isTRUE(CT)) {
        length <- pars[1]
        ct <- pars[2:(ctlen+1)]

    } else if (!isTRUE(L) & isTRUE(CT)) {
        length <- L
        ct <- pars[1:ctlen]

    } else {
        length <- pars[1]
        ct <- CT[1:ctlen]

    }

    #print(simple)


    if (modtype=='thermal' & !simple) {
        rmse <- minrmsethermal(ct, fdat, tdat, form, length, stage)

    } else if (modtype == 'day' & simple) {
        rmse <- minrmsedaysimplified(ct, fdat, tdat, form, length, stage)

    } else if (modtype == 'day') {
        rmse <- minrmseday(ct, fdat, tdat, form, length, stage)
    } else {
        stop('Only options for model types are thermal and day.')
    }

    return(rmse)

}

