#' @include flipday.R
NULL



#' Converts day of the year to day after event (DAE)
#'
#' This function takes in a day of the year and the day of a phenological event
#'     and converts the day of the year to the number of days after the
#'     phenological event. The year of each event must be specified so that the
#'     appropriate year length can be used.
#'
#' @param day numeric, the day of the year you want to convert to days after
#'     event
#' @param event numeric, the day of the event for each year of interest
#' @param years numeric, the year that each event happened
#' @return A numeric vector specifying the number of days after the event that
#'    the day of interest happened.
dayToDAE <- function(day, event, years) {

    if (length(event)!=length(years)) {
        stop('You must have a year for each event.')
    }

    # Is there an extra day between the event and the day of the year we are
    #interested in? if so leapmod will be 1, else 0
    leapmod <- ifelse(event <= 59 & day>59 & leap_year(years), 1, 0)

    #length of the years in question
    ylength <- yearlength(years)

    #extending days of the year to greater than 365 if necessary
    day <- ifelse(day < event, day+ylength, day)

    dae <- day - event + leapmod + 1 #still not sure if we shoudl be adding 1

    return(dae)
}


#' Separates out Parameter Values for optimization
#'
#' Assigns parameter values for start and threshold based on model type,
#'     which parameters are being estimated and which parameters are varying
#'     from year to year.
#'
#' @param pars numeric, a vector of parameters that are optimized using the
#'     `DEoptim` function.
#' @param modtype character, is the model a day threshold ('DT') model or a
#'     thermal time threshold model ('TTT').
#' @param S logical/numeric, is the day to start counting being optimized? If
#'     not, the value associated with the start day should be provided.
#' @param TH logical/numeric, should the model threshold be optimized. If
#'     not L is the (numeric) model threshold.
#' @param vp character, c('start', 'threshold') should either of these pars
#'     vary from year to year.
#' @param eventvec numeric, a vector that contains the day of the year for the
#'     starting event of the model. For bloom the event is harvest.
convertParameters <- function(pars, modtype, S, TH, vp, eventvec, years) {

    #Estimating start day
    if (isTRUE(S)) {
        s <- pars[1]

    } else { #Not estimating start day
        s <- S

    }

    #does start day vary from year to year?
    if (!('start' %in% vp)) {#case: DT 5, TTT3
        s <- dayToDAE(s, eventvec, years)

    }


    #Estimating threshold
    if (isTRUE(TH)) {

        if (isTRUE(S)) {
            th <- pars[2]
        } else {
            th <- pars[1]
        }

    } else { #not estimating threshold
       th <- TH

    }


    #Note if it is a TTT model threshold can't vary from year to year.
    if (modtype=='DT' & (!('threshold' %in% vp)) ) {
        th <- dayToDAE(th, eventvec, years) - s

    }


    return(list(s, th))

}



