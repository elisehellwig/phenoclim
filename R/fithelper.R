
#' Identifies which year each day corresponds to
#'
#' This function decides whether a day happened in the same year as the
#'     associated phenological event, or the year before.
#'
#' @param day numeric, a vector of start or threshold days
#' @param years numeric, a vector of years of the phenological event
#'     associated with the start days
#' @param modclass character, PlantModel or FlowerModel
#' @return A vector of years that correspond to the actual year each of the days
#'     in the day vector happened.
correctYears <- function(day, years, modclass) {

    if (modclass=='FlowerModel') {
        correctedYears <- ifelse(day>130, years-1, years)
    } else {
        correctedYears <- years
    }

    return(correctedYears)

}



#' Converts day of the year to date-time
#'
#' This function takes in a vector of days of the year and a vector of years
#'     and converts them to a date-time vector of the class POSIXct
#'
#' @param years numeric, a vector of years to use for each day of the year
#' @param days numeric, the day of the year you want to convert to date-time.
#' @param varying character,
#' @param modclass character, 'PlantModel' or 'FlowerModel'.
#' @param hours numeric, a vector of hours to use in your date-time conversion
#' @param startTime character, used to specify the same time of day for each
#'     date-time entry element. Only used if hours=NA.
#' @return A POSIXct vector of date-times
#' @export
dayToDate <- function(years, days, modclass, varying, hours=NA,
                      startTime='00:00:00', timezone='America/Los_Angeles') {

    if (length(days)==1) {
        days <- rep(days, length(years))
    }

    cyears <- correctYears(days, years, modclass)

    datestring <- as.Date(days-1, origin=paste0(cyears, '-01-01'))

    if (is.na(hours[1])) {
        dateTimeString <- paste0(datestring, ' ', startTime)

    } else {
        timestring <- paste0(sprintf("%02d", hours), ':00:00')
        dateTimeString <- paste(datestring, timestring)

    }


    posix <- as.POSIXct(dateTimeString, "%Y-%m-%d %H:%M:%OS",
                        tz=timezone)

    return(posix)

}



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
#' @return list of the correct start day vector and the threshold vector based
#'     on the type of model being run.
convertParameters <- function(pars, modtype, S, TH, vp, eventvec, years,
                              modclass) {

    #Estimating start day
    if (isTRUE(S)) {
        s <- pars[1]

    } else { #Not estimating start day
        s <- S

    }

    #does start day vary from year to year?
    if ('start' %in% vp) {
        s <- s + eventvec
    }

    #convert days of the year to dates
    s <- dayToDate(years, s, modclass)

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
    if (modtype=='DT') {

         if ('threshold' %in% vp) {
            th <- days(th)

        } else {
            th <- dayToDate(years, th, modclass)
        }

    }


    return(list(s, th))

}

convertPhenology <- function(df) {

}

