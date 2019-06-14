
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
#' @param timezone character, the timezone that the data is from.
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

#' Formats parameters in days to Dates
#'
#' Assigns parameter values for start and threshold based on model type,
#'     which parameters are being estimated and which parameters are varying
#'     from year to year.
#'
#' @param years numeric, a vector of years to use for each day of the year.
#' @param eventday numeric, a vector that contains the day of the year for the
#'     starting event of the model. For bloom the event is harvest.
#' @param startday numeric, vector of days to start count thermal time each
#'     year.
#' @param threshold numeric, the threshold of the model.
#' @param modtype character, is the model a day threshold ('DT') model or a
#'     thermal time threshold model ('TTT').
#' @param varying character, c('start', 'threshold') should either of these pars
#'     vary from year to year.
#' @param modclass character, PlantModel or FlowerModel
#' @return list of the correct start POSIXct date vector and the threshold
#'     vector based on the type of model being run. If the day of the threshold
#'     varies by year it is a 'Period' vector. If it does not, it is a POSIXct
#'     date vector.
#' @export
formatParameters <- function(years, eventday, startday, threshold, modtype,
                             modclass, varying) {

    #print('formatpars')
    #print(startday)
    #print(threshold)

     #print('fp')
    #does start day vary from year to year?
    if ('start' %in% varying) {
        startday <- startday + eventday
    }

    #convert days of the year to dates
    sdate <- dayToDate(years, startday, modclass)

    #Note if it is a TTT model threshold can't vary from year to year.
    if (modtype=='DT') {

        threshold <- round(threshold)

        if ('threshold' %in% varying) {
            threshdate <- sdate + days(threshold)

        } else {
            threshdateF <- dayToDate(years, threshold, 'FlowerModel') + days(1)
            threshdateP <- dayToDate(years, threshold, 'PlantModel') + days(1)

            tooearly <- ifelse(threshdateF<sdate, TRUE, FALSE)

            if (any(tooearly)) {
                threshdate <- threshdateP
            } else {
                threshdate <- threshdateF
            }

        }

    } else { #not actually a date, rather a number of thermal time units
        threshdate <- threshold
    }

    sth <- list(sdate, threshdate)

    return(sth)

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
#' @param years numeric, a vector of years to use for each day of the year.
#' @param modclass character, PlantModel or FlowerModel
#' @return list of the correct start POSIXct date vector and the threshold
#'     vector based on the type of model being run. If the day of the threshold
#'     varies by year it is a 'Period' vector. If it does not, it is a POSIXct
#'     date vector.
#' @export
convertParameters <- function(pars, modtype, S, TH, vp, eventvec, years,
                              modclass) {

    #print('cp')
    #Estimating start day
    if (isTRUE(S)) {
        s <- pars[1]

    } else { #Not estimating start day
        s <- S

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

    STHlist <- formatParameters(years, eventvec, s, th, modtype, modclass,
                                vp)


    return(STHlist)
}

#' Converts vectors to matrices
#'
#' This function converts vectors to matrices with the appropriate number of
#'     rows or columns depending on the number of stages and functional forms
#'     in a model.
#'
#' @param x numeric, object to be converted to a matrix
#' @param nstage numeric, number of stages in the model
#' @param nform numeric, number of forms in the model
#' @return a matrix with the appropriate number of rows and columns
convertToMatrix <- function(x, nstage, nform) {

    if (is.null(dim(x))) {

        if (nstage>1) {
            value <- as.matrix(x, nrow=nstage)
        } else if (nform>1) {
            value <- as.matrix(x, ncol=nform)
        } else {
            value <- as.matrix(x)
        }

    } else {
        value <- x
    }

    return(value)
}


