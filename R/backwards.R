#' Converts years for bloom modeling
#'
#' Models for bloom necessarily incorporte phenology data from multiple years.
#'     this is not compatible with the current phenoclim model paradigm. So this
#'     function groups the phenogical events by blooming event not by year.
#'
#' @param fdat dataframe, contains the phenology data
#' @param firstyear numeric, the first year you would like to predict bloom.
#' @param lastyear numeric, the last year you would like to predict bloom.
#' @param bloomvar character, the name of the column that contains the bloom
#'     data.
#' @param matvar character, the name of the column that contains the harvest
#'     data.
#' @param id.vars character, the name of columns that you would like to use to
#'     identify the different observations (ex. ID or cultivar). These must be
#'     these must be present in the fdat data.frame.
#' @param var character, name of a specific cultivar to extract.
#' @return A data.frame with all of the converted phenology data in it.
#' @export
yearconversion <- function(fdat, firstyear=NA, lastyear=NA,
                           bloomvar='event1', matvar='event2',
                           id.vars=NA, var=NA) {


    if (!is.na(var)){
        fdat <- fdat[fdat$cultivar==var, ]
    }

    if ( is.numeric(firstyear)) {
        if (firstyear >= min(fdat[,'year'])) {
            fdat <- fdat[fdat$year>=firstyear, ]
        }
    }

    if (is.numeric(lastyear)) {
        if ( (lastyear <= max(fdat[,'year'])))
            fdat <- fdat[fdat$year<=lastyear, ]
    }

    n <- nrow(fdat)

    #This is where the problem is because there are missing years so it does
    #know what to use for the previous year flowering or end of counting back
    #date.
    years <- fdat[,'year']
    fullyears <- ifelse(is.leapyear(years), 366, 365)
    #print(length(fullyears))
    l0prime <- fdat[2:n, bloomvar] + (fullyears[-n] - fdat[1:(n-1), matvar])
    l0mean <- mean(l0prime)
    l0 <- round(c(l0mean, l0prime))

    conv <- data.frame(year=years,
                       event1=fdat[,bloomvar],
                       event0=(fdat[,matvar] - fullyears),
                       length0=l0)

    if (!is.na(id.vars)) {
        conv <- cbind(fdat[,id.vars], conv)
        names(conv)[1:length(id.vars)] <- id.vars
    }

    return(conv)
}

#' Creates day or hour counts
#'
#' This function creates day and hour indexes to ube used to extract temperature
#'     data. It has options to work for harvest and bloom models.
#'
#' @param start num, vector of starting days.
#' @param stglength num, a stage length or vector of stage lengths
#' @param hourly logical, is the model an hourly model
#' @param forward is the model a forward fitting model (harvest) or a backward
#'      fitting model (bloom)
#' @return a vector of indexes that can be used to extract temperature data.
#' @export
startEnd <- function(start, stglength, hourly=TRUE, forward=TRUE) {

    if (hourly) {
        start <- start*24 - 23
        lengthmod <- stglength*24

    } else {
        lengthmod <- stglength

    }

    if (forward) {
        end <- start+lengthmod
        increment <- 1
    } else {
        end <- start-lengthmod
        increment <- -1
    }

    if (length(start)>1) {
        startend <- lapply(seq_along(start), function(i) {
                seq(start[i], end[i], by=increment)
            })
    } else {
        startend <- seq(start, end, by=increment)
    }



    return(startend)
}





