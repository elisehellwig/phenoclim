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



#' Moves temperature data a year later
#'
#' This function takes temperature data and converts the year to the next year
#'     and also converts the days to be negative, counting back from 0 as
#'     Dec 31.
#' @param year numeric, the year you need to add temperature data to
#' @param tdat data.frame, the object that stores all your temperature data.
#' @param start numeric, the day of the year to go back to in the previous year.
#' @param hourly logical, is the data hourly?
#' @return A data.frame that contains all of the temperature data points with
#'     negative days
yearflip <- function(year, tdat, start, hourly=TRUE) {

    beforeyear <- year-1
    beforerows <- which(tdat[,'year']==beforeyear & tdat[,'day']>=start)
    beforedat <- tdat[beforerows,]


    if (leap_year(beforeyear)) {
        beforedat$negday <- beforedat$day - 366

    } else {
        beforedat$negday <- beforedat$day - 365
    }

    if (hourly) {
        df <- data.frame(year=year,
                         day=beforedat$negday,
                         temp=beforedat$temp,
                         hour=beforedat$hour)
    } else {
        df <- data.frame(year=year,
                         day=beforedat$negday,
                         temp=beforedat$temp)
    }

    return(df)
}



#' Creates temp data for Backwards model
#'
#' This function takes temperature data and reformats it so that it is
#'     compatible with the backwards (bloom) model structure. This means
#'     extracting temperatures from the previous year and adding them to the
#'     data but with negative numbers for days.
#' @param tdat data.frame, the object that stores all your temperature data.
#' @param start numeric, the day of the year to go back to in the previous year.
#' @param hourly logical, is the data hourly?
#' @return A data.frame that contains all of the temperature data points with
#'     negative days.
#' @export
tempyearconversion <- function(tdat, start, hourly=TRUE) {

    years <- sort(unique(tdat[,'year']))
    yrs <- years[-1]
    minyr <- min(yrs)

    #print(yrs)

    tempdf <- ldply(yrs, function(y) {
        #print(y)
        yearflip(y, tdat, start, hourly)
    })

    tdatnames <- c('year','day','temp')

    if (hourly) {
        tdatnames <- c(tdatnames,'hour')
    }

    tdat <- tdat[,tdatnames]

    tdfthin <- rbind(tdat, tempdf)

    smalldays <- which(tdfthin$day<start & tdfthin$year>minyr)

    tdfsmall <- tdfthin[smalldays, ]

    return(tdfsmall)

}



