

#' Returns rows in x that are missing from y
#'
#' @param x A data.frame with all the data
#' @param y A data.frame that may have some rows missing
#' @return the rows of x that are missing in y
rowDifference <- function(x, y) {

    xchar <- apply(x, 1, paste, collapse = '')
    ychar <- apply(y, 1, paste, collapse = '')

    diff <- x[(!(xchar %in% ychar)),]

    return(diff)

}





#' Averages a vector of dates
#'
#' @param dates a vector of dates
#' @param origin The start date. For R it is "1970-01-01".
#' @return The average of all the dates
#' @examples
#' v <- runif(10, min=9014, max=20300)
#' d <- as.Date(v, origin="1970-01-01")
#' avgdate(d)
#' @export
avgdate <- function(dates, origin="1970-01-01") {
    #averages dates together

    d <- as.integer(dates) #converts the dates to integers

    #averages them and takes the next whole number up
    ad <- ceiling(sum(d, na.rm=TRUE)/sum(!is.na(dates)))

    #converts that number back to the date
    aD <- as.Date(ad, origin=origin)

    return(aD)
}


#' Checks for missing days in a time series
#'
#' @param x A data.frame containing the time series
#' @param days The days you want to check to see if they are missing
#' @param limits The min and max days to check.
#' @param hourly Logical, is the data hourly?
#' @return A vector with the days that are missing from the time series.
missingDays <- function(x, days, limits, hourly=FALSE) {

    year <- unique(x$year)

    if(length(year)>1) {
        stop('Only one year can be checked at a time.')
    }

    if (hourly) {
        cols <- c('year','day','hour')

        if (is.leapyear(year) & length(days)==365) {
            days <- expand.grid(c(days,366), 1:24)
        } else {
            days <- expand.grid(days, 1:24)
        }

    } else {
        cols <- c('year','day')

        if (is.leapyear(year) & length(days)==365) {
            days <- c(days, 366)
        }
    }

    if (is.na(limits[1])) {

        observations <- x[,cols]


    } else {

        observations <- x[x$day %in% limits[1]:limits[2], cols]

    }


    if (hourly) {

        rd <- rowDifference(days, observations)
        names(rd) <- cols

        if (dim(rd)[1]==0) {
            return(NA)
        } else {
            return(rd)
        }

    } else {
        sdiff <- setdiff(days, observations)

        if(length(sdiff)==0) {
            return(NA)
        } else {
            return(sdiff)
        }

    }


}


#' Checks time series for missing entries
#'
#' This function checks a time series, in the form of a data.frame, for days
#'     that are missing from the series.
#'
#' @param x A data.frame that contains the time series. It must have at least
#'     one column, named with the string in the datename argument.
#' @param start POSIXct or chr, the beginning date time.
#' @param end POSIXct or chr, the ending date time.
#' @param hours Logical, determines whether the function will check whether
#'     all the hourly observations are there.
#' @param datename string, the name of the
#' @param dtformat The format the date time string is in.
#' @return This function returns TRUE if there are no missing observations
#'     and returns a vector of the dates of the missing observations
#'     if there are any.
#' @export
timeSeriesCheck <- function(x, start=NA, end=NA, hours=FALSE, datename='dt',
                            dtformat="%Y-%m-%d %H:%M:%OS") {


    if (hours) {
        if (!is.POSIXct(x[1,datename])) {
            x[,datename] <- as.POSIXct(x[,datename], format=dtformat)
        }

        if (!is.POSIXct(start)) {
            start <- as.POSIXct(start, format=dtformat)
        }

        if (!is.POSIXct(end)) {
            end <- as.POSIXct(end, format=dtformat)
        }

        datetimes <- seq(start, end, by='hour')

    } else {
        if (!is.Date(x[1,datename])) {
            x[,datename] <- as.Date(x[,datename], format=dtformat)
        }

        if (!is.Date(start)) {
            start <- as.Date(start, format=dtformat)
        }

        if (!is.Date(end)) {
            end <- as.Date(end, format=dtformat)
        }

        datetimes <- seq(start, end, by=1)
    }

   # print(head(as.character(datetimes)))
  #  print(head(as.character(x[, datename])))

    missingDTs1 <- setdiff(as.character(datetimes), as.character(x[, datename]))

    if (length(missingDTs1)==0) {
        return(TRUE)
    } else {
        return(missingDTs1)
    }


}

#' Calculates the length of a day
#'
#' This function calculates the length of a day given the day of the year and
#'     the latitude of the location.
#'
#' @param jul The julian day/day of the year (1-366)
#' @param lat The latitude of the location
#' @param ret I am unsure what this argument is for
#' @return The length of the day in hours, along with the time of sunrise and
#'     sunset.
daylength <- function(jul, lat=38.5, ret='') {
    #from extractTemp.py: Cesaraccio et al (2001) "An improved model for determining degree-day values..."
    #jul is the julian date
    #lat is the latitude of the location
    #all calculations are in radians
    #dayangle is the day angle
    #dec is the sun's declination
    #hourangle is the hour angle at sunset

    dayangle <- 2*pi*(jul-1)/365
    dec <- 0.006918 - 0.399912*cos(dayangle) + 0.070257*sin(dayangle) - 0.006758*cos(2*dayangle) + 0.000907*sin(2*dayangle) - 0.002697*cos(3*dayangle) + 0.001480*sin(3*dayangle)
    hourangle <- acos(-tan(lat*pi/180)*tan(dec))

    daylen <- 24*hourangle/pi
    sunrise <- 12 - daylen/2 - dec
    sunset <- 12 + daylen/2 - dec

    ss <- c(daylen, sunrise, sunset)
    return(ss)

}


