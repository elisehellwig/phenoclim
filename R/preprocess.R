#' @include general.R

#' Average a vector of dates
#'
#' This function finds the average of a vector of objects of the class "Date"
#'     representing calendar dates.
#'
#' @param dates an object of the class "Date"
#' @return This function returns an object of the class "Date" that is the
#'     average of the dates in the vector.
#' @examples
#' n <- runif(10, 1, 15000)
#' d <- as.Date(n, origin='1970-01-01')
#' avgdate(d)
#' @export
avgdate <- function(dates) {
    #averages dates together

    d <- as.integer(dates) #converts the dates to integers

    #averages them and takes the next whole number up
    ad <- ceiling(sum(d, na.rm=TRUE)/sum(!is.na(dates)))

    #converts that number back to the date
    aD <- as.Date(ad, origin="1970-01-01")

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
        cols <- c('day','hour')

        if (is.leapyear(year) & length(days)==365) {
            days <- expand.grid(c(days,366), 1:24)
        }

    } else {
        cols <- 'day'

        if (is.leapyear(year) & length(days)==365) {
            days <- c(days, 366)
        }
    }

    if (is.na(limits)) {

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
#'     two columns, one named 'year' with the year, and one named 'day' with
#'     the julian day. If hours is TRUE, it needs a column
#' @param years A numeric vector of years to check. If NA the function will
#'     check all the years in the data.frame.
#' @param limits A numeric vector of length two giving the minimum and
#'     maximum julian day to check. If left NA, the function will check all
#'     the days of the year.
#' @param hours, Logical, determines whether the function will check whether
#'     all the hourly observations are there
#' @return This function returns TRUE if there are no missing observations
#'     and returns a list of the days (and hours) of the missing observations
#'     if there are any.
#' @export
timeSeriesCheck <- function(x, years=NA, limits=NA, hours=FALSE) {

    if (is.na(years)) {
        years <- unique(x$year)
    }

    if (is.na(limits)) {
        alldays <- 1:365

    } else {
        alldays <- limits[1]:limits[2]
    }

    missing <- lapply(years, function(y) {
        missingDays(x[x$year==y,], alldays, limits, hours)
    })

    names(missing) <- years

    noneMissing <- sapply(missing, function(v) is.na(v))

    if (all(noneMissing)) {
        return(TRUE)
    } else {
        return(missing)
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


