#' Extracts temperatures from a data frame
#'
#' This function extracts temperature data from a data frame for specific days
#'     in a year over a given set of years. It is used with the yearsums and
#'     minrmse functions.
#'
#' @param tdat a data frame containing the temperature data.
#' @param years a vector of years over which to extract temperature data.
#' @param starts a number or vector of numbers between 1 and 365 (inclusive)
#'     specifying what days of the year to begin the temperature data extraction.
#' @param ends a number or vector of numbers between 1 and 365 (inclusive)
#'     specifying what days of the year to end the temperature data extraction.
#' @param tempname name of the column that contains the temperature data. The
#'      function will try and detect the name of the column if tempname is left
#'      as NA.
#' @param yearname the name of the column that specifies the year.
#' @param dayname the name of the column that specifies the day of the year,
#'     numeric.
#' @return \code{extracttemp} returns a list the same length as \code{years},
#'     where each element of the list is a vector of all the temperatures in
#'     between \code{starts} and \code{ends} that year.
#' @details If tempname is left as NA the function will try to detect the
#'     correct column (either temp, tmin or tmax). However, it is safer to just
#'     specify the correct column name.
#' @export
extracttemp <- function(tdat, years, starts, ends, tempname=NA,
                        yearname='year', dayname='day') {


    #checks if there is a start day for each year
    if (length(starts)==length(years)) {
        start <- starts

    #checks if the start day is the same for each year
    } else if (length(starts)==1) {
        start <- rep(starts, length(years))

    } else { #if not there is a problem
        stop('Starts must either have a start day for each year or the start date must be the same for all years')

    }


    #checks if there is a end day for each year
    if (length(ends)==length(years)) {
        end <- ends

    #checks if the end day is the same for each year
    } else if (length(ends)==1) {
        ends <- rep(ends, length(starts))

    } else { #if not there is a problem
        stop('Starts must either have an end day for each year or the end day must be the same for all years')

    }

    #print(years)

    #checking to see if the function needs to dectect the temperature column
    if (is.na(tempname)) {

        #detecting the temperature column
        if ('temp' %in% names(tdat)) {
            tnames <- 'temp'

        } else if ('tmin' %in% names(tdat) & 'tmax' %in% names(tdat)) {
            tnames <- c('tmin','tmax')

        } else {
            stop('The names of the variables with temperature data must be
                    temp, tmin and tmax, or it must be specified with the
                    tempname argument.')
        }

    } else {
        tnames <- tempname
    }

    #extracting the temperature data from the data frame and putting it
    #in a list
    tlist <- lapply(1:length(years), function(i) {
        rows <- which(tdat[,yearname]==years[i] & tdat[,dayname]>=start[i] & tdat[,dayname]<=end[i])

        tdat[rows,tnames]
    })

    #nameing the elements of the list
    names(tlist) <- years
	return(tlist)
}


##############################################

#' Checks if a year is a leap year
#' @param year a numeric vector of years to test
#' @return A logical vector. True indicates the year is a leap year.
#' @examples
#' y <- seq(1990, 2010)
#' is.leapyear(y)
is.leapyear <- function(year){
  #http://en.wikipedia.org/wiki/Leap_year
  return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
}


head.list <- function(obj, n = 6L, ...)
{
    stopifnot(length(n) == 1L)
    origN <- n
    n <- if (n < 0L)
        max(length(obj) + n, 0L)
    else min(n, length(obj))
    lapply(obj[seq_len(n)], head, origN, ...)
}


rmsd <- function(fit, dat, na.rm=FALSE) {
	if (na.rm) {
		frows <- which(is.na(fit))
		fit1 <- fit[-frows]

		drows <- which(is.na(dat))
		dat1 <- dat[-drows]
	} else {
		fit1 <- fit
		dat1 <- dat
	}

	rmsd <- sqrt(sum((fit1-dat1)^2)/length(fit1))
	return(rmsd)
}
