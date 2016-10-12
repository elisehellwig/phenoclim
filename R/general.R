#' Calculates the Root Mean Squared Deviation/Error
#'
#' This function calculates the root mean squared error/deviation (RMSD/E)
#'     between two vectors of numbers.
#'
#' @param x numeric, the first vector
#' @param y numeric, the second vector
#' @param na.rm logical, should NAs be removed from the vectors?
#' @return This function returns the RMSD of the two vectors.
#' @examples
#' a <- rnorm(10)
#' b <- rnorm(10)
#' rmsd(a,b)
#' @export
rmsd <- function(x, y, na.rm=FALSE) {

    if (length(x)!=length(y)) { #make sure vectors are the same length
        stop('Vectors x and y must be of equal length.')
    }


    if (na.rm) {

        xrows <- which(is.na(x)) #identifying the NAs in both vectors
        yrows <- which(is.na(y))

        rows <- union(xrows, yrows) #getting all the rows with NAs

        x1 <- x[-rows] #removing all the rows with NAs from both x and y
        y1 <- y[-rows]

    } else {
        x1 <- x
        y1 <- y
    }

    rmsd <- sqrt(sum((x1-y1)^2)/length(x1)) #calculating the RMSD

    return(rmsd)
}

##############################################


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
    if (is.na(tempname[1])) {

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
#' @export
is.leapyear <- function(year){
  #http://en.wikipedia.org/wiki/Leap_year
  return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
}

##############################################


#' Checks if length of vector is zero
#'
#' @param x vector or list
#' @return logical, TRUE if the vector is of length 0, FALSE if it has a length
#'     greater than zero
length0 <- function(x) {
    if (length(x)==0) TRUE else FALSE
}


##############################################
#' returns a column of the phenology data.frame
#'
#' @param dat the phenology data frame
#' @param i the number of the phenology event to be extracted
#' @return A vector with the julian days of the ith phenological event
eventi <- function(dat, i) {
    d <- dat[,paste0('event',i)]
    return(d)
}



##############################################
#' Checks year presence in temperature list
#'
#' This function checks to make sure that for every year where there is
#'     phenology data there is also temperature data.
#'
#' @param phenology data.frame, the data.frame containing the phenology data.
#' @param temperature, list, the list containing all the temperature data.
#' @return Logical. Returns TRUE if all years of phenology data also have
#'     corresponding temperature data. Otherwise it returns the years that are
#'     missing temperature data.
checktempyears <- function(phenology, temperature) {

    pyears <- phenology[,'year']
    temp <- temperature

    if (is.list(temp[[1]])) {

        tyears <- lapply(temp, function(l) names(l))
        missingyears <- lapply(tyears, function(v) setdiff(pyears, v))
        mylogical <- sapply(missingyears, function(l) length0(l))

        if (all(mylogical)) {
            return(TRUE)

        } else {
            return(list(FALSE, missingyears[!mylogical]))
        }


    } else {
        tyears <- names(temp)
        missingyears <- setdiff(pyears, tyears)

        if (length0(missingyears)) {
            return(TRUE)
        } else {
            return(list(FALSE,missingyears))
        }
    }
}

##############################################

predictevent <- function(pars, temps, form, length) {

    gd <- lapply(temps, function(v) {
        plist <- parlist(v, pars)
        tt <- do.call(form, plist)
        cumsum(tt)
    })

    if (length(length)==length(gd)) {

        eventday <- sapply(1:length(gd), function(i) {
            suppressWarnings(min(which(gd[[i]]>length[i])))
        })
    } else if (length(length)==1) {

        eventday <- sapply(gd, function(v) {
            suppressWarnings(min(which(v>length)))
        })
    }


    return(eventday)
}






