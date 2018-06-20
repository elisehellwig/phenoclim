


##############################################



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

    pyears <- unique(phenology[,'year'])
    tyears <- unique(temperature[,'year'])

    missingyears <- setdiff(pyears,tyears)


    if (length0(missingyears)) {
        return(TRUE)
    } else {
        return(list(FALSE,missingyears))
    }
}

##############################################

predictevent <- function(pars, temps, form, length) {

    #changes form to asymcur since anderson form is just asymcur with cardinal
        #temps 4, 25, and 36
    if (form=='anderson') form <- 'asymcur'

    #calculates the thermal time sums
    gd <- lapply(temps, function(v) {
        plist <- parslist(v, pars) #puts the cardinal temps in the list format
        tt <- do.call(form, plist) #applies form function do them
        cumsum(tt) #creates cummulative sum
    })

    #print(sapply(gd, function(v) max(v)))

    #if the model is a GDH model then you have to divide the length by 24 to
     #give you the correct number of days, which is what we want.
    if (form %in% c('gdd', 'gddsimple')) {
        unitfactor <- 1
    } else {
        unitfactor <- 24
    }

    if (length(length)==length(gd)) { #the length depends on the year

        #figures out which day was the first day to meet the thermal time
            #threshold
        eventday <- sapply(1:length(gd), function(i) {
            suppressWarnings(min(which(gd[[i]]>length[i]))/unitfactor)
        })

    } else if (length(length)==1) { #the length does not depend on the year

        #figures out which day was the first day to meet the thermal time
            #threshold
        eventday <- sapply(gd, function(v) {
            suppressWarnings(min(which(v>length))/unitfactor)
        })
    }

    #returns the day each year when the thermal time the plant experienced
        #reached the thermal time threshold
    return(eventday)
}


##############################

#' Returns the right temperature list
#'
#' @param form character, what is the form of the model
#' @param daily list the daily temperature data
#' @param hourly list, the hourly temperature data
#' @return A list of temperature data based on the functional form of the model.
whichtemp <- function(form, daily, hourly) {

    if (form %in% c('gdd', 'gddsimple')) {
        return(daily)
    } else {
        return(hourly)
    }
}


