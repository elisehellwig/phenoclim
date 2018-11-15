#' @include parameterfunctions.R
NULL

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

#' Is X a date or time object?
#'
#' This function tests if the object x is a date or time object (Period or
#'     POSIXct).
#'
#' @param x vector, the object you want to test
#' @return logical, is the object one of the date time classes specified above.
#' @export
isDateTime <- function(x) {

    classes <- c(is.period(x),
                 is.POSIXct(x))

    isanydt <- any(classes)

    return(isanydt)
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
#' @param modclass class of model, FlowerModel or PlantModel
#' @param i the number of the phenology event to be extracted
#' @return A vector with the julian days of the ith phenological event
eventi <- function(dat, modclass, i) {

    if (modclass=='FlowerModel') {
        d <- dat[,paste0('event',i)]

    } else if (modclass=='PlantModel') {
        d <- dat[,paste0('event',i+1)]
    } else {
        stop('Modclass must be FlowerModel or PlantModel.')
    }

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
#' @param modelclass character, is the model a FlowerModel or a PlantModel
#' @return Logical. Returns TRUE if all years of phenology data also have
#'     corresponding temperature data. Otherwise it returns the years that are
#'     missing temperature data.
checktempyears <- function(phenology, temperature, modelclass) {

    if (modelclass=='FlowerModel') {
        pyears1 <- unique(phenology[,'year'])
        pyears2 <- pyears1-1
        pyears <- union(pyears1, pyears2)

    } else {
        pyears <- unique(phenology[,'year'])
    }


    tyears <- unique(temperature[,'year'])

    missingyears <- setdiff(pyears,tyears)


    if (length0(missingyears)) {
        return(TRUE)
    } else {
        return(list(FALSE,missingyears))
    }
}

##############################################

#' Finds the day the threshold is met
#'
#' This function calculates the thermal time or chill accumulated and then
#'     identifies what day the thermal time or chill threshold is met.
#'
#' @param pars cardinal temperatures or NA for utah model
#' @param temps vector of temperatures
#' @param form the functional form of the thermal time or chill accumulation
#' @param length the threshold that the model runs until
#' @return A vector of days when the threshold is reached
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

########################################################

#' Calculates the RMSE
#'
#' @param predicted numeric, the vector of predicted values
#' @param observed numeric, the vector of observed values
#' @return The root mean squared error of the two numeric vectors.
#' @examples
#' a <- rnorm(10)
#' b <- rnorm(10)
#' rmse(a,b)
#' @export
rmse <- function(predicted, observed) {

    dif <- predicted - observed #calculates residuals

    #squares, averages and takes square root of resids
    rmsd <- sqrt(mean(dif^2))

    return(rmsd)
}
