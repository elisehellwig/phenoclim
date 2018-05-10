#' @include FlowerPlant.R
NULL


#note the point of this file is to create a temperature data.frame that
#contains the data for a FlowerModel. Both with a day and a dayindex, where
#the days from the previous year are negative. This is finally accomplished in
#the function tempyearconversion().

createEnd <- function(l1, l2, classtype, startday) {

    if (classtype=='PlantModel') {
        multiplier <- 1
    } else {
        multiplier <- -1
    }

    if (startday) {
        if (l2<0) {
            End <- l2*24 + 23
        }

    } else {
        End <- l1 + multiplier*(l2*24-23)
    }

    return(End)
}




#' Moves temperature data a year later
#'
#' This function takes temperature data and converts the year to the next year,
#'     converts the days to be negative (counting back from 0 as Dec 31), and
#'     creates a day index to use in extracting temps. The day index sets the
#'     start date to be 1. This only converts data from one year. To do many
#'     years use tempyearconversion()
#'
#' @param year numeric, the year you need to add temperature data to
#' @param tdat data.frame, the object that stores all your temperature data.
#' @param start numeric, the day of the year to go back to in the previous year.
#' @param hourly logical, is the data hourly?
#' @return A data.frame that contains all of the temperature data points with
#'     negative days as well as day
yearfliptemp <- function(year, tdat, start, hourly=TRUE) {

    beforeyear <- year-1 #identifies the year before the focal year

    #identifies all data from the before year
    beforerows <- which(tdat[,'year']==beforeyear & tdat[,'day']>=start)
    beforedat <- tdat[beforerows,]#extracts all data from the before year


    #get the number of days in the before year
    if (leap_year(beforeyear)) {
        yearlength <- 366
    } else {
        yearlength <- 365
    }

    #Create the day index (starts at 1 and goes to start)
    beforedat$dayindex <- beforedat$day - (start-1)

    #creates data.frame with the temp data and the day index
    if (hourly) {
        df <- data.frame(year=year,
                         day=beforedat$day,
                         dayindex=beforedat$dayindex,
                         temp=beforedat$temp,
                         hour=beforedat$hour)
    } else {
        df <- data.frame(year=year,
                         day=beforedat$day,
                         dayindex=beforedat$dayindex,
                         temp=beforedat$temp)
    }

    return(df)
}



#' Creates temp data for FlowerModel
#'
#' This function takes temperature data and reformats it so that it is
#'     compatible with the FlowerModel model structure. This means
#'     extracting temperatures from the previous year and adding them to the
#'     data but with negative numbers for days and a day index.
#'
#' @param tdat data.frame, the object that stores all your temperature data.
#' @param start numeric, the day of the year to go back to in the previous year.
#' @param hourly logical, is the data hourly?
#' @return A data.frame that contains all of the temperature data points with
#'     negative days and a day index that starts from 1.
#' @export
tempyearconversion <- function(tdat, start, hourly=TRUE) {

    #get all the years you have temperature data for
    years <- sort(unique(tdat[,'year']))
    yrs <- years[-1] #remove first one, it is the first before year
    minyr <- min(yrs) #the first year to create temps for

    #print(yrs)

    # get the temp data for the before year for each year
    tempdf <- ldply(yrs, function(y) {
        #print(y)
        yearfliptemp(y, tdat, start, hourly)
    })

    #list only the variables we need
    tdatnames <- c('year','day','dayindex','temp')

    if (hourly) { #add variable hour if necessary
        tdatnames <- c(tdatnames,'hour')
    }

    #extract only the variables we need from our data.frame
    tdat <- tdat[,tdatnames]
    #need to convert tdat days to add 1 +365 - start

    #shift the day index to reflect the fact we have added temperature data onto
    #the beginning of the time series (from the previous year)
    shift <- yearlength(tdat[,'year']) - (start-1)
    tdat$dayindex <- tdat[,'day'] + shift

    #combine the before year data and the present year data
    tdfthin <- rbind(tdat, tempdf)

    #remove any years that are before the first year
    tdfsmall <- tdfthin[tdfthin$year >= minyr, ]


    return(tdfsmall)

}



