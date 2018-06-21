#' @include FlowerPlant.R
NULL


#note the point of this file is to create a temperature data.frame that
#contains the data for a FlowerModel. Temperature data has a day index which is
#1 when on the day of the starting event. This is finally accomplished in
#the function tempyearconversion().

#' Converts day of the year to day after event (DAE)
#'
#' This function takes in a day of the year and the day of a phenological event
#'     and converts the day of the year to the number of days after the
#'     phenological event. The year of each event must be specified so that the
#'     appropriate year length can be used.
#'
#' @param day numeric, the day of the year you want to convert to days after
#'     event
#' @param event numeric, the day of the event for each year of interest
#' @param years numeric, the year that each event happened
#' @return A numeric vector specifying the number of days after the event that
#'    the day of interest happened.
dayToDAE <- function(day, event, years) {

    if (length(event)!=length(years)) {
        stop('You must have a year for each event.')
    }

    # Is there an extra day between the event and the day of the year we are
    #interested in? if so leapmod will be 1, else 0
    leapmod <- ifelse(event <= 59 & day>59 & leap_year(years), 1, 0)

    #length of the years in question
    ylength <- yearlength(years)

    #extending days of the year to greater than 365 if necessary
    day <- ifelse(day < event, day+ylength, day)

    dae <- day - event + leapmod + 1

    return(dae)
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
                         tmin=beforedat$tmin,
                         tmax=beforedat$tmax)
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
#'    Must remove the last element to get the numbers to line up
#' @param hourly logical, is the data hourly?
#' @param modclass character, c('PlantModel','FlowerModel').
#' @return A data.frame that contains all of the temperature data points with
#'     negative days and a day index that starts from 1.
#' @export
tempyearconversion <- function(tdat, start, modclass, hourly=TRUE) {

    #get all the years you have temperature data for
    years <- sort(unique(tdat[,'year']))
    yrs <- years[-1] #remove first one, it is the first before year

    if (modclass=='FlowerModel') {
        yrs2 <- years[-length(years)]
    } else {
        yrs2 <- years
    }

    if (length(start)!=length(yrs2) & length(start)!=1) {
        stop('You must either have 1 value for start or the same number as the number of years you are creating temperature data for.')
    }

    #print(yrs)

    #list only the variables we need
    tdatnames <- c('year','day')

    if (hourly) { #add variable hour if necessary
        tdatnames <- c(tdatnames,'temp', 'hour')
    } else {
        tdatnames <- c(tdatnames,'tmin', 'tmax')
    }

    if (length(start)==1) {
        start <- rep(start, length(yrs2))
    }

    #extract only the variables we need from our data.frame
    tdat <- tdat[,tdatnames]

    #we only get out data for the years that are not the first year in our
    #dataset

    if (modclass=='FlowerModel') {
        tdat2 <- tdat[tdat$year %in% yrs, ]
    } else {
        tdat2 <- tdat[tdat$year %in% years, ]
    }


    #shift the day index to reflect the fact we have added temperature data onto
    #the beginning of the time series (from the previous year) or remove temp
        #data from the series so the index always starts at 1.


    yrlens <- yearlength(yrs2)

    shiftsmall <- -start

    shiftlist <- lapply(seq_along(start), function(i) {
        rep(shiftsmall[i], each=yrlens[i])
    })
    shift <- do.call(c, shiftlist)

     if (modclass=='FlowerModel') {
        shift <- yearlength(tdat2[,'year']) + shift

    }

    tdat2$dayindex <- tdat2[,'day'] + shift

    if (modclass=='FlowerModel') {

         # get the temp data for the before year for each year
        tempdf <- ldply(seq_along(yrs2), function(i) {
            #print(y)
            yearfliptemp(yrs[i], tdat, start[i], hourly)
        })

        tdf <- rbind(tdat2, tempdf)

    } else if (modclass=='PlantModel') {
        tdf <- tdat2 # renaming

    } else {
        stop('modclass must be either PlantModel or FlowerModel')
    }

    #need to convert tdat days to add 1 +365 - start



    #combine the before year data and the present year data

    return(tdf)

}



