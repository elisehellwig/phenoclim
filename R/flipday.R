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








#' Converts years for bloom modeling
#'
#' Models for bloom necessarily incorporte phenology data from multiple years.
#'     this is not compatible with the current phenoclim model paradigm. So this
#'     function groups the phenogical events by blooming event not by year.
#'     This function ONLY converts the phenological event days. It does not
#'     provide an indext for extracting temperatures and calculating thermal
#'     time. For that you need to use yearfliptemp.
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
phenoyearconversion <- function(fdat, firstyear=NA, lastyear=NA,
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
#' @param startday logical, is the start day being estimated in this model, as
#'     opposed to using the bloom date.
#' @param stgtype character, is the model a PlantModel or a FlowerModel?
#'     Options are 'PlantModel' or 'FlowerModel'.
#' @return a vector of indexes that can be used to extract temperature data.
#' @export
startEnd <- function(start, stglength, hourly=TRUE, stgtype) {


    if (hourly) {
        start <- start*24 - 23
        lengthmod <- stglength*24

    } else {
        lengthmod <- stglength

    }

    if (stgtype=='PlantModel') {
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


    indexEnd <- start+shift #index of the last day used to predict flowering

    #Which days are less than the last day used to predict flowering
    smalldays <- ifelse(tdfthin$day<(indexEnd), TRUE, NA)*1:length(indexEnd)

    #the days that we are going to use to predict flowering.
    smalldays <- smalldaysTF[!is.na(smalldays)]

    # extract the data we are going to use to predict flowering
    tdfsmall <- tdfthin[smalldays, ]

    #remove any years that are before the first year
    tdfsmalltoo <- tdfsmall[tdfsmall$year > minyr, ]


    return(tdfsmalltoo)

}



