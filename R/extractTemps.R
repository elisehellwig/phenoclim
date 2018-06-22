#' @include flipday.R

# This file will have all the functions needed to change a normal data frame into a templist for using in the models



#' Extracts temperatures from a data frame
#'
#' This function extracts temperature data from a data frame over a given set of
#'     years. It is used in \code(extracttemplist() ).
#'
#' @param tdat a data frame containing the temperature data.
#' @param years a vector of years over which to extract temperature data.
#' @param tempname name of the column that contains the temperature data. The
#'      function will try and detect the name of the column if tempname is left
#'      as NA.
#' @param firstevent the first thing that happens
#' @param mclass chr, c('FlowerModel', 'PlantModel')
#' @param yearname the name of the column that specifies the year.
#' @param dayname the name of the column that specifies the day of the year,
#'     numeric.
#' @param hourly logical, is this data for a model that requires hourly
#'     temperature values?
#' @param mclass character, c('PlantModel','FlowerModel').
#' @return \code{extracttemp} returns a list the same length as \code{years},
#'     where each element of the list is a vector of all the temperatures in
#'     that year.
#' @details If tempname is left as NA the function will try to detect the
#'     correct column (either temp, tmin or tmax). However, it is safer to just
#'     specify the correct column name.
#' @export
extracttemp <- function(tdat, years, firstevent, mclass,
                        tempname=NA, yearname='year',
                        hourly=TRUE) {


    tempdat <- tdat[tdat$year %in% years, ]
    yrs <- years[-1]

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

    tempnames <- c(tnames, 'day', 'dayindex', 'index')

    if (hourly) {
        tempnames <- c(tempnames,'hour')
    }

    convtemp <- tempyearconversion(tempdat, firstevent, modclass=mclass,
                                   hourly=hourly)

    #extracting the temperature data from the data frame and putting it
    #in a list
    tlist <- lapply(1:length(yrs), function(i) {
        rows <- which(convtemp[,yearname]==yrs[i])

        convtemp[rows,tempnames]
    })

    #nameing the elements of the list
    names(tlist) <- yrs
    return(tlist)
    }

##############################

#'Extracts the right templist based on the forms provided
#'
#' @param temps data.frame, where all the temperature data is stored.
#' @param years numeric, the years for which temperature data is needed.
#' @param forms list or character, the functional forms that will be used to
#'     calculate the thermal time.
#' @param starts numeric, vector of days that the first event happened (bloom for PlantModels and harvest for FlowerModels)
#' @param mclass character, c('PlantModel','FlowerModel').
#' @return A list of two the first contains the daily extracted temps if they
#'     are needed (if not it contains NA) and the second element contains the
#'     hourly extracted temps if they are needed (if not it contains NA).
#' @export
extracttemplist <- function(temps, years, forms, starts, mclass) {

    if ((length(starts)+1)!=length(years) & mclass=='FlowerModel') {
        stop('You must have temp data for one year before you have phenology data.')
    } else if ((length(starts))!=length(years) & mclass=='PlantModel') {
        stop('You must have temp data for each year you have phenology data.')
    }

    hforms <- c('linear','flat','anderson','triangle','asymcur') #GDH forms
    ttforms <- unlist(forms) #vector of thermal time forms we care about

    if ('gdd' %in% ttforms | 'gddsimple' %in% ttforms) {
        #do we care about GDD forms?
        daytemps <- unique(temps[,c('year','day','tmin','tmax')])
        #if so extract daily data for those forms
        daytemplist <- extracttemp(daytemps, years, starts,
                                   tempname=c('tmin','tmax'),
                                   hourly=FALSE, mclass=mclass)
    } else {
        daytemplist <- NA
    }


    if (ifelse(any(ttforms %in% hforms), TRUE, FALSE)) {
        #do we care about hourly forms, if so extract hourly data for those
        #forms
        hourtemplist <- extracttemp(temps, years, starts, tempname='temp',
                                    hourly=TRUE, mclass=mclass)
    } else {
        hourtemplist <- NA
    }

    return(list(daytemplist, hourtemplist))
}



