#' @include parameterlistmethods.R fithelper.R
NULL



#' Calculates thermal time sums for DT model
#'
#' Calculates thermal time sums for a given length of days after flowering for
#' a series of years.
#'
#' @param ctemps Cardinal temperatures
#' @param yrs the years we have phenology data for.
#' @param tdat list containing the temperature information
#' @param form the functional form of the thermal time accumulation
#' @param startDate POSIXct, the date to start accumulating time or thermal
#'     time towards the model threshold.
#' @param threshDate POSIXct, the date to stop accumulating thermal
#'     time (threshold does not vary).
#' @param varying character, c('start', 'threshold') should either of these
#'     pars vary from year to year.
#' @param mclass character, class of model to be estimating, options are
#'     'PlantModel' or 'FlowerModel'. If you have negative day values, you
#'     probably want flower model.
#' @return The thermal sums for a given series of years.
DTsum <- function(ctemps, yrs, tdat, form, startDate, threshDate, varying,
                  mclass) {


    #print('DTsum')
    #possible forms
   posforms <-  c('anderson','linear','flat','triangle','asymcur','gdd',
                  'gddsimple', 'chillbasic', 'utah','utahalt')

   if (!(form %in% posforms)) {
       stop('form must be linear, flat, triangle, asymcur, anderson, gdd,
            gddsimple, chillbasic, utah or utahalt.')

   }


   #print(head(startDate))
   #print(head(endDate))
    modInterval <- interval(startDate, threshDate)
    #print(3)

    if (length(modInterval)==1) {
        modInterval <- rep(modInterval, length(yrs))

    } else if (!(length(modInterval)==length(yrs))) {
        stop('modInterval must be either length one or have a length equal to the number of years in the data.')
    }


    #for each year extract temperature vector data frame or list so it can be
        #used to calculate thermal time

    if (form %in% c('gdd','gddsimple')) {
        tnames <- c('tmin','tmax')
    } else {
        tnames <- 'temp'
    }


    ids <- lapply(1:length(yrs), function(i) {

    })

    templist <- lapply(1:length(yrs), function(i) {
        tdat[which(tdat$dt %within% modInterval[i]), tnames]
    })


    tsums <- sapply(1:length(yrs), function(i) {
        #create list of parameters and data to send to do.call+form
        plist <- parslist(templist[[i]], unlist(ctemps), sum=TRUE)
        do.call(form, plist) #calculate the thermal time
    })


	return(tsums)
}


#' Calculates thermal time sums for TTT model
#'
#' Calculates thermal time sums for a given amount of thermal time after
#' flowering for a series of years.
#'
#' @param pars Cardinal temperatures
#' @param yrs the years we have phenology data for.
#' @param tdat list containing the temperature information
#' @param form the functional form of the thermal time accumulation
#' @param startDate POSIXct, the date to start accumulating time or thermal
#'     time towards the model threshold.
#' @param thresh numeric, the length of thermal time accumulation (in either
#'     days or thermal time units).
#' @param varying character, c('start', 'threshold') should either of these
#'     pars vary from year to year.
#' @param mclass character, class of model to be estimating, options are
#'     'PlantModel' or 'FlowerModel'. If you have negative day values, you
#'     probably want flower model.
#' @return The thermal sums for a given series of years.
TTTsum <- function(pars, yrs, tdat, form, startDate, thresh, varying, mclass) {

    #print(str(pars))

    #print('TTTsum')

    if (mclass=='FlowerModel') {
        endDate <- dayToDate(yrs+1, 184, 'FlowerModel')

    } else {
        endDate <- dayToDate(yrs, 365, 'PlantModel')
    }

    modInterval <- interval(startDate, endDate)

    if (length(modInterval)==1) {
        modInterval <- rep(modInterval, length(yrs))

    }


    #getting the temperatures
   if (form %in% c('gdd', 'gddsimple')) {
        tnames <- c('tmin','tmax')

   } else if (form %in% c('linear', 'flat', 'triangle', 'asymcur',
                          'anderson', 'chillbasic','utah','utahalt')) {
        tnames <- 'temp'

   } else {
        stop('type must be one of the following: gdd, gddsimple, linear
             flat, triangle, asymcur, anderson, chillbasic, utah, utahalt')
    }


    templist <- lapply(1:length(yrs), function(i) {
        td <- tdat[which(tdat$dt %within% modInterval[i]), ]
        td[order(td$dt), tnames]
    })

    #Calculating which day the plant will reach the thermal time threshold
    day <- predictevent(unlist(pars), templist, form, thresh)

    return(day)

}


#' Calculates thermal time sums
#'
#' Calculates thermal time sums for either the TTT or DT time model
#'
#' @param ctemps Cardinal temperatures
#' @param yrs numeric, the years we have phenology data for.
#' @param tdat list containing the temperature information
#' @param modtype character, specifies what type of model is being run. Can be
#'     either DT (Day Threshold) or TTT (Thermal Time Threshold).
#' @param form the functional form of the thermal time accumulation
#' @param start POSIXct, the date to start accumulating time or thermal
#'     time towards the model threshold.
#' @param thresh the length of thermal time accumulation (either a date or
#'      thermal time units).
#' @param varying character, c('start', 'threshold') should either of these
#'     pars vary from year to year.
#' @param mclass character, type of model to be estimating, options are
#'     'PlantModel' or 'FlowerModel'.
#' @param startingevent numeric, days first event happened each year.
#' @return The thermal sums for a given series of years.
#' @export
thermalsum <- function(ctemps, yrs, tdat, modtype, form, start, thresh,
                       varying, mclass, startingevent=NA) {

   # print('thermalsum')

    #print(start)
    #print(thresh)

    if (is.numeric(start) | (!isDateTime(thresh) & modtype=='DT')) {

        if (is.na(startingevent[1])) {
            stop('You must specify the starting event in days if you want to
                 input start and thresh as numerics.')
        } else {
            sth <- formatParameters(yrs, startingevent, start, thresh, modtype,
                                   mclass, varying)
        }

        if (is.numeric(start)) {
            start <- sth[[1]]
        }

        if (is.numeric(thresh)) {
            thresh <- sth[[2]]
        }

    }

    #print(start)
    #print(thresh)

    if (modtype=='DT') {
        ths <- DTsum(ctemps, yrs, tdat, form, start, thresh, varying, mclass)

    } else if (modtype=='TTT') {
        ths <- TTTsum(ctemps, yrs, tdat, form, start, thresh, varying, mclass)
        #print(ths)

    } else {
        stop('Only options for model types are DT and TTT.')
    }

    #print(ths)
    return(ths)
}


