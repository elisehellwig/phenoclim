#' @include parameterlistmethods.R flipday.R
NULL



#' Calculates thermal time sums for DT model
#'
#' Calculates thermal time sums for a given length of days after flowering for
#' a series of years.
#'
#' @param pars Cardinal temperatures
#' @param fdat the data.frame containing the phenological information
#' @param tdat list containing the temperature information
#' @param form the functional form of the thermal time accumulation
#' @param length the length of thermal time accumulation (in days). It can be
#'     either a set length of time (one number) or the total length of the
#'     stage (one length for each entry in fdat).
#' @param lims numeric, start/stop pair if no length or start day.
#' @param stage the number of the stage of the phenological model.
#' @param varying character, c('start', 'threshold') should either of these pars
#'     vary from year to year.
#' @param mclass character, class of model to be estimating, options are
#'     'PlantModel' or 'FlowerModel'. If you have negative day values, you
#'     probably want flower model.
#' @return The thermal sums for a given series of years.
DTsum <- function(pars, fdat, tdat, form, length, lims, stage, mclass) {


    if (mclass=='FlowerModel') {
        start <- 1 # all FMs start at 1 because we use an index


        if (any(is.na(lims))) { #if both limits aren't present
                                #we end at flowering
            yrlen <- ifelse(is.leapyear(fdat$year), 366, 365) #year lengths
            shift <- yrlen - which(!is.na(lims))#What is the shift?
            end <- fdat[,'event1'] + shift # index of day of flowering
        } else {

        }
        end <- length #otherwise we end at the end of the length

    } else {
        #the day of the starting event
        start <- fdat[, paste0('event',stage)]

        #the start + the length of the model/threshold
        end <- start+length

        }

	# for walnut
	#fdat is data for the 'fruit'
    years <- fdat[,'year'] #extracting the years we are interested in

    if (form %in% c('gdd','gddsimple')) { #if it is a GDD model

        #create the index of days we want temperatures for
        tempindex <- startEnd(start, end, hourly=FALSE, forward=forward)


    } else { #if it is a GDH model
        start <- (start*24)-23 #convert days to hours

        if (stgtype=='PlantModel') {# convert days to hours
            end <- start+length*24 #adding for plant model
        } else {
            end <- start-length*24 #subtracting for flower model
        }

        #create the index of hours we want temperatures for
        tempindex <- startEnd(start, end, hourly=TRUE, forward=forward)

    }



    #for each year extract temperature vector data frame or list so it can be
        #used to calculate thermal time
	templist <- lapply(1:length(years), function(i) {

	    if (is.data.frame(tdat[[1]])) {
            tdat[[as.character(years[i])]][tempindex[[i]],]

	    } else {
	        tdat[[as.character(years[i])]][tempindex[[i]]]
	    }
	})

	if (form %in% c('gdd','gddsimple', 'linear','flat','triangle','asymcur')) {

	    #calculate thermal sum
	    tsums <- sapply(1:length(start), function(i) {
	        #create list of parameters and data to send to do.call+form
	        plist <- parslist(templist[[i]], unlist(pars), sum=TRUE)
	        do.call(form, plist) #calculate the thermal time
	    })


	} else if (form=='anderson') { #same as above just if you are working with
        tsums <- sapply(1:length(start), function(i) { #anderson functional form
            plist <- parslist(templist[[i]], c(4,25,36), sum=TRUE)
            do.call('asymcur', plist)
        })

	} else {
        stop('form must be linear, flat, triangle, asymcur, anderson, gdd, or gddsimple.')
	}

	return(tsums)
}


#' Calculates thermal time sums for TTT model
#'
#' Calculates thermal time sums for a given amount of thermal time after
#' flowering for a series of years.
#'
#' @param pars Cardinal temperatures
#' @param fdat the data.frame containing the phenological information
#' @param tdat list containing the temperature information
#' @param form the functional form of the thermal time accumulation
#' @param length the length of thermal time accumulation (in days). It can be
#'     either a set length of time (one number) or the total length of the
#'     stage (one length for each entry in fdat).
#' @param stage the number of the stage of the phenological model
#' @param forward logical, Do count forward from the starting event (as
#'     opposed to backward)? If you have negative values in your event days
#'     you forward should probably be FALSE.
#' @return The thermal sums for a given series of years.
TTTsum <- function(pars, fdat, tdat, form, length, stage, forward) {

    #print(str(pars))

    years <- fdat[,'year'] #extract years
    start <- fdat[,paste0('event',stage)] #get the beginning date of the stage

    #getting the temperatures
    if (form %in% c('gdd', 'gddsimple')) {


        if (forward) { #creating index to extract temperatures we want
            tempindex <- startEnd(start, 365, hourly=FALSE, forward=TRUE)
        } else {
            tempindex <- startEnd(start, (start-length), hourly=FALSE,
                                  forward=FALSE)
        }
        #extracting temperature vectors but only data from the start date
        #until the end of the year


        templist <- lapply(1:length(years), function(i) {
            tdat[[as.character(years[i])]][tempindex[[i]],]
        })


    } else if (form %in% c('linear', 'flat', 'triangle', 'asymcur',
                           'anderson')) {


        if (forward) {  #creating index to extract temperatures we want
            tempindex <- startEnd(start, 365, hourly=TRUE, forward=TRUE)
        } else {
            tempindex <- startEnd(start, (start-length), hourly=TRUE,
                                  forward=FALSE)
        }

        #extracting temperature vectors but only data from the start hour
        #until the end of the year for forward

        templist <- lapply(1:length(years), function(i) {
            tdat[[as.character(years[i])]][tempindex[[i]]]
        })

    } else {
        stop('type must be one of the following: gdd, gddsimple, linear
             flat, triangle, asymcur, anderson')
    }

    #Calculating which day the plant will reach the thermal time threshold
    day <- predictevent(unlist(pars), templist, form, length)

    return(day)

}


#' Calculates thermal time sums
#'
#' Calculates thermal time sums for either the TTT or DT time model
#'
#' @param pars Cardinal temperatures
#' @param fdat the data.frame containing the phenological information
#' @param tdat list containing the temperature information
#' @param modtype character, specifies what type of model is being run. Can be
#'     either DT (Day Threshold) or TTT (Thermal Time Threshold).
#' @param form the functional form of the thermal time accumulation
#' @param start numeric, the day to start accumulating time or thermal time
#'     towards the model threshold.
#' @param thresh numeric, the length of thermal time accumulation (in either
#'     days or thermal time units).
#' @param stage the number of the stage of the phenological model.
#' @param mclass character, type of model to be estimating, options are
#'     'PlantModel' or 'FlowerModel'.
#' @return The thermal sums for a given series of years.
#' @export
thermalsum <- function(pars, fdat, tdat, modtype, form, start, thresh, stage,
                       varying, mclass) {

    if (!(is.numeric(thresh) | is.integer(thresh))) {
        stop('Length must be numeric or an integer')
    }

    if (modtype=='DT') {
        ths <- DTsum(pars, fdat, tdat, form, start, thresh, stage, mclass)

    } else if (modtype=='TTT') {
        ths <- TTTsum(pars, fdat, tdat, form, start, thresh, stage, mclass)
        #print(ths)

    } else {
        stop('Only options for model types are DT and TTT.')
    }

    return(ths)
}


