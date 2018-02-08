#' @include parameterlistmethods.R
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
#' @param stage the number of the stage of the phenological model
#' @return The thermal sums for a given series of years.
DTsum <- function(pars, fdat, tdat, form, length, stage) {

	# for walnut
	#fdat is data for the 'fruit'
    years <- fdat[,'year'] #extracting the years we are interested in

    if (form %in% c('gdd','gddsimple')) { #if it is a GDD model

        start <- fdat[, paste0('event',stage)] #the day of the starting event
        end <- start+length #the start + the length of the model/threshold

    } else { #if it is a GDH model
        start <- (fdat[, paste0('event',stage)]*24)-23 #convert days to hours
        end <- start+length*24 # convert days to hours
    }


    #for each year extract temperature vector data frame or list so it can be
        #used to calculate thermal time
	templist <- lapply(1:length(years), function(i) {
	    tempdays <- seq(start[i], end[i], by=1)

	    if (is.data.frame(tdat[[1]])) {
            tdat[[as.character(years[i])]][tempdays,]

	    } else {
	        tdat[[as.character(years[i])]][tempdays]
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
#' @return The thermal sums for a given series of years.
TTTsum <- function(pars, fdat, tdat, form, length, stage) {

    #print(str(pars))

    years <- fdat[,'year'] #extract years
    start <- fdat[,paste0('event',stage)] #get the beginning date of the stage

    #getting the temperatures
    if (form %in% c('gdd', 'gddsimple')) {

        #extracting temperature vectors but only data from the start date
        #until the end of the year
        templist <- lapply(1:length(years), function(i) {
            tdat[[as.character(years[i])]][start[i]:365,]
        })


    } else if (form %in% c('linear', 'flat', 'triangle', 'asymcur',
                           'anderson')) {

        #extracting temperature vectors but only data from the start hour
        #until the end of the year
        templist <- lapply(1:length(years), function(i) {
            tdat[[as.character(years[i])]][(start[i]*24-1):(365*24)]
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
#' @param length the length of thermal time accumulation (in days). It can be
#'     either a set length of time (one number) or the total length of the
#'     stage (one length for each entry in fdat).
#' @param stage the number of the stage of the phenological model
#' @return The thermal sums for a given series of years.
#' @export
thermalsum <- function(pars, fdat, tdat, modtype, form, length, stage) {

    if (!(is.numeric(length) | is.integer(length))) {
        stop('Length must be numeric or an integer')
    }

    if (modtype=='DT') {
        ths <- DTsum(pars, fdat, tdat, form, length, stage)

    } else if (modtype=='TTT') {
        ths <- TTTsum(pars, fdat, tdat, form, length, stage)
        #print(ths)

    } else {
        stop('Only options for model types are DT and TTT.')
    }

    return(ths)
}


