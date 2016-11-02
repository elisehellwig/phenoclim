#' @include parameterlistmethods.R
NULL



#' Calculates thermal time sums for tta model
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
thermalgdsum <- function(pars, fdat, tdat, form, length, stage) {

	# for walnut
	#fdat is data for the 'fruit'
    years <- fdat[,'year']

    if (form %in% c('gdd','gddsimple')) {
        start <- fdat[, paste0('event',stage)]
        end <- start+length
    } else {
        start <- (fdat[, paste0('event',stage)]*24)-23
        end <- start+length*24
    }

    #print(start)
    #print(end)
    #print(length(tdat[[1]]))

	#print(head(tdat))
	templist <- lapply(1:length(years), function(i) {

	    if (is.data.frame(tdat[[1]])) {
            tdat[[as.character(years[i])]][start[i]:end[i],]

	    } else {
	        tdat[[as.character(years[i])]][start[i]:end[i]]
	    }
	})

	if (form %in% c('gdd','gddsimple', 'linear','flat','triangle','asymcur')) {
	    #print(pars)
	    tsums <- sapply(1:length(start), function(i) {
	        plist <- parlist(templist[[i]], unlist(pars), sum=TRUE)
	        #print(str(plist))
	        do.call(form, plist)
	    })

	    #print(tsums)

	} else if (form=='anderson') {
        tsums <- sapply(1:length(start), function(i) {
            plist <- parlist(templist[[i]], c(4,25,36), sum=TRUE)
            do.call('asymcur', plist)
        })

	} else {
        stop('form must be linear, flat, triangle, asymcur, anderson, gdd, or gddsimple.')
	}

	return(tsums)
}


#' Calculates thermal time sums for da model
#'
#' Calculates thermal time sums for a given amount of GD* after flowering for
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
thermaldaysum <- function(pars, fdat, tdat, form, length, stage) {

    #print(str(pars))

    years <- fdat[,'year']
    start <- fdat[,paste0('event',stage)]

    #getting the temperatures
    if (form %in% c('gdd', 'gddsimple')) {

        templist <- lapply(1:length(years), function(i) {
            tdat[[as.character(years[i])]][start[i]:365,]
        })

       # print(2)
    } else if (form %in% c('linear', 'flat', 'triangle', 'asymcur',
                           'anderson')) {

        templist <- lapply(1:length(years), function(i) {
            tdat[[as.character(years[i])]][(start[i]*24-1):(365*24)]
        })

    } else {
        stop('type must be one of the following: gdd, gddsimple, linear
             flat, triangle, asymcur, anderson')
    }


    #calculating the event day
    day <- predictevent(unlist(pars), templist, form, length)

    return(day)

}


#' Calculates thermal time sums
#'
#' Calculates thermal time sums for either the day or thermal time model
#'
#' @param pars Cardinal temperatures
#' @param fdat the data.frame containing the phenological information
#' @param tdat list containing the temperature information
#' @param modtype character, specifies what type of model is being run. Can be
#'     either thermal (thermal time accumulation) or day (day accumulation).
#' @param form the functional form of the thermal time accumulation
#' @param length the length of thermal time accumulation (in days). It can be
#'     either a set length of time (one number) or the total length of the
#'     stage (one length for each entry in fdat).
#' @param stage the number of the stage of the phenological model
#' @return The thermal sums for a given series of years.
#' @export
thermalsum <- function(pars, fdat, tdat, modtype, form, length, stage) {

    if (modtype=='thermal') {
        ths <- thermalgdsum(pars, fdat, tdat, form, length, stage)

    } else if (modtype=='day') {
        ths <- thermaldaysum(pars, fdat, tdat, form, length, stage)
        #print(ths)

    } else {
        stop('Only options for model types are thermal and day.')
    }

    return(ths)
}


