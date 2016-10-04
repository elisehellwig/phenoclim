#' @include parameterlistmethods.R plantmethodsbasic.R general.R parameterfunctions.R



#' Calculates thermal time sums for partial model
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
	start <- fdat[, paste0('event',stage)]
	end <- start+length

	templist <- lapply(1:length(years), function(i) {

	    if (is.data.frame(tdat[[1]])) {
            tdat[[as.character(years(i))]][start[i]:end[i],]

	    } else {
	        tdat[[as.character(years(i))]][start[i]:end[i]]
	    }
	})

	if (form %in% c('gdd','gddsimple', 'linear','nocrit','triangle',
	                'trapezoid','anderson')) {
	    #print(pars)
	    tsums <- sapply(1:length(start), function(y) {

	        plist <- parlist(templist[[i]], pars, sum=TRUE)
	        do.call(form, plist)
	    })

	} else {
        stop('form must be linear, nocrit triangle, anderson, gdd, gddsimple
             or trapezoid')
	}

	return(tsums)
}



#' Calculates thermal time sums for full or combined model
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
    } else if (form %in% c('linear', 'nocrit', 'triangle', 'anderson')) {

        templist <- lapply(1:length(years), function(i) {
            tdat[[as.character(years[i])]][start[i]:365]
        })

    } else {
        stop('type must be one of the following: gdd, gddsimple, linear
             nocrit, triangle, anderson')
    }


    #calculating the event day
    day <- predictevent(pars, templist, form, length)

    return(day)

}


#' Calculates thermal time sums
#'
#' Calculates thermal time sums for either the partial, full, or combined model
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
thermalsum <- function(pars, fdat, tdat, modtype, form, length, stage) {

    if (modtype=='partial') {
        ths < thermalgdsum(pars, fdat, tdat, form, length, stage)


    } else if (modtype %in% c('full', 'combined')) {
        ths <- thermaldaysum(pars, fdat, tdat, form, length, stage)

    } else {
        stop('Only options for mod type are partial, full and combined.')
    }

    return(ths)
}


