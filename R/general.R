extracttemp <- function(tdat, years, starts, ends, tempname=NA, 
                        yearname='year', dayname='day') {
	
    #print(1)
    
    if (length(ends)==1) {
        ends <- rep(ends, length(starts))
    }
    
    #print(years)
    
    if (is.na(tempname)) {
        
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
    
    tlist <- lapply(1:length(years), function(i) {
        rows <- which(tdat[,yearname]==years[i] & tdat[,dayname]>=starts[i] & tdat[,dayname]<=ends[i])
        tdat[rows,tnames]
    })

    names(tlist) <- years
	return(tlist)
}

is.leapyear <- function(year){
  #http://en.wikipedia.org/wiki/Leap_year
  return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
}


head.list <- function(obj, n = 6L, ...)
{
    stopifnot(length(n) == 1L)
    origN <- n
    n <- if (n < 0L)
        max(length(obj) + n, 0L)
    else min(n, length(obj))
    lapply(obj[seq_len(n)], head, origN, ...)
}


rmsd <- function(fit, dat, na.rm=FALSE) {
	if (na.rm) {
		frows <- which(is.na(fit))
		fit1 <- fit[-frows]

		drows <- which(is.na(dat))
		dat1 <- dat[-drows]	
	} else {
		fit1 <- fit
		dat1 <- dat
	}

	rmsd <- sqrt(sum((fit1-dat1)^2)/length(fit1))	
	return(rmsd)
}
