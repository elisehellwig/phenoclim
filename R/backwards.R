#' Converts years for bloom modeling
#'
#' Models for bloom necessarily incorporte phenology data from multiple years.
#'     this is not compatible with the current phenoclim model paradigm. So this
#'     function groups the phenogical events by blooming event not by year.
#'
#' @param fdat dataframe, contains the phenology data
#' @param firstyear numeric, the first year you would like to predict bloom.
#' @param lastyear numeric, the last year you would like to predict bloom.
#' @param bloomvar character, the name of the column that contains the bloom
#'     data.
#' @param matvar character, the name of the column that contains the harvest
#'     data.
#' @return A data.frame with all of the converted phenology data in it.
#' @export
yearconversion <- function(fdat, firstyear=NA, lastyear=NA,
                           bloomvar='event1', matvar='event2') {


    if (is.na(firstyear)) {
        firstyear <- min(fdat[,'year'])
    } else if (firstyear >= min(fdat[,'year'])) {
        fdat <- fdat[fdat$year>=firstyear, ]
    } else {
        stop('The first year must be the same year or later as the first year you have phenology data.')
    }

    if (is.na(lastyear)) {
        lastyear <- max(fdat[,'year'])
    } else if (lastyear <= max(fdat[,'year'])) {
        fdat <- fdat[fdat$year<=lastyear, ]
    } else {
        stop('The last year must be the same year or earlier as the last year you have phenology data.')
    }

    n <- nrow(fdat)

    #This is where the problem is because there are missing years so it doesnt
    #know what to use for the previous year flowering or end of counting back
    #date.
    fullyears <- ifelse(is.leapyear(firstyear:lastyear), 366, 365)
    print(length(fullyears))
    l0prime <- fdat[2:n, bloomvar] + (fullyears[-n] - fdat[1:(n-1), matvar])
    l0mean <- mean(l0prime)
    l0 <- c(l0mean, l0prime)

    conv <- data.frame(year=firstyear:lastyear,
                       event1=fdat[,bloomvar],
                       event0=(fdat[,matvar] - fullyears),
                       length0=l0)
    return(conv)
}


