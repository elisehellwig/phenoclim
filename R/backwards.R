


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
        fdat <- fdat[fdat$year<=lastyearyear, ]
    } else {
        stop('The last year must be the same year or earlier as the last year you have phenology data.')
    }

    n <- nrow(fdat)

    fullyears <- ifelse(is.leapyear(firstyear:lastyear), 366, 365)
    print(length(fullyears))
    l0prime <- fdat[2:n, bloomvar] + (fullyears[-n] - fdat[1:(n-1), matvar])
    l0mean <- mean(l0prime)
    l0 <- c(l0mean, l0prime)

    conv <- data.frame(year=firstyear:lastyear,
                       event1=fdat[,bloomvar],
                       event0=(fdat[,matvar] - fullyears),
                       length0=l0)
}


