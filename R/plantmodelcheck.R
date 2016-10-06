

#This script contains functions to check various things to make sure the
#plant model is correctly formed.

phenologycheck <- function(n, df) {

    pnames <- c('year', paste0('event', 1:n))
    missingcols <- setdiff(pnames, names(df))

    if (length0(missingcols)) {

    }
}
