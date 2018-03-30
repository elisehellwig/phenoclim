
#' Gets phenologial data and season length predictions
extractResults <- function(pm, cultivars, form) {
    require(plyr)
    require(phenoclim)

    p <- ldply(cultivars, function(v) {
        pv <- phenology(pm[[v]])
        pv$cultivar <- v
        pv
    })


    nms <- names(p)
    basenames <- c('year','cultivar','event1','event2', 'length1')
    baseIDs <- which(nms %in% basenames)
    modIDs <- grep(form, nms)

    IDs <- c(baseIDs, modIDs)

    pfinal <- p[,IDs]

    return(pfinal)
}
