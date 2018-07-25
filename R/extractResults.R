
#' Gets phenologial data and season length predictions
#'
#' This will get results from a PlantModel or FlowerModel object
#'
#' @param pm PlantModel or FlowerModel object
#' @param cultivars character, the cultivars you want to extract results for.
#' @param form character, the functional form you want to extract results for.
#' @return A data.frame with the results.
#' @export
extractResults <- function(pm, cultivars, form) {

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
