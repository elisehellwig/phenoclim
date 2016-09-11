#' @include classes.R


showPlant <- function(object) {
    n <- object@stages+1
    avgdates <- apply(object@phenology[,1:n], 2, mean)
}
