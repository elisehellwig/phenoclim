#' @include constructors.R yearsums.R

# This document contains the function that fits phenological models for Plant
#     objects.


fit <- function(p, cores=NA) {

    n <- stages(p)

    functionlist <- lapply(1:n, function(i) {
        objective <- function(x) {
            return()
        }
    })
}
