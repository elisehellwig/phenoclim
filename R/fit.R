#' @include constructors.R yearsums.R

# This document contains the function that fits phenological models for Plant
#     objects.


fit <- function(p, lbounds, ubounds, cores=1L) {

    n <- stages(p)

    functionlist <- lapply(1:n, function(i) {
        objective <- function(x) {
            return(minrmse(p))
        }
    })

    if (n > 1 & cores > 1) {

        optimlist <- mclapply(1:length(functionlist), function(i) {
            DEoptim(functionlist[[i]], lower=lbounds,upper=ubounds)$optim
        }, mc.cores=cores)

    } else {
        optimlist <- lapply(1:length(functionlist), function(i) {
            DEoptim(functionlist[[i]], lower=lbounds, upper=ubounds)$optim
        })
    }
}
