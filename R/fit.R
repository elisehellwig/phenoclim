#' @include constructors.R minrmse.R

# This document contains the function that fits phenological models for Plant
#     objects.


fit <- function(p, lbounds, ubounds, cores=1L, estimateCT=TRUE,
                estimatelength=TRUE) {

    if (!estimateCT & !estimatelength) {
        stop('You at least estimate either the cardinal temperatures or the model length, though you can estimate both.')
    }

    if (lbounds != ubounds) {
        stop('The lower and upper bound vectors must have the same number of parameters')
    }

    ttform <- form(p)

    if (parlength(ttform, estimateCT, estimatelength)!=lbounds) {
        stop(paste0('Your bounds have the wrong number of parameter values. ',
                   'Your bounds are of length', length(lbounds),
                   ', and they should be of length',
                   parlength(ttform, estimateCT, estimatelength), '.'))
    }

    n <- stages(p)

    functionlist <- lapply(1:n, function(i) {
        objective(p, i, estimateCT, estimatelength)
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
