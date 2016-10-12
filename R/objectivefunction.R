#' @include minrmse.R
NULL


objective <- function(parameters, phenology, temperature, stage, CT, L) {

    pars <- cardinaltemps(parameters)[[stage]]
    ml <- modlength(parameters)[stage]
    events <- paste0('event', stage:(stage+1))

    fdat <- phenology[, c('year', events, paste0('length', stage))]

    if (stages(parameters)==1) {
        tdat <- temperature(p)
    } else {
        tdat <- temperature(p)[[stage]]
    }

    if (CT) ct <- TRUE else ct <- pars
    if (L) l <- TRUE else l <- pars


    fun <- function(x) {
        return(minrmse(x, fdat, tdat, modtype(parameters), form(parameters),
                       stage, ct, l))
    }

    return(fun)

}
