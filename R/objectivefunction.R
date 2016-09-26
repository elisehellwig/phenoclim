#' @include minrmse.R

objective <- function(p, stage, CT, L) {

    pars <- cardinaltemps(p)[[i]]
    ml <- modlength(p)[i]
    events <- paste0('event', i:(i+1))

    fdat <- phenology(p)[, c('year', events)]
    fdat$stagelength <- eventi(fdat, i+1) - eventi(fdat, i)


    if (stages(p)==1) {
        tdat <- temperature(p)
    } else {
        tdat <- temperature(p)[[i]]
    }

    if (CT) ct <- TRUE else ct <- pars
    if (L) l <- TRUE else l <- pars



    fun <- function(x) {
        return(minrmse(x, fdat, tdat, modtype(p), form(p), i, ct, l))
    }

    return(fun)

}
