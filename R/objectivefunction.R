objective <- function(p, i) {

    pars <- cardinaltemps(p)[[i]]
    ml <- modlength(p)[i]
    events <- paste0('event', i:(i+1))

    fdat <- phenology(p)[, c('year', events)]
    fdat$stagelength <- fdat[,events[2]] - fdat[,events[1]]


    if (stages(p)==1) {
        tdat <- temperature(p)
    } else {
        tdat <- temperature(p)[[i]]
    }

    fun <- function(x) {
        return(minrmse(x, fdat, tdat, modtype(p), form(p), i))
    }

}
