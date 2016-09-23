# This document contains functions that fit phenological models for Plant
#     objects.


minrmse <- function(pars, fdat, tdat, modtype, form, length) {

    if (type=='partial') {
        minrmsepartial(pars, fdat, tdat, form, length)

    } else if (type == 'full') {
        minrmsefull()

    } else if (type == 'combined') {
        minrmsecombined()
    } else {
        stop('Only options for model types are partial, full, and combined.')
    }

}



minrmsepartial <- function(pars, fdat, tdat, type='nocrit', sumlength=NA) {

    if (length(pars)==2 & pars[1]>pars[2]) {
        #print(2)
        rmse <- Inf

    } else if (length(pars)==3 & (pars[1]>pars[2]| pars[2]>pars[3]|pars[1]>pars[3])) {
        #print(3)
        rmse <- Inf


    } else if (length(pars)==4 & ( pars[1]>pars[2]| pars[2]>pars[3]|pars[1]>pars[3]|pars[1]>pars[4]| pars[2]>pars[4]|pars[3]>pars[4])) {
        #print(4)
        rmse <- Inf

    } else if (length(pars)>4) {
        stop('There are no models with more than four parameters')

    } else {
        ysums <- yearsums(pars, fdat, tdat, typ=type, sumlen=sumlength,
                          fn=flowername, hn=harvestname)


        season <- fdat[,'slen']
        mod <- lm(season ~ ysums)
        fit <- fitted(mod)
        rmse <- rmsd(fit, season)
    }

    if (!negative) {

        if (pars[1]<0) {
            rmse <- def.rmse
        }
    }


    return(rmse)

}




minrmsefull <- function(pars, fdat, tdat, type='nocrit', end=336,
                       def.rmse=Inf, flowername='flower',
                       harvestname='harvest', replaceinf=Inf) {

    if (type=='anderson') {

        fittedslen <- yeargd(pars, fdat, tdat, 'anderson', fn=flowername,
                             hn=harvestname, end=end, replaceinf=replaceinf)


        observedslen <- fdat[, 'slen']

        rmse <- rmsd(fittedslen, observedslen)

    } else if (length(pars)==3 & pars[2]>pars[3]) {
        #print(2)
            rmse <- def.rmse

    } else if (length(pars)==4 & (pars[2]>pars[3]| pars[3]>pars[4]|pars[2]>pars[4])) {
        #print(3)
            rmse <- def.rmse


    } else if (length(pars)==5 & ( pars[2]>pars[3] | pars[3]>pars[4] | pars[2]>pars[4]| pars[2]>pars[5] | pars[3]>pars[5] | pars[4]>pars[5])) {
        #print(4)
            rmse <- def.rmse

    } else if (length(pars)>5) {
        stop('There are no models implemented with more than 5 parameters.')

    }  else {

        fittedslen <- yeargd(pars, fdat, tdat, type, fn=flowername,
                             hn=harvestname, end=end, replaceinf=replaceinf)


        observedslen <- fdat[, 'slen']

        rmse <- rmsd(fittedslen, observedslen)
    }


    return(rmse)

}



minrmsecomb <- function(pars, fdat, tdat, type='nocrit', end=336,
                        def.rmse=Inf, negative=FALSE, flowername='flower',
                        harvestname='harvest', replaceinf=Inf) {

    if (length(pars)==3 & pars[2]>pars[3]) {
        #print(2)
        rmse <- def.rmse

    } else if (length(pars)==4 & (pars[2]>pars[3]| pars[3]>pars[4]|pars[2]>pars[4])) {
        #print(3)
        rmse <- def.rmse


    } else if (length(pars)==5 & ( pars[2]>pars[3] | pars[3]>pars[4] | pars[2]>pars[4]| pars[2]>pars[5] | pars[3]>pars[5] | pars[4]>pars[5])) {
        #print(4)
        rmse <- def.rmse

    } else if (length(pars)>5) {
        stop('There are no models implemented with more than 5 parameters.')

    }  else {

        #print(pars)

        daymet <- yeargd(pars, fdat, tdat, type, fn=flowername,
                             hn=harvestname, end=end, replaceinf=replaceinf)
        #print(daymet)

        if (any(is.infinite(daymet))) {
            rmse <- def.rmse


        } else {

            season <- fdat[, 'slen']

            mod <- lm(season ~ daymet)
            fit <- fitted(mod)
            rmse <- rmsd(fit, season)

        }

    }



    return(rmse)

}



fit <- function(plant){

}




