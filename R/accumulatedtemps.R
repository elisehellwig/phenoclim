mround <- function(x, base) {
    base*round(x/base)
}

accumulatedTemps <- function(tempdat, flowering, TTAL) {
    flowering$end <- flowering[,'flower'] + TTAL
    #print(flowering$year)
    
    d <- extracttemp(tempdat, flowering$year, flowering$flower, flowering$end)
    
    return(unlist(d))
}

thermalEval <- function(temp, pars, method) {

    if (method=='linear') {
        TT <- linear(temp, pars[1], sum=FALSE)
    }
    
    if (method=='nocrit') {
        TT <- nocrit(temp, pars[1], pars[2], sum=FALSE)
    }
    
    if (method=='triangle') {
        TT <- triangle(temp, pars[1], pars[2], pars[3], sum=FALSE)
    }
    
    if (method=='anderson') {
        TT <- anderson(temp, pars[1], pars[2], pars[3], sum=FALSE)
    }
    
    return(TT)
}

collateTT <- function(tempvec, pardat, methods, parnums) {    
    
    tt <- sapply(1:length(methods), function(i) {
        params <- as.vector(t(pardat[pardat$form==methods[i], 1:3]))
        thermalEval(tempvec, params, methods[i])
    })
    
    df <- as.data.frame(cbind(tempvec, tt))
    names(df) <- c('temp',methods)
    dfm <- melt(df, id.vars='temp', measure.vars=methods, variable.name='form', value.name='TTA')
    
    return(dfm)
}


tempfreq <- function(wdat, tdat, maxlength, binsize=5) {
    tm <- round(accumulatedTemps(tdat, wdat, maxlength))
    dt <- as.data.frame(table(tm))
    names(dt)[1] <- 'temp'
    dt$temp <- as.numeric(as.character(dt$temp))
    dt$bin <- mround(dt$temp, binsize)
    
    bf <- ldply(sort(unique(dt$bin)), function(bt) {
        total <- sum(dt[dt$bin==bt, 'Freq'])
        c(bt, total)
    })
    
    names(bf) <- c('bin', 'binfreq')
    bf$binfreq <- bf$binfreq/sum(bf$binfreq)
    
    dtb <- merge(dt, bf, by='bin')
    names(dtb)[4] <- paste0('freq', binsize)
    
    return(dtb)
}



