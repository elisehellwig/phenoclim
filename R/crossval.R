#' @include plantmodel.R
NULL


#' Crossvalidates the PlantModel Error
#'
#' Does k-fold crossvalidation on the error of a PlantModel object.
#'
#' @param plant PlantModel, the object to be crossvalidated
#' @param k numeric, number of folds for crossvalidation.
#' @param temps data.frame, contains all the temperature data.
#' @param seed numeric, the number to be used as the seed. Set to NA for no seed
#'     set.
#' @param fun character, the name of the function used to evaluate the model.
#' @param lbounds numeric, lower bounds of parameters
#' @param ubounds numeric, upper bounds of parameters.
#' @param cores integer, number of cores to be used when fitting the models
#' @param iterations numeric, number of iterations to be used when optimizing
#'     paramters.
#' @param ensemble logical, should the ensemble predictions be crossvalidated?
#' @return A PlantModel object with the error slot having the new crossvalidated
#'     error
#' @export
crossval <- function(plant, temps, k, seed, fun='rmsd', lbounds, ubounds,
                     iterations=100, cores=1L, ensemble=FALSE) {

    parlist <- parameters(plant)
    m <- length(parlist)
    p <- phenology(plant)
    stage <- 1

    if (is.numeric(seed)) {
        set.seed(seed)
    }

    p$fold <- kfold(p, k=k)


    ttforms <- sapply(parlist, function(pl) form(pl)[stage])

    if ((!('ensemble' %in% ttforms)) & (ensemble)) {
        stop('There is no ensemble prediction in the PlantModel, so there cannot be any in the crossvalidation')
    }

    if ('ensemble' %in% ttforms) {
        ttforms <- ttforms[-m]
        parlist <- parlist[1:(m-1)]
        m <- m-1
    }

    measure <- matrix(rep(NA, m*k), nrow=m)

    if (ensemble) {
        ensemblefits <- lapply(1:k, function(i) data.frame())
        ensembletest <- lapply(1:k, function(i) NA)
    }

    #print(1)
    extractedtemps <- extracttemplist(temps, p$year, ttforms)
    daytemplist <- extractedtemps[[1]]
    hourtemplist <- extractedtemps[[2]]

   # print(2)
    for (i in 1:k) {
        train <- p[p$fold!=i, ]
        test <- p[p$fold==i, ]

        #print(train$year)
        #print(test$year)
        #print(3)
        pm <- plantmodel(train, temps, parlist, lbounds, ubounds, cores,
                         iterations)
        trainmod <- olm(pm)
        parlist <- parameters(pm)
        #print(lapply(trainmod, function(tm) tm[[1]]))
        #print(4)
        predictors <- lapply(1:m, function(h) {
            paste0(modeltype(parlist[[h]])[stage], ttforms[h], stage)
        })

        response <- paste0('length', stage)

        testdata <- lapply(1:length(parlist), function(j) {
            pl <- parlist[[j]]
            #print(cardinaltemps(pl)[stage])
            tl <- whichtemp(ttforms[j], daytemplist, hourtemplist)

            as.data.frame(thermalsum(cardinaltemps(pl)[stage], test, tl,
                         modeltype(pl), ttforms[j], round(modlength(pl)),stage))
        })
        #print(5)
        #print(parameters(pm))
        for (j in 1:m) {
            names(testdata[[j]]) <- predictors[[j]]
        }

        #print(testdata)

        #print(trainmod[[1]])

        fit <-lapply(1:m, function(j) {
            unname(predict(trainmod[[j]][[stage]], newdata=testdata[[j]]))
        })

        ensemblefits[[i]] <- t(ldply(fit, function(v) v))
        names(ensemblefits[[i]]) <- ttforms



        #print(fit)
        #print(7)
        #str(fit)


        measure[,i] <- sapply(1:m, function(j) {
            do.call(fun, list(fit[[j]], test[,response]))
        })

        if (ensemble) {
            ensembletest[[i]] <- test[, response]
        }
        #print(parlist)
    }


    #print(measure)
    avgmeasure <- apply(measure, 1, mean)
    #print(8)
    if (ensemble) {
        ensemblefitvec <- lapply(ensemblefits, function(df) {
            apply(df, 1, mean)
        })

        ensembleerror <- sapply(1:k, function(i) {
            do.call(fun, list(ensemblefitvec[[i]], ensembletest[[i]]))
        })

        ensembleavgerror <- mean(ensembleerror)
        avgmeasure <- c(avgmeasure, ensembleavgerror)
    }



    error(plant) <- avgmeasure
    crossvalidated(plant) <- TRUE

    return(plant)

}




