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
#' @return A PlantModel object with the error slot having the new crossvalidated
#'     error
#' @export
crossval <- function(plant, temps, k, seed, fun='rmsd', lbounds, ubounds,
                     iterations=100, cores=1L) {

    parlist <- parameters(plant)
    m <- length(parlist)
    p <- phenology(plant)
    stage <- 1


    if (is.numeric(seed)) {
        set.seed(seed)
    }

    p$fold <- kfold(p, k=k)

    m <- length(parlist)
    measure <- matrix(rep(NA, m*k), nrow=m)

    ttforms <- sapply(parlist, function(pl) form(pl)[stage])

    extractedtemps <- extracttemplist(temps, p$year, ttforms)
    daytemplist <- extractedtemps[[1]]
    hourtemplist <- extractedtemps[[2]]


    for (i in 1:k) {
        train <- p[p$fold!=i, ]
        test <- p[p$fold==i, ]

        pm <- plantmodel(train, temps, parlist, lbounds, ubounds, cores,
                         iterations)

        trainmod <- olm(pm)

        predictors <- lapply(1:m, function(h) {
            paste0(modeltype(parlist[[h]])[stage], ttforms[h], stage)
        })

        response <- paste0('length', stage)

        testdata <- lapply(1:length(parlist), function(j) {
            pl <- parlist[[j]]
            as.data.frame(thermalsum(cardinaltemps(pl)[stage], test,
                         whichtemp(ttforms[j], daytemplist, hourtemplist),
                         modeltype(pl),ttforms[j], modlength(pl)[j],stage))
        })


        for (j in 1:m) {
            names(testdata[[j]]) <- predictors[[j]]
        }

        fit <- lapply(1:m, function(j) {
            predict(trainmod[[j]][[stage]], newdata=testdata[[j]])
        })

        measure[j,i] <- sapply(1:m, function(j) {
            do.call(fun, list(fit[[j]], test[,response]))
        })

    }

    avgmeasure <- apply(measure, 1, mean)





    error(plant) <- avgmeasure
    crossvalidated(plant) <- TRUE

    return(plant)

}




