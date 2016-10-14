#' @include plantmodel.R
NULL

#' Crossvalidating one stage
#'
#' @param pdat data.frame, the phenology data.frame.
#' @param modtype character, the type of model, either 'thermal' or 'day'.
#' @param k numeric, the number of folds in the crossvalidation.
#' @param stage numeric, the stage of the phenology model.
#' @param fun character, the name of the function used to evaluate the model.
#' @return The mean rmse
stagecrossval <- function(pdat, modtype, k, stage, fun) {

    measure <- rep(NA, k)

    for (i in 1:k) {
        train <- pdat[pdat$fold!=i, ]
        test <- pdat[pdat$fold==i, ]

        predictor <- paste0(modtype, stage)
        response <- paste0('length', stage)
        f <- formula(paste(response, ~ predictor))

        trainmod <- lm(f, train)
        testdata <- data.frame(test[,predictor])
        names(testdata) <- predictor

        fit <- predict(trainmod, newdata=testdata)

        measure[i] <- do.call(fun, list(fit, test[,response]))
    }

    avgmeasure <- mean(measure)

    return(avgmeasure)
}



#' Crossvalidates the PlantModel Error
#'
#' Does k-fold crossvalidation on the error of a PlantModel object.
#'
#' @param plant PlantModel, the object to be crossvalidated
#' @param k numeric, number of folds for crossvalidation.
#' @param seed numeric, the number to be used as the seed. Set to NA for no seed
#'     set.
#' @param fun character, the name of the function used to evaluate the model.
#' @return A PlantModel object with the error slot having the new crossvalidated
#'     error
#' @export
crossval <- function(plant, k, seed, fun='rmsd') {

    p <- phenology(plant)

    if (is.numeric(seed)) {
        set.seed(seed)
    }

    p$fold <- kfold(p, k=k)

    CVerrors <- sapply(1:stages(plant), function(i) {
        stagecrossval(p, modeltype(plant), k, i, fun)
        })


    error(plant) <- CVerrors
    crossvalidated(plant) <- TRUE

    return(plant)

}




