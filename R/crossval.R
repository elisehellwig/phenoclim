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
crossvalPlant <- function(plant, temps, k, seed, fun='rmse', lbounds, ubounds,
                     iterations=100, cores=1L) {

    parlist <- parameters(plant)
    m <- length(parlist)
    p <- phenology(plant)
    stage <- 1

    if (is.numeric(seed)) {
        set.seed(seed)
    }

    p$fold <- kfold(p, k=k)


    ttforms <- sapply(parlist, function(pl) form(pl)[stage])


    measure <- matrix(rep(NA, m*k), nrow=m)

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

            #NOTE THIS NEEDS TO BE UPDATED TO REFLECT THE USE OF DATETIME
            #INDEXING
            as.data.frame(thermalsum(cardinaltemps(pl)[stage], test, temps,
                         modeltype(pl), ttforms[j], round(threshold(pl)),stage))
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


        #print(fit)
        #print(7)
        #str(fit)


        measure[,i] <- sapply(1:m, function(j) {
            do.call(fun, list(fit[[j]], test[,response]))
        })


        #print(parlist)
    }


    #print(measure)
    avgmeasure <- apply(measure, 1, mean)
    #print(8)

    error(plant) <- avgmeasure
    crossvalidated(plant) <- TRUE

    return(plant)

}


#' Crossvalidates the PlantModel Error
#'
#' Does k-fold crossvalidation on the error of a PlantModel object.
#'
#' @param flower FlowerModel, the object to be crossvalidated
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
#' @return A FlowerModel object with the error slot having the new crossvalidated
#'     error
#' @export
crossvalFlower <- function(flower, temps, k, seed, fun='rmse', lbounds, ubounds,
                          iterations=100, cores=1L) {

    parlist <- parameters(flower)
    m <- length(parlist)
    p <- phenology(flower)

    print(1)

    if (is.numeric(seed)) {
        set.seed(seed)
    }

    p$fold <- kfold(p, k=k)


    ttforms <- sapply(parlist, function(pl) form(pl))


    measure <- matrix(rep(NA, m*k), nrow=m)

     print(2)
    for (i in 1:k) {
        train <- p[p$fold!=i, ]
        test <- p[p$fold==i, ]

        #print(train$year)
        #print(test$year)
        print(3)
        fm <- flowermodel(train, temps, parlist, lbounds, ubounds, cores,
                         iterations)
        trainmod <- olm(fm)
        parlist <- parameters(fm)
        #print(lapply(trainmod, function(tm) tm[[1]]))
        print(4)
        predictors <- lapply(1:m, function(h) {
            paste0(modeltype(parlist[[h]]), ttforms[h])
        })

        response <- 'event1'

        testdata <- lapply(1:length(parlist), function(j) {
            pl <- parlist[[j]]
            #print(cardinaltemps(pl)[stage])

            as.data.frame(thermalsum(cardinaltemps(pl)[[1]],
                                     test$year, temps, modeltype(pl),
                                     ttforms[j], round(startday(pl)),
                                     round(threshold(pl)),
                                     varyingpars(pl), 'FlowerModel',
                                     test$event0))
        })


        print(5)
        #print(parameters(pm))
        for (j in 1:m) {
            names(testdata[[j]]) <- predictors[[j]]
        }

        #print(testdata)

        #print(trainmod[[1]])

        fit <-lapply(1:m, function(j) {
            unname(predict(trainmod[[j]], newdata=testdata[[j]]))
        })


        #print(fit)
        print(7)
        #str(fit)


        measure[,i] <- sapply(1:m, function(j) {
            do.call(fun, list(fit[[j]], test[,response]))
        })


        #print(parlist)
    }


    #print(measure)
    avgmeasure <- apply(measure, 1, mean)
    print(8)

    error(flower) <- avgmeasure
    crossvalidated(flower) <- TRUE

    return(flower)

}


