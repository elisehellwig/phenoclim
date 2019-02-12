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
    stage <- 1:stages(parlist)[1]

    if (is.numeric(seed)) {
        set.seed(seed)
    }

    print(1)
    p$fold <- kfold(p, k=k)


    ttforms <- sapply(parlist, function(pl) form(pl))


    measure <- array(NA, c(max(stage), m, k))

    #<- lapply(1:m, function(n) {
     #   matrix(rep(NA, max(stage)*k), nrow=max(stage))
    #})

    print(2)
    for (i in 1:k) {
        train <- p[p$fold!=i, ]
        test <- p[p$fold==i, ]

        #print(train$year)
        #print(test$year)
        print(3)
        pm <- plantmodel(train, temps, parlist, lbounds, ubounds, cores,
                         iterations)
        trainmod <- olm(pm)
        parlist <- parameters(pm)#THIS IS WHERE THE ERROR IS
        #FIX THE VALIDITY METHOD
        #print(lapply(trainmod, function(tm) tm[[1]]))
        print(4)
        predictors <- lapply(1:m, function(h) {
            paste0(modeltype(parlist[[h]]), ttforms[h], stage)
        })

        response <- paste0('length', stage)

        testdata <- lapply(1:length(parlist), function(j) {
            pl <- parlist[[j]]
            #print(cardinaltemps(pl)[stage])

            StartThresh <- lapply(stage, function(s) {
                formatParameters(test[,'year'],
                                 test[,paste0('event',s)],
                                 startday(pl)[s],
                                 threshold(pl)[s],
                                 modeltype(pl),
                                 'PlantModel',
                                 varyingpars(pl))
            })


            as.data.frame(sapply(stage, function(s) {
                thermalsum(cardinaltemps(pl)[s],
                           test$year,
                           temps,
                           modeltype(pl),
                           ttforms[j],
                           StartThresh[[s]][[1]],
                           StartThresh[[s]][[2]],
                           varyingpars(pl),
                           'PlantModel',
                           test[, paste0('event', s)])
            }))

        })
        print(5)
        #print(parameters(pm))
        for (j in 1:m) {
            names(testdata[[j]]) <- predictors[[j]]
        }

        print(testdata)

        #print(trainmod[[1]])

        fit <-lapply(1:m, function(j) {
            as.data.frame(sapply(stage, function(s) {
                unname(predict(trainmod[[j]][[s]],
                               newdata=testdata[[j]]))
            }))

        })

        for (j in 1:m) {
            names(fit[[j]]) <- predictors[[j]]
        }

        print(fit)
        print(7)
        #str(fit)


        do.call(fun, list())

        measure[,,i] <- sapply(1:m, function(j) {
            sapply(stage, function(s) {
                do.call(fun, list(fit[[j]][,s],
                                  test[,response[s]]))
            })
        })


        #print(parlist)
    }


    print(measure)
    avgmeasure <- apply(measure, c(1,2), mean)
    print(8)

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
       # print(lbounds)
    #    print(ubounds)

        fm <- flowermodel(phenology=train,
                          temps=temps,
                          parlist=parlist,
                          lbounds=lbounds,
                          ubounds=ubounds,
                          cores=cores,
                          iterations=iterations)
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


