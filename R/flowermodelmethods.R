#' @include plantmodelclass.R flowermodelclass.R crossval.R

# methods for the FlowerModel class


setMethod("show",
          signature = 'FlowerModel',
          definition = function(object) {

              n <- object@parameters[[1]]@stages #extract stages
              pheno <- object@phenology #extract phenology data frame
              eventcols <- paste0('event', 0:1) #create event column names
              m <- length(object@parameters) #number of parameters

              parshow <- ldply(object@parameters, function(pl) {
                  showparlist(pl)
              }) #create parameter lists to display

              parshow$model <- rep(1:m, each=n) #creating model numbers
              parshow$error <- round(as.numeric(object@error),2) #rounding error
                                                                 #values
              #taking the average dates for each of the phenological
                #events
              avgdates <- round(apply(pheno[,eventcols], 2, mean))

              #more statistics on the data
              rng <- range(pheno[,'year'])
              span <- rng[2] - rng[1]
              obs <- nrow(pheno)

              #checking to see if model is cross validated and saving the result
              if (object@crossvalidated) cv <-'is' else cv <- 'is not'

              #creating what is displayed
              cat('Stages; event days: ', n,'; ' ,
                  paste(avgdates, collapse=', '), '\n', sep='')
              #cat('The data spans ', span, ' years, and has ', obs,
               #   ' observations.', '\n', sep='')
              cat('Model error is in days and ', cv, ' crossvalidated.', '\n',
                  sep='')
              cat('Model Parameters:','\n', sep='')
              print(parshow)

          })

################################################################
#Accessor methods
##top level

#' Accesses the parameters of a FlowerModel object
#' @rdname parameters
setMethod("parameters", "FlowerModel",
          function(object) {
              return(object@parameters)
          })


#' Accesses the RMSE of a FlowerModel object
#' @rdname error
setMethod("error", "FlowerModel",
          function(object) {
              return(object@error)
          })


#' Accesses the phenology data.frame of a FlowerModel object
#' @rdname phenology
setMethod("phenology", "FlowerModel",
          function(object) {
              return(object@phenology)
          })



#' Accesses the linear models of a FlowerModel object
#' @rdname olm
setMethod("olm", "FlowerModel",
          function(object) {
              return(object@olm)
          })

#' Is the model error crossvalidated?
#' @rdname crossvalidated
setMethod("crossvalidated", "FlowerModel",
          function(object) {
              return(object@crossvalidated)
          })


#' Crossvalidate model
#'
#' @rdname crossval
setMethod("crossval", "FlowerModel",
          function(object, temps, k, seed, fun, lbounds, ubounds,
                   iterations=100, cores=1L) {
              return(crossvalFlower(object, temps, k, seed, fun, lbounds,
                                    ubounds, iterations, cores))
          })


# ParameterList accessors -------------------------------------------------

#' Accesses the number of stages of a FlowerModel object
#' @rdname stages
setMethod("stages", "FlowerModel",
          function(object) {
              return(object@parameters[[1]]@stages)
          })



#' Accesses form of a FlowerModel object
#' @rdname form
setMethod("form", "FlowerModel",
          function(object) {
              frms <- lapply(object@parameters, function(parlist) {
                  #print(class(parlist))
                  form(parlist)
              })
              return(frms)
          })

#' Accesses the model type of a FlowerModel object
#' @rdname modeltype
setMethod("modeltype", "FlowerModel",
          function(object) {

              return(object@parameters[[1]]@modeltype)
          })


#' Accesses the cardinal temperatures
#' @rdname cardinaltemps
setMethod("cardinaltemps", "FlowerModel",
          function(object) {

              ct <- lapply(object@parameters, function(parlist) {
                  parlist@cardinaltemps
              })

              return(ct)
          })

#' Accesses the model lengths of a FlowerModel object
#' @rdname threshold
setMethod("threshold", "FlowerModel",
          function(object) {
              ml <- lapply(object@parameters, function(parlist) {
                  parlist@threshold
              })
              return(ml)
          })

#' Accesses the start days of a FlowerModel object
#' @rdname startday
setMethod("startday", "FlowerModel",
          function(object) {
              ml <- sapply(object@parameters, function(parlist) {
                  parlist@startday
              })
              return(ml)
          })

#' Accesses the simplified parameter of a FlowerModel object
#' @rdname simplified
setMethod("simplified", "FlowerModel",
          function(object) {
              sm <- sapply(object@parameters, function(parlist) {
                  parlist@simplified
              })
              return(sm)
          })


#' Accesses what parameters are to be optimized
#' @rdname parsOptimized
setMethod("parsOptimized", "FlowerModel",
          function(object) {
              pO <- lapply(object@parameters, function(parlist) {
                  parlist@parsOptimized
              })
              return(pO)
          })



################################################################
#validity method

setValidity("FlowerModel", function(object) {
    msg <- NULL
    valid <- TRUE

    #print(5)
    n <- stages(object)
    frm <- form(object)
    pheno <- phenology(object)
    mt <- modeltype(object)

    if (!modeltypecheck(mt)[1]) {
        valid <- FALSE
        msg <- c(msg, modeltypecheck(mt)[1])
    }

   if (!phenologycheck(n, pheno, TRUE)[1]) {
       valid <- FALSE
       msg <- c(msg, phenologycheck[-1])
   }


    if (length(threshold(object@parameters[[1]])) != n) {
        valid <- FALSE
        msg <- c(msg,
                 'The number of stages is not the same as the number of parameter value sets.')
    }


    if (valid) TRUE else msg

})

#############################
#setting stuff

#' @rdname crossvalidated-set
setMethod('crossvalidated<-', 'FlowerModel',
          function(object, value) {
              object@crossvalidated <- value

              if (validObject(object)) {
                  return(object)
              }
          })


#' @rdname error-set
setMethod('error<-', 'FlowerModel',
          function(object, value) {
              object@error <- value

              if (validObject(object)) {
                  return(object)
              }
          })



