#' @include plantmodelclass.R
NULL

# methods for the PlantModel class


setMethod("show",
          signature = 'PlantModel',
          definition = function(object) {

              n <- object@parameters[[1]]@stages #extract stages
              pheno <- object@phenology #extract phenology data frame
              eventcols <- paste0('event', 1:(n+1)) #create event column names
              lengthcols <- paste0('length', 1:(n)) #create length column names
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

              #calculating the average lengths for each of the stages
              if (is.numeric(pheno[,lengthcols])) {
                  avglengths <- round(mean(pheno[,lengthcols]))
              } else {
                  avglengths <- round(apply(pheno[,lengthcols], 2, mean))
              }

              #more statistics on the data
              rng <- range(pheno[,'year'])
              span <- rng[2] - rng[1]
              obs <- nrow(pheno)

              #checking to see if model is cross validated and saving the result
              if (object@crossvalidated) cv <-'is' else cv <- 'is not'

              #creating what is displayed
              cat('Stages; stage lengths; event days: ', n,'; ' ,
                  paste(avglengths, collapse=', '), '; ',
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

#' Accesses the parameters of a PlantModel object
#' @rdname parameters
setMethod("parameters", "PlantModel",
          function(object) {
              return(object@parameters)
          })


#' Accesses the RMSE of a PlantModel object
#' @rdname error
setMethod("error", "PlantModel",
          function(object) {
              return(object@error)
          })


#' Accesses the phenology data.frame of a PlantModel object
#' @rdname phenology
setMethod("phenology", "PlantModel",
          function(object) {
              return(object@phenology)
          })



#' Accesses the linear models of a PlantModel object
#' @rdname olm
setMethod("olm", "PlantModel",
          function(object) {
              return(object@olm)
          })

#' Is the model error crossvalidated?
#' @rdname crossvalidated
setMethod("crossvalidated", "PlantModel",
          function(object) {
              return(object@crossvalidated)
          })


######Paremeterlist accessors

#' Accesses the number of stages of a PlantModel object
#' @rdname stages
setMethod("stages", "PlantModel",
          function(object) {
              return(object@parameters[[1]]@stages)
          })

#' Accesses form of a PlantModel object
#' @rdname form
setMethod("form", "PlantModel",
          function(object) {
              frms <- lapply(object@parameters, function(parlist) {
                  #print(class(parlist))
                  form(parlist)
              })
              return(frms)
          })


#' Accesses simplified parameter of a PlantModel object
#' @rdname simplified
setMethod("simplified", "PlantModel",
          function(object) {
              frms <- lapply(object@parameters, function(parlist) {
                  #print(class(parlist))
                  simplified(parlist)
              })
              return(frms)
          })


#' Accesses the model type of a PlantModel object
#' @rdname modeltype
setMethod("modeltype", "PlantModel",
          function(object) {

              return(object@parameters[[1]]@modeltype)
          })


#' Accesses the cardinal temperatures
#' @rdname cardinaltemps
setMethod("cardinaltemps", "PlantModel",
          function(object) {

              ct <- lapply(object@parameters, function(parlist) {
                  parlist@cardinaltemps
              })

              return(ct)
          })

#' Accesses the model thresholds of a plant object
#' @rdname threshold
setMethod("threshold", "PlantModel",
          function(object) {
              ml <- lapply(object@parameters, function(parlist) {
                  parlist@threshold
              })
              return(ml)
          })


#' Accesses the start days of a plant object
#' @rdname startday
setMethod("startday", "PlantModel",
          function(object) {
              ml <- lapply(object@parameters, function(parlist) {
                  parlist@startday
              })
              return(ml)
          })


#' Accesses the start days of a plant object
#' @rdname startday
setMethod("startday", "PlantModel",
          function(object) {
              ml <- lapply(object@parameters, function(parlist) {
                  parlist@startday
              })
              return(ml)
          })


#' Accesses what parameters are to be optimized
#' @rdname parsOptimized
setMethod("parsOptimized", "PlantModel",
          function(object) {
              pO <- lapply(object@parameters, function(parlist) {
                  parlist@parsOptimized
              })
              return(pO)
          })


#' Accesses the model class of a PlantModel object
#' @rdname mclass
setMethod("mclass", "PlantModel",
          function(object) {

              return(object@parameters[[1]]@mclass)
          })


################################################################
#validity method

setValidity("PlantModel", function(object) {
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

   if (!phenologycheck(n, pheno)[1]) {
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
setMethod('crossvalidated<-', 'PlantModel',
          function(object, value) {
              object@crossvalidated <- value

              if (validObject(object)) {
                  return(object)
              }
          })


#' @rdname error-set
setMethod('error<-', 'PlantModel',
          function(object, value) {
              object@error <- value

              if (validObject(object)) {
                  return(object)
              }
          })



