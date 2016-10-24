#' @include plantmodelclass.R

# methods for the PlantModel class


setMethod("show",
          signature = 'PlantModel',
          definition = function(object) {

              n <- object@parameters[[1]]@stages
              pheno <- object@phenology
              eventcols <- paste0('event', 1:(n+1))
              lengthcols <- paste0('length', 1:(n))
              m <- length(object@parameters)

              parshow <- ldply(object@parameters, function(pl) {
                  showparlist(pl)
              })

              parshow$model <- rep(1:m, each=n)
              parshow$error <- as.numeric(object@error)

              avgdates <- round(apply(pheno[,eventcols], 2, mean))

              if (is.numeric(pheno[,lengthcols])) {
                  avglengths <- round(mean(pheno[,lengthcols]))
              } else {
                  avglengths <- round(apply(pheno[,lengthcols], 2, mean))
              }

              rng <- range(pheno[,'year'])
              span <- rng[2] - rng[1]
              obs <- nrow(pheno)

              if (object@crossvalidated) cv <-'is' else cv <- 'is not'

              cat('Stages; stage lengths; event days: ', n,'; ' ,
                  paste(avglengths, collapse=', '), '; ',
                  paste(avgdates, collapse=', '), '\n', sep='')
              #cat('The data spans ', span, ' years, and has ', obs,
               #   ' observations.', '\n', sep='')
              cat('Model error is in days and ', cv, ' crossvalidated.', '\n', sep='')
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


#' Accesses the temperature data.frame of a PlantModel object
#' @rdname temperature
setMethod("temperature", "PlantModel",
          function(object) {
              return(object@temperature)
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
                  parlist@form
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

#' Accesses the model lengths of a plant object
#' @rdname modlength
setMethod("modlength", "PlantModel",
          function(object) {
              ml <- lapply(object@parameters, function(parlist) {
                  parlist@modlength
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



################################################################
#validity method

setValidity("PlantModel", function(object) {
    msg <- NULL
    valid <- TRUE

    #print(5)
    n <- stages(object)
    temp <- temperature(object)
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

    if (!(checktempyears(pheno, temp)[[1]])) {
        valid <- FALSE
        msg <- c(msg,
                 paste('You are missing temp data for the following years',
                       paste(checktempyears(pheno, temp)[[2]], sep=", ")))

    }


    if (length(modlength(object@parameters[[1]])) != n) {
        valid <- FALSE
        msg <- c(msg,
                 'The number of stages is not the same as the number of parameter value sets.')
    }

    if (!tempclasscheck(frm, temp)[1]) {
        valid <- FALSE
        msg <- c(msg, temptypecheck(frm, temp)[-1])
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



