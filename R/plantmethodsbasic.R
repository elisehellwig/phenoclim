#' @include classes.R parameterlistmethods.R general.R

#This document has the basic methods for accessing and manipulating objects
#   of the class Plant

setMethod("show",
          signature = 'Plant',
          definition = function(object) {

              n <- object@stages+1
              avgdates <- apply(object@phenology[,1:n], 2, mean)
              avglengths <- apply(object@phenology[,(n+1):(2*n-1)], 2, mean)
              rng <- range(object@phenology[,'year'])
              span <- rng[2] - rng[1]
              obs <- length(object@phenology)
              formname <- as.character(substitute(object@form))

              cat('This plant has ', object@stages, "phenological stages")
              cat('Average length of stage: ', avglengths)
              cat('Average event dates: ', avgdates)
              cat('The data spans ', span, 'years, and has ', obs,
                  'observations.')
              cat('This is a ', object@modeltype, 'model with the ', formname, 'functional form.')
          })


##############################
###accessor methods


#' Accesses the phenology data.frame of a Plant object
#' @rdname phenology
setMethod("phenology", "Plant",
          function(object) {
              return(object@phenology)
          })


#' Accesses the temperature data.frame of a Plant object
#' @rdname temperature
setMethod("temperature", "Plant",
          function(object) {
              return(object@temperature)
          })


#' Accesses the model type of a Plant object
#' @rdname modeltype
setMethod("modeltype", "Plant",
          function(object) {
              return(object@modeltype)
          })


#' Accesses the thermal time functional form of a Plant object
#' @rdname form
setMethod("form", "Plant",
          function(object) {
              return(object@form)
          })


#' Accesses the number of stages of a Plant object
#' @rdname stages
setMethod("stages", "Plant",
          function(object) {
              return(object@stages)
          })

#' Accesses the number of stages of a Plant object
#' @rdname parameters
setMethod("parameters", "Plant",
          function(object) {
              return(object@parameters)
          })

##############################
#validity method

setValidity("Plant", function(object) {
    msg <- NULL
    valid <- TRUE

    n <- stages(object)
    temp <- temperature(object)

    if (!(modeltype %in% c('partial','full','combined','time'))) {
        valid <- FALSE
        msg <- c(msg,
                 "modeltype must be either partial, full, combined, or time")
    }

    pnames <- c('year', paste0('event', 1:n), paste0('length', 1:(n-1)))
    missingcols <- setdiff(pnames, names(phenology(object)))

    if (length0(missingcols)) {
        valid <- FALSE
        msg <- c(msg,
                 paste("You are missing the following variables in your phenology data.frame:", diff))

    }


    if (!(checktempyears(object)[[1]])) {
        valid <- FALSE
        msg <- c(msg,
                 paste('You are missing temp data for the following years',
                       checktempyears(object[[2]])))

    }

    if (length(modlength(object@parameters)) != n) {
        valid <- FALSE
        msg <- c(msg,
                 'The number of stages is not the same as the number of parameter value sets.')
    }

    if (as.character(substitute(object@form)) %in% c('gdd','gddsimple')) {

        if (is.list(temp[[1]])) {
            if (!is.data.frame(temp[[1]][[1]]) | dim(temp[[1]][[1]])[2] != 2) {
                valid <- FALSE
                msg <- c(msg,
                         'Min and max temperatures are required for gdd and gddsimple models.')
            }
        } else {
            if (!is.data.frame(temp[[1]]) | dim(temp[[1]])[2] != 2) {
                valid <- FALSE
                msg <- c(msg,
                         'Min and max temperatures are required for gdd and gddsimple models.')
            }
        }

    if (valid) TRUE else msg

}


##############################
###replacement methods


#' @rdname phenology-set
setMethod('phenology<-', 'Plant',
          function(object, value) {
              object@phenology <- value

              if (validObject(object)) {
                  return(object)
              }
          })


#' @rdname temperature-set
setMethod('temperature<-', 'Plant',
          function(object, value) {
              object@temperatue <- value

              if (validObject(object)) {
                  return(object)
              }
          })


#' @rdname modeltype-set
setMethod('modeltype<-', 'Plant',
          function(object, value) {
              object@modeltype <- value

              if (validObject(object)) {
                  return(object)
              }
          })


#' @rdname form-set
setMethod('form<-', 'Plant',
          function(object, value) {
              object@form <- value

              if (validObject(object)) {
                  return(object)
              }
          })


#' @rdname stages-set
setMethod('stages<-', 'Plant',
          function(object, value) {
              object@stages <- value

              if (validObject(object)) {
                  return(object)
              }
          })

#' @rdname parameters-set
setMethod('parameters<-', 'Plant',
          function(object, value) {
              object@parameters <- value

              if (validObject(object)) {
                  return(object)
              }
          })




