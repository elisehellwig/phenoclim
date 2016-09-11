#' @include classes.R

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

              cat('This plant has ', object@stages, "phenological stages")
              cat('Average length of stage: ', avglengths)
              cat('Average event dates: ', avgdates)
              cat('The data spans ', span, 'years, and has ', obs,
                  'observations.')
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



##############################
#validity method

setValidity("Plant", function(object) {
    msg <- NULL
    valid <- TRUE

    n <- stages(object)

    if (!(modeltype %in% c('partial','full','combined','time'))) {
        valid <- FALSE
        msg <- c(msg,
                 "modeltype must be either partial, full combined, or time")
    }

    pnames <- c('year', paste0('event', 1:n), paste0('length', 1:(n-1)))
    missingcols <- setdiff(pnames, names(phenology(object)))

    if (length(missingcols) > 0) {
        valid <- FALSE
        msg <- c(msg,
                 paste("You are missing the following variables in your phenology data.frame:", diff))

    }


    pyears <- sort(phenology(object)$year)
    tyears <- sort(names(temperature(object)))
    missingyears <- setdiff(pyears, tyears)

    if (length(missingyears) > 0) {
        valid <- FALSE
        msg <- c(msg,
                 paste("The following years have observations in the phenology data.frame but not the temperature data.frame:", missingyears))

    }

    if (valid) TRUE else msg

})


##############################
###replacement methods



