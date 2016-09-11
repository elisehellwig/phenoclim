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

setValidity("Plant", function(object) {
    msg <- NULL
    valid <- TRUE

    n <- stages(object)

    if (!(modeltype %in% c('partial','full','combined','time'))) {
        valid <- FALSE
        msg <- c(msg,
                 "modeltype must be either partial, full combined, or time")
    }

    if (length(names(phenology(object))) < 2*n + 2) {
        valid <- FALSE
        msg <- c(msg,
                 "There are not enough event day variables for the number of phenological stages in the Plant object.")

    } else if (length(names(phenology(object))) > 2*n + 2) {
        valid <- FALSE
        msg <- c(msg,
                 "There are too many variables for the number of phenological stages in the Plant object.")

    }



    pnames <- c('year', paste0('event', 1:n), paste0('length', 1:(n-1)))


    if (!setequal(pnames, names(phenology(object)))) {
        valid <- FALSE

        if (length(pnames) > length(names(phenology(object)))) {

            diff <- setdiff(pnames, names(phenology(object)))
            msg <- c(msg,
                     paste("You are missing the following variables in your phenology data frame:", diff))

        } else if (length(pnames) < length(names(phenology(object)))) {

            diff <- setdiff(names(phenology(object)), pnames)
            msg <- c(msg,
                 paste("You are missing the following variables in your phenology data frame:", diff))
        }
    }


    pyears <- sort(phenology(object)$year)
    tyears <- sort(unique(temperature(object)$year))

    if (!identical(pyears, tyears)) {
        valid <- FALSE
        msg <- c(msg,
                 "The years of the temperature data are not the same as the years of the phenology data.")

    }


})






