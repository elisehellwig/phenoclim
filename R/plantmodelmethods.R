#' @include plantmodelclass.R

# methods for the PlantModel class


setMethod("show",
          signature = 'PlantModel',
          definition = function(object) {

              n <- object@stages
              pheno <- object@phenology
              eventcols <- paste0('event', 1:n)
              lengthcols <- paste0('length', 1:(n-1))

              avgdates <- apply(pheno[,eventcols], 2, mean)
              avglengths <- apply(pheno[,lengthcols], 2, mean)
              rng <- range(pheno[,'year'])
              span <- rng[2] - rng[1]
              obs <- nrow(pheno)
              formname <- parameters(object)@form

              cat('This plant has ', object@stages, "phenological stages")
              cat('Average length of stage: ', avglengths)
              cat('Average event dates: ', avgdates)
              cat('The data spans ', span, 'years, and has ', obs,
                  'observations.')
              cat('This is a ', object@modeltype, 'model with the ', formname, 'functional form.')
          })

################################################################
#Accessor methods


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


#' Accesses the model type of a PlantModel object
#' @rdname modeltype
setMethod("modeltype", "PlantModel",
          function(object) {
              return(object@modeltype)
          })

#' Accesses the number of stages of a PlantModel object
#' @rdname stages
setMethod("stages", "PlantModel",
          function(object) {
              return(object@stages)
          })

#' Accesses the parameters of a PlantModel object
#' @rdname parameters
setMethod("parameters", "PlantModel",
          function(object) {
              return(object@parameters)
          })


#' Accesses the functional form of a PlantModel object
#' @rdname form
setMethod("form", "PlantModel",
          function(object) {
              return(form(parameters(object)))
          })

#' Accesses the cardinal temperatures
#' @rdname cardinaltemps
setMethod("cardinaltemps", "PlantModel",
          function(object) {
              return(parameters(object)@cardinaltemps)
          })

#' Accesses the model lengths of a plant object
#' @rdname modlength
setMethod("modlength", "PlantModel",
          function(object) {
              return(parameters(object)@modlength)
          })


################################################################
#validity method

setValidity("PlantModel", function(object) {
    msg <- NULL
    valid <- TRUE

    n <- stages(object)
    temp <- temperature(object)
    frm <- form(object)

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

    if (frm %in% c('gdd','gddsimple')) {

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
    }

    if (valid) TRUE else msg

})




