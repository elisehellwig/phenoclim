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

              cat('This plant has ', n, "phenological stages")
              cat('Average length of stage: ', avglengths)
              cat('Average event dates: ', avgdates)
              cat('The data spans ', span, 'years, and has ', obs,
                  'observations.')
              cat('This is a ', object@modeltype, 'model with the ', formname, 'functional form.')
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


#' Accesses the linear model of a PlantModel object
#' @rdname olm
setMethod("olm", "PlantModel",
          function(object) {
              return(object@olm)
          })


######Paremeterlist accessors

#' Accesses the number of stages of a PlantModel object
#' @rdname stages
setMethod("stages", "PlantModel",
          function(object) {
              return(object@parameters@stages)
          })

#' Accesses the model type of a PlantModel object
#' @rdname modeltype
setMethod("modeltype", "PlantModel",
          function(object) {
              return(object@parameters@modeltype)
          })


#' Accesses the cardinal temperatures
#' @rdname cardinaltemps
setMethod("cardinaltemps", "PlantModel",
          function(object) {
              return(object@parameters@cardinaltemps)
          })

#' Accesses the model lengths of a plant object
#' @rdname modlength
setMethod("modlength", "PlantModel",
          function(object) {
              return(object@parameters@modlength)
          })

#' Accesses what parameters are to be optimized
#' @rdname parsOptimized
setMethod("parsOptimized", "PlantModel",
          function(object) {
              return(object@parameters@parsOptimized)
          })



################################################################
#validity method

setValidity("PlantModel", function(object) {
    msg <- NULL
    valid <- TRUE

    n <- stages(object)
    temp <- temperature(object)
    frm <- form(object)
    pheno <- phenology(object)
    mt <- modeltype(object)

    if (!modeltypecheck(mt)[1]) {
        valid <- FALSE
        msg <- c(msg, modeltypecheck(mt)[1])
    }

   if (!phenologycheck(pheno)[1]) {
       valid <- FALSE
       msg <- c(msg, phenologycheck[-1])
   }

    if (!(checktempyears(pheno, temp)[[1]])) {
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

    if (!tempclasscheck(temp)[1]) {
        valid <- FALSE
        msg <- c(msg, temptypecheck(temp)[-1])
    }

    if (valid) TRUE else msg

})




