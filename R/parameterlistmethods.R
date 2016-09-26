#' @include parameterlistclass.R

#This document has the basic methods for accessing and manipulating objects
#   of the class ParameterList


##############################
###accessor methods

#' Accesses the modlength vector of a ParameterList object
#' @rdname modlength
setMethod("modlength", "ParameterList",
          function(object) {
              return(object@modlength)
          })


#' Accesses the cardinaltemps list of a ParameterList object
#' @rdname cardinaltemps
setMethod("cardinaltemps", "ParameterList",
          function(object) {
              return(object@cardinaltemps)
          })


#' Accesses the form of a ParameterList object
#' @rdname form
setMethod("form", "ParameterList",
          function(object) {
              return(object@form)
          })


##############################
#show method

setMethod("show",
          signature = 'ParameterList',
          definition = function(object) {
             n <- length(object@cardinaltemps[[1]])

             pars <- as.data.frame(do.call(rbind, object@cardinaltemps))
             names(pars) <- paste0('p', 1:n)

             stagelength <- data.frame(stage=1:length(object@modlength),
                               length=object@modlength)

             lengthpars <- cbind(stagelength, pars)

             print(lengthpars)

          })




##################################
##validity method

setValidity("ParameterList", function(object) {
    msg <- NULL
    valid <- TRUE

    ct <- cardinaltemps(object)
    frm <- form(object)

    if (length(object@modlength) != length(ct)) {
        valid <- FALSE
        msg <- c(msg,
                 'The number of accumulation lengths and the number of parameter sets are not the same.')
    }

    ctnum <- length(ct[[1]])
    ctsame <- sapply(ct, function(v) length(v)==ctnum)

    if (!all(ctsame)) {
        valid <- FALSE
        msg <- c(msg,
                 'Not all parameter sets have the same number of parameters.')
    }


    isnum <- sapply(ct, function(v) (is.numeric(v) | is.integer(v)) )

    if (!all(isnum)) {
        valid <-FALSE
        msg <- c(msg,
                 'Not all of your parameter values are numbers.')
    }


    if (frm %in% c('gdd', 'gddsimple','linear') & ctnum!=1) {
        valid <- FALSE
        msg <- c(msg, 'gdd, gddsimple, linear models require exactly one
                 cardinal temperature.')

    } else if (frm=='flat' & ctnum!=2) {
        valid <- FALSE
        msg <- c(msg, 'flat models require exactly two cardinal temperatures.')

    } else if (frm %in% c('anderson', 'triangle')) {
        valid <- FALSE
        msg <- c(msg, 'Anderson and triangle require exactly three cardinal temperatures.')

    }


    if (valid) return(TRUE) else return(msg)

})


###############################
#replacement methods

#' @rdname modlength-set
setMethod('modlength<-', 'ParameterList',
          function(object, value) {
              object@modlength <- value

              if (validObject(object)) {
                  return(object)
              }
          })



#' @rdname cardinaltemps-set
setMethod('cardinaltemps<-', 'ParameterList',
          function(object, value) {
              object@cardinaltemps <- value

              if (validObject(object)) {
                  return(object)
              }
          })


#' @rdname form-set
setMethod('form<-', 'ParameterList',
          function(object, value) {
              object@form <- value

              if (validObject(object)) {
                  return(object)
              }
          })




