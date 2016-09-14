#' @include parameterlistclass.R

#This document has the basic methods for accessing and manipulating objects
#   of the class ParameterList


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

##################################
##validity method

setValidity("ParameterList", function(object) {
    msg <- NULL
    valid <- TRUE

    ct <- cardinaltemps(object)

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

    if (valid) {
        return(TRUE)

    } else {
        return(msg)
    }

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






