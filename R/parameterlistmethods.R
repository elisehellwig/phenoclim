#' @include parameterlistclass.R parameterfunctions.R
NULL

#This document has the basic methods for accessing and manipulating objects
#   of the class ParameterList


##############################
###accessor methods


#' Accesses the number stages of a ParameterList object
#' @rdname stages
setMethod("stages", "ParameterList",
          function(object) {
              return(object@stages)
          })


#' Accesses the model type of a ParameterList object
#' @rdname modeltype
setMethod("modeltype", "ParameterList",
          function(object) {
              return(object@modeltype)
          })


#' Accesses whether the model in the ParameterList object is simplified
#' @rdname simplified
setMethod("simplified", "ParameterList",
          function(object) {
              return(object@simplified)
          })


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


#' Accesses which parameters to estimate for the ParameterList object
#' @rdname parsOptimized
setMethod("parsOptimized", "ParameterList",
          function(object) {
              return(object@parsOptimized)
          })

##############################
#show method

setMethod("show",
          signature = 'ParameterList',
          definition = function(object) {
              modelpars <- showparlist(object)
              print(modelpars)

          })




##################################
##validity method

setValidity("ParameterList", function(object) {
    msg <- NULL
    valid <- TRUE

    ct <- cardinaltemps(object)
    frm <- object@form
    forms <- c('gdd', 'gddsimple','linear','flat', 'asymcur','anderson',
               'triangle', 'trapezoid', 'ensemble')

    if (any(ifelse(frm %in% forms, FALSE, TRUE))) {
        valid <- FALSE
        msg <- c(msg, 'At least one form is not one of the accepted forms.')
    }

    if (!(modeltype(object) %in% c('thermal','day'))) {
        valid <- FALSE
        msg <- c(msg, 'The model type is not one of the accepted types.')
    }


    if (length(object@modlength) != length(ct)) {
        valid <- FALSE
        msg <- c(msg,
                 'The number of accumulation lengths and the number of parameter sets are not the same.')
    }

    ensemblefrm <- which(frm=='ensemble')

    if (length(ensemblefrm)!=length(ct)) {
        ctnum <- sapply(ct, function(v) length(v))

        isnum <- sapply(ct, function(v) (is.numeric(v) | is.integer(v)) )

        if (!all(isnum)) {
            valid <-FALSE
            msg <- c(msg,
                     'Not all of your parameter values are numbers.')
        }
    }

    formparnum <- sapply(frm, function(ch) parnum(ch))

    if (any(ifelse(formparnum!=ctnum, TRUE, FALSE))) {
        valid <- FALSE
        msg <- c(msg, 'The number of parameters does not fit the model form.')
    }

    pO <- c('cardinaltemps','modlength')

    if (all(ifelse(parsOptimized(object) %in% pO, FALSE, TRUE))) {
        valid <- FALSE
        msg <- c(msg, "estimate must include at least one of 'cardinaltemps' or 'modelength'. ")
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

#' @rdname stages-set
setMethod('stages<-', 'ParameterList',
          function(object, value) {
              object@stages <- value

              if (validObject(object)) {
                  return(object)
              }
          })


#' @rdname modeltype-set
setMethod('modeltype<-', 'ParameterList',
          function(object, value) {
              object@modeltype <- value

              if (validObject(object)) {
                  return(object)
              }
          })



#' @rdname simplified-set
setMethod('simplified<-', 'ParameterList',
          function(object, value) {
              object@simplified <- value

              if (validObject(object)) {
                  return(object)
              }
          })


#' @rdname parsOptimized-set
setMethod('parsOptimized<-', 'ParameterList',
          function(object, value) {
              object@parsOptimized <- value

              if (validObject(object)) {
                  return(object)
              }
          })

