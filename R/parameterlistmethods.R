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

#' Accesses the simplified parameter of a ParameterList object
#' @rdname simplified
setMethod("simplified", "ParameterList",
          function(object) {
              return(object@simplified)
          })


#' Accesses the start day of the ParameterList object
#' @rdname startday
setMethod("startday", "ParameterList",
          function(object) {
              return(object@startday)
          })


#' Accesses the threshold vector of a ParameterList object
#' @rdname threshold
setMethod("threshold", "ParameterList",
          function(object) {
              return(object@threshold)
          })


#' Accesses the varyingpars vector of a ParameterList object
#' @rdname varyingpars
setMethod("varyingpars", "ParameterList",
          function(object) {
              return(object@varyingpars)
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

#' Accesses whether model is for PlantModel or FlowerModel
#' @rdname mclass
setMethod("mclass", "ParameterList",
          function(object) {
              return(object@mclass)
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
    strt <- object@startday
    forms <- c('gdd', 'gddsimple','linear','flat', 'asymcur','anderson',
               'triangle', 'trapezoid', 'chillbasic','utah','utah_original',
               'chillPortions')
    vp <- c('start','threshold',NA)
    varpars <- object@varyingpars
    lens <- c(length(ct), length(object@threshold), length(strt))

    if (any(ifelse(frm %in% forms, FALSE, TRUE))) {
        valid <- FALSE
        msg <- c(msg, 'At least one form is not one of the accepted forms.')
    }

    if (!(modeltype(object) %in% c('DT','TTT'))) {
        valid <- FALSE
        msg <- c(msg, 'The model type is not one of the accepted types.')
    }


    if (abs(max(lens) - min(lens)) > 0.001) {
        valid <- FALSE
        msg <- c(msg,
                 'The number of thresholds, starts, and cardinal temp sets are not the same.')
    }

    if (!is.numeric(strt) | is.integer(strt)) {
        valid <- FALSE
        msg <- c(msg, 'The start value must be numeric or an integer.')
    }

    if (any(ifelse(varpars %in% vp, FALSE, TRUE))) {
        valid <- FALSE
        msg <- c(msg, 'varyingpars can only contain start and threshold or be
                 NA.')
    }

    ensemblefrm <- which(frm=='ensemble')
    ctnum <- sapply(ct, function(v) length(v))

    if (length(ensemblefrm)!=length(ct)) {

        isnum <- sapply(ct, function(v) (is.numeric(v) | is.integer(v)) )

        if ((!all(isnum)) & !(frm %in% c('utah', 'utahalt'))) {
            valid <-FALSE
            msg <- c(msg,
                     'Not all of your parameter values are numbers.')
        }
    }

    formparnum <- sapply(frm, function(ch) parnum(ch))
    formparnum2 <- ifelse(frm %in% c('utah','utahalt'), 1, formparnum)

    if (any(ifelse(formparnum!=ctnum, TRUE, FALSE))) {
        valid <- FALSE
        msg <- c(msg, 'The number of parameters does not fit the model form.')
    }

    pO <- c('cardinaltemps','threshold','start')

    if (all(ifelse(parsOptimized(object) %in% pO, FALSE, TRUE))) {
        valid <- FALSE
        msg <- c(msg, "estimate must include at least one of 'cardinaltemps' or 'start', or 'threshold'. ")
    }

    classtype <- mclass(object)
    modelclassnames <- c('FlowerModel','PlantModel')

    if (length(classtype)>1) {
        valid <- FALSE
        msg <- c(msg, 'Each parameterlist can only have one model class.')
    }

    if (!(classtype[1] %in% modelclassnames)) {
        valid <- FALSE
        msg <- c(msg, 'mclass must be PlantModel or FlowerModel')
    }


    n <- stages(object)
    if (classtype=='FlowerModel' & n!=1) {
        valid <- FALSE
        msg <- c(msg, 'If mclass is FlowerModel, there can only be one stage.')
    }

    if (valid) return(TRUE) else return(msg)

})


###############################
#replacement methods

#' @rdname threshold-set
setMethod('threshold<-', 'ParameterList',
          function(object, value) {
              object@threshold <- value

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



#' @rdname startday-set
setMethod('startday<-', 'ParameterList',
          function(object, value) {
              object@startday <- value

              if (validObject(object)) {
                  return(object)
              }
          })



#' @rdname varyingpars-set
setMethod('varyingpars<-', 'ParameterList',
          function(object, value) {
              object@varyingpars <- value

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


#' @rdname mclass-set
setMethod('mclass<-', 'ParameterList',
          function(object, value) {
              object@mclass <- value

              if (validObject(object)) {
                  return(object)
              }
          })


