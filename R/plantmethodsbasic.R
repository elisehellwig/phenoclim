#' @include classes.R

#This document has the basic methods for accessing and manipulating objects
#   of the class Plant

setMethod("show",
          signature = 'Plant',
          definition = function(object) {

          })




setValidity("Plant", function(object) {
    msg <- NULL
    valid <- TRUE

    if (!(modeltype %in% c('partial','full','combined','time'))) {
        valid <- FALSE
        msg <- c(msg,
                 "modeltype must be either partial, full combined or time")
    }

    if (length(names()))


})

