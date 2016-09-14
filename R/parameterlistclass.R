#' An S4 class to store model parameters
#'
#' ParameterList stores parameters for running the phenological models
#'
#' @slot modlength Stores the length of time or thermal time that is accumulated
#'     in the model. There should be either one value (if there is only one
#'     or if all the stages have the same parameters) or as many values as there
#'     are stages.
#' @slot cardinaltemps The cardinal temperatures for the model. This is a list
#'     that should either be of length one or the length of the number of
#'     stages. Each element of the list should contain the same number of
#'     cardinal parameters.
setClass('ParameterList',
         slots=list(modlength = "numeric",
                    cardinaltemps = "list"))


#############################################################
#generics

#' Returns the returns a vector of accumulation lengths
#'
#' @param object An object of class ParameterList
#' @return A vector of model lengths
#' @export
setGeneric('modlength', function(object) standardGeneric('modlength'))


#' Returns the returns a list of cardinal temperatures
#'
#' @param object An object of class ParameterList
#' @return A list of cardinal temperatures, one for each stage of the model.
#' @export
setGeneric('cardinaltemps', function(object) standardGeneric('cardinaltemps'))


#' Setting the modlength
#'
#' Used to change the model length without recreating the object.
#'
#' @param object An object of class ParameterList
#' @param value A vector of acccumulation lengths.
#' @export
setGeneric('modlength<-', function(object, value) standardGeneric('modlength<-'))


#' Setting the cardinal temps
#'
#'  Used to change the cardinal temperature parameters without recreating the
#'       object.
#'
#' @param object An object of class ParameterList
#' @param value A list of cardinal temperatures
#' @export
setGeneric('cardinaltemps<-', function(object, value) standardGeneric('cardinaltemps<-'))


#' Creates a ParameterList object
#'
#' @param ct A list, data.frame, or matrix of cardinal temperatures. In the case
#'     of a data.frame or matrix, each row should contain the cardinal
#'     temperatures for a given stage of the model.
#' @param length A vector of model lengths
#' @return An object of the class ParameterList.
#' @export
setGeneric('parameterlist',
           function(ct, length) standardGeneric('parameterlist'))






