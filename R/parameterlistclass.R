#' An S4 class to store model parameters
#'
#' ParameterList stores parameters for running the phenological models
#'
#' @slot stages numeric, The number of stages in the phenological model.
#' @slot modeltype character. The type of phenological model to be used. Options
#'     are 'thermal' and 'day'. See ____ for more information.
#' @slot simplified logical. Should the simplified version of the model type be
#'      used.
#' @slot form character. The functional form of the thermal time accumulation
#'     model. Current options are gdd, gddsimple, linear, flat, triangle, and
#'     anderson.
#' @slot cardinaltemps list. The cardinal temperatures for the model. This is a
#'     list that should either be of length one or the length of the number of
#'     stages. Each element of the list should contain the same number of
#'     cardinal parameters.
#' @slot modlength numeric. Stores the length of time or thermal time that is
#'     accumulated in the model. There should be either one value (if there is
#'     only one or if all the stages have the same parameters) or as many values
#'     as there are stages.
#' @slot parsOptimized Determines what parameters are optimized in the model.
#'     `parsOptimized` is a character vector that can contain "cardinaltemps",
#'     "modlength" or both, but it must contain at least one of the two.
#'     Currently this cannot vary by stage.
setClass('ParameterList',
         slots=list(stages='numeric',
                    modeltype='character',
                    simplified='logical',
                    form = 'character',
         	        cardinaltemps = "list",
                    modlength = "numeric",
                    parsOptimized = 'character'))


#############################################################
#generics

#' Returns the number of stages in the model
#'
#' @param object An object of class ParameterList
#' @return The number of stages in the model
#' @export
setGeneric('stages', function(object) standardGeneric('stages'))

#' Returns the model type
#'
#' @param object An object of class ParameterList
#' @return character, specifies what type of model the parameters are for.
#'     Options are 'thermal' and 'day'
#' @export
setGeneric('modeltype', function(object) standardGeneric('modeltype'))

#' Is the model simplified?
#'
#' @param object An object of class ParameterList
#' @return logical, is the simplified version of the model being fit? See ____
#'      for more information.
#' @export
setGeneric('simplified', function(object) standardGeneric('simplified'))


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

#' Returns the functional form
#'
#' @param object An object of class ParameterList
#' @return character, the name of the functional form of the model of thermal
#'     time accumulation.
#' @export
setGeneric('form', function(object) standardGeneric('form'))

#' Returns the parameters to estimate
#'
#' @param object An object of class ParameterList
#' @return character, the names of the parameters to be estimated
#'     (cardinaltemps, modlength or both).
#' @export
setGeneric('parsOptimized', function(object) standardGeneric('parsOptimized'))


#' Setting the modlength
#'
#' Used to change the model length without recreating the object.
#'
#' @param object An object of class ParameterList
#' @param value A vector of acccumulation lengths.
#' @export
setGeneric('modlength<-', function(object, value) {
    standardGeneric('modlength<-')})


#' Setting the cardinal temps
#'
#'  Used to change the cardinal temperature parameters without recreating the
#'       object.
#'
#' @param object An object of class ParameterList
#' @param value A list of cardinal temperatures
#' @export
setGeneric('cardinaltemps<-', function(object, value) {
    standardGeneric('cardinaltemps<-')
    })



#' Setting the form
#'
#'  Used to change the functional form without recreating the object.
#'
#' @param object An object of class ParameterList
#' @param value A list of cardinal temperatures
#' @export
setGeneric('form<-', function(object, value) standardGeneric('form<-'))


#' Setting the parameters to optimize
#'
#'  Used to change the functional form without recreating the object.
#'
#' @param object An object of class ParameterList
#' @param value A character vector containing either 'cardinaltemps',
#'     'modlength' or both.
#' @export
setGeneric('parsOptimized<-', function(object, value) {
    standardGeneric('parsOptimized<-') })



#' Setting the number of stages
#'
#'  Used to change the number of stages without recreating the object.
#'
#' @param object An object of class ParameterList
#' @param value numeric, the number of stages in teh new model
#' @export
setGeneric('stages<-', function(object, value) standardGeneric('stages<-'))


#' Setting the model type
#'
#'  Used to change the model type without recreating the object.
#'
#' @param object An object of class ParameterList
#' @param value character, the model type, either 'thermal' or 'day'. See _____
#'     for more information.
#' @export
setGeneric('modeltype<-', function(object, value) standardGeneric('modeltype<-'))


#' Setting whether the model is simplified
#'
#'  Used to change whether or not the model is simplified without recreating the
#'       object.
#'
#' @param object An object of class ParameterList
#' @param value logical, is the model simplified?
#' @export
setGeneric('simplified<-', function(object, value) {
    standardGeneric('simplified<-')} )






