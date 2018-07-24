#' An S4 class to store model parameters
#'
#' ParameterList stores parameters for running the phenological models
#'
#' @slot stages numeric, The number of stages in the phenological model.
#' @slot modeltype character. The type of phenological model to be used. Options
#'     are 'DT' for Day threshold and 'TTT' for thermal time threshold. See ____
#'     for more information.
#' @slot form character. The functional form of the thermal time accumulation
#'     model. Current options are gdd, gddsimple, linear, flat, triangle, and
#'     anderson.
#' @slot cardinaltemps list. The cardinal temperatures for the model. This is a
#'     list that should either be of length one or the length of the number of
#'     stages. Each element of the list should contain the same number of
#'     cardinal parameters.
#' @slot threshold numeric. Stores the threshold of time or thermal time that is
#'     accumulated in the model. Set to NA to run the base (simplified) model.
#' @slot startday numeric, stores the day of the year the model will start or
#'     the offset to the previous stage end that the model starts. A startday
#'     of 0 means either that the model starts the day the last stage ended.
#' @slot varyingpars character, c('threshold', 'start'). the parameters that
#'     will vary from year to year. If none will vary, set to NA.
#' @slot parsOptimized character. Determines what parameters are optimized in
#'      the model. `parsOptimized` is a character vector that can contain
#'      "cardinaltemps", "threshold", "start" but it must contain at
#'      least one (or else why optimize). Currently this cannot vary by stage.
#' @slot mclass character. Is this a parameter list going to be used to
#'     fit a 'PlantModel' or a 'FlowerModel'.
setClass('ParameterList',
         slots=list(stages='numeric',
                    modeltype='character',
                    form = 'character',
         	        cardinaltemps = "list",
                    threshold = "vector",
                    startday = 'vector',
                    varyingpars = 'character',
                    parsOptimized = 'character',
                    mclass='character'))


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


#' Returns the returns a vector of thresholds
#'
#' @param object An object of class ParameterList
#' @return The time or thermal time thresholds for the model.
#' @export
setGeneric('threshold', function(object) standardGeneric('threshold'))

#' Returns  a vector of start days
#'
#' @param object An object of class ParameterList
#' @return A vector of the start days or NAs for starting at bloom for
#'     PlantModels and harvest for FlowerModels
#' @export
setGeneric('startday', function(object) standardGeneric('startday'))


#' Returns the parameters that vary from year to year
#'
#' @param object An object of class ParameterList
#' @return A vector of the parameters that will vary from year to year for
#'     PlantModels and harvest for FlowerModels
#' @export
setGeneric('varyingpars', function(object) standardGeneric('varyingpars'))


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
#'     time.
#' @export
setGeneric('form', function(object) standardGeneric('form'))

#' Returns the parameters to estimate
#'
#' @param object An object of class ParameterList
#' @return character, the names of the parameters to be estimated
#'     (cardinaltemps, modlength or both).
#' @export
setGeneric('parsOptimized', function(object) standardGeneric('parsOptimized'))


#' Returns whether the model is a 'PlantModel' or a 'FlowerModel'
#'
#' @param object An object of class ParameterList
#' @return character, whether the model is a PlantModel or a FlowerModel
#' @export
setGeneric('mclass', function(object) standardGeneric('mclass'))



#' Setting the model threshold
#'
#' Used to change the model threshold without recreating the object.
#'
#' @param object An object of class ParameterList
#' @param value The time or thermal time threshold of the model.
#' @export
setGeneric('threshold<-', function(object, value) {
    standardGeneric('threshold<-')})


#' Setting the start day of the model
#'
#' Used to change the start day without recreating the object.
#'
#' @param object An object of class ParameterList
#' @param value A vector of start days for the model or NAs for starting at
#'     bloom for PlantModels and harvest for FlowerModels
#' @export
setGeneric('startday<-', function(object, value) {
    standardGeneric('startday<-')})



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



#' Setting the varying parameters
#'
#'  Used to change the parameters that will vary without recreating the object.
#'
#' @param object An object of class ParameterList
#' @param value A vector of parameters that will vary from year to year
#' @export
setGeneric('varyingpars<-', function(object, value) {
    standardGeneric('varyingpars<-')})



#' Setting the parameters to optimize
#'
#'  Used to change the functional form without recreating the object.
#'
#' @param object An object of class ParameterList
#' @param value A character vector containing 'cardinaltemps',
#'     'threshold', 'start' or any combination of the three.
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


#' Setting stages as PlantModels or a FlowerModels
#'
#'  Used to change whether the model uses the PlantModel class or the
#'  FlowerModel class.
#'
#' @param object An object of class ParameterList
#' @param value character, is the model to be fit a 'PlantModel' or a
#'     'FlowerModel'.
#' @export
setGeneric('mclass<-', function(object, value) {
    standardGeneric('mclass<-')} )







