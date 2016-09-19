#' @include plantmethodsbasic.R thermal.R general.R

#This script has constructor functions for both ParameterList and Plant classes

#' Creates ParameterList object
#'
#' This function creates an object of the class ParameterList. Plant objects
#'     require objects of the class ParameterList for their parameters slot.
#'
#' @param ct A list, data.frame, or matrix of cardinal temperatures. In the
#'     case of a data.frame or matrix, each row should contain the cardinal
#'     temperatures for a given stage of the model. If ct is a list, each
#'     element of the list should be a vector of cardinal temperatures for a
#'     given stage of the model.
#' @param length A vector of model lengths
#' @return An object of the class ParameterList.
#' @export
parameterlist <- function(ct, length) {

    if (class(ct)=='list') {
        newobject <- new('ParameterList', cardinaltemps=ct, modlength=length)

    } else if (class(ct) %in% c('data.frame', 'matrix') ) {
        ctlist <- lapply(1:dim(ct)[1], function(i) ct[i,])

        newobject <- new('Parameterlist', cardinaltemps=ctlist,
                         modlength=length)
    } else {
        stop('ct must be of the type list, data.frame, or matrix.')
    }

    return(newobject)
}




#' Creates Plant object
#'
#' @param pheno data.frame that contains the phenology data. Variables must
#'      include year, day (julian), and temp (or tmin and tmax depending on the
#'      model).
#' @param clim list or data.frame that contains the temperature data. If
#' @param model character, the name of the type of model to be used. Options are
#'     partial, full. and combined. See Details for more information.
#' @param form function, the functional form of the thermal time calculations.
#'     See details for more information
#' @param stages numeric, the number of phenological stages to be modeled.
#' @param parameters object of the class ParameterList. More information at
#'     \code{\link{parameterlist}}.
#' @export
plant <- function(pheno, clim, model, form, stages, parameters) {

    years <- unique(pheno[,'year'])
    cols <- paste0('event', 1:(stages+1))



    if (is.data.frame(clim)) {
        climlist <- lapply(1:stages, function(i) {


            extracttemp(clim, years, pheno[,cols[i]], pheno[,cols[i+1]])
        })

    } else if (is.list(clim)) {

        if ((length(clim)==stages & stages > 1) | stages==1) {

            climlist <- clim

        } else {
            stop('There must be a temperature list for every stage.')
        }

    } else {
        stop('clim must be a data.frame or a list.')
    }

    obj <- new('Plant',
               phenology = pheno,
               temperature = climlist,
               model = model,
               form = form,
               stages = stages,
               parameters = parameters)

    return(obj)

}
