#' Calculates chill and heat sums for the dual model
#'
#' Calculates sequential chill and heat sums to predict flowering for a series
#'     of years.
#'
#' @param pars Cardinal temperatures
#' @param yrs the years we have phenology data for.
#' @param tdat list containing the temperature information
#' @param forms the functional forms of the thermal time accumulation
#' @param startDate POSIXct, the date to start accumulating time or thermal
#'     time towards the model threshold.
#' @param thresh numeric, the length of thermal time accumulation (in either
#'     days or thermal time units).
#' @param varying character, c('start', 'threshold') should either of these
#'     pars vary from year to year.
#' @param mclass character, class of model to be estimating, options are
#'     'PlantModel' or 'FlowerModel'. If you have negative day values, you
#'     probably want flower model.
#' @return The thermal sums for a given series of years.
doublesum <- function(pars, yrs, tdat, forms, startDate, thresh, varying,
                      mclass) {


    if (mclass=='FlowerModel') {
        endDate <- dayToDate(yrs+1, 184, 'FlowerModel')

    } else {
        endDate <- dayToDate(yrs, 365, 'PlantModel')
    }

    modInterval <- interval(startDate, endDate)

    if (length(modInterval)==1) {
        modInterval <- rep(modInterval, length(yrs))

    }


}
