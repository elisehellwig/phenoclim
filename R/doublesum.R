#' @include thermalsum.R
NULL

#' Calculates chill and heat sums for the dual model
#'
#' Calculates sequential chill and heat sums to predict flowering for a series
#'     of years.
#'
#' @param pars Cardinal temperatures
#' @param yrloc data.frame, the year/locations we have phenology data for.
#' @param tdat list containing the temperature information
#' @param forms the functional forms of the thermal time accumulation
#' @param start POSIXct or numeric, the day or date to start accumulating time
#'     or thermal time towards the model threshold.
#' @param thresholds numeric, the length of thermal time accumulation (in
#'     either days or thermal time units).
#' @param varying character, c('start', 'threshold') should either of these
#'     pars vary from year to year.
#' @param mclass character, class of model to be estimating, options are
#'     'PlantModel' or 'FlowerModel'. If you have negative day values, you
#'     probably want flower model.
#' @param startingevent numeric, days first event happened each year.
#' @return The thermal sums for a given series of years.
#' @export
dualsum <- function(pars, yrloc, tdat, forms, start, thresholds, varying,
                      mclass, startingevent=NA) {

    yrs <- yrloc[,'year']
    #print('dualsum')
    startDate <- dayToDate(yrs, start, 'FlowerModel')
    #print(startDate)

    chilldays <- thermalsum(pars[[1]], yrloc, tdat, 'TTT', forms[1], start,
                            thresholds[1], NA, 'FlowerModel', startingevent)

    #print(chilldays)
    chillduration <- duration(num=chilldays, units='days')

    heatStartDate <- startDate + chillduration + duration(num=1, units='hours')

    #heatStartDate <- dayToDate(yrs, endchill+1, 'FlowerModel')
    #print(heatStartDate)

    heatdays <- thermalsum(pars[[2]], yrloc, tdat, 'TTT', forms[2],
                           heatStartDate, thresholds[2], NA, 'FlowerModel',
                           startingevent)

   # print(heatdays)

    endheat <- chilldays + heatdays
    #print(endheat)

    return(endheat)


}
