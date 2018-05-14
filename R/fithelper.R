#' @include flipday.R
NULL


#' Separates out Parameter Values for optimization
#'
#' Assigns parameter values for start and threshold based on model type,
#'     which parameters are being estimated and which parameters are varying
#'     from year to year.
#'
#' @param pars numeric, a vector of parameters that are optimized using the
#'     `DEoptim` function.
#' @param modtype character, is the model a day threshold ('DT') model or a
#'     thermal time threshold model ('TTT').
#' @param S logical/numeric, is the day to start counting being optimized? If
#'     not, the value associated with the start day should be provided.
#' @param TH logical/numeric, should the model threshold be optimized. If
#'     not L is the (numeric) model threshold.
#' @param vp character, c('start', 'threshold') should either of these pars
#'     vary from year to year.
#' @param eventvec numeric, a vector that contains the day of the year for the
#'     starting event of the model. For bloom the event is harvest.
convertParameters <- function(pars, modtype, S, TH, vp, eventvec, years) {

    #Estimating start day
    if (isTRUE(S)) {
        s <- pars[1]

    } else { #Not estimating start day

        if ('start' %in% vp) { #case: DT1-4, case TTT1-2
            s <- S

        } else { #case: DT 5, TTT3
            yearlength <- ifelse(is.leapyear(years), 366, 365)
            s <- yearlength - eventvec + S + 1
        }

    }

    #Estimating threshold
    if (isTRUE(TH)) {

        if (isTRUE(S)) {
            th <- pars[2]
        } else {
            th <- pars[1]
        }

    } else { #not estimating threshold

        if (modtype=='DT' & ('threshold' %in% vp)) {
            #case DT1,3
            th <- S + TH

        } else {
            #case DT2,4,5, TTT1-3
            th <- dayToDAE(TH, eventvec, years)

        }

    }

    return(c(s, th))

}
