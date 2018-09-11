
#' Extracts Parameters from optimization
#'
#' This function extracts either the start day, threshold or cardinal
#'     temperatures from the parameterlist or optim object depending on whether
#'     the parameter was optimized/estimated or not.
#'
#' @param estimate list, list of the following logical vectors: estimatestart,
#'     estimatethresh, estimateCT
#' @param parname character, the name of the parameter to extract, either
#'     'start', 'threshold', or 'cardinaltemps'
#' @param parlist list, list of parameterlists
#' @param optlist list, list of optim function outputs
#' @return the value of the selected parameter
#' @export
extractParameters <- function(estimate, parname, parlist, optlist) {

    n <- length(modlist)

    #expand estimatestart
    if (length(estimate[[1]])==1 & n > 1) {
        estimatestart <- rep(estimate[[1]], n)
    } else {
        estimatestart <- estimate[[1]]
    }

    #expand estimatethresh
    if (length(estimate[[2]])==1 & n > 1) {
        estimatethresh <- rep(estimate[[2]], n)
    } else {
        estimatethresh <- estimate[[2]]
    }


    #expand cardinal temps
    if (length(estimate[[3]])==1 & n > 1) {
        estimatect <- rep(estimate[[3]], n)
    } else {
        estimatect <- estimate[[3]]
    }

    #extract start values
    if (parname=='start') {
        value <- sapply(1:n, function(i) {
            if (estimatestart[i]) {
                unname(optlist[[i]][["bestmem"]][1])
            } else {
                startday(parlist[[i]])
            }
        })

        #extract threshold values
    } else if (parname=='threshold') {

        value <- sapply(1:n, function(i) {
            if (estimatestart[i] & estimatethresh[i]) {
                unname(optlist[[i]][["bestmem"]][2])
            } else if (estimatethresh[i]) {
                unname(optlist[[i]][["bestmem"]][1])
            } else {
                threshold(parlist[[i]])
            }
        })

        #extract cardinal temperatures
    } else if (parname=='cardinaltemps') {

        CTid <- estimatestart + estimatethresh + 1

        pl <- sapply(1:n, function(i) {
            length(optlist[[i]][["bestmem"]])
        })

        value <- sapply(1:n, function(i) {
            if (estimatect[i]) {
                unname(optlist[[i]][["bestmem"]][CTid[i]:pl[i]])
            } else {
                cardinaltemps(parlist[[i]])
            }
        })

    } else {
        stop("parname must be 'start', 'threshold', or 'cardinaltemps'.")
    }

    return(value)

}









