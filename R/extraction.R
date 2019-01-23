
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


    nstage <- stages(parlist) #number of stages for each functional form
    nform <- length(nstage) #number of functional forms

    #print(optlist)

    #expand estimatestart
    if (length(estimate[[1]])==1 & nform > 1) {
        estimatestart <- rep(estimate[[1]], nform)
    } else {
        estimatestart <- estimate[[1]]
    }

    #expand estimatethresh
    if (length(estimate[[2]])==1 & nform > 1) {
        estimatethresh <- rep(estimate[[2]], nform)
    } else {
        estimatethresh <- estimate[[2]]
    }


    #expand cardinal temps
    if (length(estimate[[3]])==1 & nform > 1) {
        estimatect <- rep(estimate[[3]], nform)
    } else {
        estimatect <- estimate[[3]]
    }


    #Stages are the rows and functional forms are the columns of the matrix:
    #
    #        linear  GDD
    #stage 1    ?     ?
    #stage 2    ?     ?

    #extract start values
    if (parname=='start') {
        value <- sapply(1:nform, function(i) {
            sapply(1:nstage[i], function(j) {
                if (estimatestart[i]) {
                    unname(optlist[[i]][[j]][["bestmem"]][1])
                } else {
                    startday(parlist[[i]])
                }
            })
        })

        #extract threshold values
    } else if (parname=='threshold') {

        value <- sapply(1:nform, function(i) {
            sapply(1:nstage[i], function(j) {
                if (estimatestart[i] & estimatethresh[i]) {
                    unname(optlist[[i]][[j]][["bestmem"]][2])
                } else if (estimatethresh[i]) {
                    unname(optlist[[i]][[j]][["bestmem"]][1])
                } else {
                    threshold(parlist[[i]])
                }
            })
        })

        #extract cardinal temperatures
    } else if (parname=='cardinaltemps') {

        CTid <- estimatestart + estimatethresh + 1

        pl <- sapply(1:nform, function(i) {
            sapply(1:nstage[i], function(j) {
                length(optlist[[i]][[j]][["bestmem"]])
            })

        })

        value <- lapply(1:nform, function(i) {
            lapply(1:nstage[i], function(j) {
                if (estimatect[i]) {
                    unname(optlist[[i]][[j]][["bestmem"]][CTid[i]:pl[j,i]])
                } else {
                    cardinaltemps(parlist[[i]])
                }
            })

        })

    } else {
        stop("parname must be 'start', 'threshold', or 'cardinaltemps'.")
    }

    return(value)

}









