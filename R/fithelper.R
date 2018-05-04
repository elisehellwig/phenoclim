

convertParameters <- function(pars, form, CT, S, TH, modclass) {

    ctlen <- parnum(form) #what is the number of parameter values for the form
    plen <- length(pars) #what is the total number of parameters in the model

    #creating the variable that holds the cardinal temperatures
    if (isTRUE(CT)) ct <- pars[(plen-ctlen+1):plen] else ct <- CT[1:ctlen]

    if (isTRUE(S)) {
        s <- pars[1]
    } else if (modclass)


    if (modclass=='FlowerModel') {




    } else {

    }

}
