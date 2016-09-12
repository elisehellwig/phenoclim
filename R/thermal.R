
#---- thermalfunctions ----

#' Calculates thermal time based on the flat model
#'
#' This function calculates thermal time in growing degree hours from a
#'     vector of temperatures (in degrees C) based on the Flat model
#'     described below.
#' @param Tvec A vector of hourly temperatures, in degrees C.
#' @param Tb The base cardinal temperature of the model.
#' @param To The optimal cardinal temperature of the model.
#' @param sum Logical, should the vector of thermal times be returned as a
#'     sum?
#' @return either the number or vector of numbers representing the thermal
#'     time calculated
#' @details The functional form of the flat thermal time model is as follows:
#'     \deqn{GDH = \begin{cases}
#'              0 & T\leq T_b \\
#'              T - T_b & T_b\leq T\leq T_o \\
#'              T_o & T_o\leq T
#'              \end{cases}}
#'     where \eqn{T} is the hourly temperature, \eqn{T_b} is the base
#'     temperature and \eqn{T_o} is the optimal temperature.
#' @examples
#' x <- runif(30, -5, 40)
#' flat(x, 4, 25, sum=TRUE)
#' flat(x, 4, 25, sum=FALSE)
flat <- function(Tvec, Tb, To, sum=TRUE) {
	#see notebook
	#Tb - base temperature (no GDH below this)
	#To - optimum temperature (maximum GDH here)
	#Th is a vector of hourly temperatures

    Tb <- unname(unlist(Tb))
    To <- unname(unlist(To))


	temp <- Tvec - Tb
	temp[temp<0] <- 0
	temp[temp >= (To-Tb)] <- To - Tb

	if (sum) {
		tsum <- sum(temp)
		return(tsum)
	} else {
		return(temp)
	}


}

#' Calculates thermal time based on the Anderson model
#'
#' This function calculates thermal time in growing degree hours from a
#'     vector of temperatures (in degrees C) based on the Anderson
#'     \emph{et al.} (1986) model of thermal time described below.
#' @param Tvec A vector of hourly temperatures, in degrees C.
#' @param Tb The base cardinal temperature of the model.
#' @param To The optimal cardinal temperature of the model.
#' @param Tc The critical cardinal temperature of the model.
#' @param sum Logical, should the vector of thermal times be returned as a
#'     sum?
#' @return either the number or vector of numbers representing the thermal
#'     time calculated
#' @details The functional form of the Anderson thermal time model is as
#'     follows:
#'     \deqn{ GDH = \begin{cases}
#'         0 & T\leq T_b \\
#'         \frac{T_o-T_a}{2} \left[1+\cos\left(\pi + \pi \cdot \frac{T-T_b}{T_o-T_b}\right) \right] & T_b\leq T\leq T_o \\
#'         (T_o-T_a) \left[1+\cos\left(\frac{\pi}{2} + \frac{\pi}{2} \cdot \frac{T-T_o}{T_c-T_o}\right) \right] & T_o\leq T \leq T_c \\
#'         0 & T_c \leq T
#'         \end{cases}}
#'     where \eqn{T} is the hourly temperature, \eqn{T_b} is the base
#'     temperature, \eqn{T_o} is the optimal temperature, and \eqn{T_c} is
#'     the critical temperature.
#'
#'      Anderson, J. L., E. A. Richardson, and C. D. Kesner. 1985.
#'          “Validation of Chill Unit and Flower Bud Phenology Models
#'          for ’Montmorency’ sour Cherry.” In I International Symposium on
#'          Computer Modelling in Fruit Research and Orchard Management 184,
#'          71–78. http://www.actahort.org/books/184/184_7.htm.

#' @examples
#' x <- runif(30, -5, 40)
#' anderson(x, 4, 25, 36, sum=TRUE)
#' anderson(x, 4, 25, 36, sum=FALSE)
anderson <- function(Tvec, Tb, To, Tc, sum=TRUE) {
	#from Anderson et al. 1986
	#Th is a vector of hourly temperatures

    Tb <- unname(unlist(Tb))
    To <- unname(unlist(To))
    Tc <- unname(unlist(Tc))

    	#print(Th)
	temp <- Th - Tb

	#print(temp)

	temp[temp<0] <- 0

	temp[temp>0 & temp<=(To-Tb)] <- ((To - Tb)/2)*(1 + cos(pi + pi*(temp[temp>0 & temp<=(To-Tb)])/(To - Tb) ))

	temp[temp>(To-Tb) & temp<=(Tc-Tb)] <- (To - Tb)*(1 + cos(pi/2 + pi/2*(temp[temp>(To-Tb) & temp<=(Tc-Tb)] - (To-Tb))/(Tc - To) ))

	temp[temp>(Tc-Tb)] <- 0

	tsum <- sum(temp)

	if(sum) {
		return(tsum)
	} else {
		return(temp)
	}


}


#this function is not working
# beta <- function(Th, Tb, To, Tc, a=0, b=30) {
# 	#from Marra 2002
# 	#x is current temp
# 	#Tb is base temperature
# 	#To is optimal temperature
# 	#Tc is critical temperature
#
# 	c <- To
# 	d <- Tb - Tc
# 	e <- 2 - (Tb/To)
# 	f <- Tc/To
# 	m <- e - 1/e + f - 2
# 	n <- f - 1/e + f - 2
#
#
# 	q <- (Th - c + d*m)/d
# 	print((1-q))
#
# 	y = a + b*(q^(e-1)) * ((1-q)^(f-1)) / ((m^(e-1)) * (n^(f-1)))
#
# 	#tsum <- sum(y)
# 	return(y)
#
# }



trapezoid <- function(Th, Tb, To, Ts, Tc, sum=TRUE) {
	#Th is a vector of hourly temperatures
	#looks like a trapezoid

	temp <- Th - Tb

	temp[temp<0] <- 0
	temp[temp>(To-Tb) & temp<=(Ts-Tb)] <- To - Tb

	temp[temp>(Ts-Tb) & temp<=(Tc-Tb)] <-  (Tc - Tb) - temp[temp>(Ts-Tb) & temp<=(Tc-Tb)]
	temp[temp>=(Tc-Tb)] <- 0

    if (sum) {
        tsum <- sum(temp)
    } else {
        tsum <- temp
    }


	return(tsum)


}


#' Calculates thermal time based on the linear model
#'
#' This function calculates thermal time in growing degree hours from a
#'     vector of temperatures (in degrees C) based on the linear model
#'     described below.
#' @param Tvec A vector of hourly temperatures, in degrees C.
#' @param Tb The base cardinal temperature of the model.
#' @param sum Logical, should the vector of thermal times be returned as a
#'     sum?
#' @return either the number or vector of numbers representing the thermal
#'     time calculated
#' @details The functional form of the flat thermal time model is as follows:
#'     \deqn{GDH = \begin{cases}
#'              0 & T\leq T_b \\
#'              T - T_b & T_b\leq T\leq T_o \\
#'              T_o & T_o\leq T
#'              \end{cases}}
#'     where \eqn{T} is the hourly temperature, \eqn{T_b} is the base
#'     temperature and \eqn{T_o} is the optimal temperature.
#' @examples
#' x <- runif(30, -5, 40)
#' flat(x, 4, 25, sum=TRUE)
#' flat(x, 4, 25, sum=FALSE)
linear <- function(Th, Tb, sum=TRUE) {

    Tb <- unname(unlist(Tb))

	temp <- Th - Tb
	temp[temp<0] <- 0

	if (sum) {
	    tsum <- sum(temp)
	} else {
	    tsum <- temp
	}

	return(tsum)
}



triangle <- function(Th, Tb, To, Tc, sum=TRUE) {

    Tb <- unname(unlist(Tb))
    To <- unname(unlist(To))
    Tc <- unname(unlist(Tc))

    Th[Th<=Tb] <- 0
    Th[Th>Tb & Th<=To] <- Th[Th>Tb & Th<=To] - Tb
    Th[Th>To & Th<Tc] <- Th[Th>To & Th<Tc]*((Tb - To)/(Tc - To)) - Tc*((Tb-To)/(Tc-To))
    Th[Th>=Tc] <- 0


    if (sum) {
        tsum <- sum(Th)
    } else {
        tsum <- Th
    }
    return(tsum)
}



gdd <- function(Tdat, Tb, sum=TRUE) {
    #Tmat is a matrix where the columns are Tmin and Tmax for each of the days
    #From Zalom et al. 1983/Snyder 1999
    Tmin <- Tdat[,'tmin']
    Tmax <- Tdat[,'tmax']

    #print(length(Tmin))

    Tb <- as.vector(unname(unlist(Tb)))
    #print(class(Tb))
    dd <- rep(0, length(Tmin))

    i1 <- which(Tmin>Tb)
    dd[i1] <- (Tmax[i1] + Tmin[i1])/2 - Tb

    i2 <- which(Tmax<Tb)
    i3 <- setdiff(1:length(Tmin), c(i1, i2))
    dd[i3] <- ((Tmax[i3] - Tb)/2) * (Tmax[i3] - Tb) / (Tmax[i3] - Tmin[i3])

    if (sum) {
        sdd <- sum(dd)
    } else {
        sdd <- dd
    }

    return(sdd)
}


gddsimple <- function(tdat, Tb, sum=TRUE) {
    Tavg <- (tdat[,'tmin'] + tdat[,'tmax'])/2
    dd <- Tavg - Tb
    dd[dd<0] <- 0

    if (sum) {
        return(sum(dd))
    } else {
        return(dd)
    }
}
