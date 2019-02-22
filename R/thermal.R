
#---- thermalfunctions ----

#' @name Forms
#' @rdname Forms
#' @title Functional forms for thermal time calculation
#'
#' @description These functions calculate thermal time (chill and heat) for a
#'     given vector or matrix of temperatures in degrees C.
#'
#' @param Tvec A vector of hourly temperatures, in degrees C.
#' @param tdat A matrix of minimum and maximum daily temperatures, in degrees
#'     C.
#' @param Tb The base cardinal temperature of the model.
#' @param To The optimal cardinal temperature of the model.
#' @param Ts The subcritical cardinal temperature of the model.
#' @param Tc The critical cardinal temperature of the model.
#' @param Tmax numeric, the maximum temperature where trees accumulate chill.
#' @param sum Logical, should the vector of thermal times be returned as a
#'     sum?
#' @return A vector of numbers representing the chill or heat calculated.
NULL



#' @rdname Forms
#'
#'
#' @details The functional form of the flat thermal time model is as follows:
#'     \deqn{GDH = \begin{cases}
#'              0 & T\leq T_b \\
#'              T - T_b & T_b\leq T\leq T_o \\
#'              T_o & T_o\leq T
#'              \end{cases}}
#'     where \eqn{T} is the hourly temperature, \eqn{T_b} is the base
#'     temperature and \eqn{T_o} is the optimal temperature.
#' @examples
#' temp <- seq(-5, 50)
#' gdh <- flat(temp, 4, 25, sum=FALSE)
#' plot(gdh ~ temp)
#' @export
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

#' @rdname Forms
#' @details The functional form of the Anderson thermal time model (Anderson
#'     \emph{et al.} 1986) is as follows:
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
#'          "Validation of Chill Unit and Flower Bud Phenology Models
#'          for 'Montmorency' sour Cherry." In I International Symposium on
#'          Computer Modelling in Fruit Research and Orchard Management 184,
#'          71-78. http://www.actahort.org/books/184/184_7.htm.

#' @examples
#' gdh <- asymcur(temp, 4, 25, 36, sum=FALSE)
#' plot(gdh ~ temp)
#' @export
asymcur <- function(Tvec, Tb, To, Tc, sum=TRUE) {
	#from Anderson et al. 1986
	#Th is a vector of hourly temperatures

    Tb <- unname(unlist(Tb))
    To <- unname(unlist(To))
    Tc <- unname(unlist(Tc))

    	#print(Th)
	temp <- Tvec - Tb

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


#' @rdname Forms
#'
#' @details The functional form of the trapezoid thermal time model is as
#'     follows:
#'     \deqn{ GDH = \begin{cases}
#'         0 & T\leq T_b \\
#'         T - T_b & T_b \leq T \leq T_o \\
#'         T_o - T_b & T_o\leq T \leq T_s \\
#'         T*(Tb-To)/(Tc-Ts) + Tc*(To-Tb)/(To) & T_s\leq T \leq T_c \\
#'         0 & T_c \leq T
#'         \end{cases}}
#'     where \eqn{T} is the hourly temperature, \eqn{T_b} is the base
#'     temperature, \eqn{T_o} is the optimal temperature, and \eqn{T_c} is
#'     the critical temperature.
#'
#' @examples
#' gdh <- trapezoid(temp, 4, 25, 36, 40, sum=FALSE)
#' plot(gdh ~ temp)
#' @export
trapezoid <- function(Tvec, Tb, To, Ts, Tc, sum=TRUE) {
	#Th is a vector of hourly temperatures
	#looks like a trapezoid

	temp <- Tvec

	temp[temp<=Tb] <- 0
	temp[temp>Tb & temp<=To] <- temp[temp>Tb & temp<=To] - Tb
	temp[temp>To & temp<=Ts] <- To - Tb

	temp[temp>(Ts) & temp<=(Tc)] <- temp[temp>(Ts) & temp<=(Tc)]*(Tb-To)/(Tc-Ts) + Tc*(To - Tb)/(Tc-Ts)
	temp[temp>=(Tc)] <- 0

    if (sum) {
        tsum <- sum(temp)
    } else {
        tsum <- temp
    }


	return(tsum)


}


#' @rdname Forms
#'
#' @details The functional form of the flat thermal time model is as follows:
#'     \deqn{GDH = \begin{cases}
#'     0 & T\leq T_b \\
#'     T - T_b & T_b \leq T
#'     \end{cases}}
#'     where \eqn{T} is the hourly temperature, \eqn{T_b} is the base
#'     temperature and \eqn{T_o} is the optimal temperature.
#' @examples
#' gdh <- linear(temp, 4, sum=FALSE)
#' plot(gdh ~ temp)
#' @export
linear <- function(Tvec, Tb, sum=TRUE) {

    Tb <- unname(unlist(Tb))

	temp <- Tvec - Tb
	temp[temp<0] <- 0

	if (sum) {
	    tsum <- sum(temp)
	} else {
	    tsum <- temp
	}

	return(tsum)
}


#' @rdname Forms
#' @details The functional form of the trianble thermal time model is as
#'     follows:
#'     \deqn{ GDH = \begin{cases}
#'         0 & T\leq T_b \\
#'         T - T_b & T_b \leq T \leq T_o \\
#'         \frac{(T_c - T) (T_o - T_b)}{T_c - T_o} & T_o\leq T \leq T_c \\
#'         0 & T_c \leq T
#'         \end{cases}}
#'     where \eqn{T} is the hourly temperature, \eqn{T_b} is the base
#'     temperature, \eqn{T_o} is the optimal temperature, and \eqn{T_c} is
#'     the critical temperature.
#'
#' @examples
#' gdh <- triangle(temp, 4, 25, 36, sum=FALSE)
#' plot(gdh ~ temp)
#' @export
triangle <- function(Tvec, Tb, To, Tc, sum=TRUE) {

    Tb <- unname(unlist(Tb))
    To <- unname(unlist(To))
    Tc <- unname(unlist(Tc))

    Tvec[Tvec<=Tb] <- 0
    Tvec[Tvec>Tb & Tvec<=To] <- Tvec[Tvec>Tb & Tvec<=To] - Tb
    Tvec[Tvec>To & Tvec<Tc] <- Tvec[Tvec>To & Tvec<Tc]*((Tb - To)/(Tc - To)) - Tc*((Tb-To)/(Tc-To))
    Tvec[Tvec>=Tc] <- 0


    if (sum) {
        tsum <- sum(Tvec)
    } else {
        tsum <- Tvec
    }
    return(tsum)
}


#' @rdname Forms
#' @details The functional form of the flat thermal time model is as follows:
#'     \deqn{GDD = \begin{cases}
#'         0 & T_{max}\leq T_b \\
#'         \frac{(T_{max}-T_b)^2}{2(T_{max} - T_{min})} & T_{min} \leq T_b \leq T_{max}\\
#'         T - T_{avg} & T_{min} \geq T_b\\
#'         \end{cases}}
#'     where \eqn{T_min} is the daily minimum temperature, \eqn{T_max} is the
#'     daily maximum temperature, and \eqn{T_b} is the base temperature.
#' @examples
#' temp <- data.frame(tmin=runif(10, 0, 10), tmax=runif(10, 13, 25))
#' gdd(temp, 4, sum=TRUE)
#' gdd(temp, 4, sum=FALSE)
#' @export
gdd <- function(tdat, Tb, sum=TRUE) {
    #Tmat is a matrix where the columns are Tmin and Tmax for each of the days
    #From Zalom et al. 1983/Snyder 1999
    Tmin <- tdat[,'tmin']
    Tmax <- tdat[,'tmax']

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

#' @rdname Forms
#' @details The functional form of the simple gdd thermal time model is as follows:
#'     \deqn{GDD = \begin{cases}
#'         0 & T_{avg}\leq T_b \\
#'         T - T_{avg} & T_b \leq T_{avg}\\
#'         \end{cases}}
#'     where \eqn{T_{avg}} is the average daily temperature, and \eqn{T_b} is
#'     the base temperature.
#' @examples
#' gddsimple(temp, 4, sum=TRUE)
#' gddsimple(temp, 4, sum=FALSE)
#' @export
gddsimple <- function(tdat, Tb, sum=TRUE) {
    Tavg <- (tdat[,'tmin'] + tdat[,'tmax'])/2
    Tb <- as.vector(unname(unlist(Tb)))

    dd <- Tavg - Tb
    dd[dd<0] <- 0

    if (sum) {
        return(sum(dd))
    } else {
        return(dd)
    }
}


#' @rdname Forms
#' @details This function calculates the amount of chill accumulated based on the 32F-45F (0C-7.2C) model. The model is described in Luedeling, E. and Brown,
#'     P.H., 2011. A global analysis of the comparability of winter chill
#'     models for fruit and nut trees. International Journal of Biometeorology,
#'      55(3), pp.411-421.
#' @export
chillbasic <- function(Tvec, Tmax, sum=TRUE) {

    ch <- ifelse(Tvec<Tmax & Tvec>0, 1, 0)

    if (sum) {
        ch <- sum(ch)
    }

    return(ch)
}

#' @rdname Forms
#' @details This model is described in Luedeling, E. and Brown, P.H., 2011. A
#'     global analysis of the comparability of winter chill models for fruit
#'     and nut trees. International Journal of Biometeorology, 55(3),
#'     pp.411-421.
#' @export
utah_original <- function(Tvec, sum=TRUE) {
    uch <- Tvec

    uch[Tvec<=1.4] <- 0
    uch[(Tvec>1.4 & Tvec<=2.4)] <- 0.5
    uch[(Tvec>2.4 & Tvec<=9.1)] <- 1
    uch[(Tvec>9.1 & Tvec<=12.4)] <- 0.5
    uch[(Tvec>12.4 & Tvec<=15.9)] <- 0
    uch[(Tvec>15.9 & Tvec<=18.0)] <- -0.5
    uch[(Tvec>18.0)] <- -1

    if (sum) {
        uch <- sum(uch)
    }

    return(uch)

}

#' @rdname Forms
#' @details The utah() function is a smoothed version of the model described
#'     at \url{http://fruitsandnuts.ucdavis.edu/Weather_Services/chilling_accumulation_models/about_chilling_units/}. This was done to ensure optimization
#'     procedures using this model would converge.
#'
#' @export
utah <- function(Tvec, sum=TRUE) {
    uch <- Tvec

    uch[Tvec<=1.1] <- 0
    uch[(Tvec>1.1 & Tvec<=2.2)] <- uch[(Tvec>1.1 & Tvec<=2.2)]/1.1 - 1
    uch[(Tvec>2.2 & Tvec<=8.9)] <- 1
    uch[(Tvec>8.9 & Tvec<=18.3)]<-uch[(Tvec>8.9 & Tvec<=18.3)]*(-10)/47 + 136/47
    uch[(Tvec>18.3)] <- -1

    if (sum) {
        uch <- sum(uch)
    }

    return(uch)
}



#' @rdname Forms
#' @details The chillPortions model is described in at \url{http://fruitsandnuts.ucdavis.edu/Weather_Services/chilling_accumulation_models/about_chilling_units/}
#' @export
chillPortions <- function(Tvec, sum=TRUE) {

    portions <- Dynamic_Model(Tvec, summ=FALSE)

    if (sum) {
        portions <- sum(portions)
    }

    return(portions)
}


