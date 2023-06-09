% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/thermal.R
\name{Forms}
\alias{Forms}
\alias{flat}
\alias{asymcur}
\alias{trapezoid}
\alias{linear}
\alias{triangle}
\alias{gdd}
\alias{gddsimple}
\alias{chillbasic}
\alias{utah_original}
\alias{utah}
\alias{chillPortions}
\title{Functional forms for thermal time calculation}
\usage{
flat(Tvec, Tb, To, sum = TRUE)

asymcur(Tvec, Tb, To, Tc, sum = TRUE)

trapezoid(Tvec, Tb, To, Ts, Tc, sum = TRUE)

linear(Tvec, Tb, sum = TRUE)

triangle(Tvec, Tb, To, Tc, sum = TRUE)

gdd(tdat, Tb, sum = TRUE)

gddsimple(tdat, Tb, sum = TRUE)

chillbasic(Tvec, Tmax, sum = TRUE)

utah_original(Tvec, sum = TRUE)

utah(Tvec, sum = TRUE)

chillPortions(Tvec, sum = TRUE)
}
\arguments{
\item{Tvec}{A vector of hourly temperatures, in degrees C.}

\item{Tb}{The base cardinal temperature of the model.}

\item{To}{The optimal cardinal temperature of the model.}

\item{sum}{Logical, should the vector of thermal times be returned as a
sum?}

\item{Tc}{The critical cardinal temperature of the model.}

\item{Ts}{The subcritical cardinal temperature of the model.}

\item{tdat}{A matrix of minimum and maximum daily temperatures, in degrees
C.}

\item{Tmax}{numeric, the maximum temperature where trees accumulate chill.}
}
\value{
A vector of numbers representing the chill or heat calculated.
}
\description{
These functions calculate thermal time (chill and heat) for a
    given vector or matrix of temperatures in degrees C.
}
\details{
The functional form of the flat thermal time model is as follows:
    \deqn{GDH = \begin{cases}
             0 & T\leq T_b \\
             T - T_b & T_b\leq T\leq T_o \\
             T_o & T_o\leq T
             \end{cases}}
    where \eqn{T} is the hourly temperature, \eqn{T_b} is the base
    temperature and \eqn{T_o} is the optimal temperature.

The functional form of the Anderson thermal time model (Anderson
    \emph{et al.} 1986) is as follows:
    \deqn{ GDH = \begin{cases}
        0 & T\leq T_b \\
        \frac{T_o-T_a}{2} \left[1+\cos\left(\pi + \pi \cdot \frac{T-T_b}{T_o-T_b}\right) \right] & T_b\leq T\leq T_o \\
        (T_o-T_a) \left[1+\cos\left(\frac{\pi}{2} + \frac{\pi}{2} \cdot \frac{T-T_o}{T_c-T_o}\right) \right] & T_o\leq T \leq T_c \\
        0 & T_c \leq T
        \end{cases}}
    where \eqn{T} is the hourly temperature, \eqn{T_b} is the base
    temperature, \eqn{T_o} is the optimal temperature, and \eqn{T_c} is
    the critical temperature.

     Anderson, J. L., E. A. Richardson, and C. D. Kesner. 1985.
         "Validation of Chill Unit and Flower Bud Phenology Models
         for 'Montmorency' sour Cherry." In I International Symposium on
         Computer Modelling in Fruit Research and Orchard Management 184,
         71-78. http://www.actahort.org/books/184/184_7.htm.

The functional form of the trapezoid thermal time model is as
    follows:
    \deqn{ GDH = \begin{cases}
        0 & T\leq T_b \\
        T - T_b & T_b \leq T \leq T_o \\
        T_o - T_b & T_o\leq T \leq T_s \\
        T*(Tb-To)/(Tc-Ts) + Tc*(To-Tb)/(To) & T_s\leq T \leq T_c \\
        0 & T_c \leq T
        \end{cases}}
    where \eqn{T} is the hourly temperature, \eqn{T_b} is the base
    temperature, \eqn{T_o} is the optimal temperature, and \eqn{T_c} is
    the critical temperature.

The functional form of the flat thermal time model is as follows:
    \deqn{GDH = \begin{cases}
    0 & T\leq T_b \\
    T - T_b & T_b \leq T
    \end{cases}}
    where \eqn{T} is the hourly temperature, \eqn{T_b} is the base
    temperature and \eqn{T_o} is the optimal temperature.

The functional form of the trianble thermal time model is as
    follows:
    \deqn{ GDH = \begin{cases}
        0 & T\leq T_b \\
        T - T_b & T_b \leq T \leq T_o \\
        \frac{(T_c - T) (T_o - T_b)}{T_c - T_o} & T_o\leq T \leq T_c \\
        0 & T_c \leq T
        \end{cases}}
    where \eqn{T} is the hourly temperature, \eqn{T_b} is the base
    temperature, \eqn{T_o} is the optimal temperature, and \eqn{T_c} is
    the critical temperature.

The functional form of the flat thermal time model is as follows:
    \deqn{GDD = \begin{cases}
        0 & T_{max}\leq T_b \\
        \frac{(T_{max}-T_b)^2}{2(T_{max} - T_{min})} & T_{min} \leq T_b \leq T_{max}\\
        T - T_{avg} & T_{min} \geq T_b\\
        \end{cases}}
    where \eqn{T_min} is the daily minimum temperature, \eqn{T_max} is the
    daily maximum temperature, and \eqn{T_b} is the base temperature.

The functional form of the simple gdd thermal time model is as follows:
    \deqn{GDD = \begin{cases}
        0 & T_{avg}\leq T_b \\
        T - T_{avg} & T_b \leq T_{avg}\\
        \end{cases}}
    where \eqn{T_{avg}} is the average daily temperature, and \eqn{T_b} is
    the base temperature.

This function calculates the amount of chill accumulated based on the 32F-45F (0C-7.2C) model. The model is described in Luedeling, E. and Brown,
    P.H., 2011. A global analysis of the comparability of winter chill
    models for fruit and nut trees. International Journal of Biometeorology,
     55(3), pp.411-421.

This model is described in Luedeling, E. and Brown, P.H., 2011. A
    global analysis of the comparability of winter chill models for fruit
    and nut trees. International Journal of Biometeorology, 55(3),
    pp.411-421.

The utah() function is a smoothed version of the model described
    at \url{http://fruitsandnuts.ucdavis.edu/Weather_Services/chilling_accumulation_models/about_chilling_units/}. This was done to ensure optimization
    procedures using this model would converge.

The chillPortions model is described in at \url{http://fruitsandnuts.ucdavis.edu/Weather_Services/chilling_accumulation_models/about_chilling_units/}
}
\examples{
temp <- seq(-5, 50)
gdh <- flat(temp, 4, 25, sum=FALSE)
plot(gdh ~ temp)
gdh <- asymcur(temp, 4, 25, 36, sum=FALSE)
plot(gdh ~ temp)
gdh <- trapezoid(temp, 4, 25, 36, 40, sum=FALSE)
plot(gdh ~ temp)
gdh <- linear(temp, 4, sum=FALSE)
plot(gdh ~ temp)
gdh <- triangle(temp, 4, 25, 36, sum=FALSE)
plot(gdh ~ temp)
temp <- data.frame(tmin=runif(10, 0, 10), tmax=runif(10, 13, 25))
gdd(temp, 4, sum=TRUE)
gdd(temp, 4, sum=FALSE)
gddsimple(temp, 4, sum=TRUE)
gddsimple(temp, 4, sum=FALSE)
}
