#' phenoclim: a package for phenological analysis and prediction
#'
#' @docType package
#' @name phenoclim
#'
#' @importFrom methods setClass setGeneric setMethod new validObject
#' @importFrom plyr ldply
#' @importFrom reshape2 melt
#' @importFrom DEoptim DEoptim DEoptim.control
#' @importFrom parallel mclapply
#' @importFrom stats fitted formula lm predict coef
#' @importFrom dismo kfold
#' @importFrom lubridate leap_year interval %within% days is.period is.POSIXct is.Date duration
#' @importFrom chillR Dynamic_Model
NULL
