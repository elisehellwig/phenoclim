#' @include classes.R plantmethodsbasic.R parameterlistmethods.R

#This script defines functions to create objects of the ParameterList and Plant
# classes

#' Creates ParameterList object
#'
#' @param ct A list of cardinal temperatures
#' @param length A numeric vector with the lengths of thermal time/day
#'     accumulation. There should either be one entry for each stage or one
#'     entry for all the stages.
#'
