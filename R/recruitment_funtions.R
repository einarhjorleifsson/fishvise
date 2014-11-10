#' @title ricker_srmsymc
#' 
#' @export
#' 
#' @param ssb spawning stock biomass
#' @param alpha alpha
#' @param beta beta
ricker_srmsymc <- function(ssb,alpha,beta) {alpha * ssb*exp(-beta * ssb)}

#' @title bevholt_srmsymc
#' 
#' @export
#' 
#' @param ssb spawning stock biomass
#' @param alpha alpha
#' @param beta beta
#' 
bevholt_srmsymc <- function(ssb,alpha,beta) {alpha * ssb /(beta + ssb)}

#' @title segreg_srmsymc
#' 
#' @export
#' 
#' @param ssb spawning stock biomass
#' @param alpha alpha
#' @param beta beta
segreg_srmsymc <- function(ssb,alpha,beta) {alpha*(ssb+sqrt(beta^2+0.001)-sqrt((ssb-beta)^2+0.001))}

