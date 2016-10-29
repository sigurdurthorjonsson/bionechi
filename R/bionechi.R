#' bionechi
#'
#' A collection of functions having to do with acoustics and
#' estimation of numbers and biomass and presentation of results
#' from acoustic surveys as performed at the MRI, Reykjvík, Iceland.
#'
#' @name bionechi
#' @docType package
#' @import geo
#' @import plotKML
NULL

#' Biological variables of 3300 capelin.
#'
#' A dataset containing the biological variables and other attributes 
#' of almost 3300 capelin along with the part of total numbers and 
#' biomass estimates each sampled fish represents.
#' The variables are as follows:
#'
#' \itemize{
#'   \item \code{stod} station number
#'   \item \code{sid} sample id
#'   \item \code{teg} species code, 31 is capelin
#'   \item \code{nr} number of fish in sample of upto 100 fish
#'   \item \code{l} length (cm) to the next cm below
#'   \item \code{w} weight (g), rounded to the closest 0.1 g
#'   \item \code{s} sex, 1 male, 2 female
#'   \item \code{a} age in years
#'   \item \code{m} maturity, 0 immature, 1 mature
#'   \item \code{TS} target strength (dB re 1 m^2)
#'   \item \code{sigma} acoustic cross section (m^2)
#'   \item \code{area} area id
#'   \item \code{nFish} number of fish in total the sampled fish represents
#'   \item \code{bFish} biomass (in g) of fish in total the sampled fish represents
#' }
#'
#' @docType data
#' @keywords datasets
#' @name fishData
#' @usage data(fishData)
#' @format A data frame with 3297 rows and 14 variables
#' @source MRI, to be made available at \url{http://data.hafro.is/acoustics}
NULL

#' NASC in 0.1 nmi bins/EDSUs.
#'
#' A dataset containing the survey track and acoustic density attributed to capelin
#' in 0.1 nautical mile (nmi) bins.
#' 
#' 
#'
#' \itemize{
#'   \item \code{ship,nat,surv} Ship, nation and survey as IDed in LSSS
#'   \item \code{date,time} Start date and time of EDSU in some format
#'   \item \code{logstart,logstop} Log distance at beginning and end of EDSU/bin
#'   \item \code{lon,lat} postion in decimal degrees longitude and latitude
#'   \item \code{depth} depth (md
#'   \item \code{sa} NASC (m^2/nmi^2)
#' }
#'
#' @docType data
#' @keywords datasets
#' @name rep
#' @usage data(rep)
#' @format A data frame with 34352 rows and 11 variables
#' @source MRI, to be made available at \url{http://data.hafro.is/acoustics}
NULL
