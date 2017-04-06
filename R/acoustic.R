#' Acoustic calculations
#' 
#' @description 
#' \itemize{
#'   \item \code{le2ts}: Convert length to target strength
#'   \item \code{ts2sigma}: Convert target strength to cross-section
#'   \item \code{ts2le}: Convert target strength to length
#'   \item \code{sigma2ts}: convert cross-section to target strength.
#'   \item \code{sA2sa}: Convert NASC (sA) to area backscattering coefficient (sa or ABC)
#'   \item \code{sV2sv}: Convert mean volum backscattering strength (MVBS) to volume backscattering coefficent
#' }
#' @param le length (cm)
#' @param species MRI species code (31:capelin, 30:herring, etc)
#' @param dead small value added to \code{le} related to measurement method
#' @param ts target strength (dB re 1 m^2)
#' @param type type of cross-section, \code{sp} spherical, \code{bs} backscattering
#' @param sigma cross-section (m^2)
#' @param z depth (m)
#' @param sA 
#' @param Sv
#' 
#' @return
#' \itemize{
#'   \item \code{le2ts}:
#'   \item \code{ts2sigma}:
#'   \item \code{ts2le}:
#'   \item \code{sigma2ts}:
#'   \item \code{sA2sa}:
#'   \item \code{sV2sv}:
#' }
#'
#' @examples
#' # fish length distribution, rounded to the next cm below:
#' lengths <- floor(rnorm(100, 15, 2))
#' # cross-section of capelin
#' sigmas <- ts2sigma(le2ts(lengths, 31, dead = 0.5))
#' # nautical area scattering coefficient ('back-scattering area density'):
#' NASC <- 10000
#' # area density of fish in millions per nmi^2
#' NASC/mean(sigmas)/1e6
#'
#' @rdname acoustic
#' 
#' @references 
#' MacLennan, D. N., Fernandes, P. G., and Dalen, J. 2002. 
#' A consistent approach to definitions and symbols in fisheries acoustics. 
#' ICES Journal of Marine Science, 59: 365-369.
#'
#' @name acoustic
NULL

#' @rdname acoustic
#' @export le2ts
le2ts <- function(le, species, dead = 0)
{
	sp <- c(1, 2, 5, 30, 31, 34)
	a <- c(20, 20, 20, 20, 19.1, 21.8)
	b <- c(-67.5, -67.5, -71.2, -72, -74.5, -72.6)
	i <- match(species, sp)
	a[i] * log10(le + dead) + b[i]
}

#' @rdname acoustic
#' @export ts2sigma
ts2sigma <- function(ts, type = "sp")
  switch(type,
    sp = 4. * pi * 10.^(0.1 * ts),
    bs = 10.^(0.1 * ts))

#' @rdname acoustic
ts2le <- function(ts, species, dead = 0) {
	sp <- c(1, 30, 31, 34)
	a <- c(20, 20, 19.1, 21.8)
	b <- c(-67.5, -72, -74.5, -72.6)
	i <- match(species, sp)
	10^((ts - b[i])/a[i]) - dead
}

#' @rdname acoustic
sigma2ts <- function(sigma) 10 * log10(sigma/(4 * pi))

#' @rdname acoustic
z_le2ts <- function(le, z, species, dead = 0) {
  sp <- c(31)
  a <- c(23.3)
  b <- c(-74.3)
  gamma <- -4.9
  i <- match(species, sp)
  a[i] * log10(le + dead) + b[i] + gamma * log10(1 + z/10)
}

#' @rdname acoustic
sA2sa <- function(sA) sA/(4 * pi * 1852^2)

#' @rdname acoustic
sV2sv <- function(sV) 10^(0.1 * sV)

