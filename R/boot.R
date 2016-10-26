boot_cvEA <-
function(x, nBoots = 1000) {
  replicates <- sapply(1:nBoots, 
    function(y, x) sum(sample(x, replace = TRUE)), x = x)
  list(obs = sum(x), 
    med = median(replicates), 
    mean = mean(replicates), 
    low = quantile(replicates, .05), 
    hi = quantile(replicates, .95), 
    cv = sqrt(var(replicates))/mean(replicates))
}
#' Compute summary stats of a bootstrap vector
#'
#' @name boot_stats
#' @param x A vector, e.g. of bootstrap replicates
#' @return A data frame with mean, CV and selected percentiles.
#' @keywords arith
#' @examples
#' 
#'   x <- sample(1:10)
#'   boot_stats(x)
#' 

#' @export boot_stats
#' @rdname boot_stats
boot_stats <-
function (x) 
{
    bootMean <- mean(x)
    names(bootMean) <- "mean"
    quasiCV <- sqrt(var(x))/bootMean
    names(quasiCV) <- "CV"
    bootQuants <- quantile(x, c(0.05, 0.25, 0.5, 0.75, 0.95))
    setNames(data.frame(t(c(bootMean, quasiCV, bootQuants))),
      c("mean", "CV", paste0(rep("pt", 5), c(5, 25, 50, 75, 95))))
}

#' A rough gauge of the combination of repeat coverages
#'
#' @name pool_estimates
#' @param estimates,CVs Vectors of means and CVs in repeat coverages. 
#' @return Vector of estimated mean and CV of coverages combined.
#' @keywords arith
#' @examples
#' 
#'   ests <- c(570, 720)
#'   CVs <- c(0.2, 0.2)
#'   pool_estimates(ests, CVs)
#' 

#' @export pool_estimates
#'@rdname pool_estimates
pool_estimates <- function(estimates, CVs) {
  if(length(estimates) != length(CVs)) 
    stop("Equal number of CVs and means required")
  mju <- mean(estimates)
  sigmas <- CVs*estimates
  sigma <- sqrt(sum(sigmas^2))/length(CVs)
  c(mean = mju, CV = sigma/mju)
}
