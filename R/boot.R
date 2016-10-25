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

pool_estimates <- function(estimates, CVs) {
  if(length(estimates) != length(CVs)) 
    stop("Equal number of CVs and means required")
  mju <- mean(estimates)
  sigmas <- CVs*estimates
  sigma <- sqrt(sum(sigmas^2))/length(CVs)
  result <- c(mean = mju, CV = sigma/mju)
  result
}
  
  
