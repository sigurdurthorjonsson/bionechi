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
function(x, scaleTo = 1e9) {
  bootMean <- mean(x)
  names(bootMean) <- "mean"
  quasiCV <- sqrt(var(x))/bootMean
  names(quasiCV) <- "CV"
  bootQuants <- quantile(x, c(0.05, 0.25, 0.5, 0.75, 0.95))
  c(bootMean/scaleTo, quasiCV, bootQuants/scaleTo)
}
