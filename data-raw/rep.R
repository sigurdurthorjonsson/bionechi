filename <- "data-row/2016_A01_nasc3.txt"
rep <- read.table(filename, 
  header = FALSE, sep = ",", as.is = TRUE, skip = 1)
names(rep) <- c("ship", "nat", "surv", "freq", "tr", 
  "date", "time", "logstart", "logstop", "lon", "lat", "depth", 
  "cap", "tot")
id <- which(rep$logstart > 1705)
rep <- rep[id,]
saveRDS(rep, "data/rep.rds")

