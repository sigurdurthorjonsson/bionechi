r2area <- read.csv("data-raw/bb_reitir3.csv")
r2area$lat <- (r2area$lat1 + r2area$lat3)/2
r2area$lon <- (r2area$lon1 + r2area$lon3)/2
r2area$r <- d2sr(r2area)
r2area <- r2area[ , c("r", "reg")]
names(r2area)[2] <- "area"

