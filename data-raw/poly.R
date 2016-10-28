library(plotKML)
poly <- readGPX("data-raw/boundary2.gpx")
poly <- poly$routes$boundary2
poly <- rbind(poly, poly[1,])
saveRDS(poly, file = "data/poly.rds")
