dist_mat <-
function(lat, lon, lat1 = NULL, lon1 = NULL)
{
  if (is.null(lat1)) {
    lat1 <- lon$lat
    lon1 <- lon$lon
    lon <- lat$lon
    lat <- lat$lat
  }
  sapply(1:length(lat), 
    function(z) arcdist(lat[z], lon[z], lat1, lon1))
}

geoneighbourID <-
function(lat, lon, lat1 = NULL, lon1 = NULL)
{
  if(is.null(lat1)) {
    lat1 <- lon$lat
    lon1 <- lon$lon
    lon <- lat$lon
    lat <- lat$lat
  }
  z <- dist_mat(lat, lon, lat1, lon1)
  apply(z, 2, which.min)
}

nearest_neighbour_distance <-
function(lat, lon, lat1 = NULL, lon1 = NULL)
{
  if(is.null(lat1)) {
    lat1 <- lon$lat
    lon1 <- lon$lon
    lon <- lat$lon
    lat <- lat$lat
  }
  z <- dist_mat(lat, lon, lat1, lon1)
  apply(z, 2, min)
}


grid_route <-
function(lat, lon) {
  nlat <- length(lat)
  nlon <- length(lon)
  
  latRange <- range(lat)
  lonRange <- range(lon)
  
  wLat <- lat
  wLon <- lonRange[2 - (1:length(lat))%%2]
  eLat <- lat
  eLon <- lonRange[(1:length(lat))%%2 + 1]

  WE <- data.frame(lat = as.vector(t(cbind(wLat, eLat))), 
    lon = as.vector(t(cbind(wLon, eLon))))

  if(nlat%%2 == 1) {
    nLat <- latRange[(1:length(lon))%%2 + 1]
    nLon <- rev(lon)
    sLat <- latRange[2 - (1:length(lon))%%2]
    sLon <- rev(lon)

    NS <- data.frame(lat = as.vector(t(cbind(nLat, sLat))), 
      lon = as.vector(t(cbind(nLon, sLon))))
    route <- data.frame(rbind(WE, NS))
  } 
  else {
    nLat <- latRange[(1:length(lon))%%2 + 1]
    nLon <- lon
    sLat <- latRange[2 - (1:length(lon))%%2]
    sLon <- lon

    SN <- cbind(nLat, nLon, sLat, sLon)
    route <- data.frame(rbind(matrix(WE, ncol = 2, byrow = TRUE), 
      matrix(SN, ncol = 2, byrow = TRUE)))
  }
  route
}

sq_bar_plot <-
function(mat)
{
        maxy <- max(mat)
        sq <- as.numeric(dimnames(mat)[[1]])
        for(i in seq(along = sq))
                geosubplot(barplot(mat[i,  ], ylim = c(0, maxy), axes = F),
                        pos = sq.corners(sq[i]))
}

sq_corners <-
function(square)
{
        lat <- r2d(square)
        lat$lat <- c(lat$lat + 0.25, lat$lat - 0.25)
        lat$lon <- c(lat$lon - 0.5, lat$lon + 0.5)
        lat
}

sqM_plot <-
function(text, sqdata, sqtype, dlat = 5, dlon = 10, ...)
{
        lat <- switch(sqtype,
                minsq = minsq2d(sqdata$sq, dlat = dlat, dlon = dlon),
                hexsq = hexsq2d(sqdata$sq),
                ssq = sr2d(sqdata$sq),
                sq = r2d(sqdata$sq))
        geoplot(lat, grid = F, pch = " ")
        switch(sqtype,
                minsq = mingrid(sqdata$sq, dlat = dlat, dlon = dlon),
                hexsq = hexgrid(sqdata$sq),
                ssq = ssqgrid(sqdata$sq),
                sq = sqgrid(sqdata$sq))
        geotext(lat$lat, lat$lon, sqdata$M, ...)
        mtext(paste("Average Sa-values -", text))
        return()
}
