r_prop_in_area <- function(r, area, valueOut = "p", scale = "nmi") {
  if (!(scale == "nmi" | scale == "km")) 
    stop("Unit square nautical miles or kilometers only")
  fullArea <- rA(r, scale = "km")
  partialArea <- sapply(r, function(x) 
    geoarea(geointersect(rPeri(x), area, in.or.out = 0)))
  if(scale == "nmi" & valueOut == "A") {
    fullArea <- fullArea/1.852^2
    partialArea <- partialArea/1.852^2
  }
  switch(valueOut, 
    p = partialArea/fullArea,
    A = partialArea)
}

sr_prop_in_area <- function(sr, area, valueOut = "p", scale = "nmi") {
  if (!(scale == "nmi" | scale == "km")) 
    stop("Unit square nautical miles or kilometers only")
  fullArea <- srA(sr, scale = "km")
  partialArea <- sapply(sr, function(x) 
    geoarea(geointersect(srPeri(x), area, in.or.out = 0)))
  if(scale == "nmi" & valueOut == "A") {
    fullArea <- fullArea/1.852^2
    partialArea <- partialArea/1.852^2
  }
  switch(valueOut, 
    p = partialArea/fullArea,
    A = partialArea)
}

mr_prop_in_area <- function(mr, area, dlat = 5, dlon = 10, 
    valueOut = "p", scale = "nmi") {
  if (!(scale == "nmi" | scale == "km")) 
    stop("Unit square nautical miles or kilometers only")
  fullArea <- mrA(mr, dlat = dlat, dlon = dlon, scale = "km")
  partialArea <- sapply(mr, function(x) 
    geoarea(geointersect(mrPeri(x, dlat = dlat, dlon = dlon), 
      area, in.or.out = 0)))
  if(scale == "nmi" & valueOut == "A") {
    fullArea <- fullArea/1.852^2
    partialArea <- partialArea/1.852^2
  }
  switch(valueOut, 
    p = partialArea/fullArea,
    A = partialArea)
}

dr_prop_in_area <- function(dr, area, dlat = 1, dlon = 2, 
    valueOut = "p", scale = "km") {
  if (!(scale == "nmi" | scale == "km")) 
    stop("Unit square nautical miles or kilometers only")
  fullArea <- drA(dr, dlat = 1, dlon = 2)
  partialArea <- sapply(dr, function(x) 
    geoarea(geointersect(drPeri(x, dlat = 1, dlon = 2), area, in.or.out = 0)))
  if(scale == "nmi" & valueOut == "A") {
    fullArea <- fullArea/1.852^2
    partialArea <- partialArea/1.852^2
  }
  switch(valueOut, 
    p = partialArea/fullArea,
    A = partialArea)
}
