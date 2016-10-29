#' Given rect code, find area or proption rect area within a perimeter.
#' 
#' @name rect_within_peri
#' @aliases r_within_peri sr_within_peri mr_within_peri dr_within_peri
#' @param r,sr,mr,dr Rectangle codes for different types of rects.
#' @param peri Perimeter e.g. demarcating geographical distribution of fish.
#' @param valueOut If \code{"p"} (the default) return proportion, 
#'   else if \code{"A"} return area.
#' @param scale If \code{valueOut} is \code{"area"}, 
#'   \code{"nmi"} returns area in square nautical miles, 
#'   otherwise \code{"km"} in square kilometers.
#' 
#' @note Not yet robust against rectangles outside the given perimeter. 
#'   Destined for package \code{geo}?
#' @seealso \code{\link{deg2rect}}, \code{\link{rectArea}},
#' \code{\link{geoarea}}.
#' @keywords spatial

#' @export r_within_peri
#' @rdname rect_within_peri

r_within_peri <- function(r, peri, valueOut = "p", scale = "nmi") {
  if (!(scale == "nmi" | scale == "km")) 
    stop("Unit square nautical miles or kilometers only")
  fullArea <- rA(r, scale = "km")
  partialArea <- sapply(r, function(x) 
    geoarea(geointersect(rPeri(x), peri, in.or.out = 0)))
  if(scale == "nmi" & valueOut == "A") {
    fullArea <- fullArea/1.852^2
    partialArea <- partialArea/1.852^2
  }
  switch(valueOut, 
    p = partialArea/fullArea,
    A = partialArea)
}

#' @export sr_within_peri
#' @rdname rect_within_peri

sr_within_peri <- function(sr, peri, valueOut = "p", scale = "nmi") {
  if (!(scale == "nmi" | scale == "km")) 
    stop("Unit square nautical miles or kilometers only")
  fullArea <- srA(sr, scale = "km")
  partialArea <- sapply(sr, function(x) 
    geoarea(geointersect(srPeri(x), peri, in.or.out = 0)))
  if(scale == "nmi" & valueOut == "A") {
    fullArea <- fullArea/1.852^2
    partialArea <- partialArea/1.852^2
  }
  switch(valueOut, 
    p = partialArea/fullArea,
    A = partialArea)
}

#' @export mr_within_peri
#' @rdname rect_within_peri

mr_within_peri <- function(mr, peri, dlat = 5, dlon = 10, 
    valueOut = "p", scale = "nmi") {
  if (!(scale == "nmi" | scale == "km")) 
    stop("Unit square nautical miles or kilometers only")
  fullArea <- mrA(mr, dlat = dlat, dlon = dlon, scale = "km")
  partialArea <- sapply(mr, function(x) 
    geoarea(geointersect(mrPeri(x, dlat = dlat, dlon = dlon), 
      peri, in.or.out = 0)))
  if(scale == "nmi" & valueOut == "A") {
    fullArea <- fullArea/1.852^2
    partialArea <- partialArea/1.852^2
  }
  switch(valueOut, 
    p = partialArea/fullArea,
    A = partialArea)
}

#' @export dr_within_peri
#' @rdname rect_within_peri

dr_within_peri <- function(dr, peri, dlat = 1, dlon = 2, 
    valueOut = "p", scale = "km") {
  if (!(scale == "nmi" | scale == "km")) 
    stop("Unit square nautical miles or kilometers only")
  fullArea <- drA(dr, dlat = 1, dlon = 2)
  partialArea <- sapply(dr, function(x) 
    geoarea(geointersect(drPeri(x, dlat = 1, dlon = 2), peri, in.or.out = 0)))
  if(scale == "nmi" & valueOut == "A") {
    fullArea <- fullArea/1.852^2
    partialArea <- partialArea/1.852^2
  }
  switch(valueOut, 
    p = partialArea/fullArea,
    A = partialArea)
}
