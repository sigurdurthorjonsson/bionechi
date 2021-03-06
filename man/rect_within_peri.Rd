% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rect_within_peri.R
\name{rect_within_peri}
\alias{rect_within_peri}
\alias{r_within_peri}
\alias{sr_within_peri}
\alias{mr_within_peri}
\alias{dr_within_peri}
\title{Given rect code, find area or proption rect area within a perimeter.}
\usage{
r_within_peri(r, peri, valueOut = "p", scale = "nmi")

sr_within_peri(sr, peri, valueOut = "p", scale = "nmi")

mr_within_peri(mr, peri, dlat = 5, dlon = 10, valueOut = "p", scale = "nmi")

dr_within_peri(dr, peri, dlat = 1, dlon = 2, valueOut = "p", scale = "km")
}
\arguments{
\item{r, sr, mr, dr}{Rectangle codes for different types of rects.}

\item{peri}{Perimeter e.g. demarcating geographical distribution of fish.}

\item{valueOut}{If \code{"p"} (the default) return proportion, 
else if \code{"A"} return area.}

\item{scale}{If \code{valueOut} is \code{"area"}, 
\code{"nmi"} returns area in square nautical miles, 
otherwise \code{"km"} in square kilometers.}
}
\description{
Given rect code, find area or proption rect area within a perimeter.
}
\note{
Not yet robust against rectangles outside the given perimeter. 
  Destined for package \code{geo}?
}
\seealso{
\code{\link{deg2rect}}, \code{\link{rectArea}},
\code{\link{geoarea}}.
}
\keyword{spatial}
