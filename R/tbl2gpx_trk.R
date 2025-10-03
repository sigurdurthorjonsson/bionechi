#' Write table coordinates out as track in a gpx-file
#'
#' Write lat and lon in a table out as track in a gpx-file
#' using sf-utilities.
#'
#' @param tbl Table with postions given as 'lat' and 'lon'
#' @param fn Filename to write gpx to.
#'
#' @export

tbl2gpx_trk <- function(tbl,fn) {

if(!(str_split_i(fn,"\\.",-1)=="gpx"))
  stop("File name suffix should be 'gpx'")

tbl |>
  dplyr::select(X=lon,Y=lat) |>
  base::as.matrix() |>
  sf::st_linestring() |>
  sf::st_sfc(crs=4326) |>
  sf::st_write(fn,
    layer_options="FORCE_GPX_TRACK=YES",
    delete_dsn=TRUE)

}
