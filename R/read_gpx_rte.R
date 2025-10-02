#' Read routepoints from gpx-file
#'
#' Read routepoints from gpx-file, with sf::st_read and sf_st_coordiantes. 
#' In the context of capelin acoustic estimation used for reading in a perimeter 
#' of the capelin distribution area..
#'
#' @param gp_fn Path and file name of gpx-file
#' @export

read_gpx_rte <- function(gpx_fn) {

gpx_fn |>
  sf::st_read(layer="route_points",
    quiet=TRUE) |>
  sf::st_coordinates() |>
  tibble::as_tibble() |>
  dplyr::select(lat=Y,lon=X) -> tbl
  tbl |> 
    bind_rows(tbl[1,])
}
