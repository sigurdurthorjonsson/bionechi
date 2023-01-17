#' Read ListUserFile16
#'
#' Read LSSS-report ListUserFile16 giving 3D (layered) NASC by species.
#'
#' @param rep_file Path and filename of ListUserFile16
#' @export

read_luf16 <- function(rep_file) {
  options(readr.num_columns = 0)
  rep_file %>%
    readr::read_table() %>%
    dplyr::select_all(tolower) %>%
    dplyr::rename(lon = longitud, 
      lat = latitude,
      logstart=log1,
      logstop=log2) %>%
    tidyr::unite(date,year,mo,da,sep="-") %>%
    tidyr::unite(date_time, date, utc, sep = " ") %>%
    dplyr::mutate(date_time = lubridate::ymd_hms(date_time))
}

