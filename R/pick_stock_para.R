#' Aggregate numbers and biomass of selected stock components
#'
#' @name pick_stock_para
#' @param df A data frame/tibble with numbers and biomass by:
#'
#'   \itemize{
#'     \item Area, \code{area}, 
#'     \item Length, \code{l}, 
#'     \item Age, \code{a} and
#'     \item Maturity, \code{m}. 
#'   }
#' grouped by area.
#' @return A data frame/tibble of the selected parameters as columns by area.
#' @keywords arith
#' 

#' @export pick_stock_para
#' @rdname pick_stock_para
pick_stock_para <- function(df) {
  df %>%
    summarize(N = sum(nFish),
    B = sum(bFish)) -> num_bio
  df %>%
    filter(m == 0) %>%
    summarize(immN = sum(nFish),
      immB = sum(bFish)) %>%
    right_join(num_bio, by = "area") -> num_bio
  df %>%
    filter(m == 0 & a == 1) %>%
    summarize(immN1 = sum(nFish),
      immB1 = sum(bFish)) %>%
    right_join(num_bio, by = "area") -> num_bio
  df %>%
    filter(m == 0 & a == 2) %>%
    summarize(immN2 = sum(nFish),
      immB2 = sum(bFish)) %>%
    right_join(num_bio, by = "area") -> num_bio
  df %>%
    filter(m == 1) %>%
    summarize(SSN = sum(nFish),
      SSB = sum(bFish)) %>%
    right_join(num_bio, by = "area") -> num_bio
  df %>%
    filter(m == 1 & a == 2) %>%
    summarize(SSN2 = sum(nFish),
      SSB2 = sum(bFish)) %>%
    right_join(num_bio, by = "area") -> num_bio
  df %>%
    filter(m == 1 & a == 3) %>%
    summarize(SSN3 = sum(nFish),
      SSB3 = sum(bFish)) %>%
    right_join(num_bio, by = "area") -> num_bio
  df %>%
    filter(m == 1 & a == 4) %>%
    summarize(SSN4 = sum(nFish),
      SSB4 = sum(bFish)) %>%
    right_join(num_bio, by = "area")
}
