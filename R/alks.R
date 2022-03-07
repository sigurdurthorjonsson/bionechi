#' Make age-length key (alk)
#'
#' Given fish data with age and length, tablulate and prepare an age-length key
#'
#' @param alk_data Data frame with biological data from a set
#'   of trawl samples to be used as basis for the age-length key.
#' @details First implementation bin-ns the lengths on the centimeter below.
#' @keywords alk
#' @return An alk-tibble
#' @export
#' @examples
#' \dontrun{fishData %>%
#'   filter(!is.na(a)) -> alk_data
#' alk <- make_alk(alk_data)
#'   }

make_alk <- function(alk_data) {
  alk_data %>%
    dplyr::group_by(l,a) %>%   
    dplyr::summarize(n=n()) %>%
    dplyr::right_join(alk_data %>%
      dplyr::group_by(l) %>%
      dplyr::summarize(n=n()),by="l") %>%
    dplyr::mutate(p=n.x/n.y) %>%
    dplyr::group_by(l) %>%
    dplyr::mutate(cump=cumsum(p))
}

#' Predict age
#'
#' Given fish length, predict age from empirical age group propotions.
#'
#' @param l Fish length
#' @param alk An alk-tibble with cum age group props by length
#' @details For a single fish at a time, used inside a 'purrr::map_dbl'-call
#' @keywords alk
#' @return Stochastic age
#' @export
#' @examples
#' \dontrun{fishData %>%
#'   filter(!is.na(a)) -> alk_data
#' alk <- make_alk(alk_data)
#' set.sed(2345)
#' predict_age(15,alk)
#' predict_age(15,alk)
#' set.seed(2345)
#' predict_age(15,alk)
#' purrr::map_dbl(9:19,predict_age,alk=alk)
#'   }

predict_age <- function(l,alk) {
  draw <- runif(1)
  if(l>max(alk$l)) l <- max(alk$l)
  if(l<min(alk$l)) l <- min(alk$l)
  tibble(l=l) %>%
    dplyr::left_join(alk,by="l") %>%
    dplyr::filter(cump > draw) %>%
    dplyr::pull(a) %>%
    first() 
}

#' Fix missing age.
#'
#' Given a set of observations of fish length and age, some ages missing,
#' predict the missing once from observed age group proportions by length.
#'
#' @param fishData A data frame of observations of fish in otolith sample
#'   with columns 'l' for length and 'a' for age.
#' @keywords age, length
#' @export
#' @examples
#' \dontrun{
#'   fishData <- fix_missing_age(fishData)
#'   }

fix_missing_age <- function(fishData) {
  if(!any(is.na(fishData$a))) return(fishData)
  fishData <- split(fishData, is.na(fishData$a))
  fishData[[1]] %>%
    dplyr::mutate(l = floor(l)) -> alk_data
  alk <- make_alk(alk_data)
  fishData[[2]] %>%
    dplyr::mutate(a = purrr::map_dbl(floor(l),
      predict_age, alk = alk)) -> fishData[[2]]
  bind_rows(fishData)
}



