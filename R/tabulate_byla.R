#' @title Tabulate abundance by length and age with additional info on the margin
#'
#' @description
#' Tabulate abundance by length and age with additional marginal info consisting of:
#' \itemize{
#'   \item Marginal sums of abundance and biomass for length categories and age groups
#'   \item Mean length at age and mean weight at length and age
#'   \item Percentage age distribution
#' }
#'
#' @return
#' List of matrices of abundance by length and age hybridized with other info
#' on the margin, for the stock combined (\code{all}) and immature (\code{imm}) 
#' and mature (\code{mat}) parts of the stock.
#'
#' @details
#' Table traditionally included in the reports of the Northern Pelagic 
#' and Blue Whiting and the North Western Working Groups of ICES 
#' as well as in survey reports.
#'
#' @examples
#' data(fishData)
#' tabulate_byla(fishData)
#'
#' @param fishData data.frame of numbers and biomass by length and age
#' @param nScale, bScale scaling factor, default one million
tabulate_byla <-
function(fishData, nScale = 1e6, bScale = nScale) {
# scale to millions and tonnes
  fishData$nFish <- fishData$nFish/nScale
  fishData$bFish <- fishData$bFish/bScale
# all fish
  Nla <- aggregate(nFish ~ l + a, fishData, sum)
  Bla <- aggregate(bFish ~ l + a, fishData, sum)
  laMat <- tapply(Nla$nFish, list(Nla$l, Nla$a), sum)
  Nl <- aggregate(nFish ~ l, Nla, sum)
  Bl <- aggregate(bFish ~ l, Bla, sum)
  mw <- Bl$bFish/Nl$nFish
  all <- cbind(laMat, Nl$nFish, Bl$bFish, mw)
  dimnames(all)[[2]][(ncol(all) - 2):ncol(all)] <- 
    c("Total N", "Total B", "Mean weight (g)")
  Na <- aggregate(nFish ~ a, Nla, sum)
  pa <- Na$nFish/sum(Na$nFish)
  Ba <- aggregate(bFish ~ a, Bla, sum)
  mw <- Ba$bFish/Na$nFish
  ml <- tapply(Nla$l*Nla$nFish, Nla$a, sum)/tapply(Nla$nFish, Nla$a, sum)
  all <- rbind(all, c(Na$nFish, NA, NA, NA), c(100*pa, NA, NA, NA),
    c(Ba$bFish, NA, NA, NA), c(mw, NA, NA, NA), c(ml, NA, NA, NA))
  all[nrow(all) - 4, ncol(all) - 2] <- sum(Nl$nFish)
  all[nrow(all) - 4, ncol(all) - 1] <- sum(Bl$bFish)
  all[nrow(all) - 1, ncol(all) - 2] <- sum(Bl$bFish)/sum(Nl$nFish)
  all[nrow(all), ncol(all) - 2] <- sum(Nl$nFish*Nl$l)/sum(Nl$nFish)
  dimnames(all)[[1]][(nrow(all)-4):nrow(all)] <- 
    c("Total N", "Percentage (%)", "Total B", "Mean weight (g)", "Mean length (cm)")

# immature fish
  Nla <- aggregate(nFish ~ l + a, subset = m == 0, fishData, sum)
  Bla <- aggregate(bFish ~ l + a, subset = m == 0, fishData, sum)
  laMat <- tapply(Nla$nFish, list(Nla$l, Nla$a), sum)
  Nl <- aggregate(nFish ~ l, Nla, sum)
  Bl <- aggregate(bFish ~ l, Bla, sum)
  mw <- Bl$bFish/Nl$nFish
  imm <- cbind(laMat, Nl$nFish, Bl$bFish, mw)
  dimnames(imm)[[2]][(ncol(imm) - 2):ncol(imm)] <- 
    c("Total N", "Total B", "Mean weight (g)")
  Na <- aggregate(nFish ~ a, Nla, sum)
  pa <- Na$nFish/sum(Na$nFish)
  Ba <- aggregate(bFish ~ a, Bla, sum)
  mw <- Ba$bFish/Na$nFish
  ml <- tapply(Nla$l*Nla$nFish, Nla$a, sum)/tapply(Nla$nFish, Nla$a, sum)
  imm <- rbind(imm, c(Na$nFish, NA, NA, NA), c(100*pa, NA, NA, NA),
    c(Ba$bFish, NA, NA, NA), c(mw, NA, NA, NA), c(ml, NA, NA, NA))
  imm[nrow(imm) - 4, ncol(imm) - 2] <- sum(Nl$nFish)
  imm[nrow(imm) - 4, ncol(imm) - 1] <- sum(Bl$bFish)
  imm[nrow(imm) - 1, ncol(imm) - 2] <- sum(Bl$bFish)/sum(Nl$nFish)
  imm[nrow(imm), ncol(imm) - 2] <- sum(Nl$nFish*Nl$l)/sum(Nl$nFish)
  dimnames(imm)[[1]][(nrow(imm)-4):nrow(imm)] <- 
    c("Total N", "Percentage (%)", "Total B", "Mean weight (g)", "Mean length (cm)")

# mature fish
  Nla <- aggregate(nFish ~ l + a, subset = m == 1, fishData, sum)
  Bla <- aggregate(bFish ~ l + a, subset = m == 1, fishData, sum)
  laMat <- tapply(Nla$nFish, list(Nla$l, Nla$a), sum)
  Nl <- aggregate(nFish ~ l, Nla, sum)
  Bl <- aggregate(bFish ~ l, Bla, sum)
  mw <- Bl$bFish/Nl$nFish
  mat <- cbind(laMat, Nl$nFish, Bl$bFish, mw)
  dimnames(mat)[[2]][(ncol(mat) - 2):ncol(mat)] <- 
    c("Total N", "Total B", "Mean weight (g)")
  Na <- aggregate(nFish ~ a, Nla, sum)
  pa <- Na$nFish/sum(Na$nFish)
  Ba <- aggregate(bFish ~ a, Bla, sum)
  mw <- Ba$bFish/Na$nFish
  ml <- tapply(Nla$l*Nla$nFish, Nla$a, sum)/tapply(Nla$nFish, Nla$a, sum)
  mat <- rbind(mat, c(Na$nFish, NA, NA, NA), c(100*pa, NA, NA, NA),
    c(Ba$bFish, NA, NA, NA), c(mw, NA, NA, NA), c(ml, NA, NA, NA))
  mat[nrow(mat) - 4, ncol(mat) - 2] <- sum(Nl$nFish)
  mat[nrow(mat) - 4, ncol(mat) - 1] <- sum(Bl$bFish)
  mat[nrow(mat) - 1, ncol(mat) - 2] <- sum(Bl$bFish)/sum(Nl$nFish)
  mat[nrow(mat), ncol(mat) - 2] <- sum(Nl$nFish*Nl$l)/sum(Nl$nFish)
  dimnames(mat)[[1]][(nrow(mat)-4):nrow(mat)] <- 
    c("Total N", "Percentage (%)", "Total B", "Mean weight (g)", "Mean length (cm)")

  list(mat = mat, imm = imm, all = all)
}
