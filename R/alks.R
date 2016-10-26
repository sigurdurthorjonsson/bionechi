#' Greatest lower bound.
#'
#' Finds index of greatest lower bound.
#'
#' @param x Numeric.
#' @param y Numeric vector.
glb <-
function(x, y)
max((1:length(y))[y < x])

#' Least upper bound.
#'
#' Finds index of least upper bound.
#'
#' @param x Numeric.
#' @param y Numeric vector.
lub <-
function(x, y)
min((1:length(y))[y > x])

#' Split a length distribution according to an age-length-key.
#'
#' Split a length distribution according to an age-length-key. Uses utilities
#' \code{\link{lub}} and \code{\link{glb}} to establish an age distribution
#' for lengths missing from the alk.
#'
#' @param Nl Named vector giving the length distribution.
#' @param alk Named matrix age length key (length by age in spite of the name).
make_my_age <-
function(Nl, alk)
{
        alk <- sweep(alk, 1., apply(alk, 1., sum), "/")
        aged.lengths <- as.numeric(dimnames(alk)[[1.]])
        n.aged.l <- length(aged.lengths)
        obs.lengths <- as.numeric(names(Nl))
        n.obs.l <- length(obs.lengths)
        newalk <- matrix(nrow = length(obs.lengths), ncol = ncol(alk))
        dimnames(newalk) <- list(obs.lengths, dimnames(alk)[[2.]])
        for(i in seq(along = obs.lengths)) {
                x <- obs.lengths[i]
                if(is.na(match(x, aged.lengths))) {
                        if(x < min(aged.lengths))
                                newalk[i,  ] <- alk[1.,  ]
                        else {
                                if(x > max(aged.lengths))
                                        newalk[i,  ] <- alk[n.aged.l,  ]
                                else {
                                        id.a <- glb(x, aged.lengths)
                                        id.b <- lub(x, aged.lengths)
                                        a <- aged.lengths[id.a]
                                        b <- aged.lengths[id.b]
                                        newalk[i,  ] <- ((b - x)/(b - a)) *
                                                alk[id.a,  ] + ((x - a)/(b -
                                                a)) * alk[id.b,  ]
                                }
                        }
                }
                else newalk[i,  ] <- alk[match(x, aged.lengths),  ]
        }
        Nl * newalk
}
