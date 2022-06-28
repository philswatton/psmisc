#' Effective Number of Parties
#'
#' \code{enp} implements several measures assessing the effective
#' number of parties or similar concepts. Defaults to Laakso and
#' Taagepera's (1979) Effective Number of Parties. See details for
#' more information.
#'
#'
#'
#' @param p Numeric vector of party vote or seat shares. If \code{type} is 5 or 6, numeric vector of seat counts, see details.
#' @param type Single integer denoting which measure to use. See details.
#' @param S Single integer denoting the size of the legislature. Must be used for \code{type} 5 and 6, ignored otherwise. See details.
#' @param range Function returns minimum and maximum values along best estimate for \code{type} 5 and 6, ignored otherwise. See details.
#'
#' @return Numeric vector of length 1
#'
#' @details
#'
#' Supplying the integer corresponding to a particular measure to the type
#' argument will mean that the function uses that measure. Defaults to Laakso
#' and Taagepera's (1979) Effective Number of Parties (\code{type = 3}).
#'
#' **Type 1**
#'
#' Rae's (1968) fractionalization index. Ranges from 0 to 1. See
#' Wildgen (1971) for critique. Calculated as:
#' \deqn{1-\sum_i^M{p_i^2}}{1-sum(p^2)}
#'
#'
#' **Type 2**
#'
#' Hyperfractionalization index first used in Kesselman (1966),
#' further elucidated in Wildgen (1971). Calculated as:
#'
#' \deqn{antilog(-\sum_i^M(p_ilog(p_i))}{antilog(-sum(p*log(p)))}
#'
#'
#' **Type 3**
#'
#' Laakso and Taagepera's (1979) Effective Number of Parties. Calculated as
#' \deqn{\frac{1}{\sum_i^M{p_i^2}}}{1/sum(p^2)}
#'
#'
#' **Type 4**
#'
#' Molinar's (1991) NP index. Calculated as:
#' \deqn{1 + N\frac{\left( \sum_i^M p^2\right) -p_1^2}{\sum p^2}}{1 + N*((sum(p^2)-p1^2)/sum(p^2))}
#'
#' Where \eqn{N} is Laakso and Taagepera's (1979) Effective Number of Parties.
#'
#'
#' **Type 5**
#'
#' Taagepera's (1997) Effective Number of Parties for incomplete data.
#' Here, \code{p} should be a vector of seat counts instead of a vector of proportions.
#' It should contain only the seat counts for known parties (i.e. not decomposed
#' into other).
#'
#' \code{S} must be specified and is the total number of seats.
#'
#' This is based on Laakso and Taagepera's (1979) Effective
#' Number of Parties, using counts instead of proportions or percentages. This
#' is given by:
#'
#' \deqn{\frac{S^2}{\sum_i^M{p_i^2}}}{S^2/sum(p^2)}
#'
#' where \eqn{S} is the total number of seats. Taagepera shows that the
#' plausible range of the effective number of parties is:
#'
#' \deqn{\frac{S^2}{R^2 + \sum_i^M{p_i^2}} < N < \frac{S^2}{R + \sum_i^M{p_i^2}}}{S^2/sum(R^2 + p^2) < N < S^2/sum(R + p^2)}
#'
#' where \eqn{R} is the number of seats allocated to 'other' parties and \eqn{N}
#' is the effective number of parties. The best estimate is the mean of the two
#' extremes, and this is the estimate returned by 5. If \code{range = T}, these
#' are also returned.
#'
#'
#' **Type 6**
#'
#' Extension of \code{type = 5}. Calculations assume that no component
#' of \eqn{R} is larger than the smallest element of \eqn{p_i}{p}, \eqn{p_L}{pL}.
#' The plausible range used for calculating the best estimate becomes:
#'
#' \deqn{\frac{S^2}{Rp_L + \sum_i^M{p_i^2}} < N < \frac{S^2}{R + \sum_i^M{p_i^2}}}{S^2/sum(R*pL + p^2) < N < S^2/sum(R + p^2)}
#'
#' As before, if \code{range = T}, the minimum and maximum are also returned.
#'
#'
#' **Type 7**
#'
#' Taagepera's (1999) alternative NP index.
#' \deqn{1 + N - (\frac{N}{(\frac{1}{p_1})^2}}{1 + N - (N/(1/p1))^2}
#'
#' where \eqn{N} is Laakso and Taagepera's (1979) Effective Number of Parties.
#'
#'
#' **Type 8**
#'
#' Golosov's alternative (2010) measure. Calculated as
#' \deqn{\sum_i^M(p_i/(p_i + p_1^2 - p_i^2))}{sum(p/(p + p1^2 - p^2))}
#'
#'
#' @references
#' \insertRef{kesselman1966}{psmisc}
#'
#' \insertRef{rae1968}{psmisc}
#'
#' \insertRef{wildgen1971}{psmisc}
#'
#' \insertRef{laakso1979}{psmisc}
#'
#' \insertRef{molinar1991}{psmisc}
#'
#' \insertRef{taagepera1997}{psmisc}
#'
#' \insertRef{taagepera1999}{psmisc}
#'
#' \insertRef{golosov2010}{psmisc}
#'
#'
#' @export
#'
enp <- function(p, type=3, S=NULL, range=F) {

  # Input check
  if (!is.numeric(p) | !is.atomic(p)) stop("p must be a numeric vector")

  # Type check
  if (length(type) > 1) {
    type <- type[1]
    warning("type should be length 1. Only the first element will be used")
  }
  if (!type %in% 1:8) stop("Type must be an integer between 1 and 8 inclusive. See details for more information.")

  # All measures other than types 5 abd 6
  if (!type %in% c(5,6)) {

    # Additional input checks
    if (any(0 > p | p > 1)) stop("For this type p must be a vector of proportions")
    if (sum(p) != 1) stop("For this type p must sum to 1.")

    # Measures
    if (type == 1) est <- 1 - sum(p^2) #fractionalisation
    if (type == 2) est <- exp(-sum(p*log(p, base = exp(1)))) #hyperfractionalisation
    if (type == 3) est <- 1/sum(p^2) #ENP
    if (type == 4) est <- 1 + enp(p, type=3)*((sum(p^2)-p[which.max(p)]^2)/sum(p^2)) #Molinar NP
    if (type == 7) {
      N <- enp(p, type=3)
      est <- 1 + N - (N/(1/p[which.max(p)]))^2 # Taagepera 1999 NP
    }
    if (type == 8) est <- sum(p/(p + p[which.max(p)]^2 - p^2)) # Golosov 2010 ENP

    # Return
    return(est)

  } else {

    # Additional input checks
    if (any(p %% 1 != 0)) stop("For types 5 and 6 p must be a vector of integer vectors")
    if (any(p < 0)) stop("For type 5 and 6 p must be a vector of integer vectors")

    # Checks on S
    if (is.null(S)) stop("For type 5 and 6 S must be supplied")
    if (length(S) > 1) {
      S <- S[1]
      warning("S should be length 1. Only the first element will be used")
    }
    if (!is.numeric(S) | !is.atomic(S)) stop("S must be a length 1 numeric")
    if (S %% 1 != 0) stop("S must be an integer greater than 0")

    # Checks on range
    if (length(range) > 1) {
      range <- range[1]
      warning("range should be length 1. Only the first element will be used")
    }
    if (!range %in% c(T,F)) stop("range should be TRUE or FALSE.")

    # Check S and p
    if (S < sum(p)) stop("S cannot be smaller than the sum of p")

    # Calculate R
    R <- S - sum(p)

    # Calculate maximum
    max <- S^2/sum(R + p^2)

    # Calculate minimum - diff between types 5 and 6
    if (type == 5) min <- S^2/sum(R^2 + p^2)
    if (type == 6) min <- S^2/sum(R*p[which.min(p)] + p^2)

    # Calculate estimate
    est <- (min + max)/2

    # Return
    if (range) return(data.frame(est=est, min=min, max=max))
    return(est) #if not returning the ranges

  }

}

