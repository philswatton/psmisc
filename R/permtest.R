#' Permutation test
#'
#' Perform a one-sided or two-sided permutation test. Please note input validation
#' has not been implemented yet and so user caution is required when using this function.
#' Pay careful attention in particular to the '\code{group}' input. If performing a
#' one-sided test, it's best if this is a factor ordered in the desired direction.
#'
#' @param x Numeric vector to be tested
#' @param group Factor with two unique values. Pay attention to ordering.
#' @param iter The number of iterations to run the permutation test for.
#' @param alternative The alternative hypothesis. Must be one of "\code{two-sided}" (default), "\code{greater}", or "\code{less}".
#'
#' @return A list containing four objects:\tabular{ll}{
#'    \code{obs_diff} \tab A length one double vector containing the observed mean difference. \cr
#'    \tab \cr
#'    \code{d} \tab A double vector of length iter containing the mean differences from the permutations. \cr
#'    \tab \cr
#'    \code{p} \tab The p-value for the test. If \code{alternative} was "\code{two-sided}", this is the proportion of absolute mean differences greater than the observed mean difference. Otherwise, if \code{alternative} was "\code{greater}" or "\code{less}", this is the proportion of mean differences greater/less than the observed mean difference. \cr
#'    \tab \cr
#'    \code{alternative} \tab The alternative hypothesis for the test.
#' }
#' @export
#'
permtest <- function(x, group, iter=1000, alternative = "two-sided") {

  # Input validation

  # Prepare groups - need to think carefully abt managing group A vs B - t.test does this by separating y and x, and taking df and formula as an alternative
  groups <- unique(group)
  if (length(groups) != 2) stop("Error: there must two unique groups. Consider checking for NAs in your group variable.")

  # Observed differences
  obs_diff <- mean(x[group == groups[1]]) - mean(x[group == groups[2]])

  # Prepare empty vector
  d <- numeric(iter)

  # Run permutations
  for (i in 1:iter) {
    group <- sample(group, replace=F)
    d[i] <- mean(x[group == groups[1]]) - mean(x[group == groups[2]])
  }

  # P-value
  p <- sum(abs(d) > abs(obs_diff))/iter #two-sided

  #
  return(list(obs_diff, d, p, alternative))

}
