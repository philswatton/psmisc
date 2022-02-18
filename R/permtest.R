#' Permutation test
#'
#' Perform a one-sided or two-sided permutation test.
#'
#' @param x Numeric vector of data values for the first group.
#' @param y Numeric vector of data values for the second group.
#' @param group_names Optional character vector of group names.
#' @param iter The number of iterations to run the permutation test for.
#' @param alternative The alternative hypothesis. Must be one of "\code{two-sided}" (default), "\code{greater}", or "\code{less}".
#'
#' @return A list containing four objects:\tabular{ll}{
#'    \code{obs_diff} \tab A length one double vector containing the observed mean difference. \cr
#'    \tab \cr
#'    \code{d} \tab A double vector of length \code{iter} containing the mean differences from the permutations. \cr
#'    \tab \cr
#'    \code{p} \tab The p-value for the test. If \code{alternative} was "\code{two-sided}", this is the proportion of absolute mean differences greater than the observed mean difference. Otherwise, if \code{alternative} was "\code{greater}" or "\code{less}", this is the proportion of mean differences greater/less than the observed mean difference. \cr
#'    \tab \cr
#'    \code{alternative} \tab The alternative hypothesis for the test.
#' }
#' @export
#'
permtest <- function(x, y, group_names = NULL, iter=1000, alternative = "two-sided") {

  # Validate x and y
  if (!is.numeric(x) | !is.numeric(y)) {
    stop("x and y must both be numeric vectors")
  }

  # Remove missing data
  numNA <- sum(is.na(x)) + sum(is.na(y))
  if (numNA > 1) {
    x <- x[!is.na(x)]
    y <- y[!is.na(y)]
    warning(paste0(numNA, " observations removed due to missingness"))

    if (length(x) == 0) stop("no observations in x after filtering for missing data")
    if (length(y) == 0) stop("no observations in y after filtering for missing data")
  }

  # Prepare group names
  if (is.null(group_names)) {
    group_names <- c("A", "B")
  } else if (!is.character(group_names)) {
    stop("if supplied, group_names must be character")
  } else if (length(group_names) != 2) {
    stop("if supplied, group_names must be of length 2")
  }

  # Validate iter
  if (!is.numeric(iter)) {
    stop("iter must be numeric")
  } else if (length(iter) < 1) {
    stop("only one value can be supplied for iter")
  } else if (iter %% 1 != 0 | iter < 1) {
    stop("iter must be a natural number")
  }

  # Validate alternative
  if (length(alternative) > 1) {
    stop("only one value can be supplied for alternative")
  } else if (!(alternative %in% c("two-sided", "greater", "less"))) {
    stop("alternative must be one of \"two-sided\", \"greater\", or \"less\"")
  }

  # Get difference in means of x and y
  obs_diff <- mean(x) - mean(y)

  # Prepare
  var <- c(x,y)
  group <- c(rep(group_names[1], length(x)), rep(group_names[2], length(y)))

  # Run permutations
  d <- sapply(1:1000, function(i) {
    group <- sample(group, replace=F)
    mean(var[group == group_names[1]]) - mean(var[group == group_names[2]])
  })

  # P-value
  if (alternative == "two-sided") {
    p <- sum(abs(d) > abs(obs_diff))/iter
  } else if (alernative == "greater") {
    p <- sum(d > obs_diff)/iter
  } else {
    p <- sum(d < obs_diff)/iter
  }

  # Output
  return(list(obs_diff = obs_diff,
              differences=d,
              p=p,
              alternative=alternative))

}
