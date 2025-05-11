#' Aldrich-McKlevey Scaling
#'
#' Performs Aldrich-McKlevey's (1977) scaling method for perceptual
#' data. The function will automatically filter out missing data.
#'
#' @param stim_placements A \code{data.frame} or \code{matrix} containing numeric values of respondent placements of stimuli, with stimuli on columns and respondents on rows.
#' @param self_placements An optional numeric vector containing respondent self-placements. If provided, must have same number of elements as \code{stim_placements} has rows.
#' @param compute_respondent_variables Whether to compute individual respondent displacement parameters, defaults to \code{TRUE}. Ignored and treated as if \code{TRUE} if self_placements is provided.
#' @param polarity An optional integer giving the column index of a stimulus that you wish to have a negative value. All stimuli and respondent self-placements will also be rescaled accordingly.
#' @param method Either "matrix" or "QR". See details.
#' @param verbose Whether to print diagnostic messages specific to the function while running. Defaults to \code{FALSE}.
#'
#' @return An object of class "\code{amscale}", containing the following elements:\tabular{ll}{
#'    \code{stimuli} \tab Double vector containing scaled stimuli estimates. \cr
#'    \tab \cr
#'    \code{respondent} \tab If \code{compute_respondent_variables} was not \code{NULL} or if \code{self_placements} was provided, a \code{data.frame} with the same number of rows as the input containing respondent displacement intercepts and weights. If \code{self_placements} was provided, additionally contains estimated respondent ideal points. \cr
#'    \tab \cr
#'    \code{n_input} \tab The number of respondents in the initial input. \cr
#'    \tab \cr
#'    \code{n_used} \tab The number of respondents used for scaling the stimuli in the model after filtering for missing data. \cr
#'    \tab \cr
#'    \code{n_filtered} \tab The number of respondents filtered out due to missing data. \cr
#'    \tab \cr
#'    \code{n_dropped} \tab The number of respondents dropped to enable scaling. Will always be 0 if \code{method="QR"}. \cr
#'    \tab \cr
#'    \code{n_stim} \tab The number of stimuli scaled in the model. \cr
#'    \tab \cr
#'    \code{fit} \tab The adjusted fit statistic proposed in Aldrich and McKelvey's paper (1977) for the model. \cr
#'    \tab \cr
#' }
#'
#' @details
#'
#' Aldrich-McKelvey scaling takes a matrix of respondent placements of some stimuli
#' (typically parties and/or candidates) on some scale (typically ideological dimensions)
#' and estimates the positions of the stimuli on that scale. Unlike simply taking the mean,
#' this process has some robustness to rationalisation bias, where respondents distort
#' the scale in line with their own ideological preferences (see Bølstad 2020).
#'
#' This function implements two versions of Aldrich-McKelvey scaling. First
#' \code{method="matrix"} implements the canonical version. Second, \code{method="QR"}
#' implements a version using QR decomposition, which eliminates the need to drop some
#' respondents (see Swatton 2021), but can be slower to compute.
#'
#' Where respondent self-placements are provided, respondent ideal points on the same
#' scale can be estimated, through first estimating individual respondent distortion parameters.
#' Where not provided, these distortion parameters can still be estimated, but this can
#' be skipped by setting \code{compute_respondent_variables=FALSE}.
#'
#' @references
#' \insertRef{aldrich1977}{psmisc}
#'
#' \insertRef{poole2016}{psmisc}
#'
#' \insertRef{bolstad2020capturing}{psmisc}
#'
#' \insertRef{swatton2021}{psmisc}
#'
#' @export
#'
amscale <- function(
    stim_placements,
    self_placements=NULL,
    compute_respondent_variables=TRUE,
    polarity=NULL,
    method="matrix",
    verbose=FALSE
) {
  # Validate input is a df or matrix
  if (!is.matrix(stim_placements) & !is.data.frame(stim_placements)) {
    stop("stim_placements should be of class data.frame or matrix")
  }

  # N (total number of respondents in the input) and J (number of stimuli)
  N <- nrow(stim_placements)
  J <- ncol(stim_placements)

  # If input is df, convert to matrix, then prepare stimuli names
  if (is.data.frame(stim_placements)) stim_placements <- as.matrix(stim_placements)
  if (is.null(colnames(stim_placements))) stimNames <- paste0("stim", 1:J) else stimNames <- colnames(stim_placements)

  # Ensure matrix is numeric
  if (!is.numeric(stim_placements)) stop("stim_placements must a matrix or data.frame containing only numeric values")

  # If provided, validate respondent self placements
  if (!is.null(self_placements)) {
    if (!(is.numeric(self_placements) & is.atomic(self_placements))) {
      stop("self_placements should either be NULL or a numeric vector")
    }
    if (length(self_placements) != N) stop("if provided, self_placements should contain as many observations as stimuli_placements has rows")
  }

  # Validate polarity
  if (!is.null(polarity)) {
    if (!is.numeric(polarity)) {
      stop("polarity should be NULL or numeric")
    }
    if (polarity %% 1 != 0) {
      stop("polarity should be a natural number")
    }
    if (polarity > J | polarity < 1) {
      stop("if provided, polarity should index one of the columns of x")
    }
  }

  # Validate remaining parameters
  if (!is.logical(compute_respondent_variables)) stop("compute_respondent_variables must be either TRUE or FALSE")
  if (!is.logical(verbose)) stop("verbose must be either TRUE or FALSE")
  if (!method %in% c("matrix", "QR")) stop("method must be one of 'matrix' or 'QR'")

  # Filter for complete cases
  id <- 1:N
  complete_cases_id <- id[complete.cases(stim_placements)]
  mat <- stim_placements[complete_cases_id,]

  # Calculate n of complete cases
  n <- nrow(mat)
  n_filtered <- N - n
  n_dropped <- 0

  # Break up matrix into constituent Xi matrices with constant column added
  Xi <- lapply(1:n, function(i) matrix(c(replicate(J, 1), mat[i,]), nrow=J))

  # Compute A conditional on method
  if (method=="matrix") {
    A_plus_keep <- amscale_compute_A_matrix(Xi)
    A <- A_plus_keep$A
    keep_cases <- A_plus_keep$keep_cases
    complete_cases_id <- complete_cases_id[keep_cases]
    Xi <- Xi[keep_cases]
    n_dropped <- n - sum(keep_cases)
    n <- sum(keep_cases)
  }
  if (method=="QR") A <- amscale_compute_A_QR(Xi)

  # Calculate A - nI
  AnI <- A - (n * diag(J))

  # Perform decomposition to estimate stimuli values and fit
  decomposition <- amscale_decompose_AnI(AnI, J, stimNames, verbose)
  stimuli <- decomposition$stimuli
  e2 <- decomposition$e2

  # Optionally adjust polarity
  if (!is.null(polarity)) stimuli <- set_polarity(stimuli, polarity)

  # Calculate model fit
  fit <- amscale_fit(n, J, e2)

  # Optionally compute respondent variables
  if (compute_respondent_variables | !is.null(self_placements)) {
    respondent <- amscale_respondent_variables(Xi, self_placements, stimuli, id, complete_cases_id)
  } else respondent <- NULL

  # Construct output class
  output <- amscale_constructor(
    stimuli=stimuli,
    respondent=respondent,
    n_input=N,
    n_used=n,
    n_filtered=n_filtered,
    n_dropped=n_dropped,
    n_stim=J,
    fit=fit
  )

  # Return
  return(output)

}


amscale_compute_A_matrix <- function(Xi) {
  Ai <- lapply(Xi, function(xi) {
    tryCatch(
      xi %*% solve(t(xi) %*% xi) %*% t(xi),
      error = function(msg) NA
    )
  })
  keep_cases <- !is.na(Ai)
  A <- Reduce(`+`, Ai[keep_cases])
  return(list(A=A, keep_cases=keep_cases))
}


amscale_compute_A_QR <- function(Xi) {
  Qi <- lapply(Xi, function(xi) qr.Q(qr(xi)))
  A <- Reduce(`+`, lapply(Qi, function(qi) qi %*% t(qi)))
  return(A)
}


amscale_decompose_AnI <- function(AnI, J, stimNames, verbose) {
  # Using SVD as substitute for eigendecomposition
  decomp <- svd(AnI, nu=J, nv=J)
  u <- decomp$u
  d <- decomp$d
  v <- decomp$v

  # Drop near-0 singular values and vectors
  # https://scicomp.stackexchange.com/questions/350/what-should-be-the-criteria-for-accepting-rejecting-singular-values
  eps <- 1e-7
  if (.Machine$sizeof.pointer == 8L) eps <- 1e-16
  test <- d < J * eps * max(d)
  if (any(test)) {
    if (verbose) message("Removing one or more near-0 singular values and corresponding singular vectors.")
    d <- d[!test]
    v <- v[,!test]
    u <- u[,!test]
  }

  # Establish which eigenvalues would have been positive
  pos <- apply((u > 0) == (v > 0), 2, any)
  d[!pos] <- d[!pos] * -1

  # Calculate index for highest negative nonzero eigenvalue
  index <- which.max(replace(d, d >= 0, NA))

  # Get value of highest negative nonzero eigenvalue (for calculation of model fit)
  e2 <- -d[index]

  # Get value of stimuli
  stimuli <- v[,index]
  names(stimuli) <- stimNames

  # Return
  return(list(stimuli=stimuli, e2=e2))
}


set_polarity <- function(stimuli, polarity) {
  if (stimuli[polarity] > 0) stimuli <- -stimuli
  return(stimuli)
}


amscale_fit <- function(n, J, e2) (n * e2)/(J*(n + e2)^2)


amscale_respondent_variables <- function(
    Xi, self_placements, stimuli, id, complete_cases_id
) {
  # Calculate
  solutions <- lapply(Xi, function(xi) lm(stimuli ~ xi[,2])$coefficients)
  intercept <- sapply(solutions, function(solution) solution[1])
  weight <- sapply(solutions, function(solution) solution[2])

  # Put into DF same length as inputs
  respondent <- data.frame(id)
  solution <- data.frame(complete_cases_id, intercept, weight)
  respondent <- merge(
    respondent, solution, by.x = "id", by.y = "complete_cases_id", all.x=T, all.y=F
  )

  # Finalise by removing idvec
  respondent <- respondent[2:3]

  ## Calculate ideal points if vector of self-placements provided
  if (!is.null(self_placements)) {
    respondent$idealpt <- respondent$intercept + (respondent$weight * self_placements)
  }

  return(respondent)
}


amscale_integer_stop <- function(x) stopifnot(is.numeric(x), is.atomic(x), x %% 1 == 0)


amscale_constructor <- function(
    stimuli, respondent, n_input, n_used, n_filtered, n_dropped, n_stim, fit
) {

  stopifnot(is.numeric(stimuli), is.atomic(stimuli))
  stopifnot(is.data.frame(respondent))
  amscale_integer_stop(n_input)
  amscale_integer_stop(n_used)
  amscale_integer_stop(n_filtered)
  amscale_integer_stop(n_dropped)
  amscale_integer_stop(n_stim)
  stopifnot(is.numeric(fit), is.atomic(fit))

  instance <- structure(
    list(
      stimuli=stimuli,
      respondent=respondent,
      n_input=n_input,
      n_used=n_used,
      n_filtered=n_filtered,
      n_dropped=n_dropped,
      n_stim=n_stim,
      fit=fit
    ),
    class="amscale"
  )

  return(instance)

}


