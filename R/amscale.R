#' Aldrich-McKlevey Scaling
#'
#' Performs Aldrich-McKlevey's (1977) scaling method for perceptual
#' data using the QR decomposition method (Swatton 2021) for computation.
#' The function will automatically filter missing data.
#'
#' @param x A dataframe or matrix containing numeric values of respondent placements of stimuli, with stimuli on columns and respondents on rows.
#' @param resp An optional numeric vector containing respondent self-placements.
#' @param polarity An optional integer giving the column index of a stimulus that you wish to have a negative value. All stimuli and respondent self-placements will also be coded accordingly.
#'
#' @return An object of class "\code{amscale}". The function \code{summary} prints a summary of the model and
#' its results. An object of class "\code{amscale}" contain the following elements:\tabular{ll}{
#'    \code{stimuli} \tab Double vector containing scaled stimuli estimates. \cr
#'    \tab \cr
#'    \code{respondent} \tab Dataframe of the same length as the input containing respondent intercepts, weights; and if resp was not NULL respondent ideal points. \cr
#'    \tab \cr
#'    \code{fit} \tab The adjusted fit statistic proposed in Aldrich and McKelvey's paper (1977) for the model. \cr
#'    \tab \cr
#'    \code{ninput} \tab The number of respondents in the initial input. \cr
#'    \tab \cr
#'    \code{nresp} \tab The number of respondents used for scaling the stimuli in the model after filtering for missing data. \cr
#'    \tab \cr
#'    \code{nstim} \tab The number of stimuli scaled in the model.
#' }
#'
#' @references
#' \insertRef{aldrich1977}{psmisc}
#'
#' \insertRef{swatton2021}{psmisc}
#'
#' @export
#'
amscale <- function(x, resp = NULL, polarity = NULL) {

  # Function implementing Aldrich-McKelvey Scaling - V2


  ## Step 1: Validate and prepare input

  # Validate input is a df or matrix
  if (!is.matrix(x) & !is.data.frame(x)) {
    stop("x should be data frame or matrix")
  }

  # Calculate N (total number of respondents in the input) and J (number of stimuli)
  N <- nrow(x)
  J <- ncol(x)

  # If input is df, convert to matrix, get names
  if (is.data.frame(x)) {

    # Get names
    stimNames <- names(x)

    # Convert to matrix
    x <- as.matrix(x)
  } else {

    if (is.null(colnames(x))) {
      stimNames <- paste0("stim", 1:J)
    } else {
      stimNames <- colnames(x)
    }

  }

  # Ensure matrix contains only numeric values
  if (!is.numeric(x)) {
    stop("x should be a matrix or dataframe containing only numeric values")
  }

  # If provided, validate respondent index
  if (!is.null(resp)) {
    if (!is.numeric(resp)) {
      stop("resp should either be NULL or a numeric vector")
    }

    # Make sure x and resp have the same N
    if (length(resp) != N) {
      stop("if provided, resp should contain as many observations as x")
    }
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


  ## Step 2: Prepare Xi and Qi matrices

  # Create ID vector and a second vector index complete cases of x
  id <- 1:N
  ccindex <- id[complete.cases(x)]

  # Filter x for complete cases
  mat <- x[ccindex,]

  # Calculate n of complete cases (in terms of responses)
  n <- nrow(mat)

  # Break up into constituent Xi matrices
  Xi <- lapply(1:n, function(i) matrix(c(replicate(J, 1), t(mat[i,])), nrow=J))

  # Calculate QR decompositions for each respondent
  QRi <- lapply(1:n, function(i) qr(Xi[[i]]))
  Qi <- lapply(1:n, function(i) qr.Q(QRi[[i]]))


  ## Step 3: Calculate A, I, (A - nI)

  # Calculate A
  A <- Reduce('+', lapply(1:n, function(i) Qi[[i]] %*% t(Qi[[i]])))

  # Calculate I_J
  I <- diag(J)

  # Calculate A - nI
  AnI <- A - (n * I)


  ## Step 4: De facto eigendecomposition via SVD

  # Calculate SVD
  decomp <- svd(AnI)
  u <- decomp$u
  d <- decomp$d
  v <- decomp$v

  # Establish which eigenvalues were positive
  pos <- apply((u > 0) == (v >0), 2, any)
  d[!pos] <- d[!pos] * -1

  # Calculate index for highest negative nonzero eigenvalue
  index <- which.max(replace(d, d >= 0, NA))

  # Get value of highest negative nonzero eigenvalue (for calculation of model fit)
  e2 <- -d[index]

  # Get value of stimuli
  stimuli <- v[,index]
  names(stimuli) <- stimNames

  # if a polarity has been specified, ensure stimuli are positive
  if (!is.null(polarity)) {
    if (stimuli[polarity] > 0) {
      stimuli <- -stimuli
    }
  }


  ## Step 5: Calculate model fit
  fit <- (n * e2)/(J*(n + e2)^2)


  ## Step 6: Calculate respondent intercepts & weights

  # Calculate
  solutions <- lapply(1:n, function(i) lm(stimuli ~ Xi[[i]][,2])$coefficients)
  intercept <- sapply(1:n, function(i) solutions[[i]][1])
  weight <- sapply(1:n, function(i) solutions[[i]][2])

  # Put into DF same length as inputs
  respondent <- data.frame(id)
  solution <- data.frame(ccindex, intercept, weight)
  respondent <- merge(respondent, solution, by.x = "id", by.y = "ccindex", all.x=T, all.y=F)

  # Finalise by removing idvec
  respondent <- respondent[2:3]


  ## Step 7: Calculate ideal points
  if (!is.null(resp)) {
    respondent$idealpt <- respondent$intercept + (respondent$weight * resp)
  }


  ## Step 8: Output

  # Construct output
  out <- psmisc:::amscale_constructor(stimuli, respondent, fit, N, n, J)

  # Return
  return(out)

}
