#' Aldrich-McKlevey Scaling
#'
#' An implementation of Aldrich-McKlevey's (1977) scaling method for perceptual data.
#'
#' @param x A dataframe or matrix containing numeric values of respondent placements of stimuli, with stimuli on columns and respondents on rows.
#' @param resp A optional numeric vector containing respondent self-placements.
#' @param polarity An optional integer giving the column index of a stimulus that you wish to have a negative value. All stimuli and respondent self-placements will also be coded accordingly.
#'
#' @return A list containing four objects:
#'
#' stimuli contains a double vector of the scaled stimuli.
#'
#' respondent is a dataframe of the same length of the input containing respondent intercepts, weights; and if resp was not NULL self-placements and ideal points.
#'
#' fit is a single value for AM's fit statistic for the model.
#'
#' eigenvalues are the eigenvalues computed from the matrix A - nI
#'
#' @export
#'
#' @examples
amscale <- function(x, resp = NULL, polarity = NULL) {

  # Function implementing Aldrich-McKelvey Scaling - V2

  # Step 1: Take input df (optionally also take respondent vector and polarity direction)
  # Step 2: Optionally save locations of full respondents before reducing to complete cases
  # Step 3: Count number of respondents who have placed all stimuli (do BEFORE )
  # Step 3: Convert stimuli placements to individual matrices Xi
  # Step 4: Count number of stimuli q and number of respondents w/ complete responses n (have to do AFTER)
  # Step 5: Calculate A = sum(Xi(Xi'Xi)^-1Xi')
  # Step 6: Obtain eigenvector corresponding to the highest negative eigenvalue of (A - nI) as the solution
  # Step 7: Convert solution conditional on polarity
  # Step 8: Calculate model fit
  # Step 9: Calculate respondent intercepts & weights
  # Step 10: If respondent self-placements provided, obtain ideal pts using intercepts & weights

  # TODO:
  # - Always return non-invertable respondents (?)
  # - Consider replacing the use of solve() for other methods of calculating inverses - e.g. QR decomposition
  # - Write summary() method
  # - Complete the output




  ## Step 1: Validate and prepare input

  # Validate input is a df or matrix
  if (!is.matrix(x) & !is.data.frame(x)) {
    stop("Error: x should be data frame or matrix")
  }

  # If input is df, convert to matrix
  if (is.data.frame(x)) {

    # Get names
    stimNames <- names(x)

    # Convert to matrix
    x <- as.matrix(x)
  }

  # Ensure matrix contains only numeric values
  if (!is.numeric(x)) {
    stop("Error: x should be a matrix or dataframe containing only numeric values")
  }

  # Calculate N (total number of respondents in the input) and J (number of stimuli)
  N <- nrow(x)
  J <- ncol(x)

  # If provided, validate respondent index
  if (!is.null(resp)) {
    if (!is.numeric(resp)) {
      stop("Error: resp should either be NULL or a numeric vector")
    }

    # Make sure x and resp have the same N
    if (length(resp) != N) {
      stop("Error: if provided, resp should contain the same number of rows as x")
    }
  }

  # Validate polarity
  if (!is.null(polarity)) {
    if (!is.integer(polarity)) {
      stop("Error: polarity should be NULL or an integer value")
    }
    if (polarity > J) {
      stop("Error: if provided, polarity should index one of the columns of x")
    }
  }




  ## Step 2: Prepare Xi matrices

  # Create ID vector and a second vector index complete cases of x
  id <- 1:N
  ccindex <- id[complete.cases(x)]

  # Filter x for complete cases
  mat <- x[ccindex,]

  # Calculate n of complete cases (in terms of responses)
  cc <- nrow(mat)

  # Break up into consituent Xi matrices
  Xi <- lapply(1:cc, function(i) matrix(c(replicate(J, 1), t(mat[i,])), nrow=J))

  # Check if respondent matrix is invertible
  tests <- sapply(1:cc, function(i) class(try(solve(t(Xi[[i]]) %*% Xi[[i]]), silent=T))[1] == "matrix")

  # Filter out respondents with non-invertible matrices
  if (any(!tests)) {
    Xi <- Xi[tests]
    ccindex <- ccindex[tests]
    n <- sum(tests)
    warning(paste0(c("Xi'Xi was not inveritible for ", sum(!tests), " respondents.")))
  }




  ## Step 3: Calculate A, I, (A - nI)

  # Calculate A
  A <- Reduce('+', lapply(1:n, function(i) Xi[[i]] %*% solve(t(Xi[[i]]) %*% Xi[[i]]) %*% t(Xi[[i]])))

  # Calculate I_J
  I <- diag(J)

  # Calculate A - nI
  AnI <- A - (n * I)




  ## Step 4: Calculate result

  # Calculate eigenvalues and eigenvectors
  eig <- eigen(AnI)

  # get eigenvalues
  eigenvalues <- eig$values

  # Calculate index for highest negative nonzero eigenvalue
  index <- which.max(replace(eigenvalues, eigenvalues >= 0, NA))

  # get value of highest negative nonzero eigenvalue (for calculation of model fit)
  e2 <- -eigenvalues[index] # note the eigenvalue correspondents to -1* sum of squared errors

  # get stimuli
  stimuli <- eig$vectors[,index]
  names(stimuli) <- stimNames

  # if a polarity has been specified, ensure stimuli are positive
  if (!is.null(polarity)) {
    if (stimuli[polarity] < 0) {
      stimuli <- -stimuli
    }
  }





  ## Step 5: Calculate model fit
  fit <- (n * e2)/(J*(n + e2)^2)




  ## Step 6: Calculate respondent intercepts & weights

  # Calculate
  solutions <- lapply(1:n, function(i) solve(t(Xi[[i]]) %*% Xi[[i]]) %*% t(Xi[[i]]) %*% stimuli)
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




  ## Return

  return(list(stimuli = stimuli,
              respondents = respondent,
              eigenvalues = eigenvalues,
              fit = fit))

  # Debugging list
  # return(list(stimuli, A, AnI, eig))

}
