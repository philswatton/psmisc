#' Aldrich-McKlevey Scaling
#'
#' An implementation of Aldrich-McKlevey's (1977) scaling method for perceptual data.
#'
#' @param x A dataframe or matrix containing integer values of respondent placements of stimuli.
#' @param respindex An optional integer giving the column index of respondent self-placements.
#' @param polarity An optional integer giving the column index of a stimulus you wish to be positive in value. All stimuli will be calculated in relation to this declaration.
#' @param iter To calculate the true stimuli values, amscale() uses matlib's Eigen() function, which uses iterative QR decomposition. Set the max number of iterations via this parameter.
#'
#' @return A list containing four objects.
#'
#'         stimuli contains a double vector of the scaled stimuli.
#'
#'         respondent is a dataframe of the same length of the input containing respondent
#'         intercepts, weights; and if respindex was specified self-placements and ideal points.
#'
#'         fit is a single value for AM's fit statistic for the model.
#'
#' @export
#'
#' @examples
amscale <- function(x, respindex = NULL, polarity=NULL, iter=1000) {

  # Function implementing Aldrich-McKlevey Scaling

  # Step 1: Take input df, conditionally split into respondent self-placements & stimuli placements
  # Step 2: Convert stimuli placements to individual matrices Xi & count of placements q along w/ count of respondents n
  # Step 3: Calculate A = sum(Xi(Xi'Xi)^-1Xi')
  # Step 4: Choose appropriate eigenvector of A - nI as the solution
  # Step 5: Calculate model fit
  # Step 6: Calculate respondent intercepts & weights
  # Step 7: If respondent self-placements provided, convert to respondent ideal points using intercepts & weights




  # TODO:
  # - Write validation for polarity input
  # - Allow users to just stick in a full data frame and specify which columns they want to scale
  # - Consider replacing the use of solve() for other methods of calculating inverses - e.g. QR decomposition
  # - Write summary() method
  # - Complete the output




  ## Step 1: Validate and prepare input

  # Validate input is a df or matrix
  if (!is.matrix(x) & !is.data.frame(x)) {
    stop("Error: x should be data frame or matrix")
  }

  # Get names
  stimNames <- names(x) # need to update this to do conditionally if unput is a df, generate stims if a matrix

  # If input is df, convert to matrix
  if (is.data.frame(x)) {
    x <- as.matrix(x)
  }

  # Ensure matrix contains only integer values
  if (!is.numeric(x)) {
    stop("Error: x should contain only integer or missing values")
  } else if (!all(mapply(function(y) y %% 1 == 0 | is.na(y), x))) {
    stop("Error: x should contain only integer or missing values")
  }

  # Calculate N (total number of respondents in the input) and q (number of stimuli)
  N <- nrow(x)
  q <- ncol(x)

  # If provided, validate respondent index
  if (!is.null(respindex)) {
    if (respindex %% 1 != 0 | length(respindex) != 1)
    stop("Error: Respondent index should be a single integer value giving the location of the respondent self placements in the input")
  }

  # If respondent placements provided, partition into respondent & stimuli matrices
  if (!is.null(respindex)) {
    resp <- x[respindex]
    x <- x[,-respindex]
    stimNames <- stimNames[-respindex]
    q <- q - 1
  }




  ## Step 2: Prepare Xi matrices

  # Create id vector to facilitate keeping output respondents df to the same size of the original
  idvec <- 1:N
  idvecCC <- idvec[complete.cases(x)] # this will be used to bind respondent results at the end

  # Filter x for complete cases
  mat <- x[complete.cases(x),]

  # Calculate cc (n of respondents with complete answers for the stimuli)
  cc <- nrow(mat)
  n <- cc # total n of respondents

  # Break up into consituent Xi matrices
  Xi <- lapply(1:cc, function(i) matrix(c(replicate(q, 1), t(mat[i,])), nrow=q))

  # Check if respondent matrix is invertible
  tests <- sapply(1:cc, function(i) class(try(solve(t(Xi[[i]]) %*% Xi[[i]]), silent=T))[1] == "matrix")

  # Filter out respondents with non-invertible matrices
  if (any(!tests)) {
    Xi <- Xi[tests]
    idvecCC <- idvecCC[tests]
    n <- sum(tests)
    warning(paste0(c("Xi'Xi was not inveritible for ", sum(!tests), " respondents.")))
  }




  ## Step 3: Calculate A, I, (A - nI)

  # Calculate A
  A <- Reduce('+', lapply(1:n, function(i) Xi[[i]] %*% solve(t(Xi[[i]]) %*% Xi[[i]]) %*% t(Xi[[i]])))

  # Calculate I_q
  I <- diag(q)

  # Calculate A - nI
  AnI <- A - (n * I)




  ## Step 4: Calculate result

  # Calculate eigenvalues and eigenvectors
  # eig <- eigen(AnI)
  eig <- matlib::Eigen(AnI, max.iter = iter)

  # Calculate index for highest negative nonzero eigenvalue
  index <- which.max(replace(eigenvalues, eigenvalues >= 0, NA))

  # get value of highest negative nonzero eigenvalue (for calculation of model fit)
  e2 <- -eig$values[index] # note the eigenvalue correspondents to -1* sum of squared errors

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
  fit <- (n * e2)/(q*(n + e2)^2)




  ## Step 6: Calculate respondent intercepts & weights

  # Calculate
  solutions <- lapply(1:n, function(i) solve(t(Xi[[i]]) %*% Xi[[i]]) %*% t(Xi[[i]]) %*% stimuli)
  intercept <- sapply(1:n, function(i) solutions[[i]][1])
  weight <- sapply(1:n, function(i) solutions[[i]][2])

  # Put into DF same length as inputs
  respondent <- data.frame(idvec)
  solution <- data.frame(idvecCC, intercept, weight)
  respondent <- merge(respondent, solution, by.x = "idvec", by.y = "idvecCC", all.x=T, all.y=F)

  # Finalise by removing idvec
  respondent <- respondent[2:3]



  ## Step 7: Calculate ideal points

  if (!is.null(respindex)) {
    respondent$selfplace <- resp
    respondent$idealpt <- respondent$intercept + (respondent$weight * respondent$selfplace)
  }




  ## Return

  # Return list
  out <- list()
  out$stimuli <- stimuli
  out$respondents <- respondent
  out$eigenvalues <- eigenvalues
  out$fit <- fit
  return(out)

  # Debugging list
  # return(list(stimuli, A, AnI, eig))

}
