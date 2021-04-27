amscale <- function(x, respondent = NULL) {

  # Function implementing Aldrich-McKlevey Scaling

  # Step 1: Take input df, conditionally split into respondent self-placements & stimuli placements
  # Step 2: Convert stimuli placements to individual matrices Xi & count of placements q along w/ count of respondents n
  # Step 3: Calculate A = sum(Xi(Xi'Xi)^-1Xi')
  # Step 4: Choose appropriate eigenvector of A - nI as the solution
  # Step 5: Calculate model fit
  # Step 6: Calculate respondent intercepts & weights
  # Step 7: If respondent self-placements provided, convert to respondent ideal points using intercepts & weights




  ## Step 1: Validate and prepare input

  # Validate input is a df or matrix
  if (!is.matrix(x) & !is.data.frame(x)) {
    stop("Error: x should be data frame or matrix")
  }

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
  if (!is.null(respondent)) {
    if (respondent %% 1 != 0 | length(respondent) != 1)
    stop("Error: Respondent index should be a single integer value giving the location of the respondent self placements in the input")
  }

  # If respondent placements provided, partition into respondent & stimuli matrices
  if (!is.null(respondent)) {
    resp <- x[,respondent]
    x <- x[,-respondent]
  }




  ## Step 2: Prepare Xi matrices

  # Create id vector to facilitate keeping output respondents df to the same size of the original
  idvec <- 1:N
  idvecCC <- idvec[complete.cases(x)] # this will be used to bind respondent results at the end

  # Filter x for complete cases
  mat <- x[complete.cases(x),]

  # Calculate n (n of respondents with complete answers for the stimuli)
  n <- nrow(mat)

  # Break up into consituent Xi matrices
  Xi <- lapply(1:n, function(i) matrix(c(replicate(q, 1), t(mat[i,])), nrow=q))




  ## Step 3: Calculate A, I, (A - nI)

  # Calculate A
  A <- Reduce('+', lapply(1:n, function(i) Xi[[i]] %*% ((t(Xi[[i]]) %*% Xi[[i]])^-1) %*% t(Xi[[i]])))

  # Calculate I_q
  I <- diag(q)

  # Calculate A - nI
  AnI <- A - (n * I)




  ## TODO: Step 4: Calculate result

  # Calculate Eigenvalues and eigenvectors
  # eig <- eigen(AnI)

  # get index of highest negative nonzero eigenvalue
  # index <- which.min(replace(eig$values, eig$values >= 0, NA))

  # get value of highest negative nonzero eigenvalue (for calculation of model fit)
  # e2 <- eig$values[index] # note this correspondents to -1* sum of squared errors

  # get stimuli
  # stimuli <- eig$vectors[,index]
  # names(stimuli) <- names(x)



  ## TODO: Step 5: Calculate model fit




  ## TODO: Step 6: Calculate respondent intercepts & weights




  ## TODO: Step 7: Calculate result





  # return(list(A, I, AnI, index, stimuli))
  return(list(Xi, A, AnI))
  # return(stimuli)

}


# Full EES to Scale
ees <- readRDS("C:/Users/User/Documents/Research/scalingExploration/data/ees2019.rds")
ees <- ees[ees$country=="United Kingdom",2:8]
names(ees) <- c("lrSelf","lrCon","lrLab","lrLD","lrGreen","lrUKIP","lrBXP")


# Version w/out Self placements
eesMat <- ees[2:7]

mat <- eesMat[complete.cases(eesMat),]
n <- nrow(mat)
q <- 6


Xi <- lapply(1:n, function(i) matrix(c(replicate(q, 1), t(mat[i,])), nrow=q))

temp <- lapply(1:n, function(i) Xi[[i]] %*% ((t(Xi[[i]]) %*% Xi[[i]])^-1) %*% t(Xi[[i]]))

Reduce('+', temp)

Reduce('+', lapply(1:n, function(i) Xi[[i]] %*% ((t(Xi[[i]]) %*% Xi[[i]])^-1) %*% t(Xi[[i]])))

A <- Reduce('+', lapply(1:n, function(i) Xi[[i]] %*% ((t(Xi[[i]]) %*% Xi[[i]])^-1) %*% t(Xi[[i]])))

AnI <- (A - (n * diag(q)))




temp2 <- Reduce('rbind', temp)

is.na(temp2)



temp2[sapply(1:n, function(i) any(is.na(temp2[i,]))),]



any(is.na(temp2))

apply(simplify2array(temp), c(1,2), sum)


eigen(AnI)

# compare <- basicspace::aldmck(eesMat, polarity=2)
compare$stimuli


amscale(ees, respondent = 1)
amscale(eesMat)



stim <- amscale(eesMat)
names(stim) <- names(eesMat)
stim

