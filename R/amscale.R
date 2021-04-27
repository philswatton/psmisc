amscale <- function(x, respondent = NULL) {

  # Function implementing Aldrich-McKlevey Scaling

  # Step 1: Take input df, conditionally split into respondent self-placements & stimuli placements
  # Step 2: Convert stimuli placements to individual matrices Xi & count of placements q along w/ count of respondents n
  # Step 3: Calculate A = sum(Xi(Xi'Xi)^-1Xi')
  # Step 4: Choose appropriate eigenvector of A - nI as the solution
  # Step 5: Calculate model fit
  # Step 6: Calculate respondent intercepts & weights
  # Step 7: If respondent self-placements provided, convert to respondent ideal points using intercepts & weights




  ## Step 1

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
  if (!is.null(respondent) & (respondent %% 1 != 0 | length(respondent) != 1)) {
    stop("Error: Respondent index should be a single integer value giving the location of the respondent self placements in the input")
  }

  # If respondent placements provided, partition into respondent & stimuli matrices
  if (!is.null(respondent)) {
    resp <- x[respondent]
    x <- x[-respondent]
  }




  ## Step 2

  # Create id vector to facilitate keeping output respondents df to the same size of the original
  idvec <- 1:N
  idvecCC <- idvec[complete.cases(x)] # this will be used to bind respondent results at the end

  # Filter x for complete cases
  mat <- x[complete.cases(x),]

  # Calculate n (n of respondents with complete answers for the stimuli)
  n <- nrow(mat)





  return(x)

}


# Full EES to Scale
ees <- readRDS("C:/Users/User/Documents/Research/scalingExploration/data/ees2019.rds")
ees <- ees[ees$country=="United Kingdom",2:8]
names(ees) <- c("lrSelf","lrCon","lrLab","lrLD","lrGreen","lrUKIP","lrBXP")


# Version w/out Self placements
eesMat <- ees[2:7]
n <- 1000
q <- 6


lapply(1:n, function(z) matrix(c(replicate(q, 1), t(eesMat[z,])), nrow=6))

Xi <- matrix(c(replicate(q, 1), t(eesMat[3,])), nrow=6)


amscale(matrix(c("Hello")))

ees[-1]


complete.cases(eesMat)

