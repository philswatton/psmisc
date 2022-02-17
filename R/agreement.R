#' Van der Ejik's Agreement
#'
#' An implementation of Van der Ejik's (2001) agreement coefficient show dispersion in an ordinal distribution. Ranges from -1 (complete polarisation) to 1 (complete agreement).
#'
#' @param x Atomic vector containing integer values.
#' @param values Atomic vector containing unique values of the input vector. Necessary if the vector contains 0 of a given value.
#'
#' @return double
#'
#' @references
#' \insertRef{van2001}{psmisc}
#'
#' @export
#'
agreement <- function(x, values=NULL) {

  # Function implementing Van der Eijk's Agreement

  # Arguments
  # x: Ordinal vector to input
  # values: Vector giving all unique values of the frequency distirbution. Necessary if any categories contain a 0


  # Validate x
  if (!all(x %% 1 == 0 | is.na(x))) {
    stop("Error: input x should be atomic vector containing only integer values")
  }


  # Validate values
  if (!is.null(values) & !all(values %% 1 == 0) | any(duplicated(values))) {
    stop("Error: input values should be NULL or atomic vector containing only integer values with no duplicates")
  }


  # Unique values and total number of unique values
  if (is.null(values)) {
    values <- unique(x)[!is.na(unique(x))]
  }
  values <- sort(values)
  K <- length(values)


  # A only exists with three or more unique values
  if (K < 3) {
    stop("Error: Van der Ejik's agreement requires 3 or more unique values for calculation")
  }


  # Create empty frequency distribution
  dist <- data.frame(x = values, freq = replicate(K, 0))


  # Calculate frequencies
  for (i in values) {
    dist[dist$x == i, "freq"] <- sum(x == i, na.rm = T)
  }


  # Initialise 'remainder'
  r <- dist$freq


  # Total cases
  tot <- sum(r)


  # Initialise agreement A
  A <- 0


  # Iteratively calculate agreement A
  for (i in 1:K) {

    # Check remainder is not empty
    if (sum(r) == 0) break

    # Create empty layer
    layer <- replicate(length(r), 0)

    # Get minimum non-zero value
    m <- r[which.min(replace(r, r == 0, NA))]

    # Fill up layer
    layer[r > 0] <- m

    # Remove from remainder
    r[r > 0] <- r[r > 0] - m

    # Break into pattern & proportion of cases, calculate S (n of non-empty columns)
    pat <- as.numeric(layer > 0)
    prop <- sum(layer)/tot
    S <- sum(pat)

    # Initialise TU and TDU
    TDU <- 0
    TU <- 0

    # Calculate TU and TDU
    for (i in 1:(K - 2)) {
      for (j in (i + 1):(K - 1)) {
        for (l in (j + 1):K) {

          if (pat[i] == 1 & pat[j] == 1 & pat[l] == 0) {
            TU <- TU + 1
          } else if (pat[i] == 0 & pat[j] == 1 & pat[l] == 1) {
            TU <- TU + 1
          } else if (pat[i] == 1 & pat[j] == 0 & pat[l] == 1) {
            TDU <- TDU + 1
          }

        }
      }
    }


    # Calculate U
    if (TDU == 0 & TU == 0) {
      U <- 1
    } else {
      U <- ((K-2)*TU - (K-1)*TDU)/((K-2)*(TU+TDU))
    }


    # Add agreement of layer to A weighted by proportion
    A <- A + (U * (1 - (S - 1)/(K - 1)))*prop
  }


  return(A)

}




