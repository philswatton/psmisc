agreement <- function(x, values=NULL) {

  # Function implementing Van der Eijk's Agreement
  # Step 1: Decompose input ordinal distribution into 'layers' of uniform distributions
  # Step 2: Calculate Agreement of each layer
  # Step 3: calculate aggregated Agreement of overall distribution conditional

  # Arguments
  # x: Ordinal vector to input
  # values: Vector giving all unique values of the frequency distirbution. Necessary if any categories contain a 0


  # Validate x
  if (!all(x %% 1 == 0 | is.na(x))) {
    stop("Error: input x should be atomic vector containing only integer values")
  }


  # Validate values
  if (!is.null(values) & !all(values %% 1 == 0 | is.na(values))) {
    stop("Error: input values should be NULL or atomic vector containing only integer values")
  }


  # Unique values and total number of unique values
  if (is.null(values)) {
    values <- unique(x)[!is.na(unique(x))]
  }
  n <- length(values)


  # Create empty frequency distribution
  dist <- data.frame(x = values, freq = replicate(n, 0))


  # Calculate frequencies
  for (i in values) {
    dist[dist$x == i, "freq"] <- sum(x == i, na.rm = T)
  }


  # TODO: Break frequency dist into uniform layers


  # TODO: calculate frequencies for each layer


  # TODO: calculate binary equivalents for each layer (has/doesn't have values)


  # TODO: calculate agreement for each layer


  # TODO: aggregate agreement for layers, weighted by layer frequencies


  return(list(x, values, n, dist))

}





# Initial testing with simple vectors
t1 <- c(1, 1, 1, 2, 5, 6, 1, 3, 4, 5, 6, 3, 4, 2, 7, 5, 7, 4, 1, 2)
t2 <- c(1, NA, 1, 2, 5, 6, 1, 3, 4, 5, 6, 3, 4, 2, 7, 5, 7, 4, 1, 2)
t3 <- c(1, NA, 1, 2, 5, 6, 1, 3, 4, 5, 6, 3, 4, 2, 6, 5, 6, 4, 1, 2)


agreement(t1)
agreement(t2)
agreement(t3)
agreement(t3, 1:7)







