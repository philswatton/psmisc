ordTable <- function(data, vars, values=NULL) {

  # Function for producing descriptive stat tables of ordinal data

  # Initialise for first variable
  vec <- data[vars[1]][[1]]
  out <- data.frame(name = vars[1], median = median(vec, na.rm=T), agreement = agreement(vec))

  # Keep adding more for subsequent variables if they've been listed
  if (length(vars) > 1) {
    for (i in 2:length(vars)) {
      vec <- data[vars[i]][[1]]
      out <- rbind(out, data.frame(name = vars[i], median = median(vec, na.rm=T), agreement = agreement(vec)))
    }
  }

  return(out)

}
