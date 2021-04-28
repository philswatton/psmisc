ordTable <- function(data, vars, values=NULL) {

  # Function for producing descriptive stat tables of ordinal data

  # TODO (inputs):
  # - Allow 'vars' to be NULL and simply taking a complete df as input
  # - Input validation (data is df or matrix, vars is an atomic vector of characters or integers, etc)
  #   - Possibly give option for user to manually specify names - will be important if matrices are taken as input
  # - Implement the 'values' option to allow a list of NULLs & ranges to be given

  # TODO (behaviour):
  # - Allow the user to specify which columns to report <- this will probably take the most work
  # - Consider implementing other measures of ordinal dispersion in the package to allow the user to choose which to report
  # - Search for other ordinal-specific measures

  # Initialise for first variable
  vec <- data[vars[1]][[1]]
  out <- data.frame(name = vars[1], stats::median(vec, na.rm=T), agreement(vec), min(vec, na.rm=T), stats::quantile(vec, probs=c(.25), na.rm=T), stats::quantile(vec, probs=c(.75), na.rm=T), max(vec, na.rm=T), sum(!is.na(vec)))

  # Keep adding more for subsequent variables if they've been listed
  if (length(vars) > 1) {
    for (i in 2:length(vars)) {
      vec <- data[vars[i]][[1]]
      out <- rbind(out, data.frame(name = vars[i], stats::median(vec, na.rm=T), agreement(vec), min(vec, na.rm=T), stats::quantile(vec, probs=c(.25), na.rm=T), stats::quantile(vec, probs=c(.75), na.rm=T), max(vec, na.rm=T), sum(!is.na(vec))))
    }
  }

  names(out) <- c("names", "median", "agreement", "min", "25q", "75q", "max", "N")
  rownames(out) <- NULL #need to work out why it's adding them - probably due to use of quantile() and subsetting
  return(out)

}
