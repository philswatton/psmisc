#' Title Table of Summary Statistics for Ordinal Data
#'
#' @param data Dataframe containing the desired variables.
#' @param vars Character or numeric vector containing integers naming or indexing the columns containing the ordinal variables you wish to summarise.
#' @param compute Character vector instructing ordTable which descriptive statistics to calculate
#' @param values NOT YET IMPLEMENTED
#'
#' @return A dataframe containing the median, Van der Ejik's agreement statistic, min, 25th quantile, 75th quantile, max, N.
#' @export
#'
#' @examples
ordTable <- function(data, vars, compute=c("median", "agreement", "min", "25q", "75q", "max", "N"), values=NULL) {

  # Function for producing descriptive stat tables of ordinal data

  # TODO (inputs):
  # - Allow 'vars' to be NULL and simply taking a complete df as input
  # - Input validation (data is df or matrix, vars is an atomic vector of characters or integers, etc)
  #   - Possibly give option for user to manually specify names - will be important if matrices are taken as input
  # - Implement the 'values' option to allow a list of NULLs & ranges to be given

  # TODO (behaviour):
  # - Consider implementing other measures of ordinal dispersion in the package to allow the user to choose which to report
  # - Search for other ordinal-specific measures






  # Generate dataframe function conditional on compute vector
  makeRow <- function(x, name) {

    row <- data.frame(name = name)

    if ("median" %in% compute) {
      row$median <- stats::median(x, na.rm=T)
    }

    if ("agreement" %in% compute) {
      row$agreement <- agreement(x)
    }

    if ("min" %in% compute) {
      row$min <- min(vec, na.rm=T)
    }

    if ("25q" %in% compute) {
      row$`25q` = stats::quantile(vec, probs=c(.25), na.rm=T)
    }

    if ("75q" %in% compute) {
      row$`75q` = stats::quantile(vec, probs=c(.75), na.rm=T)
    }

    if ("max" %in% compute) {
      row$max <- max(vec, na.rm=T)
    }

    if ("N" %in% compute) {
      row$N <- sum(!is.na(x))
    }

    # Put into order specified by the user
    row <- row[c("name",compute)]

    return(row)
  }



  # Initialise for first variable
  vec <- data[vars[1]][[1]]
  # out <- data.frame(name = vars[1], stats::median(vec, na.rm=T), agreement(vec), min(vec, na.rm=T), stats::quantile(vec, probs=c(.25), na.rm=T), stats::quantile(vec, probs=c(.75), na.rm=T), max(vec, na.rm=T), sum(!is.na(vec)))
  out <- makeRow(vec, vars[1])


  # Keep adding more for subsequent variables if they've been listed
  if (length(vars) > 1) {
    for (i in 2:length(vars)) {
      vec <- data[vars[i]][[1]]
      # out <- rbind(out, data.frame(name = vars[i], stats::median(vec, na.rm=T), agreement(vec), min(vec, na.rm=T), stats::quantile(vec, probs=c(.25), na.rm=T), stats::quantile(vec, probs=c(.75), na.rm=T), max(vec, na.rm=T), sum(!is.na(vec))))
      out <- rbind(out, makeRow(vec, vars[i]))
    }
  }

  # Return the output
  return(out)

}
