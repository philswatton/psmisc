#' Summarising Aldrich-McKelvey Models
#'
#' @param object An object of class 'amscale'.
#' @param digits Number of digits for rounding stimuli locations.
#'
#' @return \code{summary.amscale} computes and presents the most salient information from the Aldrich-McKelvey scaling procedure implemented in \code{amscale}.
#' @export
#'
summary.amscale <- function(object, digits=3) {

  cat("\nNumber of respondents: ", object$ninput, sep="")
  cat("\nNumber of respondents used in scaling: ", object$nresp, sep="")
  cat("\nPercentage of respondents used: ", round((object$nresp/object$ninput)*100, digits=1), "%", sep="")
  cat("\n\n", sep="")
  cat("Number of stimuli scaled: ", object$nstim, sep="")
  cat("\nModel fit: ", object$fit, sep="")
  cat("\n\n", sep="")

  cat("\nStimuli Location:\n")
  stim <- matrix(round(object$stimuli, digits=digits), ncol=1)
  rownames(stim) <- names(object$stimuli)
  colnames(stim) <- "Location"
  print(stim)

}
