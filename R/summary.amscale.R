#' Summarising Aldrich-McKelvey Models
#'
#' @param object An object of class 'amscale'.
#' @param digits Number of digits for rounding stimuli locations.
#'
#' @return \code{summary.amscale} computes and prints to console the most salient information from the Aldrich-McKelvey scaling procedure implemented in \code{amscale}.
#'
#' @export
#'
summary.amscale <- function(object, digits=3) {
  writeLines("Stimuli Locations:\n-----------------")
  stimNames <- names(object$stimuli)
  stimValues <- object$stimuli
  stimNamesLength <- max(nchar(stimNames))
  writeLines(
    paste0(
      paste0(stimNames, ": ") |> format(width=stimNamesLength + 2),
      round(stimValues, digits=digits) |> format(nsmall=1),
      collapse="\n"
    )
  )

  writeLines("")

  writeLines(
    paste0(
      c(
        "N. respondents input:    ",
        "N. respondents used:     ",
        "N. respondents filtered: ",
        "N. respondents dropped:  "
      ),
      c(
        object$n_input,
        object$n_used,
        object$n_filtered,
        object$n_dropped
      ),
      collapse = "\n"
    )
  )

  writeLines("")

  writeLines(paste0("Fit: ", round(object$fit, digits=digits)))
}
