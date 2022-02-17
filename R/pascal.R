#' Pascal's Triangle
#'
#' A function to extract a single element from Pascal's triangle. Note that Pascal's
#' triangle uses 0-based indexing, so the first row is row 0 and the first element in
#' a row is the 0th element.
#'
#' @param rownum A length one numeric vector containing an integer value denoting the row of Pascal's triangle to extract the desired element from.
#' @param element NULL or a length one numeric vector containing an integer value denoting the element within the specified row of Pascal's triangle to extract. Required if \code{rownum} > 1.
#'
#' @return A length one numeric vector containing the element from Pascal's triangle to be returned.
#' @export
#'
#' @examples
#' pascal(0)
#' pascal(2,2)
#' pascal(4,3)
#'
pascal <- function(rownum, element=NULL) {

  # Input Validation
  if (!is.numeric(rownum)) {
    stop("Error: rownum must be numeric")
  } else if (rownum %% 1 != 0) {
    stop("Error: value of rownum must be integer")
  } else if (length(rownum) > 1) {
    stop("Error: rownum should be a single number")
  }

  if (!is.null(element)) {
    if (!is.numeric(element)) {
      stop("Error: element must be NULL or numeric")
    } else if (element %% 1 != 0) {
      stop("Error: value of element must be integer")
    } else if (length(element) > 1) {
      stop("Error: element should be a single number")
    } else if (element > rownum) {
      stop("Error: element must be less than or equal to rownum")
    }
  }

  if (rownum > 0 & is.null(element)) {
    stop("Error: For rownum greater than 1, element must be specified")
  }

  # Code
  if (rownum == 0) {
    return(1)
  } else if (element %in% c(0, rownum)) {
    return(1)
  } else {
    return(pascal(rownum - 1, element - 1) + pascal(rownum - 1, element))
  }

}
