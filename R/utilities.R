vfs_check <- function(S,M) {
  if(!is.numeric(S) | !is.atomic(S)) stop("S must be a numeric vector")
  if(!is.numeric(M) | !is.atomic(M)) stop("M must be a numeric vector")
  if (any(!is.na(M>S) & M>S)) stop("M cannot be larger than S")
}
