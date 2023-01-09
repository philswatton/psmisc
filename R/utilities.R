vfs_check <- function(S,M,t) {
  if (!is.numeric(S) | !is.atomic(S)) stop("S must be a numeric vector")
  if (!is.numeric(M) | !is.atomic(M)) stop("M must be a numeric vector")
  if (any(S < 0 & !is.na(S))) stop("All values of S must be greater than 0")
  if (any(M < 0 & !is.na(M))) stop("All values of M must be greater than 0")
  if (!is.null(t) & (!is.numeric(t) | !is.atomic(t))) stop("t must either be NULL or a numeric vector")
  if (length(S) != length(M)) stop("S and M must have the same length")
  if (!is.null(t)) {
    if (length(t) != length(S)) stop("t must have the same length as S and M")
  }
  if (any((t < 0 | t > 1) & !is.na(t))) stop("values of t must range between 0 and 1")
  if (any(!is.na(M>S) & M>S)) stop("M cannot be larger than S")
}
