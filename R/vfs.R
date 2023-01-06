#' Votes from Seats Laws
#'
#' Implementations of the four laws from the book **Votes from Seats**.
#'
#' @param S Vector of assembly sizes.
#' @param M Vector of average district sizes.
#'
#' @details
#'
#' These functions implement the four fundamental laws from the book
#' **Votes from Seats** in R.
#'
#' These laws are:
#'
#' **Number of effective parties in terms of seats:**
#' \deqn{N_s = (MS)^\frac{1}{6}}{Ns = (MS)^(1/6)}
#'
#' **Seat share of the largest party**
#' \deqn{s_1 = (MS)^\frac{-1}{8}}{s1 = (MS)^(-1/8)}
#'
#' **Number of effective parties in terms of votes:**
#' \deqn{N_v = ((MS)^\frac{1}{4}+1)^(\frac{2}{3})}{Nv = ((MS)^(1/4)+1)^(2/3)}
#'
#' **Vote share of the largest party**
#' \deqn{v_1 = ((MS)^\frac{1}{4}+1)^(\frac{-1}{2})}{v1 = ((MS)^(1/4)+1)^(-1/2)}
#'
#'
#' @return
#'
#' \code{Ns}, \code{s1}, \code{Nv}, \code{v1} all return a vector of
#' numeric values. \code{vfs} returns a dataframe containing six columns,
#' \code{S}, \code{M}, and a column for each of the measures.
#'
#' @references
#' \insertRef{shugart2017}{psmisc}
#'
#' @export
#'
vfs <- function(S, M, ret_SM=T) {

  outdf <-  data.frame(Ns=psmisc::Ns(S,M),
                       s1=psmisc::s1(S,M),
                       Nv=psmisc::Nv(S,M),
                       v1=psmisc::v1(S,M)
  )

  if (ret_SM) {

    outdf <- cbind(data.frame(S=S,
                              M=M),
                   outdf)

  }

  return(outdf)

}

#' @rdname vfs
#' @export
Ns <- function(S, M) {
  psmisc:::vfs_check(S,M)
  return((M*S)**(1/6))
}

#' @rdname vfs
#' @export
s1 <- function(S, M) {
  psmisc:::vfs_check(S,M)
  return((M*S)**(-1/8))
}

#' @rdname vfs
#' @export
Nv <- function(S, M) {
  psmisc:::vfs_check(S,M)
  return(((M*S)**(1/4)+1)**(2/3))
}

#' @rdname vfs
#' @export
v1 <- function(S, M) {
  psmisc:::vfs_check(S,M)
  return(((M*S)**(1/4)+1)**(-1/2))
}
