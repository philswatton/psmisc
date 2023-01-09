#' Votes from Seats Laws
#'
#' Implementations of the four laws from the book **Votes from Seats**.
#'
#' @param S Vector of assembly sizes. If \code{t} is provided, the vector of basic tier assembly sizes.
#' @param M Vector of average district sizes.  If \code{t} is provided, the vector of basic tier average district sizes.
#' @param t Optional, vector of upper tier seat shares.
#' @param ret_SM A \code{TRUE} or \code{FALSE} value denoting whether or not to return the values of \code{S} and \code{M}, and if supplied, \code{t}
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
#' **Seat share of the largest party:**
#' \deqn{s_1 = (MS)^\frac{-1}{8}}{s1 = (MS)^(-1/8)}
#'
#' **Number of effective parties in terms of votes:**
#' \deqn{N_v = ((MS)^\frac{1}{4}+1)^\frac{2}{3}}{Nv = ((MS)^(1/4)+1)^(2/3)}
#'
#' **Vote share of the largest party:**
#' \deqn{v_1 = ((MS)^\frac{1}{4}+1)^\frac{-1}{2}}{v1 = ((MS)^(1/4)+1)^(-1/2)}
#'
#' If \code{t} is supplied, then these are instead calculated as:
#'
#' **Number of effective parties in terms of seats:**
#' \deqn{N_s = 2.5^t (MS)^\frac{1}{6}}{Ns = (2.5^t)*(MS)^(1/6)}
#'
#' **Seat share of the largest party:**
#' \deqn{s_1 = 0.5^t (MS)^\frac{1}{8}}{s1 = (0.5^t)*(MS)^(1/8)}
#'
#' **Number of effective parties in terms of votes:**
#' \deqn{N_v = (4^t (MS)^\frac{1}{4} + 1)^\frac{2}{3}}{Nv = ((4^t)*((MS)^(1/4)) + 1)^(2/3)}
#'
#' **Vote share of the largest party:**
#' \deqn{v_1 = (4^t (MS)^\frac{1}{4} + 1)^\frac{-1}{2}}{v1 = ((4^t)*((MS)^(1/4)) + 1)^(-1/2)}
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
vfs <- function(S, M, t=NULL, ret_SM=T) {
  if (length(ret_SM) != 1) stop("ret_SM must be length 1")
  if (!ret_SM %in% c(T,F)) stop("ret_SM must be TRUE or FALSE")
  outdf <-  data.frame(Ns=psmisc::Ns(S,M,t),
                       s1=psmisc::s1(S,M,t),
                       Nv=psmisc::Nv(S,M,t),
                       v1=psmisc::v1(S,M,t)
  )
  if (ret_SM) {
    SM <- cbind(data.frame(S=S, M=M))
    if (!is.null(t)) SM <- cbind(SM,t=t)
    outdf <- cbind(SM, outdf)
  }
  return(outdf)
}

#' @rdname vfs
#' @export
Ns <- function(S, M, t=NULL) {
  psmisc:::vfs_check(S,M,t)
  if(is.null(t)) return((M*S)**(1/6))
  if(!is.null(t)) return((2.5**t)*(M*S)**(1/6))
}

#' @rdname vfs
#' @export
s1 <- function(S, M, t=NULL) {
  psmisc:::vfs_check(S,M,t)
  if(is.null(t)) return((M*S)**(-1/8))
  if(!is.null(t)) return((0.5**t)*(M*S)**(1/8))
}

#' @rdname vfs
#' @export
Nv <- function(S, M, t=NULL) {
  psmisc:::vfs_check(S,M,t)
  if(is.null(t)) return(((M*S)**(1/4)+1)**(2/3))
  if(!is.null(t)) return(((4**t)*((M*S)**(1/4)) + 1)**(2/3))
}

#' @rdname vfs
#' @export
v1 <- function(S, M, t=NULL) {
  psmisc:::vfs_check(S,M,t)
  if(is.null(t)) return(((M*S)**(1/4)+1)**(-1/2))
  if(!is.null(t)) return(((4**t)*((M*S)**(1/4)) + 1)**(-1/2))
}
