#' Smith normal form
#'
#' For an integer matrix M, this computes the matrices D, P, and Q
#' such that \emph{D = PMQ}, which can be seen as an analogue of the
#' singular value decomposition. All are integer matrices, and P and
#' Q are unimodular (have determinants +- 1).
#'
#' @param mat a matrix (integer entries)
#' @param code return only the M2 code? (default: \code{FALSE})
#' @name snf
#' @return a list of integer matrices D, P, and Q
#' @examples
#'
#' \dontrun{ requires Macaulay2 be installed and an interactive session
#'
#' ##### basic usage
#' ########################################
#'
#' M <- matrix(c(
#'    2,  4,   4,
#'   -6,  6,  12,
#'   10, -4, -16
#' ), nrow = 3, byrow = TRUE)
#'
#' snf(M)
#'
#' (mats <- snf(M))
#' P <- mats$P; D <- mats$D; Q <- mats$Q
#'
#' P %*% M %*% Q                # = D
#' solve(P) %*% D %*% solve(Q)  # = M
#'
#' det(P)
#' det(Q)
#'
#'
#' M <- matrix(c(
#'      1,    2,    3,
#'      1,   34,   45,
#'   2213, 1123, 6543,
#'      0,    0,    0
#' ), nrow = 4, byrow = TRUE)
#' (mats <- snf(M))
#' P <- mats$P; D <- mats$D; Q <- mats$Q
#' P %*% M %*% Q                # = D
#'
#'
#' ##### other options
#' ########################################
#'
#' snf.(M)
#' snf(M, code = TRUE)
#'
#'
#'
#'
#' }
#'






#' @rdname snf
#' @export
snf <- function (mat, code = FALSE) {

  # run m2
  args <- as.list(match.call())[-1]
  eargs <- lapply(args, eval, envir = parent.frame())
  pointer <- do.call(snf., eargs)
  if(code) return(invisible(pointer))

  # parse output
  parsed_out <- m2_parse(pointer)

  # list and out
  list(
    D = parsed_out[[1]],
    P = parsed_out[[2]],
    Q = parsed_out[[3]]
  )
}





#' @rdname snf
#' @export
snf. <- function (mat, code = FALSE) {

  # arg checking
  # if (is.m2_matrix(mat)) mat <- mat$rmatrix
  if (is.m2_pointer(mat)) {
    param <- m2_name(mat)
  } else {
    if (!is.integer(mat)) stopifnot(all(mat == as.integer(mat)))
    param <- paste0("matrix", listify_mat(mat))
  }

  # create code and message
  m2_code <- sprintf("smithNormalForm %s", param)
  if(code) { message(m2_code); return(invisible(m2_code)) }

  # run m2 and return pointer
  m2.(m2_code)
}




