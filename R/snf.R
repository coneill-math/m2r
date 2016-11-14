#' Smith normal form
#'
#' For an integer matrix M, this computes the matrices D, P, and Q
#' such that \emph{D = PMQ}, which can be seen as an analogue of the
#' singular value decomposition. All are integer matrices, and P and
#' Q are unimodular (have determinants +- 1).
#'
#' @param mat a matrix (integer entries)
#' @param code logical; message code to user? (default = FALSE)
#' @name snf
#' @return a list of integer matrices D, P, and Q
#' @examples
#'
#' \dontrun{ requires Macaulay2 be installed and an interactive session
#'
#' M <- matrix(c(
#'    2,  4,   4,
#'   -6,  6,  12,
#'   10, -4, -16
#' ), nrow = 3, byrow = TRUE)
#'
#' snf.(M)
#' snf.(M, code = TRUE)
#' (mats <- snf(M))
#' P <- mats$P; D <- mats$D; Q <- mats$Q
#'
#' P %*% M %*% Q                # = D
#' solve(P) %*% D %*% solve(Q)  # = M
#'
#' det(P)
#' det(Q)
#'
#' snf.(matrix_m2.(M), code = TRUE)
#' snf(matrix_m2.(M), code = TRUE)
#'
#'
#' }
#'






#' @rdname snf
#' @export
snf <- function (mat, code = FALSE) {

  # run m2
  pointer <- do.call(snf., as.list(match.call())[-1])

  # parse output
  parsed_out <- m2_parse(pointer)

  # list and out
  list(
    D = parsed_out[[1]]$rmatrix,
    Q = t(parsed_out[[2]]$rmatrix),
    P = t(parsed_out[[3]]$rmatrix)
  )

}





#' @rdname snf
#' @export
snf. <- function (mat, code = FALSE) {

  # arg checking
  if (is.m2_matrix(mat)) mat <- mat$rmatrix
  if (is.m2_pointer(mat)) {
    param <- mat$m2_name
  } else {
    if (!is.integer(mat)) stopifnot(all(mat == as.integer(mat)))
    param <- paste0("matrix", listify_mat(mat))
  }

  # create code and message
  m2_code <- sprintf("smithNormalForm %s", param)
  if(code) message(m2_code)

  # run m2 and return pointer
  m2.(m2_code)
}




