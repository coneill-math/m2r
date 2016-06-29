#' Smith normal form
#'
#' For an integer matrix M, this computes the matrices D, P, and Q
#' such that \emph{D = PMQ}, which can be seen as an analogue of the
#' singular value decomposition. All are integer matrices, and P and
#' Q are unimodular (have determinants +- 1).
#'
#' @param mat a matrix (integer entries)
#' @param code logical; message code to user? (default = FALSE)
#' @return a list of integer matrices D, P, and Q
#' @export
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
#' (mats <- snf(M))
#' P <- mats$P; D <- mats$D; Q <- mats$Q
#'
#' P %*% M %*% Q                # = D
#' solve(P) %*% D %*% solve(Q)  # = M
#'
#' det(P)
#' det(Q)
#'
#' }
#'
snf <- function (mat, code = FALSE) {

  if(!is.integer(mat)) stopifnot(all(mat == as.integer(mat)))

  # create code and run
  m2_code <- sprintf("smithNormalForm matrix%s", listify_mat(mat))
  if(code) message(m2_code)
  char <- m2(m2_code)

  # clean output:
  # "(matrix {{12, 0, 0}, {0, 6, 0}, {0, 0, 2}},matrix {{2, 0, -1}, {1, 1, 0}, {1, 0, 0}},matrix {{-3, -1, 2}, {4, 3, -3}, {3, 2, -2}})"
  char <- str_sub(char, 9, -2)
  char <- str_split(char, ",matrix ")[[1]]

  # make into list and return
  list(
    D = delistify(char[1], as.integer, rbind),
    Q = t(delistify(char[2], as.integer, rbind)),
    P = t(delistify(char[3], as.integer, rbind))
  )
}



