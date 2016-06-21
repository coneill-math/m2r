#' Create a new matrix in Macaulay2
#'
#' Create a new matrix in Macaulay2
#'
#' @param mat a matrix
#' @param ring a ring containing the matrix entries
#' @param code message code to user? (default = FALSE)
#' @return a reference to a Macaulay2 ring
#' @export
#' @examples
#'
#' \dontrun{ requires Macaulay2 be installed
#'
#' (mymatrix <- matrix_m2(matrix(c(1,2,3,4,5,6), nrow = 3, ncol = 2)))
#'
#' mymatrix$m2name
#' m2(mymatrix$m2name)
#' m2(paste0("class(", mymatrix$m2name, ")"))
#'
#' }
#'

matrix_m2 <- function(mat, ring = NA, code = FALSE) {

  if (missing(ring)) {
    ring_str <- ""
  } else {
    ring_str <- paste0("*1_", ring$m2name)
  }

  # make matrix name
  matrix_name <- name_and_increment("matrix", "m2_matrix_count")

  # prepare matrix string
  matrix_str <- listify( apply(mat, 1, listify) )

  # construct code and message
  # matrix{{1,2,3},{4,5,6}}
  line <- sprintf("%s = matrix%s", matrix_name, matrix_str)
  if(code) message(line)

  # run m2
  m2(line)

  # construct R-side matrix, class and return
  matrix <- list(
    m2name = matrix_name,
    rmatrix = mat
  )
  class(matrix) <- c("Matrix", "m2")
  matrix
}
