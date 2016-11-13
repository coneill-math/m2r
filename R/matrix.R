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
#' mymatrix$m2_name
#' m2(mymatrix$m2_name)
#' m2(paste0("class(", mymatrix$m2_name, ")"))
#'
#' }
#'
matrix_m2 <- function(mat, ring, code = FALSE) {

  ret <- do.call(matrix_m2., as.list(match.call())[-1])

  # construct R-side matrix, class and return
  matrix <- list(
    m2_name = ret$m2_name,
    rmatrix = mat
  )
  class(matrix) <- c("m2", "m2_matrix")
  matrix
}

matrix_m2. <- function(mat, ring, code = FALSE) {

  if (missing(ring)) {
    ring_str <- ""
  } else {
    ring_str <- paste0("*1_", ring$m2_name)
  }

  # make matrix name
  matrix_name <- name_and_increment("matrix", "m2_matrix_count")

  # prepare matrix string
  mat2 <- matrix(
    lapply(mpolyList_to_m2_str(mat), function(.) paste0("(",. , ")", ring_str)),
    nrow(mat), ncol(mat)
  )
  matrix_str <- listify_mat(mat2)

  # construct code and message
  # matrix{{1,2,3},{4,5,6}}
  line <- sprintf("%s = matrix%s", matrix_name, matrix_str)
  if(code) message(line)

  # run m2
  ret <- m2.(line)

  ret$m2_name <- matrix_name
  ret
}

m2_parse_function.m2_map <- function(x) {

  # TODO: x ---->>>> mat


  matrix <- list(
    m2_name = NULL,
    rmatrix = mat
  )
  class(matrix) <- c("m2_matrix", "m2")
  matrix
}
