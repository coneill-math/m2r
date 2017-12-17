#' LLL algorithm
#'
#' Macaulay2's implementation of the LLL algorithm. This implementation is still
#' under development and is currently untested.
#'
#' @param mat a matrix (integer entries)
#' @param code return only the M2 code? (default: \code{FALSE})
#' @name LLL
#' @seealso
#' \url{https://en.wikipedia.org/wiki/Lenstra–Lenstra–Lovász_lattice_basis_reduction_algorithm}
#'
#' @return a list of integer matrices D, P, and Q
#' @examples
#'
#' \dontrun{ requires Macaulay2
#'
#' ##### basic usage
#' ########################################
#'
#' M <- matrix(c(
#'   1, 1, 1, 1,
#'   2, 0, 3, 4,
#'   1, 0, 0, 0,
#'   0, 1, 0, 0,
#'   0, 0, 1, 0,
#'   0, 0, 0, 1
#' ), nrow = 6, byrow = TRUE)
#'
#' LLL(M)
#'
#'
#'
#' ##### other options
#' ########################################
#'
#' LLL.(M)
#' LLL(M, code = TRUE)
#'
#'
#'
#' }
#'






#' @rdname LLL
#' @export
LLL <- function (mat, code = FALSE) {

  # run m2
  args <- as.list(match.call())[-1]
  eargs <- lapply(args, eval, envir = parent.frame())
  pointer <- do.call(LLL., eargs)
  if(code) return(invisible(pointer))

  # parse output
  m2_parse(pointer)
}





#' @rdname LLL
#' @export
LLL. <- function (mat, code = FALSE) {

  # arg checking
  # if (is.m2_matrix(mat)) mat <- mat$rmatrix
  if (is.m2_pointer(mat)) {
    param <- m2_name(mat)
  } else {
    if (!is.integer(mat)) stopifnot(all(mat == as.integer(mat)))
    param <- paste0("matrix", listify_mat(mat))
  }

  # create code and message
  m2_code <- sprintf("LLL %s", param)
  if(code) { message(m2_code); return(invisible(m2_code)) }

  # run m2 and return pointer
  m2.(m2_code)
}




