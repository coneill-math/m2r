#' Create a new matrix in Macaulay2
#'
#' Create a new matrix in Macaulay2
#'
#' @param mat a matrix
#' @param ring a ring containing the matrix entries
#' @param name the \code{m2_name} of the object, which is it's name
#'   on the M2 side
#' @param code return only the M2 code? (default: \code{FALSE})
#' @return a reference to a Macaulay2 ring
#' @name m2_matrix
#' @examples
#'
#' \dontrun{ requires Macaulay2 be installed
#'
#' (mymatrix <- m2_matrix(matrix(c(1,2,3,4,5,6), nrow = 3, ncol = 2)))
#'
#' mymatrix$m2_name
#' m2(mymatrix$m2_name)
#' m2(paste0("class(", mymatrix$m2_name, ")"))
#'
#' ring(c("x","y","z"))
#' mat <- matrix(mp(c("x","y","x+y","y-2","x-3","y-z")), nrow = 2, ncol = 3)
#' m2_matrix(mat)
#'
#'
#'
#' }



#' @rdname m2_matrix
#' @export
m2_matrix <- function(mat, ring, name, code = FALSE) {

  pointer <- do.call(m2_matrix., as.list(match.call())[-1])

  # construct R-side matrix, class and return
  matrix <- list(
    m2_name = pointer$m2_name,
    rmatrix = mat
  )
  class(matrix) <- c("m2", "m2_matrix")
  matrix
}



#' @rdname m2_matrix
#' @export
m2_matrix. <- function(mat, ring, name, code = FALSE) {

  # arg check
  if(!all( m2_exists(vars(mat)) )) {
    stop(sprintf(
      "all variables (%s) in mat must be defined in M2.",
      paste(vars(mat), collapse = ", ")
    ), call. = FALSE)
  }

  # prep ring string
  ring_str <- if(missing(ring)) "" else paste0("*1_", ring$m2_name)

  # make matrix name
  if(missing(name)) {
    matrix_name <- name_and_increment("matrix", "m2_matrix_count")
  } else {
    matrix_name <- name
  }


  # prepare matrix string
  mat2 <- matrix(
    vapply(mpolyList_to_m2_str(mat), function(.) sprintf("(%s)%s", ., ring_str), character(1)),
    nrow(mat), ncol(mat)
  )
  matrix_str <- listify_mat(mat2)

  # construct code and message
  # matrix{{1,2,3},{4,5,6}}
  m2_code <- sprintf("%s = matrix %s", matrix_name, matrix_str)
  if(code) { message(m2_code); return(invisible(m2_code)) }

  # run m2 and add name
  out <- m2.(m2_code)
  out$m2_name <- matrix_name

  # return
  out
}





m2_parse_function.m2_map <- function(x) {

  R1 <- x[[1]]
  R2 <- x[[2]]

  if (is.m2_module(R1)) R1 <- R1[[1]]
  if (is.m2_module(R2)) R2 <- R2[[1]]

  if (!identical(R1, R2)) {
    stop("Parsing error: map between different rings not supported")
  }

  if (is.integer(x[[3]]) && x[[3]] == 0) {
    if (is.null(R1$vars)) {
      mat <- matrix(numeric(0), nrow = 0, ncol = 0)
    } else {
      mat <- matrix(character(0), nrow = 0, ncol = 0)
    }
  } else {
    if (!is.list(x[[3]]) || !is.list(x[[c(3,1)]])) {
      stop("Parsing error: unsupported map format")
    }

    if (!is.null(R1$vars)) {
      # convert to mpolys
      for (i in 1:length(x[[3]])) {
        x[[c(3,i)]] = lapply(x[[c(3,i)]], function(.) mp(.))
      }
    }

    nrow <- length(x[[3]])
    ncol <- length(x[[c(3,1)]])

    if (is.null(R1$vars)) {
      mat <- t(matrix(unlist(x[[3]]), nrow = ncol, ncol = nrow))
    } else {
      mat <- t(matrix(unlist(x[[3]], recursive=FALSE), nrow = ncol, ncol = nrow))
    }

  }

  matrix <- list(
    m2_name = "",
    rmatrix = mat,
    ring = R1
  )
  class(matrix) <- c("m2_matrix", "m2")
  matrix
}
