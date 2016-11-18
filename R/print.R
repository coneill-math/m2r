#' Print a Macaulay2 object
#'
#' Print a Macaulay2 object
#'
#' @param x A Macaulay2 object
#' @param ... Ignored arguments
#' @return Invisible character vector of the printed object.
#' @name m2_print
#' @examples
#'
#' \dontrun{ requires Macaulay2 be installed
#'
#' R <- ring(c("x1", "x2", "x3"))
#' s <- print(R)
#' s
#'
#' m <- m2_matrix(matrix(c(1,2,3,4,5,6), nrow = 3, ncol = 2))
#' s <- print(m)
#' s
#'
#' }
#'

#' @export
#' @rdname m2_print
print.PolynomialRing <- function(x, ...){

  s <- sprintf(
    "M2 PolynomialRing: %s[%s], %s order",
    x$coefring, paste(x$vars, collapse = ","), x$order
  )
  cat(s)

  invisible(s)
}

#' @export
#' @rdname m2_print
print.Ideal <- function(x, ...){
  cat("M2 Ideal")
}


#' @export
#' @rdname m2_print
print.Matrix <- function(x, ...){

  s <- sprintf("M2 Matrix: %s", m2(x$m2_name))
  cat(s)

  invisible(s)
}








