#' Macaulay2 object tests
#'
#' Predicate functions for Macaulay2 objects.
#'
#' @param x an object
#' @return logical(1)
#' @name is
#' @examples
#'
#' \dontrun{ requires Macaulay2 be installed
#'
#' R <- ring(c("x1", "x2", "x3"))
#' is.m2(R)
#' is.ring(R)
#' is.ring(10)
#' is.ring(mp("x+1"))
#'
#' }


#' @export
#' @rdname is
is.m2 <- function (x) {
  inherits(x, "m2")
}


#' @export
#' @rdname is
is.ring <- function (x) {
  inherits(x, "PolynomialRing")
}

