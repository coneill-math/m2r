#' Print a Macaulay2 ring
#'
#' Print a Macaulay2 ring
#'
#' @param x A \code{PolynomialRing} object
#' @param ... Ignored arguments
#' @usage \method{print}{PolynomialRing}(x, ...)
#' @return Invisible character vector of the printed object.
#' @export
#' @examples
#'
#' \dontrun{ requires Macaulay2 be installed
#'
#' ring(c("x1", "x2", "x3"))
#' s <- print(ring(c("x1", "x2", "x3")))
#' s
#'
#' }
#'
print.PolynomialRing <- function(x, ...){

  s <- sprintf(
    "M2 PolynomialRing: %s[%s]  (%s)",
    x$coefring, paste(x$vars, collapse = ","), x$order
  )
  cat(s)

  invisible(s)
}








