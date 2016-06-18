#' Give the structure of a Macaulay2 ring
#'
#' Give the structure of a Macaulay2 ring
#'
#' @param object A \code{PolynomialRing} object
#' @param ... Ignored arguments
#' @usage \method{str}{PolynomialRing}(object, ...)
#' @return Invisible the object passed in.
#' @export
#' @examples
#'
#' \dontrun{ requires Macaulay2 be installed
#'
#' R <- ring(c("x1", "x2", "x3"))
#' str(R)
#'
#' }
#'
str.PolynomialRing <- function(object, ...){

  cat("M2 Object\n")
  cat("    Type : PolynomialRing\n")
  cat(sprintf("    Vars : %s\n", paste(object$vars, collapse = ", ")))
  cat(sprintf("   Order : %s\n", object$order))
  cat(sprintf("  R Name : %s\n", deparse(substitute(object))))
  cat(sprintf(" M2 Name : %s\n", object$m2name))

  invisible(object)
}
