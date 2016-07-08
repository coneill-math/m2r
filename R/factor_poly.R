#' Factor a polynomial
#'
#' Factor a polynomial
#'
#' @param mpoly of class mpoly
#' @param ring of class PolynomialRing
#' @param code logical; message code to user? (default = FALSE)
#' @return a string representation of the factored polynomial.
#' @export
#' @examples
#'
#' \dontrun{ requires Macaulay2 be installed and an interactive session
#'
#' (myring <- ring(c("x1","x2"), coefring = "QQ", order = "lex"))
#' factor_poly(mp("x1^4 - x2^4"), myring)
#' }
#'

factor_poly <- function (mpoly, ring, code = FALSE) {
  if ((ring$coefring != "QQ") & (ring$coefring != "ZZ")) {
    stop("factor_poly only supports rings with coefficient in ZZ or QQ")
  }
  m2_mpoly <- mpolyList_to_m2_str(mpoly)
  m2_code <- sprintf("use %s; factor (%s)",ring$m2name, m2_mpoly)

  if(code) message(m2_code)
  m2_out <- m2(m2_code)
  m2_out
}
