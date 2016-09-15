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
#' (myring <- ring(c("x","y"), coefring = "QQ", order = "lex"))
#' p <- mp("x^4 - y^4")
#' factor_poly(p, myring)
#'
#'
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
  m2_out <- str_replace_all(m2_out, "\\*", " ")
  m2_out <- str_split(m2_out, " ")[[1]]
  m2_out <- lapply(m2_out, function(p) {
    if(str_sub(p, nchar(p)) == ")") return(c(p, "1"))
    str_split(str_sub(p, 2), ")\\^")[[1]]
  })

  list(
    poly  = mp(vapply(m2_out, `[`, character(1), 1)),
    power = as.integer(vapply(m2_out, `[`, character(1), 2))
  )
}
