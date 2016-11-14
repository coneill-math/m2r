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
#' (QQxy <- ring(c("x","y"), "QQ", "lex"))
#' p <- mp("x^4 - y^4")
#' factor_poly.(p, QQxy)
#' factor_poly(p, QQxy)
#' mp("(x-y) (x+y) (x^2+y^2)")
#'
#'
#' (QQxyz <- ring(c("x","y", "z"), "QQ", "lex"))
#' (p <- mp("(x^2 - y) (x^2 + y) (x + y)^2 (x - z)^2"))
#' factor_poly.(p, QQxyz)
#' factor_poly(p, QQxyz)
#'
#' (p <- mp("(x-1)^3 (y-1)^3"))
#' factor_poly.(p, QQxyz)
#' factor_poly(p, xyz)
#'
#' }
#'
factor_poly <- function (mpoly, ring, code = FALSE) {

  # run m2
  pointer <- do.call(factor_poly., as.list(match.call())[-1])

  # parse output
  parsed_out <- m2_parse(pointer)

  # reformat and out
  mpolyList <- lapply(parsed_out, function(.) mp(.[[1]]))
  class(mpolyList) <- "mpolyList"
  powers <- vapply(parsed_out, `[[`, integer(1), 2)
  list(mpolyList = mpolyList, power = powers)

}






factor_poly. <- function (mpoly, ring, code = FALSE, ...) {

  # basic arg checking
  if (ring$coefring != "QQ" & ring$coefring != "ZZ") {
    stop("factor_poly only supports coefficent rings ZZ or QQ")
  }

  # preparing param
  # if(is.m2_pointer(mpoly)) {
  #
  # } else {
  #
  # }

  # create code
  m2_mpoly <- mpolyList_to_m2_str(mpoly)
  m2_code <- sprintf("factor(%s)", m2_mpoly)

  # add ring name if desired
  if(!missing(ring)) {
    m2_code <- paste0(sprintf("use %s; ", ring$m2_name), m2_code)
  }

  # message
  if(code) message(m2_code)

  # run m2 and return pointer
  m2.(m2_code)


  # m2_out <- str_replace_all(m2_out, "\\*", " ")
  # m2_out <- str_split(m2_out, " ")[[1]]
  # m2_out <- lapply(m2_out, function(p) {
  #   if(str_sub(p, nchar(p)) == ")") return(c(p, "1"))
  #   str_split(str_sub(p, 2), ")\\^")[[1]]
  # })
  #
  # list(
  #   poly  = mp(vapply(m2_out, `[`, character(1), 1)),
  #   power = as.integer(vapply(m2_out, `[`, character(1), 2))
  # )
}
