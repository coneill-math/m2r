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
  m2_out_vec <- substring(m2_out, seq(1,nchar(m2_out),1), seq(1,nchar(m2_out),1))

  paren_count <- 0
  factor_vec <- c()
  power_vec <- c()
  start_index <- 1
  last_close_paren_index <- 0
  for (i in 1:length(m2_out_vec)){
    char <- m2_out_vec[i]
    if (char == "(") {
      paren_count <- paren_count + 1
      # We have found a new factor
      if((paren_count == 1) & (i != 1)) {
        # i-2 because the current character is an open paren and the previous
        # character is a *
        factor_str <- substring(m2_out, start_index, last_close_paren_index)
        factor_vec <- c(factor_vec, (factor_str))
        if(m2_out_vec[last_close_paren_index + 1] == "^"){
          power_str <- substring(m2_out,last_close_paren_index + 2,i-2)
        } else {
          power_str <- 1
        }
        power_vec <- c(power_vec, power_str)
        start_index <- i
      }
    }
    if (char == ")") {
      paren_count <- paren_count - 1
      last_close_paren_index <- i
    }
  }

  # This case happens if the user inputs a polynomial without any variables.
  if(last_close_paren_index == 0){
    factor_vec <- c(mp(m2_out))
    power_vec <- c(1)
  } else {
    # This case takes care of the last factor of the polynomial
    factor_str <- substring(m2_out, start_index, last_close_paren_index)
    factor_vec <- c(factor_vec, (factor_str))
    if (nchar(m2_out) != last_close_paren_index){
      if(m2_out_vec[last_close_paren_index + 1] == "^"){
        power_str <- substring(m2_out,last_close_paren_index + 2,nchar(m2_out))
    }} else {
      power_str <- 1
    }
    power_vec <- c(power_vec, power_str)
  }
  matrix(c(factor_vec,power_vec),ncol=2)
}
