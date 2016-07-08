#' Factor an integer into primes
#'
#' Factor an integer into primes
#'
#' @param n an integer or a polynomial
#' @param gmp logical; use multiple precision arithmetic? see
#'   \code{\link{as.bigz}}
#' @param code logical; message code to user? (default = FALSE)
#' @return if \code{gmp = FALSE}, a character or integer matrix with
#'   columns named \code{prime} and \code{power}. if all the numbers
#'   have fewer than 10 digits, the matrix returned contains
#'   integers; otherwise it contains characters. if \code{gmp =
#'   TRUE}, a big integer matrix.
#' @export
#' @examples
#'
#' \dontrun{ requires Macaulay2 be installed and an interactive session
#'
#' factor_n(2^2*3^7*5^2)
#' factor_n(3234432540)
#' factor_n(323443254223453)
#' factor_n(rpois(1, 1e4))
#' factor_n("32344325422364353453")
#' factor_n("32344325422364353453", gmp = TRUE)
#' m2("11 * 479 * 6138607975396537")
#' 11 * 479 * 6138607975396537
#'
#' }
#'
factor_n <- function (n, gmp = FALSE, code = FALSE) {

  # compute factor with m2, e.g. "2^2*67*97*9433"
  if(is.numeric(n)) n <- as.character(format(n,scientific = F)) # for big Z's
  m2_code <- sprintf("factor %s", n)
  if(code) message(m2_code)
  char <- m2(m2_code)

  # break it into factors, e.g. c("2^2", "67", "97", "9433")
  chars <- str_split(char, "\\*")[[1]]

  # make a list with the primes and their powers
  list <- lapply(chars, function (factor) {
    if(str_detect(factor, fixed("^"))) {
      return( str_split(factor, fixed("^"))[[1]] )
    } else {
      return( c(factor, 1L) )
    }
  })

  # reformat
  mat <- t(simplify2array(list))

  # convert to int
  if (gmp) {
    mat <- as.bigz(mat)
  } else if( all(nchar(mat) < 10) ) { # conv 2 int if small
    dim <- dim(mat)
    mat <- as.integer(mat)
    dim(mat) <- dim
    colnames(mat) <- c("prime", "power")
  } else { # if char array
    colnames(mat) <- c("prime", "power")
  }

  # return
  mat
}



