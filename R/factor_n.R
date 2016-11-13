#' Factor an integer into primes
#'
#' Factor an integer into primes
#'
#' @param n an integer or a polynomial
#' @param code logical; message code to user? (default: \code{FALSE})
#' @param gmp logical; use multiple precision arithmetic? see
#'   \code{\link{as.bigz}}
#' @return if \code{gmp = FALSE}, a character or integer matrix with
#'   columns named \code{prime} and \code{power}. if all the numbers
#'   have fewer than 10 digits, the matrix returned contains
#'   integers; otherwise it contains characters. if \code{gmp =
#'   TRUE}, a big integer matrix.
#' @export
#' @examples
#'
#' \dontrun{ requires Macaulay2 be installed
#'
#' 2^2 * 3^7 * 5^2 # = 218700
#' factor_n(218700)
#' factor_n.(218700)
#'
#' factor_n(3234432540)
#' factor_n(323443254223453)
#' factor_n(rpois(1, 1e4))
#'
#' # notes on big integers
#' factor_n("32344325422364353453") # parses improperly; int too big
#' m2_parse(factor_n.("32344325422364353453")$ext_str)
#' # factor_n("32344325422364353453", gmp = TRUE)
#' m2("11 * 479 * 6138607975396537")
#' 11 * 479 * 6138607975396537
#'
#' }
#'
factor_n <- function (n, code = FALSE, gmp = FALSE) {

  # run m2
  pointer <- do.call(factor_n., as.list(match.call())[-1])

  # parse output
  parsed_out <- m2_parse(pointer$ext_str)

  # reformat
  mat <- matrix(unlist(parsed_out), ncol = 2, byrow = TRUE)
  colnames(mat) <- c("prime", "power")

  # convert to int
  # if (gmp) {
  #   mat <- as.bigz(mat)
  # } else if( all(nchar(mat) < 10) ) { # conv 2 int if small
  #   dim <- dim(mat)
  #   mat <- as.integer(mat)
  #   dim(mat) <- dim
  #   colnames(mat) <- c("prime", "power")
  # } else { # if char array
  #   colnames(mat) <- c("prime", "power")
  # }

  # return
  mat
}



factor_n. <- function (n, code = FALSE, ...) {

  # arg checking
  # compute factor with m2, e.g. "2^2*67*97*9433"
  if (is.m2_pointer(n)) {
    param <- n$m2_name
  } else {
    if (is.numeric(n)) {
      param <- as.character(format(n,scientific = F)) # for big Z's
    } else {
      param <- n
    }
  }

  # create code and message
  m2_code <- sprintf("factor %s", param)
  if(code) message(m2_code)

  # run m2 and return pointer
  m2.(m2_code)

}



