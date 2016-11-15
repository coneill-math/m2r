#' Factor an integer into primes
#'
#' Factor an integer into primes
#'
#' @param n an integer or a polynomial
#' @param code return only the M2 code? (default: \code{FALSE})
#' @param gmp (broken) logical; use multiple precision arithmetic?
#'   see \code{\link{as.bigz}}
#' @param ... ...
#' @return a data frame with integer columns \code{prime} and
#'   \code{power} or \code{m2_pointer} referencing the factorization
#'   in M2.
#' @name factor_n
#' @examples
#'
#' \dontrun{ requires Macaulay2 be installed
#'
#' ##### basic usage
#' ########################################
#'
#' 2^2 * 3^7 * 5^2 # = 218700
#' factor_n(218700)
#' factor_n.(218700)
#'
#' (df <- factor_n(218700))
#' df$prime
#' df$power
#' str(df)
#'
#'
#' factor_n(218700, code = TRUE)
#'
#'
#' ##### other options
#' ########################################
#'
#' (integer_pointer <- m2.("218700"))
#' integer_pointer$m2_name
#' factor_n(integer_pointer, code = TRUE)
#' factor_n(integer_pointer)
#'
#'
#'
#' factor_n(3234432540)
#' factor_n(323443254223453)
#' factor_n(rpois(1, 1e4))
#'
#'
#' ##### known issues
#' ########################################
#'
#' # R doesn't handle big ints well. note in the following
#' # the m2 code number is different than the supplied number
#' factor_n(32344325422364353453, code = TRUE)
#'
#' # this can be circumvented by passing a string instead
#' factor_n("32344325422364353453", code = TRUE)
#'
#' # but if the factors are large, R can't handle the parsing well
#' factor_n("32344325422364353453")
#'
#' # here's a workaround:
#' factor_pointer <- factor_n.("32344325422364353453")
#' factor_pointer$ext_str
#' extract_factors <- function(pointer) {
#'   require(stringr)
#'   str <- pointer$ext_str
#'   str <- str_sub(str, 19, -2)
#'   str <- str_extract_all(str, "\\{[0-9]+,[0-9]+\\}")[[1]]
#'   str <- str_sub(str, 2, -2)
#'   str <- str_split(str, ",")
#'   df <- as.data.frame(t(simplify2array(str)))
#'   names(df) <- c("prime", "power")
#'   df
#' }
#' (df <- extract_factors(factor_pointer))
#'
#'
#' # using gmp (currently broken)
#' # factor_n("32344325422364353453", gmp = TRUE)
#' m2("11 * 479 * 6138607975396537")
#' 11 * 479 * 6138607975396537
#'
#' }
#'




#' @rdname factor_n
#' @export
factor_n <- function (n, code = FALSE, gmp = FALSE, ...) {

  # run m2
  pointer <- do.call(factor_n., as.list(match.call())[-1])
  if(code) return(invisible(pointer))

  # parse output
  parsed_out <- m2_parse(pointer)

  # reformat
  df <- as.data.frame(matrix(unlist(parsed_out), ncol = 2, byrow = TRUE))
  names(df) <- c("prime", "power")

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
  df
}


#' @rdname factor_n
#' @export
factor_n. <- function (n, code = FALSE, ...) {

  # arg checking
  # compute factor with m2, e.g. "2^2*67*97*9433"
  if (is.m2_pointer(n)) {
    param <- n$m2_name
  } else {
    if (is.numeric(n)) {
      param <- as.character(format(n, scientific = FALSE)) # for big Z's
    } else {
      param <- n
    }
  }

  # create code and message
  m2_code <- sprintf("factor %s", param)
  if(code) { message(m2_code); return(invisible(m2_code)) }

  # run m2 and return pointer
  m2.(m2_code)

}



