#' Factor a polynomial
#'
#' Factor a polynomial
#'
#' @param mpoly a character parseable by \code{\link{mp}}, an
#'   \code{mpoly} object, or a pointer to a polynomial in M2
#' @param ring a \code{\link{ring}} object or a pointer to such an
#'   object
#' @param code return only the M2 code? (default: \code{FALSE})
#' @param ... ...
#' @return a named list with elements \code{factor} (an
#'   \code{\link{mpolyList}} object) and \code{power}, an integer
#'   vector
#' @name factor_poly
#' @examples
#'
#' \dontrun{ requires Macaulay2 be installed and an interactive session
#'
#' ##### basic usage
#' ########################################
#'
#' (QQxy <- ring(c("x","y"), "QQ"))
#' factor_poly("x^4 - y^4", QQxy)
#'
#' p <- mp("x^4 - y^4")
#' factor_poly.(p, QQxy)
#' factor_poly(p, QQxy)
#' factor_poly(p, QQxy, code = TRUE)
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
#' factor_poly(p, QQxyz)
#'
#' }



#' @rdname factor_poly
#' @export
factor_poly <- function (mpoly, ring, code = FALSE) {

  # run m2
  args <- as.list(match.call())[-1]
  eargs <- lapply(args, eval, envir = parent.frame())
  pointer <- do.call(factor_poly., eargs)
  if(code) return(invisible(pointer))

  # parse output
  parsed_out <- m2_parse(pointer)

  # reformat and return
  list(
    factor = structure(
      lapply(parsed_out, function(.) mp(.[[1]])),
      class = "mpolyList"
    ),
    power = vapply(parsed_out, `[[`, integer(1), 2)
  )

}





#' @rdname factor_poly
#' @export
factor_poly. <- function (mpoly, ring, code = FALSE, ...) {

  # basic arg checking
  if (!is.m2_pointer(ring) &&
      m2_meta(ring, "coefring") != "QQ" &&
      m2_meta(ring, "coefring") != "ZZ") {
    stop("factor_poly only supports coefficent rings ZZ or QQ")
  }

  # prepare mpoly param
  if (is.m2_pointer(mpoly)) {
    mpoly_param <- m2_name(mpoly)
  } else if (is.mpoly(mpoly) || is.mpolyList(mpoly)) {
    mpoly_param <- mpolyList_to_m2_str(mpoly)
  } else {
    mpoly_param <- as.character(mpoly)
  }

  # prepare ring param (if present)
  if (!missing(ring)) {
    # prepare mpoly param
    if (is.m2_pointer(ring)) {
      ring_param <- m2_name(ring)
    } else if (is.character(ring)) {
      ring_param <- ring
    } else if (is.ring(ring)) {
      ring_param <- m2_name(ring)
    }
  }

  # create code
  m2_code <- sprintf("factor(%s)", mpoly_param)

  # add ring name if desired
  if (!missing(ring)) {
    m2_code <- paste0(sprintf("use %s; ", ring_param), m2_code)
  }

  # message
  if (code) { message(m2_code); return(invisible(m2_code)) }

  # run m2 and return pointer
  m2.(m2_code)

}
