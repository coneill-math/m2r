#' Compute a Grobner basis with Macaulay2
#'
#' Compute a Grobner basis with Macaulay2
#'
#' \code{gb} uses nonstandard evaluation; \code{gb_} is the standard
#' evaluation equivalent.
#'
#' @param x a character vector of polynomials to be parsed by
#'   \code{\link{mp}}, a \code{mpolyList} object, an
#'   \code{\link{ideal}} or pointer to an ideal
#' @param ring if \code{x} is an ideal, \code{ring} need not be
#'   specified.
#' @param degree_limit parameter that stops computation after
#'   S-polynomials have reached a given degree. Only meaningful in
#'   homogeneous case.
#' @param raw_chars if \code{TRUE}, the character vector will not be
#'   parsed by \code{\link{mp}}, saving time (default:
#'   \code{FALSE}). the down-side is that the strings must be
#'   formated for M2 use directly, as opposed to for
#'   \code{\link{mp}}. (e.g. \code{"x*y+3"} instead of \code{"x y +
#'   3"})
#' @param code return only the M2 code? (default: \code{FALSE})
#' @param ... ...
#' @return an mpolyList object
#' @seealso \code{\link{mp}}
#' @name gb
#' @examples
#'
#' \dontrun{ requires Macaulay2 be installed
#'
#'
#' ##### basic usage
#' ########################################
#'
#' # the last ring evaluated is the one used in the computation
#' (QQtxyz <- ring(c("t","x","y","z"), coefring = "QQ"))
#' gb("t^4 - x", "t^3 - y", "t^2 - z")
#' gb("t^4 - x", "t^3 - y", "t^2 - z", code = TRUE)
#'
#' # standard evaluation version
#' gb_(c("t^4 - x", "t^3 - y", "t^2 - z"))
#'
#' # different rings
#' (QQxyzt <- ring(c("x","y","z","t"), coefring = "QQ"))
#' gb("t^4 - x", "t^3 - y", "t^2 - z")
#'
#' # you can specify a specific ring apart from the last used
#' # (this resets what the last used ring is)
#' gb("t^4 - x", "t^3 - y", "t^2 - z", ring = QQtxyz)
#' gb("t^4 - x", "t^3 - y", "t^2 - z")
#'
#'
#'
#' ##### more advanced usage
#' ########################################
#'
#' # defining a ring on the fly
#' gb_(c("t^4 - x", "t^3 - y", "t^2 - z"), ring = "QQ[t,x,y,z]")
#' gb_(c("t^4 - x", "t^3 - y", "t^2 - z"), ring = "QQ[t,x,y,z]", code = TRUE)
#'
#' # interaction with pointers
#' (QQtxyz. <- ring.(c("t","x","y","z"), coefring = "QQ"))
#' gb_(c("t^4 - x", "t^3 - y", "t^2 - z"), ring = QQtxyz., code = TRUE)
#'
#' I <- ideal(c("t^4 - x", "t^3 - y", "t^2 - z"), QQtxyz.)
#' gb_(I)
#'
#'
#' ##### still broken
#' ########################################
#'
#' gb("x*y", "x*z", "x", raw_chars = TRUE)
#'
#' (QQtxyz <- ring(c("t","x","y","z"), coefring = "QQ"))
#' I. <- ideal.(c("t^4 - x", "t^3 - y", "t^2 - z"), QQtxyz)
#' gb_(I.)
#'
#'
#'
#'
#'
#' gb_(mp(c("x y - z^2", "y^2 - w^2")))
#'
#' gb(c("x y-z^2", "y^2-w^2"), ring = "QQ[w,x,y,z]")
#' gb(mp("x y-z^2"), mp("y^2-w^2"), ring = "QQ[w,x,y,z]")
#' gb(mp("x y-z^2"), mp("y^2-w^2"), degree_limit = 2)
#'
#'
#'
#'
#'
#'
#' # standard evaluation
#' gb_.(   c("t^4 - x", "t^3 - y", "t^2 - z") , "QQ[t,x,y,z]")
#' gb_.(mp(c("t^4 - x", "t^3 - y", "t^2 - z")), "QQ[t,x,y,z]")
#' gb_.(mp(c("t^4 - x", "t^3 - y", "t^2 - z")), code = TRUE)
#' gb_.(   c("t^4 - x", "t^3 - y", "t^2 - z") , "QQ[t,x,y,z]", degree_limit = 2)
#'
#'
#'
#' gb_(   c("t^4 - x", "t^3 - y", "t^2 - z") , "QQ[t,x,y,z]")
#'
#'
#'
#' (QQtxyz <- ring(c("t", "x","y","z"), coefring = "QQ"))
#' gb_.(mp(c("t^4 - x", "t^3 - y", "t^2 - z")), QQtxyz, code = TRUE)
#' gb_.(mp(c("t^4 - x", "t^3 - y", "t^2 - z")), QQtxyz)
#' gb_.(   c("t^4 - x", "t^3 - y", "t^2 - z") , QQtxyz)
#' gb_.(   c("t^4 - x", "t^3 - y", "t^2 - z") , "QQ[t,x,y,z]", code = TRUE)
#' gb_.(   c("t^4 - x", "t^3 - y", "t^2 - z") , "QQ[t,x,y,z]")
#' gb_(c("t^4 - x", "t^3 - y", "t^2 - z"))
#' gb_(ideal("t^4 - x", "t^3 - y", "t^2 - z"))
#' gb_(c("t^4 - x", "t^3 - y", "t^2 - z"), raw_chars = TRUE)
#'
#'
#'
#' }


#' @export
#' @rdname gb
gb <- function(..., ring, degree_limit, raw_chars = FALSE, code = FALSE) {

  # grab args
  x <- list(x = lapply(pryr::dots(...), eval))
  otherArgs <- as.list(match.call(expand.dots = FALSE))[-c(1:2)]

  # eval
  args <- lapply(c(x, otherArgs), eval)

  # run standard evaluation gb
  do.call("gb_", args)
}














# value version of f (standard user version)
#' @rdname gb
#' @export
gb_ <- function(x, ring, degree_limit,  raw_chars = FALSE, code = FALSE, ...) {

  # run m2
  args <- as.list(match.call())[-1]
  eargs <- lapply(args, eval, envir = parent.frame())
  pointer <- do.call(gb_., eargs)
  if(code) return(invisible(pointer))

  # parse output
  parsed_out <- m2_parse(pointer)

  # more parsing
  out <- structure(parsed_out$rmatrix[1,], class = "mpolyList")

  # return
  out

}








# reference version of f (returns pointer to m2 object)
#' @rdname gb
#' @export
gb_. <- function(x, ring, degree_limit, raw_chars = FALSE, code = FALSE, ...) {

  # basic arg checking
  if(is.m2_ideal(x) && !missing(ring)) {
    warning(
      "Grobner bases of ideal objects always use their rings\n",
      "if you want to change the ring, use (ideal)$gens"
    )
  }
  if(is.m2_ideal_pointer(x) && !missing(ring)) {
    warning("Grobner bases of ideal objects always use their rings")
  }

  # create m2_code params
  if (raw_chars) {
    ideal_param <- paste0(x, collapse = ", ")
  } else {
    if (is.character(x)) {
      ideal_param <- sprintf(
        "ideal(%s)",
        listify(mpolyList_to_m2_str(mp(x)))
      )
    } else if (is.list(x) && all(vapply(x, is.character, logical(1)))) {
      ideal_param <- sprintf(
        "ideal(%s)",
        listify(mpolyList_to_m2_str(mp(unlist(x))))
      )
    } else if (is.list(x) && all(vapply(x, is.mpoly, logical(1)))) {
      ideal_param <- sprintf(
        "ideal(%s)",
        listify(mpolyList_to_m2_str(   x ))
      )
    } else if (is.list(x) && all(vapply(x, is.numeric, logical(1)))) {
      # this is like c(mp("x y"), mp("x z"), mp("x"))
      stop(
        "you appear to have used c() on mpolys.\n",
        "  this input format is not accepted, use list() instead.",
        call. = FALSE
      )
    } else if (is.mpolyList(x)) {
      ideal_param <- sprintf(
        "ideal(%s)",
        listify(mpolyList_to_m2_str(   x ))
      )
    } else if (is.m2_ideal(x)) {
      ideal_param <- x$m2_name
      ring_param <- x$ring$m2_name
    } else if (is.m2_ideal_pointer(x)) {
      warning("broken.")
      ideal_param <- x$m2_name
      ring_param <- x$ring$m2_name
    } else {
      stop("unrecognized input x. see ?gb", call. = FALSE)
    }
  }

  if (!missing(ring) && !(is.m2_ideal(x) || is.m2_ideal_pointer(x))) {
    if (is.m2_polynomialring(ring)) {
      ring_param <- ring$m2_name
    } else if (is.m2_polynomialring_pointer(ring)) {
      ring_param <- ring$m2_name
    } else if(is.character(ring) && length(ring) == 1) {
      ring_param <- ring
    } else {
      stop("unrecognized ring parameter.", call. = FALSE)
    }
  }


  # construct m2_code from regularized essential parameters
  m2_code <- sprintf(
    "gens gb(%s, DegreeLimit => %s)",
    ideal_param,
    if(missing(degree_limit)) "{}" else as.character(degree_limit)
  )
  if(!missing(ring) || (is.m2_ideal(x) || is.m2_ideal_pointer(x))) {
    m2_code <- paste0(sprintf("use %s; ", ring_param), m2_code)
  }

  # message
  if(code) { message(m2_code); return(invisible(m2_code)) }

  # run m2 and return pointer
  m2.(m2_code)

}
