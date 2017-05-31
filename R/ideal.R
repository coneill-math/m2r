#' Create a new ideal in Macaulay2
#'
#' Create a new ideal in Macaulay2
#'
#' @param x a listing of polynomials. several formats are accepted,
#'   see examples.
#' @param ring the referent ring in Macaulay2
#' @param raw_chars if \code{TRUE}, the character vector will not be
#'   parsed by \code{\link{mp}}, saving time (default:
#'   \code{FALSE}). the down-side is that the strings must be
#'   formated for M2 use directly, as opposed to for
#'   \code{\link{mp}}. (e.g. \code{"x*y+3"} instead of \code{"x y +
#'   3"})
#' @param code return only the M2 code? (default: \code{FALSE})
#' @param ideal an ideal object of class \code{m2_ideal} or
#'   \code{m2_ideal_pointer}
#' @param ... ...
#' @return a reference to a Macaulay2 ideal
#' @name ideal
#' @examples
#'
#' \dontrun{ requires Macaulay2
#'
#' (QQxy <- ring(c("x","y"), coefring = "QQ"))
#' ideal(   c("x+y", "x^2+y^2") , QQxy)
#' ideal(mp(c("x+y", "x^2+y^2")), QQxy)
#'
#' ideal(mp(c("x+y", "x^2+y^2")), QQxy, code = TRUE)
#'
#' (QQxy. <- ring.(c("x","y"), coefring = "QQ"))
#' ideal(   c("x+y", "x^2+y^2") , QQxy.)
#' ideal(mp(c("x+y", "x^2+y^2")), QQxy.)
#'
#' I  <- ideal (c("x+y", "x^2+y^2"), QQxy.)
#' I. <- ideal.(c("x+y", "x^2+y^2"), QQxy.)
#' is.m2_ideal(I)
#' is.m2_ideal(I.)
#' is.m2_ideal_pointer(I)
#' is.m2_ideal_pointer(I.)
#'
#' # radical
#' QQxy <- ring(c("x","y"), "QQ")
#' I <- ideal(c("(x^2 + 1)^2 y", "y + 1"))
#' radical(I)
#' radical.(I)
#'
#' }




#' @rdname ideal
#' @export
ideal <- function(x, ring, raw_chars = FALSE, code = FALSE, ...) {

  # run ideal.
  args <- as.list(match.call())[-1]
  eargs <- lapply(args, eval, envir = parent.frame())
  pointer <- do.call(ideal., eargs)
  if(code) return(invisible(pointer))

  # parse output
  parsed_out <- m2_parse(pointer)

  # construct R-side ideal, class and return
  m2_structure(
    m2_name = m2_name(pointer),
    m2_class = "m2_ideal",
    m2_meta = list(
      ring = m2_meta(m2_meta(parsed_out, "rmap"), "ring"),
      gens = structure(m2_meta(parsed_out, "rmap")[1,], class = "mpolyList")
    )
  )

}




#' @rdname ideal
#' @export
ideal. <- function(x, ring, raw_chars = FALSE, code = FALSE, ...) {

  # make ideal name
  ideal_name <- name_and_increment("ideal", "m2_ideal_count")

  # make ideal_param
  if (raw_chars) {
    ideal_param <- listify(x)
  } else {
    if (is.character(x)) {
      mpolys <- mpolyList_to_m2_str(mp(x))
      ideal_param <- listify(mpolys)
    } else if (is.list(x) && all(vapply(x, is.character, logical(1)))) {
      mpolys <- mpolyList_to_m2_str(mp(unlist(x)))
      ideal_param <- listify(mpolys)
    } else if (is.list(x) && all(vapply(x, is.mpoly, logical(1)))) {
      mpolys <- structure(x, class = "mpolyList")
      mpoly_strings_for_m2 <- mpolyList_to_m2_str(mpolys)
      ideal_param <- listify(mpoly_strings_for_m2)
    } else if (is.list(x) && all(vapply(x, is.numeric, logical(1)))) {
      # this is like c(mp("x y"), mp("x z"), mp("x"))
      stop(
        "you appear to have used c() on mpolys.\n",
        "  this input format is not accepted, use list() instead.",
        call. = FALSE
      )
    } else if (is.mpolyList(x)) {
      mpoly_strings_for_m2 <- mpolyList_to_m2_str(x)
      ideal_param <- listify(mpoly_strings_for_m2)
    } else {
      stop("unrecognized input x. see ?ideal", call. = FALSE)
    }
  }

  # make ring_param
  if(!missing(ring)) {
    if (is.m2_polynomialring(ring)) {
      ring_param <- m2_name(ring)
    } else if (is.m2_polynomialring_pointer(ring)) {
      ring_param <- m2_name(ring)
    } else {
      stop("unrecognized input ring. see ?ideal", call. = FALSE)
    }
  }

  # construct code and message
  m2_code <- sprintf("%s = ideal(%s)", ideal_name, ideal_param)
  if(!missing(ring)) {
    m2_code <- paste0(sprintf("use %s; ", ring_param), m2_code)
  }
  if(code) { message(m2_code); return(invisible(m2_code)) }

  # run m2
  out <- m2.(m2_code)

  # change name and return
  m2_name(out) <- ideal_name
  out
}




m2_parse_function.m2_ideal <- function(x) {
  m2_structure(
    m2_name = "",
    m2_class = "m2_ideal",
    m2_meta = list(rmap = x[[1]])
  )
}


m2_parse_function.m2_monomialideal <- function(x) {
  m2_structure(
    m2_name = "",
    m2_class = "m2_ideal",
    m2_meta = list(rmap = x[[1]])
  )
}



#' @rdname ideal
#' @export
print.m2_ideal <- function(x, ...) {

  # from print.m2_polynomialring
  s <- sprintf(
    "ring %s[%s] (%s)",
    m2_meta(m2_meta(x, "ring"), "coefring"),
    paste(m2_meta(m2_meta(x, "ring"), "vars"), collapse = ","),
    m2_meta(m2_meta(x, "ring"), "order")
  )

  # ideal stuff
  if (length(m2_meta(x, "gens")) > 1) {
    with_gen <- "with generators:"
  } else {
    with_gen <- "with generator:"
  }
  cat("M2 Ideal of", s, with_gen, "\n")
  gens_strings <- print(m2_meta(x, "gens"), silent = TRUE)
  cat(paste("<", paste(gens_strings, collapse = ",  "), ">"))
  # cat(str_pad(gens_strings, nchar(gens_strings)+2, side = "left"), sep = "\n")
  invisible(x)

}












#' @rdname ideal
#' @export
radical <- function(ideal, code = FALSE, ...) {

  # run ideal.
  args <- as.list(match.call())[-1]
  eargs <- lapply(args, eval, envir = parent.frame())
  pointer <- do.call(radical., eargs)
  if(code) return(invisible(pointer))

  # parse output
  parsed_out <- m2_parse(pointer)

  # construct R-side ideal, class and return
  m2_structure(
    m2_name = m2_name(pointer),
    m2_class = "m2_ideal",
    m2_meta = list(
      ring = m2_meta(m2_meta(parsed_out, "rmap"), "ring"),
      gens = structure(m2_meta(parsed_out, "rmap")[1,], class = "mpolyList"),
      radical_of = ideal
    )
  )

}



#' @rdname ideal
#' @export
radical. <- function(ideal, code = FALSE, ...) {

  # arg check
  if (!is.m2_ideal(ideal) && !is.m2_ideal_pointer(ideal))
    stop("unrecognized input ideal. see ?ideal", call. = FALSE)

  # make radical ideal name
  radical_name <- name_and_increment("ideal", "m2_ideal_count")

  # construct code and message
  m2_code <- sprintf("%s = radical(%s)", radical_name, m2_name(ideal))
  if(code) { message(m2_code); return(invisible(m2_code)) }

  # run m2
  out <- m2.(m2_code)

  # change name and return
  m2_name(out) <- radical_name
  out
}


