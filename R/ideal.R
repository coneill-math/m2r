#' Create a new ideal in Macaulay2
#'
#' Create a new ideal in Macaulay2
#'
#' @param mpolyList list of mpolys
#' @param ring the referent ring in Macaulay2
#' @param code return only the M2 code? (default: \code{FALSE})
#' @param x (for \code{print}) x
#' @param ... ...
#' @return a reference to a Macaulay2 ideal
#' @name ideal
#' @examples
#'
#' \dontrun{ requires Macaulay2 be installed
#'
#' (QQxy <- ring(c("x","y"), coefring = "QQ"))
#' ideal(mp(c("x+y", "x^2+y^2")), QQxy)
#' (QQxy <- ring(c("x","y"), coefring = "QQ", code = TRUE))
#' ideal(mp(c("x+y", "x^2+y^2")), QQxy, code = TRUE)
#'
#' }




#' @rdname ideal
#' @export
ideal <- function(mpolyList, ring, code = FALSE, ...) {

  # run ideal.
  pointer <- do.call(ideal., as.list(match.call())[-1])
  if(code) return(invisible(pointer))

  # parse output
  parsed_out <- m2_parse(pointer)

  # construct R-side ideal, class and return
  ideal <- list(
    m2_name = pointer$m2_name,
    ring = parsed_out$rmap$ring,
    gens = structure(parsed_out$rmap$rmatrix[1,], class = "mpolyList")
  )

  # could also want to parse ideal to polys here
  structure(ideal, class = c("m2_ideal", "m2"))

}




#' @rdname ideal
#' @export
ideal. <- function(mpolyList, ring, code = FALSE, ...) {

  # make ideal name
  ideal_name <- name_and_increment("ideal", "m2_ideal_count")
  mpoly_strings_for_m2 <- mpolyList_to_m2_str(mpolyList)

  # construct code and message
  m2_code <- sprintf(
    "use %s; %s = ideal(%s)",
    ring$m2_name, ideal_name, listify(mpoly_strings_for_m2)
  )
  if(code) { message(m2_code); return(invisible(m2_code)) }

  # run m2
  out <- m2.(m2_code)

  # change name and return
  out$m2_name <- ideal_name
  out
}




m2_parse_function.m2_ideal <- function(x) {
  out <- list(m2_name = "", rmap = x[[1]])
  structure(out, class = c("m2_ideal", "m2"))
}



#' @rdname ideal
#' @export
print.m2_ideal <- function(x, ...) {

  # from print.m2_polynomialring
  s <- sprintf(
    "M2 PolynomialRing %s[%s] (%s)",
    x$ring$coefring, paste(x$ring$vars, collapse = ","), x$ring$order
  )

  # ideal stuff
  cat("Ideal of", s, "with generators:", "\n")
  gens_strings <- print(x$gens, silent = TRUE)
  cat(paste("<", paste(gens_strings, collapse = ",  "), ">"))
  # cat(str_pad(gens_strings, nchar(gens_strings)+2, side = "left"), sep = "\n")
  invisible(x)

}




