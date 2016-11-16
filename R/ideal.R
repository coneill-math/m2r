#' Create a new ideal in Macaulay2
#'
#' Create a new ideal in Macaulay2
#'
#' @param mpolyList list of mpolys
#' @param ring the referent ring in Macaulay2
#' @param code return only the M2 code? (default: \code{FALSE})
#' @param ... ...
#' @return a reference to a Macaulay2 ideal
#' @name ideal
#' @examples
#'
#' \dontrun{ requires Macaulay2 be installed
#'
#' (QQxy <- ring(c("x","y"), coefring = "QQ"))
#' ideal(mp(c("x+y", "x^2+y^2")), QQxy)
#' ideal(mp(c("x+y", "x^2+y^2")), QQxy, code = TRUE)
#'
#' }

#-------------------------------------------------------------------------------
#' @rdname ideal
#' @export
ideal <- function(mpolyList, ring, code = FALSE, ...) {

  # run ideal.
  pointer <- do.call(ideal., as.list(match.call())[-1])
  if(code) return(invisible(pointer))

  # construct R-side ideal, class and return
  routput <- m2_parse(pointer)
  polys <- lapply(routput$rmap$rmatrix, function(.) mp(.))
  ideal <- list(
    m2_name = pointer$m2_name
    , ring = routput$rmap$ring
    , polys = polys
    )

  # could also want to parse ideal to polys here
  structure(ideal, class = c("m2_ideal", "m2"))

}

#-------------------------------------------------------------------------------
#' @rdname ideal
#' @export
ideal. <- function(mpolyList, ring, code = FALSE, ...) {

  # make ideal name
  ideal_name <- name_and_increment("ideal", "m2_ideal_count")
  m2_polys_str <- paste(mpolyList_to_m2_str(mpolyList), collapse = ",")

  # construct code and message
  m2_code <- sprintf(
    "use %s; %s = ideal {%s}",
    ring$m2_name, ideal_name, m2_polys_str
  )
  if(code) { message(m2_code); return(invisible(m2_code)) }

  # run m2
  out <- m2.(m2_code)

  # change name and return
  out$m2_name <- ideal_name
  out
}

#-------------------------------------------------------------------------------
m2_parse_function.m2_ideal <- function(x) {
  out <- list(m2_name = NULL, rmap = x[[1]])
  structure(out, class = c("m2_ideal", "m2"))
}
