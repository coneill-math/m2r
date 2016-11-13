#' Create a new ideal in Macaulay2
#'
#' Create a new ideal in Macaulay2
#'
#' @param mpolyList list of mpolys
#' @param ring the referent ring in Macaulay2
#' @param code logical; message code to user? (default = FALSE)
#' @return a reference to a Macaulay2 ideal
#' @export
#' @examples
#'
#' \dontrun{ requires Macaulay2 be installed
#'
#' (myring <- ring(c("x","y"), coefring = "QQ", order = "lex"))
#' (myideal <- ideal(mp(c("x+y", "x^2+y^2")), myring,code=TRUE))
#'
#' }

ideal <- function(mpolyList, ring, code = FALSE) {
  ret <- do.call(ideal., as.list(match.call())[-1])

  # construct R-side ideal, class and return
  message(str(ret$ext_str))
  ideal <- list(
    m2_name = ret$m2_name,
    rmap = m2_parse(ret$ext_str)
  )
  # could also want to parse ideal to polys here.
  class(ideal) <- c("m2", "m2_ideal")
  ideal
}

ideal. <- function(mpolyList,ring, code = FALSE) {
  # make ideal name
  idealname <- name_and_increment("ideal", "m2_ideal_count")
  m2_polys_str <- paste(mpolyList_to_m2_str(mpolyList), collapse = ",")

  # construct code and message
  line <- sprintf(
    "use %s; %s = ideal {%s}",
    ring$m2_name, idealname, m2_polys_str
  )
  if(code) message(line)

  # run m2
  ret <- m2.(line)
  ret$m2_name <- idealname

  ret
}

m2_parse_function.m2_ideal <- function(x) {
  ret <- list(
    m2_name = NULL,
    rmap = x[[1]]
  )
  class(ret) <- c("m2_ideal", "m2")
  ret
}
