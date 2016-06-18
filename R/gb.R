#' Compute a Grobner basis with Macaulay2
#'
#' Compute a Grobner basis with Macaulay2
#'
#' @param mpolyList an mpolyList object
#' @param ring ring
#' @return an mpolyList object
#' @export
#' @examples
#'
#' \dontrun{ requires Macaulay2 be installed
#'
#' ( p1 <- mp("t^4 - x") )
#' ( p2 <- mp("t^3 - y") )
#' ( p3 <- mp("t^2 - z") )
#' ( ms <- mpolyList(p1, p2, p3) )
#' gb(ms)
#'
#' # easier
#' gb(mp(c("t^4 - x", "t^3 - y", "t^2 - z")))
#'
#' }

gb <- function(mpolyList, ring = NA) {

  # Convert mpolylist to strings strings readable by M2.
  poly_str <- suppressMessages(paste0( lapply(mpolyList, print, stars=TRUE), collapse=", "))
  poly_str <- str_replace_all(poly_str, "\\*\\*", "^")
  ideal_str <- paste("I := ideal(", poly_str, ");")

  vars_str <- paste0(vars(mpolyList), collapse = ",")
  ring_str <- paste("R := QQ[", vars_str, "];")

  m2_out <- m2(paste(ring_str, ideal_str, "gens gb I"))
  m2_out <- str_sub(m2_out, 10, -3)
  m2_out <- str_replace_all(m2_out, "\\*", " ")
  mpoly::mp(str_split(m2_out, ", ")[[1]])
}
