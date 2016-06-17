#' Compute a Grobner basis with gb
#'
#' Compute a Grobner basis with gb
#'
#' @param mpolyList an mpolyList object
#' @param ring ring
#' @param vars vars
#' @return an mpolyList object
#' @export
#' @examples
#'
#' \dontrun{ requires Macaulay2 be installed
#'
#' start_m2()
#'
#' # compute a grobner basis of a polynomial ideal
#' (gb <- m2("
#'   R = QQ[a..d]
#'   I = ideal(a^3-b^2*c, b*c^2-c*d^2, c^3)
#'   G = gens gb I
#' "))
#'
#' # parse it into an mpoly object
#' library(stringr)
#' gb <- str_sub(gb, 10, -3)
#' gb <- str_replace_all(gb, "\\*", " ")
#' mpoly::mp(str_split(gb, ", ")[[1]])
#'
#' stop_m2()
#'
#' }
gb <- function(mpolyList, ring = NA, vars = vars(mpolyList)) {
  NULL
}
