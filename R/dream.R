#' Launch a Macaulay2 dream session
#'
#' Launch a Macaulay2 dream session
#'
#' @inheritParams start_m2
#' @return \code{TRUE} invisibly
#' @export
#' @examples
#'
#' \dontrun{ requires Macaulay2 be installed
#'
#' dream()
#'
#' # m2 code below
#' if (FALSE) {
#'
#'   1 + 1
#'   R := QQ[t,x,y,z]
#'   I := ideal(t^4  -  x, t^3  -  y, t^2  -  z)
#'   gens gb I # error?
#'
#' }
#'
#' }
#'
dream <- function (port = 27436L, timeout = 10) {
  if(!interactive()) stop("dream is only available in interactive sessions.")
  start_m2(port, timeout)
  repeat {
    i <- readline("i : ")
    if(i == "exit") break
    o <- m2(i)
    cat(paste("o : ", o))
  }
  invisible(TRUE)
}
