#' Clears a Macaulay2 variable
#'
#' Clears a Macaulay2 variable
#'
#' @param x the M2 object to clear
#' @return Invisible the object passed in.
#' @export
#' @examples
#'
#' \dontrun{ requires Macaulay2 be installed
#'
#' R <- ring(c("x1", "x2", "x3"))
#' m2(paste("class", m2_name(R)))
#'
#' rm_m2(R)
#'
#' m2(paste("class", m2_name(R)))
#'
#' }
#'

rm_m2 <- function (x) {

  if (!is.m2(x)) {
    return(invisible())
  }

  m2(paste(m2_name(x), "=symbol", m2_name(x)))

  invisible()
}
