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
#' m2(paste("class", R$m2_name))
#'
#' rm_m2(R)
#'
#' m2(paste("class", R$m2_name))
#'
#' }
#'

rm_m2 <- function (x) {

  if (!is.m2(x)) {
    return(invisible())
  }

  m2(paste(x$m2_name, "=symbol", x$m2_name))

  invisible()
}
