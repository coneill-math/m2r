#' Create a new ring instance in Macaulay2
#'
#' Create a new ring instance in Macaulay2
#'
#' @param vars vector of variable names
#' @param coefring coefficient ring
#' @param order a term order
#' @return a reference to a Macaulay2 ring
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
#' }

ring <- function(vars, coefring = "CC", order = "lex") {

  # verify valid coefring argument
  if (!is.element(coefring, c("CC", "RR", "QQ", "ZZ"))) {
    # TODO: implement finite field support!
    stop("Invalid coefficient field")
  }

  ringnum <- getOption("m2_ring_count")
  if (is.null(ringnum)) {
    ringnum = 0
  }

  ringname <- sprintf("m2rintring%08d", ringnum)
  setOption("m2_ring_count", ringnum + 1)

  # sortedvars <- vars(reorder(mp(paste(vars, collapse = " ")), order = order))
  sortedvars <- vars

  line <- paste0(ringname, " = ", coefring, "[", paste(sortedvars, collapse = ","), "]")
  m2(line)

  ring <- list(m2name = ringname, coefring = coefring, vars = sortedvars)
  class(ring) <- "m2ring"

  ring

}
