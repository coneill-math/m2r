#' Create a new ring in Macaulay2
#'
#' Create a new ring in Macaulay2
#'
#' @param vars vector of variable names
#' @param coefring coefficient ring
#' @param order a term order
#' @param code message code to user? (default = FALSE)
#' @return a reference to a Macaulay2 ring
#' @export
#' @examples
#'
#' \dontrun{ requires Macaulay2 be installed
#'
#' (myring <- ring(c("x1","x2","x3","y"), coefring = "QQ", order = "lex"))
#'
#' myring[["m2name"]]
#' myring[["vars"]]
#' myring[["coefring"]]
#' myring[["order"]]
#'
#' m2(paste0("class(", myring[["m2name"]], ")"))
#'
#' }
#'
ring <- function(vars,
  coefring = c("CC", "RR", "QQ", "ZZ"),
  order = c("grevlex", "lex", "glex"),
  code = FALSE
) {

  # check args
  coefring <- match.arg(coefring)
  order <- match.arg(order)

  # caps order for M2
  order <- switch(order, lex = "Lex", glex = "GLex", grevlex = "GRevLex")

  # set glex order
  if (order == "GLex") {
    # {Weights => {1,1,1,1,1}, Lex => 4}
    m2order <- sprintf(
      "{Weights => {%s}, Lex => %d}",
      paste(rep("1",length(vars)), collapse = ","),
      length(vars)-1
    )
  } else {
    # {Lex => 5}
    m2order <- paste0("{", order, " => ", length(vars), "}")
  }

  # grab # of current rings, set ring number, and increment
  ringnum <- getOption("m2_ring_count")
  ringnum <- ifelse(is.null(ringnum), 1L, strtoi(ringnum))
  setOption("m2_ring_count", ringnum + 1)

  # make ring name
  ringname <- sprintf("m2rintring%08d", ringnum)

  # sortedvars <- vars(reorder(mp(paste(vars, collapse = " ")), order = order))
  sortedvars <- vars

  # construct code and message
  line <- sprintf(
    "%s = %s[%s,MonomialOrder=>%s]",
    ringname, coefring, paste(sortedvars, collapse = ","), m2order
  )
  if(code) message(line)

  # run m2
  m2(line)

  # construct R-side ring, class and return
  ring <- list(
    m2name = ringname, coefring = coefring,
    vars = sortedvars, order = tolower(order)
  )
  class(ring) <- c("PolynomialRing", "m2")
  ring
}
