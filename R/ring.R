#' Create a new ring in Macaulay2
#'
#' Create a new ring in Macaulay2
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
#' (myring <- ring(c("x1","x2","x3","y"), coefring = "QQ", order = "Lex"))
#'
#' myring[["m2name"]]
#' myring[["vars"]]
#' myring[["coefring"]]
#' myring[["order"]]
#'
#' m2(paste0("class(", myring[["m2name"]], ")"))
#'
#' }

ring <- function(vars, coefring = "CC", order = "GRevLex") {

  # verify valid coefring argument
  if (!is.element(coefring, c("CC", "RR", "QQ", "ZZ"))) {

    stop("Invalid coefficient ring")

  }

  if (!is.element(order, c("Lex", "GLex", "GRevLex"))) {

    stop("Invalid term order")

  }

  if (order == "GLex") {

    # {Weights=>{1,1,1,1,1},Lex=>4}
    m2order <- paste0("{Weights=>{", paste(rep("1",length(vars)), collapse = ","), "},Lex=>", length(vars) - 1, "}")

  } else {

    # {Lex => 5}
    m2order <- paste0("{", order, "=>", length(vars), "}")

  }

  ringnum <- getOption("m2_ring_count")
  if (is.null(ringnum)) {

    ringnum = 1

  } else {

    ringnum = strtoi(ringnum)

  }

  ringname <- sprintf("m2rintring%08d", ringnum)
  setOption("m2_ring_count", ringnum + 1)

  # sortedvars <- vars(reorder(mp(paste(vars, collapse = " ")), order = order))
  sortedvars <- vars

  line <- paste0(
    ringname, "=",
    coefring, "[",
    paste(sortedvars, collapse = ","), ",",
    "MonomialOrder=>", m2order,
    "]"
  )
  m2(line)

  ring <- list(m2name = ringname, coefring = coefring, vars = sortedvars, order = order)
  class(ring) <- "m2PolynomialRing"

  ring

}
