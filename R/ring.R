




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
  print(ringname)
  sortedvars <- vars(reorder(mp(paste(vars, collapse = " ")), order = order))

  line <- paste0(ringname, " = ", coefring, "[", paste(sortedvars, collapse = ","), "]")
  print(line)
  m2(line)
  print("B")
  ring <- list(m2name = ringname, coefring = coefring, vars = sortedvars)
  class(ring) <- "m2ring"

  ring

}
