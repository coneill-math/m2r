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
#' myring$m2name
#' myring$vars
#' myring$coefring
#' myring$order
#'
#' m2(paste0("class(", myring$m2name, ")"))
#'
#' }
#'


# m2 coefficient rings currently supported
m2_coefrings <- function() {
  c("CC", "RR", "QQ", "ZZ")
}



# m2 term orders currently supported
m2_termorders <- function() {
  c("grevlex", "lex", "glex")
}





ring <- function(vars,
                  coefring = m2_coefrings(),
                  order = m2_termorders(),
                  code = FALSE
) {

  ring <- ring.(vars, coefring, order, code)

  # construct R-side ring, class and return
  ring <- list(
    m2name = ring$m2name, coefring = coefring,
    vars = vars, order = order
  )
  class(ring) <- c("m2_polynomialring", "m2")
  ring

}



ring. <- function(vars,
  coefring = m2_coefrings(),
  order = m2_termorders(),
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

  # make ring name
  ringname <- name_and_increment("ring", "m2_ring_count")

  # sortedvars <- vars(reorder(mp(paste(vars, collapse = " ")), order = order))
  sortedvars <- vars

  # construct code and message
  line <- sprintf(
    "%s = %s[%s,MonomialOrder=>%s]",
    ringname, coefring, paste(sortedvars, collapse = ","), m2order
  )
  if(code) message(line)

  # run m2
  ret <- m2.(line)

  ret$m2name <- ringname
  ret
}





# m2 coefficient rings currently supported
field_as_ring <- function(coefring) {

  ring <- list(
    m2name = coefring, coefring = coefring,
    vars = NULL, order = "grevlex"
  )
  class(ring) <- c("m2_polynomialring", "m2")

  ring

}



m2_parse_object_as_function.m2_polynomialring <- function(x, params) {

  monoid <- params[[1]][[1]]
  vars <- append(x$vars, unlist(monoid[[1]]))
  order <- "grevlex"

  for (i in 2:length(monoid)) {
    if (class(monoid[[i]])[1] == "m2_option" &&
        monoid[[i]][[1]] == "MonomialOrder") {
      for (j in 1:length(monoid[[i]][[2]])) {
        if (class(monoid[[i]][[2]][[j]])[1] == "m2_option" &&
            monoid[[i]][[2]][[j]][[1]] %in% c("Lex", "GLex", "GRevLex")) {
          order <- monoid[[i]][[2]][[j]][[1]]
          order <- switch(order, Lex = "lex", GLex = "glex", GRevLex = "grevlex")
        }
      }
    }
  }

  ring <- list(
    m2name = NULL, coefring = x$coefring,
    vars = vars, order = order
  )
  class(ring) <- c("m2_polynomialring", "m2")

  ring

}



