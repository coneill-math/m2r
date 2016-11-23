#' Create a new ring in Macaulay2
#'
#' Create a new ring in Macaulay2
#'
#' @param vars vector of variable names
#' @param coefring coefficient ring (default: \code{"CC"})
#' @param order a term order (default: \code{"grevlex"})
#' @param code return only the M2 code? (default: \code{FALSE})
#' @param x formal argument for print method
#' @param ... ...
#' @return a reference to a Macaulay2 ring
#' @name ring
#' @examples
#'
#' \dontrun{ requires Macaulay2 be installed
#'
#' ##### basic usage
#' ########################################
#'
#' ring(c("x", "y"))
#' ring(c("x", "y"), code = TRUE)
#'
#' (myring <- ring(c("x1","x2","x3","y"), coefring = "QQ", order = "lex"))
#'
#' myring$m2_name
#' myring$vars
#' myring$coefring
#' myring$order
#'
#' ##### other options
#' ########################################
#'
#' ring.(c("x", "y"))
#' ring.(c("x", "y"), code = TRUE)
#'
#' }
#'




#' @rdname ring
#' @export
ring <- function(
  vars,
  coefring = m2_coefrings(),
  order = m2_termorders(),
  code = FALSE,
  ...
) {

  # arg checking
  coefring <- match.arg(coefring)
  order <- match.arg(order)

  # run ring.
  pointer <- ring.(vars, coefring, order, code)
  if(code) return(invisible(pointer))

  # construct R-side ring, class and return
  ring <- list(
    m2_name = pointer$m2_name, coefring = coefring,
    vars = vars, order = order
  )
  class(ring) <- c("m2_polynomialring", "m2")
  ring

}



#' @rdname ring
#' @export
ring. <- function(
  vars,
  coefring = m2_coefrings(),
  order = m2_termorders(),
  code = FALSE,
  ...
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
  if(code) { message(line); return(invisible(line)) }

  # run m2
  ret <- m2.(line)

  ret$m2_name <- ringname
  ret
}





# m2 coefficient rings currently supported
field_as_ring <- function(coefring) {

  ring <- list(m2_name = coefring, coefring = coefring, vars = NULL, order = "grevlex")
  class(ring) <- c("m2_polynomialring", "m2")

  ring

}



m2_parse_object_as_function.m2_polynomialring <- function(x, params) {

  monoid <- params[[c(1)]]
  vars <- c(x$vars)
  order <- "grevlex"

  for (i in 1:length(monoid)) {
    if (!is.m2_option(monoid[[i]])) {
      vars <- c(vars, unlist(monoid[[i]]))
    } else if (monoid[[c(i,1)]] == "MonomialOrder") {
      for (j in 1:length(monoid[[c(i,2)]])) {
        if (
          is.m2_option(monoid[[c(i,2,j)]]) &&
          monoid[[c(i,2,j,1)]] %in% c("Lex", "GLex", "GRevLex")
        ) {
          order <- monoid[[c(i,2,j,1)]]
          order <- switch(order, Lex = "lex", GLex = "glex", GRevLex = "grevlex")
        }
      }
    }
  }

  ring <- list(m2_name = "", coefring = x$coefring, vars = vars, order = order)
  class(ring) <- c("m2_polynomialring", "m2")

  ring

}




# m2 coefficient rings currently supported
#' @rdname ring
#' @export
m2_coefrings <- function() c("CC", "RR", "QQ", "ZZ")




# m2 term orders currently supported
#' @rdname ring
#' @export
m2_termorders <- function() c("grevlex", "lex", "glex")




m2_ring_class_names <- function() {
  c(
    "Ring","PolynomialRing","QuotientRing",
    "InexactFieldFamily","InexactField"
  )
}


#' @rdname ring
#' @export
print.m2_polynomialring <- function(x, ...){

  s <- sprintf(
    "M2 Ring: %s[%s], %s order",
    x$coefring, paste(x$vars, collapse = ","), x$order
  )
  cat(s)

  invisible(s)
}
