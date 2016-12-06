#' Utility tools for M2
#'
#' Utility tools for M2
#'
#' @param name name of object
#' @name m2_utility
#' @examples
#'
#' \dontrun{ requires Macaulay2 be installed
#'
#' m2("a = 5")
#' m2_ls()
#' m2_exists("a")
#' m2("b = 1")
#' m2_exists(c("a","b","c"))
#'
#' m2_getwd()
#'
#'
#' }



#' @rdname m2_utility
#' @export
m2_name <- function (x) {
  if ( is.m2(x) ) {
    attr(x, "m2_name")
  } else {
    character(0)
  }
}


#' @rdname m2_utility
#' @export
m2_meta <- function (x, attr) {
  if ( !is.m2(x) ) return(NULL)
  if ( !exists(attr) ) return(attr(x, "m2_meta"))
  attr(x, "m2_meta")[[attr]]
}


#' @rdname m2_utility
#' @export
m2_exists <- function(name) {
  if(!is.character(name)) name <- deparse(substitute(name))
  name %in% m2_ls()
}


#' @rdname m2_utility
#' @export
m2_ls <- function() {
  parsed_out <- m2("userSymbols()")
  parsed_out <- str_sub(parsed_out, 2, -2)
  parsed_out <- str_split(parsed_out, ",")[[1]]
  str_sub(parsed_out, 8)
}


#' @rdname m2_utility
#' @export
m2_rm <- function() {
  stop("not yet implemented.")
}


#' @rdname m2_utility
#' @export
m2_getwd <- function() {
  str_sub(m2("currentDirectory()"), 2, -2)
}
