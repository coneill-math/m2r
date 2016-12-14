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
#' x <- 1
#' class(x) <- "m2"
#' attr(x, "m2_meta") <- list(a = 1, b = 2)
#' m2_meta(x)
#' m2_meta(x, "b")
#' m2_meta(x, "b") <- 5
#' m2_meta(x, "b")
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
`m2_name<-` <- function (x, value) {
  stopifnot( is.m2(x) )
  attr(x, "m2_name") <- value
  x
}


#' @rdname m2_utility
#' @export
m2_meta <- function (x, m2_attr) {
  if ( !is.m2(x) ) return(NULL)
  if ( missing(m2_attr) ) return(attr(x, "m2_meta"))
  attr(x, "m2_meta")[[m2_attr]]
}


#' @rdname m2_utility
#' @export
`m2_meta<-` <- function (x, m2_attr, value) {
  stopifnot( is.m2(x) )
  if (missing(m2_attr)) {
    attr(x, "m2_meta") <- value
  } else {
    meta <- m2_meta(x)
    meta[[m2_attr]] <- value
    attr(x, "m2_meta") <- meta
  }
  x
}


#' @rdname m2_utility
#' @export
m2_structure <- function (x = NA, m2_name, m2_class, m2_meta) {

  if (!missing(m2_class)) class(x) <- c(m2_class, "m2")
  if (!missing(m2_name)) m2_name(x) <- m2_name
  # if (m2_meta(x) != NULL) m2_meta(x)
  if (!missing(m2_meta)) m2_meta(x) <- m2_meta

  x
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
