#' Macaulay2 in R
#'
#' m2r provides a persistent interface to Macaulay2
#' (\url{http://www.math.uiuc.edu/Macaulay2/}) and front-end tools facilitating
#' its use in the R ecosystem. For details, see vignette("m2r").
#'
#' @docType package
#' @import mpoly gmp
#' @importFrom memoise memoise forget
#' @importFrom stringr str_replace_all str_sub str_split fixed str_sub
#'   str_detect str_replace str_replace_all str_c
#' @importFrom pryr dots
#' @useDynLib m2r
#' @importFrom Rcpp sourceCpp
#' @name m2r
#' @aliases m2r package-m2r
NULL
