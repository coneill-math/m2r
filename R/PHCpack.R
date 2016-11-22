#' PHCpack
#'
#' Call PHCpack to solve a zero-dimensional system
#'
#' Note that \code{solve_system()} doesn't take in an input ring
#' because the solver only works over the complex numbers.
#'
#' @param mpolyList An mpolyList object
#' @return (currently) the output of an m2() call (string?)
#' @export
#' @examples
#'
#'
#' \dontrun{ requires Macaulay2 be installed
#'
#' # for this to work, you need to have modified your
#' # init-PHCpack.m2 file instead of changing your .bashrc
#' # file to establish the path of phc
#' # (**clarify**, maybe checkout algstat::polySolve)
#'
#' (mpolyList <- mp(c("t^4 - x", "t^3 - y", "t^2 - z", "x+y+z")))
#' solve_system(mpolyList)
#' mixed_volume(mpolyList)
#' }
#'

m2_pts_str_to_list <-function(m2_out) {
  m2_out <- str_sub(m2_out,2,-2)
  m2_out <- str_replace_all(m2_out, "p53", "")
  m2_out <- str_replace_all(m2_out, "\"", "")
  m2_out <- str_replace_all(m2_out, "toCC\\(", "complex(real=")
  m2_out <- str_replace_all(m2_out, ",complex", "Dcomplex")
  m2_out <- str_replace_all(m2_out, "\\},", "\\}")
  m2_out <- str_replace_all(m2_out, ",",",imaginary=")
  m2_out <- str_replace_all(m2_out, "Dcomplex", ",complex")
  m2_out <- str_replace_all(m2_out, "\\{", "c\\(")
  m2_out <- str_replace_all(m2_out, "\\}", "\\),")
  m2_out <- paste0("list(",str_sub(m2_out,0,-2),")")
  m2_out <- eval(parse(text=m2_out))
  m2_out
}

#' @rdname solve_system
#' @export
solve_system <- function (mpolyList) {
  pointer <- solve_system.(mpolyList)

  solutions <- list(
    m2_name = pointer$m2_name,
    sols = m2_pts_str_to_list(pointer$ext_str)
  )
  class(solutions) <- c("m2", "m2_solutions")
  solutions
}

#' @rdname solve_system.
#' @export
solve_system. <- function (mpolyList) {

  poly_str <- paste(mpolyList_to_m2_str(mpolyList), collapse = ",")
  var_str <- suppressMessages(paste0(vars(mpolyList),collapse=","))
  m2_code <- sprintf('
    needsPackage "PHCpack"
    R := CC[%s];
    pts := solveSystem {%s};
    for i in 0..(#pts-1) list (toExternalString pts#i#Coordinates)
  ', var_str, poly_str)

  m2.(m2_code)
}

#' @rdname mixed_volume
#' @export
mixed_volume <- function (mpolyList) {
  # If mpoly supported complex coefficients, then this should be modified to
  # support a start system
  poly_str <- paste(mpolyList_to_m2_str(mpolyList), collapse = ",")
  var_str <- suppressMessages(paste0(vars(mpolyList),collapse=","))
  m2_code <- sprintf('
    needsPackage "PHCpack"
    R := CC[%s];
    mixedVolume( {%s})
  ', var_str, poly_str)

  m2.(m2_code)$ext_str
}
