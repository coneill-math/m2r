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
#' mixed_volume(mpolyList,startsystem=TRUE)
#' }
#'

solve_system <- function (mpolyList) {

  poly_str <- mpolyList_to_m2_str(mpolyList)

  var_str <- suppressMessages(paste0(vars(mpolyList),collapse=","))
  # TODO: rename
  m2_code <- sprintf('
    needsPackage "PHCpack"
    R := CC[%s];
    pts := solveSystem {%s};
    for i in 0..(#pts-1) list (toExternalString pts#i#Coordinates)
  ', var_str, poly_str)

  m2_pts_str_to_list(m2(m2_code))
}

track_paths <- function (targetsystem, startsystem, solutions, gamma = 0, tdegree = 2) {
# TODO
  m2("")
}

mixed_volume <- function (mpolyList, startsystem = FALSE) {

  poly_str <- mpolyList_to_m2_str(mpolyList)

  var_str <- suppressMessages(paste0(vars(mpolyList),collapse=","))
  if (!startsystem) {
    m2_code <- sprintf('
      needsPackage "PHCpack"
      R := CC[%s];
      mixedVolume( {%s})
    ', var_str, poly_str)
    m2_out <- m2(m2_code)
    m2_out
  } else {
    m2_code <- sprintf('
      needsPackage "PHCpack"
      R := CC[%s];
      (mixedVolVal, startSystemVal, solPtsVal) = mixedVolume( {%s}, StartSystem => true);
      ', var_str, poly_str)
    m2_out <- m2(m2_code)

    mixed_volume_value <- strtoi(m2("mixedVolVal"))

    # TODO: figure out a way to represent the start system with coefficients over C
    # m2_out <- m2("startSystemVal")
    # m2_out <- str_sub(m2_out, 1, -1)
    # m2_out <- str_replace_all(m2_out, "\\*", " ")
    # mp(str_split(m2_out, ", ")[[1]])

    pts_value <- m2_pts_str_to_list(m2("for i in 0..(#solPtsVal-1) list (toExternalString solPtsVal#i#Coordinates)"))
  }
}
