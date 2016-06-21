#' @examples
#'
#' Note that this doesn't take in an input ring
#' because this solver only works over the complex numbers
#'
#' \dontrun{ requires Macaulay2 be installed
#'
#' For this to work, you need to have modified your init-PHCpack.m2 file
#' instead of changing your .bashrc file to establish the path of phc
#'
#' (mpolyList <- mp(c("t^4 - x", "t^3 - y", "t^2 - z", "x+y+z")))
#' solve_system(mpolyList)
#' }

solve_system <- function (mpolyList) {

  # allow for character vectors
  if(is.character(mpolyList)) mpolyList <- mp(mpolyList)

  # convert mpolylist to strings readable by m2
  poly_str <- suppressMessages(paste0( lapply(mpolyList, print, stars=TRUE), collapse=", "))
  poly_str <- str_replace_all(poly_str, "\\*\\*", "^")

  var_str <- suppressMessages(paste0(vars(mpolyList),collapse=","))
  # TODO: rename
  m2_code <- sprintf(
    "needsPackage \"PHCpack\";
    R := CC[%s];
    pts := solveSystem {%s};
    for i in 0..(#pts-1) list (pts#i#Coordinates)",
    var_str,
    poly_str
    )

  m2_out <- m2(m2_code)
  m2_out
}
