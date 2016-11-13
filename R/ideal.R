#' Create a new ideal in Macaulay2
#'
#' Create a new ideal in Macaulay2
#'
#' @param mpolyList list of mpolys
#' @param ring the referent ring in Macaulay2
#' @param code logical; message code to user? (default = FALSE)
#' @return a reference to a Macaulay2 ideal
#' @export
#' @examples
#'
#' \dontrun{ requires Macaulay2 be installed
#'
#' (myring <- ring(c("x","y"), coefring = "QQ", order = "lex"))
#' (myideal <- ideal(mp(c("x+y", "x^2+y^2")), myring,code=TRUE))
#'
#' myideal$m2_name
#' myideal$ring
#' myideal$gens
#' myideal$gbgens
#'
#' m2(paste0("class(", myideal$m2_name, ")"))
#'
#' }

ideal <- function(mpolyList,
                 ring,
                 code = FALSE
)
{
  # make ideal name
  idealname <- name_and_increment("ideal", "m2_ideal_count")

  m2_polys_str <- mpolyList_to_m2_str(mpolyList)

  # construct code and message
  line <- sprintf(
    "use %s; %s = ideal {%s}",
    ring$m2_name, idealname, m2_polys_str
  )
  if(code) message(line)

  # run m2
  m2(line)

  # construct R-side ideal, class and return
  ideal <- list(
    m2_name = idealname, ring = ring,
    gens = mpolyList, gbgens = NULL)
  class(ideal) <- c("Ideal", "m2")
  ideal
}
