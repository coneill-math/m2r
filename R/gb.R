#' Compute a Grobner basis with Macaulay2
#'
#' Compute a Grobner basis with Macaulay2
#'
#' \code{gb} uses nonstandard evaluation; \code{gb_} is the standard
#' evaluation equivalent.
#'
#' @param ... a nonstandard evaluation listing of mpoly or vector of
#'   character strings containing referencing multivariate
#'   polynomials to pass to \code{\link{mp}}
#' @param mpolyList an mpolyList object
#' @param ring ring
#' @param code message code to user? (default = FALSE)
#' @return an mpolyList object
#' @seealso \code{\link{mp}}
#' @name gb
#' @examples
#'
#' \dontrun{ requires Macaulay2 be installed
#'
#' gb("t^4 - x", "t^3 - y", "t^2 - z")
#' gb("t^4 - x", "t^3 - y", "t^2 - z", code = TRUE)
#'
#'
#'
#'
#' # grobner bases with different orders
#' # the default is lex order with the variable ordering
#' vars(mp(c("t^4 - x", "t^3 - y", "t^2 - z")))
#' gb("t^4 - x", "t^3 - y", "t^2 - z", code = TRUE)
#'
#'
#' # you can use different orderings
#' (ps <- mp(c("t^4 - x", "t^3 - y", "t^2 - z")))
#'
#' R <- ring(c("t","x","y","z"), coefring = "QQ", order = "lex")
#' gb(ps, ring = R)
#'
#' R2 <- ring(c("x","y","t","z"), coefring = "QQ", order = "lex")
#' gb(ps, ring = R2)
#'
#'
#'
#' # more nonstandard evaluation
#' gb("t^4 - x", "t^3 - y", "t^2 - z")
#'
#' gb(
#'   mp("t^4 - x"),
#'   mp("t^3 - y"),
#'   mp("t^2 - z")
#' )
#'
#' (ps <- mp(c("t^4 - x", "t^3 - y", "t^2 - z")))
#' gb(ps)
#'
#'
#' # standard evaluation
#' gb_(mp(c("t^4 - x", "t^3 - y", "t^2 - z")))
#' gb_(c("t^4 - x", "t^3 - y", "t^2 - z"))
#'
#'
#'
#'
#'
#' }


#' @export
#' @rdname gb
gb <- function(..., ring, code = FALSE) {

  # grab args
  args <- as.list(match.call(expand.dots = FALSE))[-1]
  dots <- args[["..."]]

  # eval
  dots <- lapply(dots, eval)

  # parse by cases
  if(all(vapply(dots, is.character, logical(1)))) {
    dots <- list(mpolyList = mp(unlist(dots)))
  } else {
    dots <- lapply(dots, eval) # eliminate symbols
    if(is.mpolyList(dots[[1]])) {
      names(dots) <- "mpolyList"
    } else { # if it's a list of mpoly's
      class(dots) <- "mpolyList"
      dots <- list(mpolyList = dots)
    }
  }

  # inject it in args
  args[[1]] <- dots[[1]]
  names(args)[1] <- "mpolyList"

  # run standard evaluation gb
  do.call("gb_", args)
}




#' @export
#' @rdname gb
gb_ <- function(mpolyList, ring, code = FALSE) {

  # allow for character vectors
  if(is.character(mpolyList)) mpolyList <- mp(mpolyList)

  # convert mpolylist to strings readable by m2
  poly_str <- mpolyList_to_m2_str(mpolyList)

  # make ideal, vars, and ring strings for m2
  ideal_str <- paste0("I := ideal(", poly_str, ")\n")
  if(missing(ring)) {
    ring_str <- sprintf(
      "R := QQ[%s]\n",
      paste0(vars(mpolyList), collapse = ",")
    )
  } else {
    ring_str <- paste0("use ", ring$m2name, "\n")
  }

  # aggregate m2 code, message if wanted, run m2
  m2_code <- paste0(ring_str, ideal_str, "gens gb I")
  if(code) message(m2_code)
  m2_out <- m2(m2_code)

  # comb code, mpoly parse, and out
  m2_out <- str_sub(m2_out, 10, -3)
  m2_out <- str_split(m2_out, ", ")[[1]]
  m2_out <- str_replace_all(m2_out, "\\*", " ")
  mp(m2_out)
}


