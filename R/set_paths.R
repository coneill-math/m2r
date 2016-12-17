#' Set path to Macaulay2 (M2)
#'
#' This function sets the path to external programs either by (1)
#' passing it a character string or (2) using
#' \code{\link{file.choose}}.
#'
#' When m2r is loaded it attempts to find M2.  How it looks depends
#' on your operating system.
#'
#' If you're using a Mac or Linux machine, it looks based on your
#' system's path.  Unfortunately, R changes the system path in such
#' a way that the path that R sees is not the same as the path that
#' you'd see if you were working in the terminal. (You can open the
#' Terminal app on a Mac by going to
#' /Applications/Utilities/Terminal.)  Consequently, m2r tries to
#' guess the file in which your path is set.  To do so, it first
#' checks if your home directory (type echo ~/ in the terminal to
#' figure out which directory this is if you don't know) for the
#' file named .bash_profile.  If this file is present, it runs it
#' and then checks your system's path variable (echo $PATH).  If
#' it's not present, it does the same for .bashrc and then .profile.
#' In any case, once it has its best guess at your path, it looks
#' for "M2".
#'
#' On Windows, m2r just uses Sys.which() on "whereis" to determine
#' where M2 is.
#'
#' @param path A character string, the path to M2
#' @return An invisible character string, the path found.  More
#'   importantly, the function has the side effect of setting the
#'   option "m2_path"
#' @export
#' @name m2_path
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @examples
#'
#' \dontrun{ requires Macaulay2
#'
#' getOption("m2_path")
#' set_m2_path()
#'
#'
#' ## each of these functions can be used statically as well
#' (m2_path <- getOption("m2_path"))
#' set_m2_path("/path/to/m2/directory")
#' getOption("m2_path")
#' set_m2_path(m2_path) # undoes example
#'
#'
#' }


#' @rdname m2_path
#' @export
set_m2_path <- function(path){

  if(missing(path) && interactive()){

    path <- dirname(file.choose())
    if(is.win() && str_detect(path,"C:/")){
      mpath <- str_replace(dirname(path), "C:/", "/cygdrive/c/")
    }
    set_m2r_option(m2_path = path)
    return(invisible(path))

  } else if (!missing(path)) {

    set_m2r_option(m2_path = path)
    return(invisible(path))

  } else {
    stop(
      "If the session is not interactive, a path must be specified.",
      call. = FALSE
    )
  }
}





#' @rdname m2_path
#' @export
get_m2_path <- function() getOption("m2r")$m2_path




#' @rdname m2_path
#' @export
get_m2_con <- function() getOption("m2r")$m2_con



#' @rdname m2_path
#' @export
get_m2_procid <- function() getOption("m2r")$m2_procid







