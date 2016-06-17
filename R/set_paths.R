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
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @examples
#'
#' \dontrun{ # the below code requires suggested external software
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
#'
set_m2_path <- function(path){

  if(missing(path) && interactive()){

    set_m2_path <- dirname(file.choose())
    if(is.win() && str_detect(m2_path,"C:/")){
      m2_path <- str_replace(dirname(m2_path), "C:/", "/cygdrive/c/")
    }
    options(m2_path = m2_path)
    return(invisible(m2_path))

  } else if (!missing(path)) {

    options(m2_path = path)
    return(invisible(path))

  } else {
    stop(
      "If the session is not interactive, a path must be specified.",
      call. = FALSE
    )
  }
}
