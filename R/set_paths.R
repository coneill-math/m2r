#' @aliases set_m2_path
#' @export
set_m2_path <- function(path){

	if(missing(path) && interactive()){

		set_m2_path <- dirname(file.choose())
		if(is.win() && str_detect(m2_path,"C:/")){
			m2_path <- str_replace(dirname(m2_path), "C:/", "/cygdrive/c/")
		}
		options(m2_path = m2_path)
		return(invisible(m2_path))

	} else if(!missing(path)){

		options(m2_path = path)
		return(invisible(path))

	} else {
		stop(
			"If the session is not interactive, a path must be specified.",
			call. = FALSE
		)
	}
}
