is.mac <- function() grepl("darwin", R.version$platform)
is.win <- function() .Platform$OS.type == "windows"
is.linux <- function() (.Platform$OS.type == "unix") && (is.mac() == FALSE)
is.unix <- function() .Platform$OS.type == "unix"
is.solaris <- function() grepl("solaris", R.version$os)


file.path2 <- function(...){
	dots <- list(...)
	if(.Platform$OS.type == "unix"){
		sep <- "/"
	} else {
		sep <- "\\"
	}
	paste0(dots, collapse = sep)
}

file.write_to_temp <- function(contents) {
	tempname <- tempfile()

	writeLines(contents, tempname)
	return(tempname)
}
