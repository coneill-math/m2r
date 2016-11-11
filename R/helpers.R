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

write_to_temp <- function(contents) {
  tempname <- tempfile()
  writeLines(contents, tempname)
  tempname
}

name_and_increment <- function(prefix, option) {
  # grab # of current rings, set ring number, and increment
  num <- getOption(option)
  num <- ifelse(is.null(num), 1L, strtoi(num))
  setOption(option, num + 1)

  # make name
  sprintf("m2rint%s%08d", prefix, num)
}




listify <- function (strings) paste0("{", paste(strings, collapse = ","), "}")

listify_mat <- function (mat) listify(apply(t(mat), 1, listify))
# (mat <- matrix(1:9, nrow = 3))
# listify_mat(mat)




delistify <- function (string, f, collapse, ...) {

  # dewhiten
  s <- str_replace_all(string, "[\\s]+", "")

  # kill outside {}
  s <- str_sub(s, 3, -3)

  # break it up
  s <- str_split(s, fixed("},{"))[[1]]
  s <- str_split(s, fixed(","))

  # f, if available
  if ( !missing(f) ) s <- lapply(s, f, ...)

  # collapse, if available
  if ( !missing(collapse) ) s <- do.call(collapse, s)

  # return
  s

}
# string <- "{{1,2,3}, {1,34,45}, {2213,1123,6543}, {0,0,0}}"
# delistify(string)
# str(delistify(string))
# delistify(string, as.integer)
# delistify(string, as.integer, rbind)






mpolyList_to_m2_str <- function(mpolyList) {

  # allow for character vectors
  if (class(mpolyList) == "mpoly") {
    mpolyList <- mpolyList(mpolyList)
  }

  # parse it if it's a character string
  if(is.character(mpolyList)) mpolyList <- mp(mpolyList)

  # convert mpolylist to strings readable by m2
  paste0( lapply(mpolyList, print, silent = TRUE), collapse=", ")

}


