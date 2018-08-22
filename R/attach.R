.onAttach <- function(...) {

  packageStartupMessage('  Please cite m2r! See citation("m2r") for details.')

  # find M2 on a Mac or Linux
  if (is.mac() || is.linux()) unix_search_and_set("M2", "Macaulay2", "m2_path")

  # find M2 on a PC - directs to cloud immediately
  if (is.win()) win_search_and_set("m2")

  # check that the programs were found
  startup_check_for_program()

  # set gmp
  set_m2r_option(gmp = FALSE)

  # return
  invisible(TRUE)
}




.onDetach <- function(...) {
  stop_m2()
  options(m2r = NULL)
}
# restart R
# library(m2r)
# m2("1+1")
# getOption("m2r")
# detach("package:m2r")
# getOption("m2r")




# unix_find looks for a specific executable in a specific directory
# (or its children)
# however, we don't just use this on / because it'd take forever
# so unix_search_and_set uses unix_find to search specific directories
unix_find <- function(exec, where){

  # query the system and clean attributes
  query <- sprintf("find %s -name %s", where, exec)
  finding <- suppressWarnings(system(query, intern = TRUE, ignore.stderr = TRUE))
  attributes(finding) <- NULL

  # get the bin first
  path <- finding[stringr::str_detect(finding, paste0("bin/", exec))][1]

  # bertini isn't in a bin directory
  if(is.na(path)) path <- finding[1]

  # return
  path
}




startup_check_for_program <- function(){

  if(!is.null(get_m2_path())){
    psms("  M2 found in %s", get_m2_path())
    return(invisible(FALSE))
  }

  if(is.null(get_m2_path())){
    psms("  M2 not found; defaulting to cloud.")
    psms("  Use set_m2_path(\"/path/to/m2\") to run M2 locally.")
    return(invisible(FALSE))
	}

  invisible(TRUE)
}




psm  <- packageStartupMessage

psms <- function(fmt, ...) packageStartupMessage(sprintf(fmt, ...))


setOption <- function(optionName, value){
  eval(parse(text = sprintf('options("%s" = "%s")', optionName, value)))
}


unix_search_and_set <- function(exec, baseName, optionName){

  # grab path and parse
  if (file.exists("~/.bash_profile")) {
    profile_to_look_for <- ".bash_profile"
  } else if (file.exists("~/.bashrc")) {
    profile_to_look_for <- ".bashrc"
  } else if (file.exists("~/.profile")) {
    profile_to_look_for <- ".profile"
  } else {
    return(invisible(FALSE))
  }

  # PATH <- system(sprintf("source ~/%s; echo $PATH", profile_to_look_for), intern = TRUE)
  # the above doesn't work on ubuntu, which uses the dash shell (which doesn't have source)
  PATH <- system(sprintf("echo 'source ~/%s; echo $PATH' | /bin/bash", profile_to_look_for), intern = TRUE)
  dirs_to_check <- stringr::str_split(PATH, ":")[[1]]

  # check for main dir name
  ndx_with_baseName_dir  <- which(stringr::str_detect(tolower(dirs_to_check), baseName))
  baseName_path <- dirs_to_check[ndx_with_baseName_dir]

  # seek and find
  for(path in dirs_to_check){
    found_path <- unix_find(exec, path)
    if(!is.na(found_path)) break
  }

  # break in a failure
  if(is.na(found_path)) return(invisible(FALSE))

  # set option and exit
  set_m2r_option(m2_path = dirname(found_path))

  # invisibly return path
  invisible(dirname(found_path))
}





whereis_is_accessible <- function() unname(Sys.which("whereis")) != ""

win_find <- function(s){
  wexe <- unname(Sys.which("whereis"))
  x <- system(paste(wexe, s), intern = TRUE)
  str_sub(x, nchar(s)+2)
}

win_search_and_set <- function(optionName){

  set_m2r_option(m2_path = NULL)

}





# set_m2r_option both sets options for m2r in the list m2r in options
# and initialized the list when m2r is attached to the search path
# (search())
set_m2r_option <- function(...) {

  # if there is no m2r option (package is being initialized)
  # create the list with the arguments and return
  if ("m2r" %notin% names(options())) {
    options(m2r = list(...))
    return(invisible())
  }

  # otherwise, go through arguments sequentially and add/update
  # them in the list m2r in options
  m2r <- getOption("m2r")
  arg_list <- lapply(as.list(match.call())[-1], eval, envir = parent.frame())
  for (k in seq_along(arg_list)) {
    if (names(arg_list)[k] %in% names(m2r)) {
      m2r[names(arg_list)[k]] <- arg_list[k]
    } else {
      m2r <- c(m2r, arg_list[k])
    }
  }

  # set new m2r
  options(m2r = m2r)

  # return
  invisible()
}
# (l <- list(a = 1, b = 2, c = 3))
# l[d] <- 5
# l










