.onAttach <- function(...) {
	.g_m2con <<- NULL

	# find M2 on a Mac or Linux
	if(is.mac() || is.linux()){
		unix_search_and_set("M2", "Macaulay2", "m2_path")
	}

	# find M2 on a PC
	if(is.win()){
		if(!any(stringr::str_detect(tolower(list.files("C:\\")), "cygwin"))){
			psm("Cygwin is required to run m2 on a Windows platform.")
			psm("  It needs to be in your C:\\ drive, but wasn't found.")
			return(invisible())
		}
		if(!whereis_is_accessible()){ # check for whereis, return if not found
			psm(
				"The whereis function was not found, so algstat can't find the required exe's.\n",
				"  Try setting the path with set_m2_path()."
			)
			return()
		}
		win_search_and_set("m2")
	}

	# check that the programs were found
	startup_check_for_program("m2_path")

	# return
	invisible(TRUE)
}

.onDetach <- function(...) {
	# TODO: kill M2
	m2_end_interact()
}

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

startup_check_for_program <- function(optionName){

	longName <- longName(optionName)
	setFun <- setFun(optionName)

	if(!is.null(getOption(optionName))){
		psms("  %s found in %s", longName, getOption(optionName))
		return(invisible(FALSE))
	}

	if(is.null(getOption(optionName))){
		psms("  %s not found. Set the location with %s", longName, setFun)
		return(invisible(FALSE))
	}

	if(length(list.files(getOption(optionName))) == 0){
		psms("  %s appears to be installed, but it's not where it was expected.", longName)
		psms("  Suggestion : run %s", setFun)
		return(invisible(FALSE))
	}

	invisible(TRUE)

}

longName <- function(optionName){
	switch(optionName,
		   m2_path = "M2"
	)
}

execName <- function(optionName){
	switch(optionName,
		   m2_path = "count"
	)
}

setFun <- function(optionName){
	switch(optionName,
		   m2_path = "set_m2_path()"
	)
}

psm  <- packageStartupMessage
psms <- function(fmt, ...) packageStartupMessage(sprintf(fmt, ...))

setOption <- function(optionName, value){
	eval(parse(text = sprintf('options("%s" = "%s")', optionName, value)))
}

unix_search_and_set <- function(exec, baseName, optionName){
	# grab path and parse
	profile_to_look_for <-
		if(file.exists("~/.bash_profile")){
			".bash_profile"
		} else if(file.exists("~/.bashrc")){
			".bashrc"
		} else if(file.exists("~/.profile")){
			".profile"
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
	if(is.na(found_path)) return()

	# set option and exit
	setOption(optionName, dirname(found_path))

	# invisibly return path
	invisible(dirname(found_path))
}
