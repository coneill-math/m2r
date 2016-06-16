




.g_m2code <- "
m2rint_inout = openInOut \"$:6666\";
while true do (
	m2rint_inline = read m2rint_inout;
	if m2rint_inline == \"\" then break;
	m2rint_outline = toString value(m2rint_inline);
	m2rint_inout << m2rint_outline << \"\\n\" << flush;
);

close m2rint_inout;
"




m2_start_m2 <- function(port) {
	if(is.mac() || is.unix()) {
		system2(
			file.path2("/usr/local/macaulay2/bin", "M2"),
			stdout = "/Users/chris/Downloads/tempfileout.txt",
			stderr = "/Users/chris/Downloads/tempfileerr.txt",
			stdin = file.write_to_temp(.g_m2code),
			wait = FALSE)
		Sys.sleep(1)
	}
	# else if(is.win()) {
	# 	# TODO: fix later
	# 	matFile <- file.path2(dir2, "countCode.latte")
	# 	matFile <- chartr("\\", "/", matFile)
	# 	matFile <- str_c("/cygdrive/c", str_sub(matFile, 3))
	#
	# 	system2(
	# 		"cmd.exe",
	# 		paste(
	# 			"/c env.exe",
	# 			file.path(getOption("latte_path"), "count"),
	# 			opts, matFile
	# 		), stdout = "countOut", stderr = "countErr"
	# 	)
	#
	# }
}

m2_begin_interact <- function(port = 6666) {
	# TODO: Implement port parameter
	# TODO: kill M2 if necessary/other error stuff


	if (!is.null(.g_m2con)) {
		invisible(1)
	}

	# start M2 server process
	m2_start_m2(port)

	# initialize client socket
	.g_m2con <<- socketConnection(host="localhost", port = 6666, blocking=TRUE, server=FALSE, open="r+", timeout=60*60*24*7)

	invisible(0)
}

m2_reset_interact <- function(port = 6666) {
	m2_end_interact()
	m2_begin_interact(port)
}

m2_end_interact <- function() {
	if (!is.null(.g_m2con)) {
		writeLines("", .g_m2con)
		close(.g_m2con)
		.g_m2con <<- NULL
	}
}

m2_interact <- function(line) {
	if (is.null(.g_m2con)) {
		return("m2r Error: M2 not started")
	}

	if (line == "") {
		return("")
	}

	writeLines(line, .g_m2con)
	server_resp <- readLines(.g_m2con, 1)

	return(server_resp)
}

# client <- function(){
# 	while(TRUE){
# 		con <- socketConnection(host="localhost", port = 6666, blocking=TRUE, server=FALSE, open="r+")
# 		f <- file("stdin")
# 		open(f)
# 		print("Enter text to be upper-cased, q to quit")
# 		sendme <- readLines(f, n=1)
# 		if(tolower(sendme)=="q"){
# 			break
# 		}
# 		write_resp <- writeLines("sendme5", con)
# 		server_resp <- readLines(con, -1)
# 		print(server_resp)
# 		print(paste("Your upper cased text:  ", server_resp))
# 		close(con)
# 	}
# }
# client()
