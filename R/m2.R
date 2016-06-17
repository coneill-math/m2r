#' Call and reset a Macaulay2 process
#'
#' Call and reset a Macaulay2 process
#'
#' @param port port for Macaulay2 socket
#' @param code Macaulay2 code
#' @return m2 return value
#' @name m2_call
#' @examples
#'
#' \dontrun{ requires Macaulay2 be installed
#'
#' m2("1 + 1")
#'
#' m2("factor 32004")
#'
#' # compute a grobner basis of a polynomial ideal
#' (gb <- m2("
#'   R = QQ[a..d]
#'   I = ideal(a^3-b^2*c, b*c^2-c*d^2, c^3)
#'   G = gens gb I
#' "))
#'
#' # parse it into an mpoly object
#' library(stringr)
#' library(mpoly)
#' gb <- str_sub(gb, 10, -3)
#' gb <- str_replace_all(gb, "\\*", " ")
#' mp(str_split(gb, ", ")[[1]])
#'
#' m2("a = 1 + 1")
#' m2("a")
#' reset_m2()
#' m2("a")
#'
#' }

m2_listen_code <- function (port) {
  sprintf("
    m2rint_inout = openInOut \"$:%d\";
    while true do (
      m2rint_inline = read m2rint_inout;
      if m2rint_inline == \"\" then break;

      m2rint_retcode = 0;
      try ( m2rint_outline = toString value(m2rint_inline); )
  		else ( m2rint_outline = \"Macaulay2 Error!\"; m2rint_retcode = 1; );

      m2rint_numlines = 1 + #select(///\\\\n///, m2rint_outline);

      m2rint_inout << m2rint_retcode << \" \" << m2rint_numlines << \"\\n\" << flush;
      m2rint_inout << m2rint_outline << \"\\n\" << flush;
    );
    close m2rint_inout;
  ", port)
}
# cat(m2_listen_code(27436L))

start_m2 <- function(port = 27436L) {

  # grab connection
  m2_con <- getOption("m2_con")

  # send kill code
  if (!is.null(m2_con)) {
    return(invisible(0))
  }

  message("Starting M2")

  # prep for m2 server process
  if(is.mac() || is.unix()) {

    system2(
      file.path2(getOption("m2_path"), "M2"),
      stdout = NULL, stderr = NULL,
      stdin = write_to_temp(m2_listen_code(port)),
      wait = FALSE
    )
    Sys.sleep(1)

  } else if(is.win()) {

    # TODO: fix later
    # matFile <- file.path2(dir2, "countCode.latte")
    # matFile <- chartr("\\", "/", matFile)
    # matFile <- str_c("/cygdrive/c", str_sub(matFile, 3))
    # system2(
    #   "cmd.exe",
    #   paste(
    #     "/c env.exe",
    #     file.path(getOption("latte_path"), "count"),
    #     opts, matFile
    #   ),
    #   stdout = "countOut", stderr = "countErr"
    # )

  }

  # initialize client socket
  options(m2_con =
  	socketConnection(
      host = "localhost", port = port,
      blocking = TRUE, server = FALSE,
      open = "r+", timeout = 60*60*24*7
    )
  )

}

stop_m2 <- function() {

  # grab connection
  m2_con <- getOption("m2_con")

  # send kill code
  if (!is.null(m2_con)) {
    writeLines("", m2_con)
    close(m2_con)
    options(m2_con = NULL)
  }

}

#' @export
#' @rdname m2_call
reset_m2 <- function(port = 27436L) {

  # TODO: check port availability

  stop_m2()
  start_m2(port)

}

#' @export
#' @rdname m2_call
m2 <- function(code) {

  # ensure m2 is running
  start_m2()

  # preempt m2 kill code
  if (code == "") return("")

  # grab connection
  m2_con <- getOption("m2_con")

  # write to connection
  writeLines(code, m2_con)

  # read from connection and return
  outinfo <- readLines(m2_con, 1)

  info <- strsplit(outinfo, " ", fixed = TRUE)[[1]]
  retcode <- strtoi(info[1])
  numlines <- strtoi(info[2])

  output <- paste(readLines(m2_con, numlines), collapse="\n")

  if (retcode == 1) {
    stop(output)
  }

  output
}
