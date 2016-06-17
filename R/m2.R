#' Initialize, call, and stop a Macaulay2 process
#'
#' Initialize, call, and stop a Macaulay2 process
#'
#' @param port port for Macaulay2 socket
#' @param code Macaulay2 code
#' @return m2 return value
#' @name m2_init
#' @examples
#'
#' \dontrun{ requires Macaulay2 be installed
#'
#' start_m2()
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
#' stop_m2()
#'
#'
#'
#' }





m2_listen_code <- function (port) {
  sprintf("
    m2rint_inout = openInOut \"$:%d\";
    while true do (
      m2rint_inline = read m2rint_inout;
      if m2rint_inline == \"\" then break;
      m2rint_outline = toString value(m2rint_inline);
      m2rint_inout << m2rint_outline << \"\\n\" << flush;
    );
    close m2rint_inout;
  ", port)
}
# cat(m2_listen_code(6666L))









#' @export
#' @rdname m2_init
start_m2 <- function(port = 27436L) {

  # prep for m2 server process
  if(is.mac() || is.unix()) {

    system2(
      file.path2(getOption("m2_path"), "M2"),
      stdout = NULL, stderr = NULL,
      stdin = write_to_temp(m2_listen_code(port)),
      wait = FALSE
    )
    Sys.sleep(1)

  }  else if(is.win()) {

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





#' @export
#' @rdname m2_init
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
#' @rdname m2_init
m2 <- function(code) {

  # preempt m2 kill code
  if (code == "") return("")

  # grab connection
  m2_con <- getOption("m2_con")

  # write to connection
  writeLines(code, m2_con)

  # read from connection and return
  readLines(m2_con, 1)

}
