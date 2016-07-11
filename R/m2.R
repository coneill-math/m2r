#' Call and reset a Macaulay2 process
#'
#' Call and reset a Macaulay2 process
#'
#' @param port port for Macaulay2 socket
#' @param timeout number of seconds before aborting
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
#' m2("
#'   R = QQ[a..d]
#'   I = ideal(a^3-b^2*c, b*c^2-c*d^2, c^3)
#'   G = gens gb I
#' ")
#'
#' m2("a = 1 + 1")
#' m2("a")
#' reset_m2()
#' m2("a")
#'
#' }

m2_listen_code <- function (port, timeout) {
  sprintf("
    m2rintinout = openInOut \"$:%d\";
    while true do (
      m2rintinline = read m2rintinout;
      if m2rintinline == \"\" then break;

      m2rintretcode = 0;
      m2rintoutline = 0;
      try (
        m2rintoutval = value(m2rintinline);
        m2rintoutline = toString m2rintoutval;
        m2rintouttype = class m2rintoutval;
        m2rintouttypetype = class m2rintouttype;
      );

      if (try (m2rintoutline == 0) else false) then (
        m2rintoutline = \"Macaulay2 Error!\";
        m2rintretcode = 1;
      );

      m2rintnumlines = 1 + #select(\"\\n\", m2rintoutline);

      m2rintinout << m2rintretcode << \" \" << m2rintnumlines << \" \"
                  << toString(m2rintouttype) << \" \"
                  << toString(m2rintouttypetype) << \"\\n\"
                  << m2rintoutline << \"\\n\" << flush;
    );
    close m2rintinout;
  ", port)
}
# cat(m2_listen_code(27436L, 10))

do_start_m2 <- function(port = 27436L, timeout = 10) {

  # grab connection
  m2_con <- getOption("m2_con")

  # only start if not already running
  if (!is.null(m2_con)) {
    return(invisible(0))
  }

  message("Starting M2")

  # prep for m2 server process
  if(is.mac() || is.unix()) {

    system2(
      file.path2(getOption("m2_path"), "M2"),
      stdout = NULL, stderr = NULL,
      stdin = write_to_temp(m2_listen_code(port, timeout)),
      wait = FALSE
    )

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
  con <- NULL
  for (i in seq.int(0,20*timeout)) {

    tryCatch(
      con <- suppressWarnings(
        socketConnection(
          host = "localhost", port = port,
          blocking = FALSE, server = FALSE,
          open = "r+", timeout = 60*60*24*7
        )
      ),
      error = function(e) {  }
    )

    if (!is.null(con)) {
      break()
    } else {
      Sys.sleep(0.05)
    }

  }

  if (is.null(con)) {
    return(invisible(1))
  }

  options(m2_con = con)
  options(m2_procid = strtoi(m2("processID()")))
  options(m2_port = port)
  options(m2_timeout = timeout)
  return(invisible(0))
}

start_m2 <- function(port = 27436L, timeout = 10, attempts = 10) {


  for(i in seq.int(0,attempts-1)) {
    if (do_start_m2(port,timeout) == 0) {
      break
    }
    if(i == attempts - 1) {
      message(sprintf("%s attempts made at connecting. Aborting start_m2",attempts))
    } else {
      message(sprintf("Unable to connect to M2 on port %s. Attempting to connect on port %s", port, port + 1))
      port = port + 1
    }
  }
}

stop_m2 <- function() {

  # grab connection
  m2_con <- getOption("m2_con")
  m2_procid <- getOption("m2_procid")

  # send kill code
  if (!is.null(m2_con)) {
    writeLines("", m2_con)
    close(m2_con)

    # not elegant, but a necessary safety measure
    Sys.sleep(0.01)
    tools::pskill(m2_procid)

    options(m2_con = NULL)
    options(m2_procid = NULL)
  }

}

#' @export
#' @rdname m2_call
reset_m2 <- function(port = 27436L, timeout = 10) {

  stop_m2()
  start_m2(port, timeout)

}

#' @export
#' @rdname m2_call
m2 <- function(code, timeout = -1) {

  # ensure m2 is running
  start_m2()

  # preempt m2 kill code
  if (code == "") return("")

  # grab connection
  m2_con <- getOption("m2_con")
  m2_procid <- getOption("m2_procid")

  # write to connection
  writeLines(code, m2_con)

  i <- 0
  outinfo <- NULL
  while (TRUE) {
    # read output info
    outinfo <- readLines(m2_con, 1)

    if (length(outinfo) > 0) {
      break()
    }

    i <- i + 1
    if (timeout > 0 && i >= timeout * 20) {
      break()
    } else {
      Sys.sleep(0.05)
    }
  }

  if (length(outinfo) > 0) {
    # read output from connection and return
    info <- strsplit(outinfo, " ", fixed = TRUE)[[1]]

    retcode <- strtoi(info[1])
    numlines <- strtoi(info[2])
    type1 <- info[3]
    type2 <- info[4]
  } else {
    # cancel command if needed
    tools::pskill(m2_procid, tools::SIGINT)
    Sys.sleep(0.01)

    retcode <- 2
    numlines <- -1
  }

  output <- paste(readLines(m2_con, numlines), collapse="\n")

  if (retcode == 2) {
    stop_m2()
    # start_m2(getOption("m2_port"), getOption("m2_timeout"))
    stop("Command timed out, M2 connection lost")
  } else if (retcode == 1) {
    stop(output)
  }

  if (type2 == "Type") {
    setOption("m2_returntype", type1)
  } else if (type2 == "Ring") {
    setOption("m2_returntype", paste("RingElement", type1))
  } else {
    setOption("m2_returntype", paste(c(type1,type2), sep = ","))
  }

  output
}
