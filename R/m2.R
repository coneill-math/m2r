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
#' m2.("1+1")
#' m2("1+1")
#'
#' m2.("peek(QQ[x,y,z])")
#' m2("peek(QQ[x,y,z])")
#'
#' m2.("new MutableList from {1,2,3}")
#' m2("new MutableList from {1,2,3}")
#'
#' }





m2_listen_code <- function (port, timeout) {
  sprintf("
    m2rintinout = openInOut \"$:%d\";
    m2rintruncount = 1;
    while true do (
      m2rintinline = read m2rintinout;
      if m2rintinline == \"\" then break;

      m2rintretcode = 0;
      m2rintoutvalsucceeded = false;
      m2rintoutlinesucceeded = false;
      try (
        m2rintoutval := value(m2rintinline);
        m2rintoutvalsucceeded = true;

        m2rintoutclass = class m2rintoutval;
        m2rintoutclassclass = class m2rintoutclass;

        m2rintvarname = \"m2o\" | toString(m2rintruncount);
        value(m2rintvarname | \" = m2rintoutval;\");
        m2rintruncount = m2rintruncount + 1;

        m2rintoutline = toExternalString m2rintoutval;
        m2rintoutlinesucceeded = true;
      );

      if not m2rintoutvalsucceeded then (
        m2rintoutline = \"Macaulay2 Error!\";
        m2rintretcode = 1;
      ) else if not m2rintoutlinesucceeded then (
        m2rintoutline = \"Macaulay2 toExternalString Error!\";
        m2rintretcode = 2;
      );

      m2rintnumlines = 1 + #select(\"\\n\", m2rintoutline);

      m2rintinout << m2rintretcode << \" \" << m2rintnumlines << \" \"
                  << toString(m2rintvarname) << \" \"
                  << toString(m2rintoutclass) << \" \"
                  << toString(m2rintoutclassclass) << \"\\n\"
                  << m2rintoutline << \"\\n\" << flush;
    );
    close m2rintinout;
  ", port)
}
# cat(m2_listen_code(27436L, 10))








start_m2 <- function(port = 27436L, timeout = 10, attempts = 10) {

  for(i in seq.int(0,attempts-1)) {
    if (do_start_m2(port, timeout) == 0) break
    if (i == attempts - 1) {
      message(sprintf("%s attempts made at connecting. Aborting start_m2", attempts))
    } else {
      message(sprintf("Unable to connect to M2 on port %s. Attempting to connect on port %s", port, port + 1))
      port = port + 1
    }
  }

}






do_start_m2 <- function(port = 27436L, timeout = 10) {

  # only start if not already running
  if (!is.null(get_m2_con())) return(invisible(0))

  message("Starting M2")

  # prep for m2 server process
  if(is.mac() || is.unix()) {

    system2(
      file.path2(get_m2_path(), "M2"),
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
  for (i in 0:(20*timeout)) {

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
      break
    } else {
      Sys.sleep(0.05)
    }

  }

  if (is.null(con)) return(invisible(1L))

  # set options
  set_m2r_option(
    m2_con = con,
    m2_port = port,
    m2_timeout = timeout
  )
  set_m2r_option(m2_procid = strtoi(m2("processID()")))

  invisible(0L)
}









stop_m2 <- function() {

  if (!is.null(get_m2_con())) { # for detaching when m2 never run

    # send kill code
    writeLines("", get_m2_con())
    close(get_m2_con())

    # not elegant, but a necessary safety measure
    Sys.sleep(0.01)
    tools::pskill(get_m2_procid())

    set_m2r_option(m2_con = NULL, m2_procid = NULL)

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
  do.call(m2., as.list(match.call())[-1])$ext_str
}





#' @export
#' @rdname m2_call
m2. <- function(code, timeout = -1) {

  # ensure m2 is running
  start_m2()

  # preempt m2 kill code
  if (code == "") return("")

  # write to connection
  writeLines(code, get_m2_con())

  i <- 0
  outinfo <- NULL
  repeat {
    # read output info
    outinfo <- readLines(get_m2_con(), 1)

    if (length(outinfo) > 0) break

    i <- i + 1
    if (timeout > 0 && i >= timeout * 20) {
      break
    } else {
      Sys.sleep(0.0005)
    }
  }

  if (length(outinfo) > 0) {
    # read output from connection and return
    info <- strsplit(outinfo, " ", fixed = TRUE)[[1]]

    retcode <- strtoi(info[1])
    numlines <- strtoi(info[2])
    m2_name <- info[3]
    m2_class <- info[4]
    m2_class_class <- info[5]
  } else {
    # cancel command if needed
    tools::pskill(get_m2_procid(), tools::SIGINT)
    Sys.sleep(0.01)

    retcode <- -1L
    numlines <- -1L
  }

  output <- paste(readLines(get_m2_con(), numlines), collapse = "\n")

  if (retcode == -1L) {
    # timeout occurred, kill M2 instance and stop
    stop_m2()
    # start_m2(getOption("m2_port"), getOption("m2_timeout"))
    stop("Command timed out, M2 connection lost")
  } else if (retcode == 1L) {
    # user's code string errored, alert them
    stop(output)
  } else if (retcode == 2L) {
    # toExternalString failed, make ext_str NULL
    output <- NULL
  }

  # aggregate and class
  out <- list(
    ext_str = output,
    m2_name = m2_name,
    m2_class = m2_class,
    m2_class_class = m2_class_class
  )
  class(out) <- c("m2_pointer", "m2")

  # return
  out
}





# if (type2 == "Type") {
#   setOption("m2_returntype", type1)
# } else if (type2 == "Ring") {
#   setOption("m2_returntype", paste("RingElement", type1))
# } else {
#   setOption("m2_returntype", paste(c(type1,type2), sep = ","))
# }





#' @export
#' @rdname m2_call
print.m2_pointer <- function (x, ...) {
  cat("M2 Pointer Object\n")
  if(is.null(x$ext_str)) x$ext_str <- ""
  w <- min(c(options()$width, 80), na.rm = TRUE) - 19
  if(nchar(x$ext_str) > w) {
    ext_str <- str_c(str_sub(x$ext_str, 1, w-4), "...")
  } else {
    ext_str <- x$ext_str
  }
  cat(sprintf("  ExternalString : %s\n", ext_str))
  # cat(sprintf("          R Name : %s\n", deparse(substitute(x))))
  cat(sprintf("         M2 Name : %s\n", x$m2_name))
  cat(sprintf("        M2 Class : %s (%s)\n", x$m2_class, x$m2_class_class))
}



