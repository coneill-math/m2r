
do_launch_local_m2 <- function(port = 27436L, timeout = 10) {

  # prep for m2 server process
  if(is.mac() || is.unix()) {
    # cat(file.path2(get_m2_path(), "M2"), args = c("--script", system.file("server", "m2rserverscript.m2", package = "m2r"), toString(port)))
    system2(
      file.path2(get_m2_path(), "M2"), args = c("--script", system.file("server", "m2rserverscript.m2", package = "m2r"), toString(port)),
      stdout = NULL, stderr = NULL, stdin = "",
      # stdin = write_to_temp(m2_listen_code(port, timeout)),
      wait = FALSE
    )

  } else if(is.win()) {

    # from dos command line:
    # env.exe PATH=/usr/bin:/cygdrive/c/cygwin/lib/lapack /usr/bin/M2

    # tried (among others):
    # env.exe PATH=/usr/bin:$PATH:/cygdrive/c/cygwin/lib/lapack /usr/bin/M2
    # env.exe PATH=/usr/bin:$PATH:/cygdrive/c/cygwin/lib/lapack:/usr/share/Macaulay2:/usr/share/doc/Macaulay2:/usr/share/Macaulay2/Core /usr/bin/M2

    # it won't stop complaining unless you do this:
    # env.exe PATH=/usr/bin:/cygdrive/c/cygwin/lib/lapack /usr/bin/M2 --no-setup
    # note: if you run that, "exit" won't work in M2, you have to kill the process
    # manually from the task manager

    # this launches M2 with no errors... but kills it immediately
    # system("cmd /K C:\\cygwin\\bin\\env.exe PATH=/usr/bin:$PATH:/usr/share/Macaulay2/Core:/usr/share/doc/Macaulay2:/cygdrive/c/cygwin/lib/lapack /usr/bin/M2 --no-setup")

    # and this launches a session that doesn't die immediately session, but otherwise doesn't work.
    # e.g. m2("1+1") hangs

    # system2(
    #   "cmd",
    #   "/K C:\\cygwin\\bin\\env.exe PATH=/cygdrive/c/cygwin/lib/lapack:/usr/bin /usr/bin/M2 --no-setup",
    #   stdout = NULL, stderr = NULL,
    #   stdin = write_to_temp(m2_listen_code(port, timeout)),
    #   wait = FALSE
    # )

    stop("Running local instances of M2 is not yet supported.")

  }

}
