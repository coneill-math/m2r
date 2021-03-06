% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set_paths.R
\name{m2_path}
\alias{m2_path}
\alias{set_m2_path}
\alias{get_m2_path}
\alias{get_m2_connection}
\alias{get_m2_con}
\alias{get_m2_procid}
\alias{get_m2_port}
\title{Set path to Macaulay2 (M2)}
\usage{
set_m2_path(path = NULL)

get_m2_path()

get_m2_connection()

get_m2_con()

get_m2_procid()

get_m2_port()
}
\arguments{
\item{path}{A character string, the path to M2}
}
\value{
An invisible character string, the path found.  More importantly, the
function has the side effect of setting the global m2r option "m2_path"
}
\description{
These are helper functions that deal with pathing to Macaulay2 and asking if it
is present. When the Macaulay2 package is loaded it attempts to find the
Macaulay2 executable by looking for an environment variable indicating where it
is, i.e. its path as specified in your .Renviron file.
}
\details{
For easiest use, you'll want to specify the path the Macaulay2 executable in
your ~/.Renviron file. It should look something like

\code{M2=/Applications/Macaulay2-1.10/bin}

You can set this permanently with \code{\link[=edit_r_environ]{edit_r_environ()}}. Note that absolute
paths should be specified, not relative paths, e.g. don't use ~/path/to/exe.

You can change this for the current session using \code{\link[=set_m2_path]{set_m2_path()}}, which
accepts a character string or, if missing, uses \code{\link[=file.choose]{file.choose()}} to let you
interactively; you just select an arbitrary executable.

On Windows, m2r just defaults to the cloud implementation. Local M2 instances
are not currently supported on Windows.
}
\examples{

\dontrun{ requires Macaulay2


getOption("m2r")
get_m2_path()
set_m2_path()


## each of these functions can be used statically as well
(m2_path <- get_m2_path())
set_m2_path("/path/to/m2/directory")
get_m2_path()
set_m2_path(m2_path) # undoes example


# if you'd like to use the cloud, after you library(m2r)
# and before you use m2() type
set_m2_path(NULL)

# alternatively, if you have already been using m2, do:
stop_m2()
set_m2_path(NULL)
m2("1+1")


}
}
\author{
David Kahle \email{david@kahle.com}
}
