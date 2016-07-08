mpolyList_to_m2_str <- function(mpolyList) {
  # allow for character vectors
  if (class(mpolyList) == "mpoly") {
    mpolyList <- mpolyList(mpolyList)
  }
  if(is.character(mpolyList)) mpolyList <- mp(mpolyList)

  # convert mpolylist to strings readable by m2
  poly_str <- suppressMessages(paste0( lapply(mpolyList, print, stars=TRUE), collapse=", "))
  poly_str <- str_replace_all(poly_str, "\\*\\*", "^")

  poly_str
}

m2_pts_str_to_list <-function(m2_out) {
  m2_out <- str_sub(m2_out,2,-2)
  m2_out <- str_replace_all(m2_out, "p53", "")
  m2_out <- str_replace_all(m2_out, "toCC\\(", "complex(real=")
  m2_out <- str_replace_all(m2_out, ",complex", "Dcomplex")
  m2_out <- str_replace_all(m2_out, "\\},", "\\}")
  m2_out <- str_replace_all(m2_out, ",",",imaginary=")
  m2_out <- str_replace_all(m2_out, "Dcomplex", ",complex")
  m2_out <- str_replace_all(m2_out, "\\{", "c\\(")
  m2_out <- str_replace_all(m2_out, "\\}", "\\),")
  m2_out <- paste0("list(",str_sub(m2_out,0,-2),")")
  m2_out <- eval(parse(text=m2_out))
  m2_out
}
