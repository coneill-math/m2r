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
