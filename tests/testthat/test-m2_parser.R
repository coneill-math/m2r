context("m2r parser")

m2_parse_test <- function(m2_str, m2_expected_output) {
  out <- m2_parse(m2_str)
  if (is.m2(out) && "m2_name" %in% names(out)) out$m2_name <- ""

  expect_equal(out, m2_expected_output)
}

test_that("m2_parse parses basic data structures",{
  m2_list <- "{1,2,3}"
  m2_list_output <- structure(list(1,2,3),class = c("m2_list","m2"))
  m2_parse_test(m2_list,m2_list_output)

  m2_array <- "[1,2,3]"
  m2_array_output <- structure(list(1,2,3), class = c("m2_array", "m2"))
  m2_parse_test(m2_array,m2_array_output)

  m2_sequence <- "(1,2,3)"
  m2_sequence_output <- structure(list(1,2,3), class = c("m2_sequence", "m2"))
  m2_parse_test(m2_sequence, m2_sequence_output)

  m2_str <- "\"my string\""
  m2_str_output <- "my string"
  m2_parse_test(m2_str, m2_str_output)

  m2_opt <- "c => 4"
  m2_opt_output <- structure(list(structure("c",class = c("m2_symbol", "m2")), 4), class = c("m2_option", "m2"))
  m2_parse_test(m2_opt,m2_opt_output)

  m2_new_thing <- "new HashTable from {A => 1}"
  m2_new_thing_output <- structure(list(structure(list(structure("A", class = c("m2_symbol", "m2")),1),class = c("m2_option", "m2"))), class = c("m2_hashtable", "m2"))
  m2_parse_test(m2_new_thing, m2_new_thing_output)

  # toExternalString(matrix{{1,2,3},{4,5,6}})
  m2_matrix <- "map((ZZ)^2,(ZZ)^3,{{1, 2, 3}, {4, 5, 6}})"
  m2_matrix_output <- structure(list(m2_name = "", rmatrix = matrix(c(1,4,2,5,3,6), nrow = 2, ncol = 3), ring = field_as_ring("ZZ")), class = c("m2_matrix", "m2"))
  m2_parse_test(m2_matrix,m2_matrix_output)

  # toExternalString(matrix{{}})
  m2_empty_matrix <- "map((ZZ)^1,(ZZ)^0,0)"
  m2_empty_matrix_output <- structure(list(m2_name = "", rmatrix = matrix(numeric(0), nrow = 0, ncol = 0), ring = field_as_ring("ZZ")), class = c("m2_matrix", "m2"))
  m2_parse_test(m2_empty_matrix,m2_empty_matrix_output)

  # Test case where one ring has another ring as a prefix
  m2_ring1 <- m2("R = CC[x]")
  m2_ring2 <- m2("S = R[y]")
  m2_second_ring <- "R(monoid[y, Degrees => {1}, Heft => {1}, MonomialOrder => VerticalList{MonomialSize => 32, GRevLex => {1}, Position => Up}, DegreeRank => 1])"
  m2_second_ring_output <- structure(list(m2_name = "", coefring = "CC", vars = c("x","y"), order = "grevlex"), class = c("m2_polynomialring", "m2"))
  m2_parse_test(m2_second_ring,m2_second_ring_output)

  # map(S,R,{s^3-t^2, s^3-t, s-t})
  # m2_ring1 <- m2("R = QQ[a,b,c]")
  # m2_ring2 <- m2("S = QQ[s,t]")
  # m2_ring_map <- "map(S,R,{s^3-t^2, s^3-t, s-t})"
  # m2_parse(m2_ring_map)
  # m2_ring_map_output <- " "
})

test_that("m2 parses harder data structures", {
  m2_poly_list <- "{b*c*e-a*d*f, a*c*e-b*d*f}"
  m2_poly_list_output <- structure(list(structure("b*c*e-a*d*f", class = c("m2_expression", "m2")),structure("a*c*e-b*d*f", class = c("m2_expression", "m2"))),class = c("m2_list", "m2"))
  m2_parse_test(m2_poly_list, m2_poly_list_output)

  # toExternalString(factor(2^4*3^3))
  m2_factor <- "new Product from {new Power from {2,4},new Power from {3,3}}"
  m2_factor_output <- structure(list(structure(list(2,4),class = c("m2_power" , "m2")),structure(list(3,3),class = c("m2_power" , "m2"))), class = c("m2_product", "m2"))
  m2_parse_test(m2_factor, m2_factor_output)

  m2_compound_test <- "new OptionTable from {Headline => \"test\", Configuration => new OptionTable from {}}"
  m2_compound_test_output <- structure(list(structure(list(structure("Headline",class=c("m2_symbol", "m2")),"test"),class = c("m2_option","m2")), structure(list(structure("Configuration",class = c("m2_symbol", "m2")),structure(list(),class=c("m2_optiontable","m2"))), class = c("m2_option", "m2"))), class = c("m2_optiontable", "m2"))
  m2_parse_test(m2_compound_test, m2_compound_test_output)

  # R = QQ[a,b,c,d,e,f]
  # toExternalString(ideal(a*b*c-d*e*f,a*c*e-b*d*f))
  m2("R = QQ[a,b,c,e,d,f]")
  m2_ideal <- "map((R)^1,(R)^{1},{{a*b*c-d*e*f, a*c*e-b*d*f}})"
  m2_ideal_output <- structure(list(m2_name = "", rmatrix = structure(list(structure(list(c("a"=1,"b"=1,"c"=1,"coef"=1), c("d"=1,"e"=1,"f"=1,"coef"=-1)), class=c("mpoly")), structure(list(c("a"=1,"c"=1,"e"=1,"coef"=1), c("b"=1,"d"=1,"f"=1,"coef"=-1)), class=c("mpoly"))), dim=c(1,2)), ring = structure(list(m2_name = "R", coefring = "QQ", vars = c("a","b","c","e","d","f"),order="grevlex"),class = c("m2_polynomialring","m2"))),class=c("m2_matrix","m2"))
  m2_parse_test(m2_ideal, m2_ideal_output)

  # toExternalString(CC[x,y])
  m2_ring <- "CC_53(monoid[x..z, a..d, Degrees => {2:1}, Heft => {1}, MonomialOrder => VerticalList{MonomialSize => 32, GRevLex => {2:1}, Position => Up}, DegreeRank => 1])"
  m2_ring_output <- structure(list(m2_name = "", coefring = "CC", vars = c("x", "y", "z", "a", "b", "c", "d"), order = "grevlex"), class = c("m2_polynomialring", "m2"))
  m2_parse_test(m2_ring, m2_ring_output)
})
