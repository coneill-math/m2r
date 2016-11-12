context("m2r parser")

m2_parse_test <- function(m2_str, m2_expected_output) {
  expect_equal(m2_parse(m2_tokenize(m2_str)), m2_expected_output)
}

test_that("m2_parse parses basic data structures",{
  m2_list <- "{1,2,3}"
  m2_list_output <- structure(list(1,2,3),class = c("m2_list","m2"))
  m2_parse_test(m2_list,m2_list_output)

  m2_array <- "[1,2,3]"
  m2_array_output <- structure(list(1,2,3), class = c("m2_array", "m2"))
  m2_parse_test(m2_array,m2_array_output)

  m2_str <- "\"my string\""
  str(m2_parse(m2_tokenize(m2_str)))
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
  m2_matrix_output <- " "
  #str(m2_parse(m2_tokenize(m2_matrix)))
  #m2_parse_test(m2_matrix,m2_matrix_output)
})

test_that("m2 parses compound data structures", {
  # toExternalString(CC[x,y])
  m2_ring <- "CC_53(monoid[x..y, Degrees => {2:1}, Heft => {1}, MonomialOrder => VerticalList{MonomialSize => 32, GRevLex => {2:1}, Position => Up}, DegreeRank => 1])"
  m2_ring_output <- " "
  #m2_parse_test(m2_ring, m2_ring_output)

  m2_poly_list <- "{b*c*e-a*d*f, a*c*e-b*d*f}"
  m2_poly_list_output <- structure(list(structure("b*c*e-a*d*f", class = c("m2_expression", "m2")),structure("a*c*e-b*d*f", class = c("m2_expression", "m2"))),class = c("m2_list", "m2"))
  m2_parse_test(m2_poly_list, m2_poly_list_output)

  # toExternalString(factor(2^4*3^3))
  m2_factor <- "new Product from {new Power from {2,4},new Power from {3,3}}"
  m2_factor_output <- structure(list(structure(list(2,4),class = c("m2_power" , "m2")),structure(list(3,3),class = c("m2_power" , "m2"))), class = c("m2_product", "m2"))
  #m2_parse_test(m2_factor, m2_factor_output)

  # toExternalString(ideal(a*b*c-d*e*f,a*c*e-b*d*f,a*d*f-b*c*e))
  m2_ideal <- "ideal map((R)^1,(R)^{{-3},{-3},{-3}},{{a*b*c-d*e*f, a*c*e-b*d*f, -b*c*e+a*d*f}})"
  m2_ideal_output <- " "
  #m2_parse_test(m2_ideal, m2_ideal_output)

  m2_compound_test <- "new OptionTable from {Headline => \"test\", Configuration => new OptionTable from {}}"
  m2_compound_test_output <- structure(list(structure(list(structure("Headline",class=c("m2_symbol", "m2")),"test"),class = c("m2_option","m2")), structure(list(structure("Configuration",class = c("m2_symbol", "m2")),structure(list(),class=c("m2_optiontable","m2"))), class = c("m2_option", "m2"))), class = c("m2_optiontable", "m2"))
  m2_parse_test(m2_compound_test, m2_compound_test_output)
})
