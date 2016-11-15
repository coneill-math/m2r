context("factor_poly")


# define objects common to the tests
expected_output <- list(mpolyList = mp(c("x-y", "x+y", "x^2+y^2")), power = c(1L, 1L, 1L))


# run tests
test_that("factor_poly(char, m2_polynomialring)", {
  expect_equal(factor_poly("x^4 - y^4", ring(c("x","y"), "QQ")), expected_output)
})

test_that("factor_poly(mpoly, m2_polynomialring)", {
  expect_equal(factor_poly(mp("x^4 - y^4"), ring(c("x","y"), "QQ")), expected_output)
})

test_that("factor_poly(m2_pointer, m2_polynomialring)", {
  expect_equal(factor_poly(m2.("x^4 - y^4"), ring(c("x","y"), "QQ")), expected_output)
})

test_that("factor_poly(char, m2_pointer)", {
  expect_equal(factor_poly("x^4 - y^4", ring.(c("x","y"), "QQ")), expected_output)
})


test_that("factor_poly(mpoly, m2_pointer)", {
  expect_equal(factor_poly(mp("x^4 - y^4"), ring.(c("x","y"), "QQ")), expected_output)
})

test_that("factor_poly(m2_pointer, m2_pointer)", {
  expect_equal(factor_poly(m2.("x^4 - y^4"), ring.(c("x","y"), "QQ")), expected_output)
})








context("factor_poly.")


# define objects common to the tests
# for some reason moving the next two lines to the top of this file doesn't work
QQxy <- ring(c("x","y"), "QQ")
QQxy_pointer <- ring.(c("x","y"), "QQ")

expected_output <- structure(
  list(
    ext_str = "new Product from {new Power from {x-y,1},new Power from {x+y,1},new Power from {x^2+y^2,1}}",
    m2_name = "m2o#",
    m2_class = "Product",
    m2_class_class = "WrapperType"
  ),
  .Names = c("ext_str", "m2_name", "m2_class", "m2_class_class"),
  class = c("m2_pointer", "m2")
)


# run tests
test_that("factor_poly.(char, m2_polynomialring)", {
  o <- factor_poly.("x^4 - y^4", QQxy)
  o$m2_name <- "m2o#"
  expect_equal(o, expected_output)
})

test_that("factor_poly.(mpoly, m2_polynomialring)", {
  o <- factor_poly.(mp("x^4 - y^4"), QQxy)
  o$m2_name <- "m2o#"
  expect_equal(o, expected_output)
})

test_that("factor_poly.(m2_pointer, m2_polynomialring)", {
  o <- factor_poly.(m2.("x^4 - y^4"), QQxy)
  o$m2_name <- "m2o#"
  expect_equal(o, expected_output)
})

test_that("factor_poly.(char, m2_pointer)", {
  o <- factor_poly.("x^4 - y^4", QQxy_pointer)
  o$m2_name <- "m2o#"
  expect_equal(o, expected_output)
})


test_that("factor_poly.(mpoly, m2_pointer)", {
  o <- factor_poly.(mp("x^4 - y^4"), QQxy_pointer)
  o$m2_name <- "m2o#"
  expect_equal(o, expected_output)
})

test_that("factor_poly.(m2_pointer, m2_pointer)", {
  o <- factor_poly.(m2.("x^4 - y^4"), QQxy_pointer)
  o$m2_name <- "m2o#"
  expect_equal(o, expected_output)
})
