context("factor_n")

test_that("factor_n(int)", {

  expect_equal(
    factor_n(218700),
    structure(
      c(2L, 3L, 5L, 2L, 7L, 2L),
      .Dim = c(3L, 2L), .Dimnames = list(NULL, c("prime", "power"))
    )
  )

})

test_that("factor_n(m2_pointer)", {

  expect_equal(
    factor_n(m2.("218700")),
    structure(
      c(2L, 3L, 5L, 2L, 7L, 2L),
      .Dim = c(3L, 2L), .Dimnames = list(NULL, c("prime", "power"))
    )
  )

})






context("factor_n.")

test_that("factor_n.(int)", {

  expect_equal(
    factor_n.(218700)[-2],
    list(
      ext_str = "new Product from {new Power from {2,2},new Power from {3,7},new Power from {5,2}}",
      m2_class = "Product",
      m2_class_class = "WrapperType"
    )
  )

})

test_that("factor_n.(m2_pointer)", {

  expect_equal(
    factor_n.(m2.("218700"))[-2],
    list(
      ext_str = "new Product from {new Power from {2,2},new Power from {3,7},new Power from {5,2}}",
      m2_class = "Product",
      m2_class_class = "WrapperType"
    )
  )

})







