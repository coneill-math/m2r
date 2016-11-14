context("factor_poly")

test_that("factor_poly(mpoly, m2_polynomialring)", {

  expect_equal(
    factor_poly(
      mp("x^4 - y^4"),
      ring(c("x","y"), "QQ", "lex")
    ),
    list(
      mpolyList = mp(c("x-y", "x+y", "x^2+y^2")),
      power = c(1L, 1L, 1L)
    )
  )

})
