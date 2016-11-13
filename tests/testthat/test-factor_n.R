context("factor_n")

test_that("factor_n obtains correct result", {
  expect_equal(
    factor_n(218700),
    structure(
      c(2L, 3L, 5L, 2L, 7L, 2L),
      .Dim = c(3L, 2L), .Dimnames = list(NULL, c("prime", "power"))
    )
  )

})
