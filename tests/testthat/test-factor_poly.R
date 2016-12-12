context("factor_poly ")


test_that("factor_poly(formats)", {

  # this MUST come first!
  param2 <- list(
    ring (c("x","y"), "QQ"),
    ring.(c("x","y"), "QQ")
  )

  param1 <- list(
    c  ("x^4 - y^4"),
    mp ("x^4 - y^4"),
    m2.("x^4 - y^4")
  )

  expected_output <- list(
    factor = mp(c("x-y", "x+y", "x^2+y^2")),
    power = c(1L, 1L, 1L)
  )

  apply(expand.grid(param1, param2), 1, FUN = function(x) {

    factors <- factor_poly(x[[1]], x[[2]])

    expect_equal(factors, expected_output)

  })

})


test_that("factor_poly(1)", {

  # this MUST come first!
  param2 <- list(
    ring (c("x","y"), "QQ")
  )

  param1 <- list(
    c  (1),
    c  ("1"),
    mp ("1")
  )

  expected_output <- list(
    factor = structure(list(), class = "mpolyList"),
    power = integer(0)
  )

  apply(expand.grid(param1, param2), 1, FUN = function(x) {

    factors <- factor_poly(x[[1]], x[[2]])

    expect_equal(factors, expected_output)

  })

})





context("factor_poly.")


test_that("factor_poly(formats)", {

  # this MUST come first!
  param2 <- list(
    ring (c("x","y"), "QQ"),
    ring.(c("x","y"), "QQ")
  )

  param1 <- list(
    c  ("x^4 - y^4"),
    mp ("x^4 - y^4"),
    m2.("x^4 - y^4")
  )

  apply(expand.grid(param1, param2), 1, FUN = function(x) {

    factors <- factor_poly.(x[[1]], x[[2]])

    expected_output <- m2_structure(
      m2_name = m2_name(factors),
      m2_class = "m2_pointer",
      m2_meta = list(
        ext_str = "new Product from {new Power from {x-y,1},new Power from {x+y,1},new Power from {x^2+y^2,1}}",
        m2_class = "Product",
        m2_class_class = "WrapperType"
      )
    )

    expect_equal(factors, expected_output)

  })

})

