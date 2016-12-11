context("factor_n ")

test_that("factor_n(int)", {

  param1 <- list(
    218700,
    m2.("218700")
  )

  apply(expand.grid(param1, list(1)), 1, FUN = function(x) {

    factors <- factor_n(x[[1]])

    obj <- data.frame(prime = c(2L, 3L, 5L), power = c(2L, 7L, 2L))

    expect_equal(factors, obj)

  })

})




context("factor_n.")

test_that("factor_n.(formats)", {

  param1 <- list(
    218700,
    m2.("218700")
  )

  apply(expand.grid(param1, list(1)), 1, FUN = function(x) {

    factors <- factor_n.(x[[1]])

    obj <- m2_structure(
      m2_name = m2_name(factors),
      m2_class = "m2_pointer",
      m2_meta = list(
        ext_str = "new Product from {new Power from {2,2},new Power from {3,7},new Power from {5,2}}",
        m2_class = "Product",
        m2_class_class = "WrapperType"
      )
    )

    expect_equal(factors, obj)

  })

})







