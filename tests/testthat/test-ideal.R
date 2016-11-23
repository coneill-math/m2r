context("ideal ")

test_that("ideal(chars, ring = m2_polynomialring",{
  QQxy <- ring(c("x","y"), "QQ")
  I <- ideal(c("x+y", "x^2+y^2"), QQxy)
  obj <- structure(
    list(
      m2_name = I$m2_name,
      ring = QQxy,
      gens = mp(c("x+y", "x^2+y^2"))
    ),
    class = c("m2_ideal", "m2")
  )
  expect_equal(I, obj)
})

test_that("ideal(mpolyList, ring = m2_polynomialring",{
  QQxy <- ring(c("x","y"), "QQ")
  I <- ideal(mp(c("x+y", "x^2+y^2")), QQxy)
  obj <- structure(
    list(
      m2_name = I$m2_name,
      ring = QQxy,
      gens = mp(c("x+y", "x^2+y^2"))
    ),
    class = c("m2_ideal", "m2")
  )
  expect_equal(I, obj)
})

test_that("ideal(list o chars, ring = m2_polynomialring",{
  QQxy <- ring(c("x","y"), "QQ")
  I <- ideal(list("x+y", "x^2+y^2"), QQxy)
  obj <- structure(
    list(
      m2_name = I$m2_name,
      ring = QQxy,
      gens = mp(c("x+y", "x^2+y^2"))
    ),
    class = c("m2_ideal", "m2")
  )
  expect_equal(I, obj)
})

test_that("ideal(list o mpolys, ring = m2_polynomialring",{
  QQxy <- ring(c("x","y"), "QQ")
  I <- ideal(list(mp("x+y"), mp("x^2+y^2")), QQxy)
  obj <- structure(
    list(
      m2_name = I$m2_name,
      ring = QQxy,
      gens = mp(c("x+y", "x^2+y^2"))
    ),
    class = c("m2_ideal", "m2")
  )
  expect_equal(I, obj)
})

test_that("ideal(c'd mpolys, ring = m2_polynomialring",{
  QQxy <- ring(c("x","y"), "QQ")
  expect_error(
    ideal(c(mp("x+y"), mp("x^2+y^2")), QQxy),
    "you appear to have used c*"
  )
})





test_that("ideal(chars, ring = m2_polynomialring_pointer",{
  QQxy  <- ring (c("x","y"), "QQ")
  QQxy. <- ring.(c("x","y"), "QQ")
  I <- ideal(c("x+y", "x^2+y^2"), QQxy.)
  obj <- structure(
    list(
      m2_name = I$m2_name,
      ring = m2_parse(QQxy.),
      gens = mp(c("x+y", "x^2+y^2"))
    ),
    class = c("m2_ideal", "m2")
  )
  expect_equal(I, obj)
})

test_that("ideal(mpolyList, ring = m2_polynomialring_pointer",{
  QQxy. <- ring.(c("x","y"), "QQ")
  I <- ideal(mp(c("x+y", "x^2+y^2")), QQxy.)
  obj <- structure(
    list(
      m2_name = I$m2_name,
      ring = m2_parse(QQxy.),
      gens = mp(c("x+y", "x^2+y^2"))
    ),
    class = c("m2_ideal", "m2")
  )
  expect_equal(I, obj)
})

test_that("ideal(list o chars, ring = m2_polynomialring_pointer",{
  QQxy. <- ring.(c("x","y"), "QQ")
  I <- ideal(list("x+y", "x^2+y^2"), QQxy.)
  obj <- structure(
    list(
      m2_name = I$m2_name,
      ring = m2_parse(QQxy.),
      gens = mp(c("x+y", "x^2+y^2"))
    ),
    class = c("m2_ideal", "m2")
  )
  expect_equal(I, obj)
})

test_that("ideal(list o mpolys, ring = m2_polynomialring_pointer",{
  QQxy. <- ring.(c("x","y"), "QQ")
  I <- ideal(list(mp("x+y"), mp("x^2+y^2")), QQxy.)
  obj <- structure(
    list(
      m2_name = I$m2_name,
      ring = m2_parse(QQxy.),
      gens = mp(c("x+y", "x^2+y^2"))
    ),
    class = c("m2_ideal", "m2")
  )
  expect_equal(I, obj)
})

test_that("ideal(c'd mpolys, ring = m2_polynomialring_pointer",{
  QQxy. <- ring.(c("x","y"), "QQ")
  expect_error(
    ideal(c(mp("x+y"), mp("x^2+y^2")), QQxy.),
    "you appear to have used c*"
  )
})









context("ideal.")

# test_that("ideal.(chars, ring = m2_polynomialring",{
#   QQxy <- ring(c("x","y"), "QQ")
#   I. <- ideal.(c("x+y", "x^2+y^2"), QQxy)
#   obj <- structure(
#     list(
#       m2_name = I$m2_name,
#       ring = QQxy,
#       gens = mp(c("x+y", "x^2+y^2"))
#     ),
#     class = c("m2_ideal", "m2")
#   )
#   expect_equal(I, obj)
# })





