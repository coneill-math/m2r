context("gb  ")

test_that("gb(... = chars", {
  ring_(c("x","y","z"), "QQ")
  expect_equal(gb("x y", "x z", "x"), mpolyList(mp("x")))
})

test_that("gb(... = chars, ring = m2_polynomialring", {
  ring_(c("x","y","z"), "QQ")
  expect_equal(gb("x y", "x z", "x"), mpolyList(mp("x")))
})

test_that("gb(... = chars, ring = m2_polynomialring_pointer", {
  ring_.(c("x","y","z"), "QQ")
  expect_equal(gb("x y", "x z", "x"), mpolyList(mp("x")))
})

test_that("gb(... = chars, ring = char", {

})








context("gb_ ")

test_that("gb_(chars, ring = m2_polynomialring",{
  R <- ring_(c("x","y","z"), "QQ")
  out <- gb_(c("x y", "x z", "x"), ring = R)
  expect_equal(out, mpolyList(mp("x")))
})

test_that("gb_(chars, ring = m2_polynomialring_pointer",{
  R. <- ring_.(c("x","y","z"), "QQ")
  out <- gb_(c("x y", "x z", "x"), ring = R.)
  expect_equal(out, mpolyList(mp("x")))
})

test_that("gb_(chars, ring = char",{
  out <- gb_(c("x y", "x z", "x"), ring = "QQ[z,y,x]")
  expect_equal(out, mpolyList(mp("x")))
})



test_that("gb_(mpolyList, ring = m2_polynomialring",{
  R <- ring_(c("x","y","z"), "QQ")
  mpolys <- mp(c("x y", "x z", "x"))
  out <- gb_(mpolys, ring = R)
  expect_equal(out, mpolyList(mp("x")))
})

test_that("gb_(mpolyList, ring = m2_polynomialring_pointer",{
  R. <- ring_(c("x","y","z"), "QQ")
  mpolys <- mp(c("x y", "x z", "x"))
  out <- gb_(mpolys, ring = R.)
  expect_equal(out, mpolyList(mp("x")))
})

test_that("gb_(mpolyList, ring = char",{

  mpolys <- mp(c("x y", "x z", "x"))
  out <- gb_(mpolys, ring = "QQ[x,y,z]")
  expect_equal(out, mpolyList(mp("x")))
})



test_that("gb_(list o chars, ring = m2_polynomialring",{
  R <- ring_(c("x","y","z"), "QQ")
  out <- gb_(list("x y", "x z", "x"), ring = R)
  expect_equal(out, mpolyList(mp("x")))
})

test_that("gb_(list o chars, ring = m2_polynomialring_pointer",{
  R. <- ring_.(c("x","y","z"), "QQ")
  out <- gb_(list("x y", "x z", "x"), ring = R.)
  expect_equal(out, mpolyList(mp("x")))
})

test_that("gb_(list o chars, ring = char",{
  out <- gb_(list("x y", "x z", "x"), ring = "QQ[z,y,x]")
  expect_equal(out, mpolyList(mp("x")))
})



test_that("gb_(list o mpolys, ring = m2_polynomialring",{
  R <- ring_(c("x","y","z"), "QQ")
  out <- gb_(list(mp("x y"), mp("x z"), mp("x")), ring = R)
  expect_equal(out, mpolyList(mp("x")))
})

test_that("gb_(list o mpolys, ring = m2_polynomialring_pointer",{
  R. <- ring_.(c("x","y","z"), "QQ")
  out <- gb_(list(mp("x y"), mp("x z"), mp("x")), ring = R.)
  expect_equal(out, mpolyList(mp("x")))
})

test_that("gb_(list o mpolys, ring = char",{
  out <- gb_(list(mp("x y"), mp("x z"), mp("x")), ring = "QQ[z,y,x]")
  expect_equal(out, mpolyList(mp("x")))
})



test_that("gb_(c'd mpolys, ring = m2_polynomialring",{
  R <- ring_(c("x","y","z"), "QQ")
  expect_error(
    gb_(c(mp("x y"), mp("x z"), mp("x")), ring = R),
    "you appear to have used c*"
  )
})

test_that("gb_(c'd mpolys, ring = m2_polynomialring_pointer",{
  R. <- ring_.(c("x","y","z"), "QQ")
  expect_error(
    gb_(c(mp("x y"), mp("x z"), mp("x")), ring = R.),
    "you appear to have used c*"
  )
})

test_that("gb_(c'd mpolys, ring = char",{
  expect_error(
    gb_(c(mp("x y"), mp("x z"), mp("x")), ring = "QQ[z,y,x]"),
    "you appear to have used c*"
  )
})



test_that("gb_(m2_ideal, ring = m2_polynomialring",{
  I <- ideal_(mp(c("x y", "x z", "x")), ring_(c("x","y","z"), "QQ"))
  out <- gb_(I)
  expect_equal(out, mpolyList(mp("x")))
})

test_that("gb_(m2_ideal, ring = m2_polynomialring_pointer",{
  I <- ideal_(mp(c("x y", "x z", "x")), ring_.(c("x","y","z"), "QQ"))
  out <- gb_(I)
  expect_equal(out, mpolyList(mp("x")))
})


test_that("gb_(m2_ideal_pointer, ring = m2_polynomialring",{
  R <- ring_(c("x","y","z"), "QQ")
  I. <- ideal_.(mp(c("x y", "x z", "x")), R)
  out <- gb_(I.)
  expect_equal(out, mpolyList(mp("x")))
})

test_that("gb_(m2_ideal_pointer., ring = m2_polynomialring_pointer",{
  R. <- ring_.(c("x","y","z"), "QQ")
  I. <- ideal_.(mp(c("x y", "x z", "x")), R.)
  out <- gb_(I.)
  expect_equal(out, mpolyList(mp("x")))
})











context("gb_.")

# test_that("gb_.(... = chars, ring = m2_polynomialring",{
#
# })
#
# test_that("gb_.(... = chars, ring = m2_polynomialring_pointer",{
#
# })
