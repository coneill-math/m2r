context("ideal  ")



context("ideal_ ")



test_that("ideal_ takes a variety of params",{

  param2 <- list(
    ring_ (c("x","y"), "QQ"),
    ring_.(c("x","y"), "QQ")
  )

  param1 <- list(
    list(c("x+y", "x^2+y^2"), FALSE),
    list(mp(c("x+y", "x^2+y^2")), FALSE),
    list(list("x+y", "x^2+y^2"), FALSE),
    list(list(mp("x+y"), mp("x^2+y^2")), FALSE),
    list("x+y, x^2+y^2", TRUE),
    list(c("x+y", "x^2+y^2"), TRUE)
  )

  apply(expand.grid(param1, param2), 1, FUN = function(x) {

    I <- ideal_(x[[c(1,1)]], x[[2]], raw_chars = x[[c(1,2)]])

    obj <- m2_structure(
      m2_name = m2_name(I),
      m2_class = "m2_ideal",
      m2_meta = list(
        ring = m2_parse(x[[2]]),
        gens = mp(c("x+y", "x^2+y^2"))
      )
    )

    expect_equal(I, obj)

  })

})



test_that("ideal_ errors with certain params",{

  param1 <- list(
    list(c(mp("x+y"), mp("x^2+y^2")), FALSE),
    list("x+y x^2+y^2", TRUE)
  )

  param2 <- list(
    ring_ (c("x","y"), "QQ"),
    ring_.(c("x","y"), "QQ")
  )

  apply(expand.grid(param1, param2), 1, FUN = function(x) {

    msg <- "you appear to have used c*"
    if (x[[c(1,2)]]) msg <- "Macaulay2 Error"

    expect_error(
      ideal_(x[[c(1,1)]], x[[2]], raw_chars = x[[c(1,2)]]),
      msg
    )

  })

})






context("ideal_.")


test_that("ideal_. takes a variety of params",{

  param1 <- list(
    list(c("x+y", "x^2+y^2"), FALSE),
    list(mp(c("x+y", "x^2+y^2")), FALSE),
    list(list("x+y", "x^2+y^2"), FALSE),
    list(list(mp("x+y"), mp("x^2+y^2")), FALSE),
    list("x+y, x^2+y^2", TRUE),
    list(c("x+y", "x^2+y^2"), TRUE)
  )

  param2 <- list(
    ring_ (c("x","y"), "QQ"),
    ring_.(c("x","y"), "QQ")
  )

  apply(expand.grid(param1, param2), 1, FUN = function(x) {

    I <- ideal_.(x[[c(1,1)]], x[[2]], raw_chars = x[[c(1,2)]])

    obj <- m2_structure(
      m2_name = m2_name(I),
      m2_class = "m2_pointer",
      m2_meta = list(
        ext_str = paste(
          "ideal map((", m2_name(x[[2]]),
          ")^1,(", m2_name(x[[2]]),
          ")^{{-1},{-2}},{{x+y, x^2+y^2}})",
          sep = ""
        ),
        m2_class = "Ideal",
        m2_class_class = "Type"
      )
    )

    expect_equal(I, obj)

  })

})



test_that("ideal_. errors with certain params",{

  param1 <- list(
    list(c(mp("x+y"), mp("x^2+y^2")), FALSE),
    list("x+y x^2+y^2", TRUE)
  )

  param2 <- list(
    ring_ (c("x","y"), "QQ"),
    ring_.(c("x","y"), "QQ")
  )

  apply(expand.grid(param1, param2), 1, FUN = function(x) {

    msg <- "you appear to have used c*"
    if (x[[c(1,2)]]) msg <- "Macaulay2 Error"

    expect_error(
      ideal_.(x[[c(1,1)]], x[[2]], raw_chars = x[[c(1,2)]]),
      msg
    )

  })

})










