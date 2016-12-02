context("snf")

test_that("snf works properly", {

  M <- matrix(c(
       1,    2,    3,
       1,   34,   45,
    2213, 1123, 6543,
       0,    0,    0
  ), nrow = 4, byrow = TRUE)

  param1 <- list(
    M,
    m2_matrix(M),
    m2_matrix.(M)
  )

  apply(expand.grid(param1, list(1)), 1, FUN = function(x) {

    mats <- snf(x[[1]])
    P <- mats$P; D <- mats$D; Q <- mats$Q

    # test P
    expect_equal(
      P, matrix(c(
        1L, 33471L, -43292L, 0L,
        0L, 1L, 0L, 0L,
        0L, 0L, 1L, 0L,
        0L, 0L, 0L, 1L
      ), nrow = 4, byrow = TRUE)
    )

    # test D
    expect_equal(
      D, matrix(c(
        135654L, 0L, 0L,
        0L, 1L, 0L,
        0L, 0L, 1L,
        0L, 0L, 0L
      ), nrow = 4, byrow = TRUE)
    )

    # test Q
    expect_equal(
      Q, matrix(c(
        171927L, -42421L,  54868L,
         93042L, -22957L,  29693L,
        -74119L,  18288L, -23654L
      ), nrow = 3, byrow = TRUE)
    )

    # this is testing M2 more than m2r, skipping
    #
    # # decomposition
    # expect_equal(D, P %*% M %*% Q)
    #
    # # unitary
    # expect_equal(abs(det(P)), 1)
    # expect_equal(abs(det(Q)), 1)
    #
    # # D is diag
    # expect_equal(
    #   D,
    #   rbind(diag(diag(D)), matrix(0L, ncol = ncol(D), nrow = nrow(D) - length(diag(D))))
    # )

  })

})
