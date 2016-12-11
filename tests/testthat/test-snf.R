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

    expected_output <- list(
      D = m2_structure(
        matrix(c(
          135654L, 0L, 0L,
          0L, 1L, 0L,
          0L, 0L, 1L,
          0L, 0L, 0L
        ), nrow = 4, byrow = TRUE),
        m2_name = m2_name(mats$D),
        m2_class = "m2_matrix",
        m2_meta = list(
          ring = coefring_as_ring("ZZ")
        )
      ),
      P = m2_structure(
        matrix(c(
          1L, 33471L, -43292L, 0L,
          0L, 1L, 0L, 0L,
          0L, 0L, 1L, 0L,
          0L, 0L, 0L, 1L
        ), nrow = 4, byrow = TRUE),
        m2_name = m2_name(mats$P),
        m2_class = "m2_matrix",
        m2_meta = list(
          ring = coefring_as_ring("ZZ")
        )
      ),
      Q = m2_structure(
        matrix(c(
          171927L, -42421L,  54868L,
          93042L, -22957L,  29693L,
          -74119L,  18288L, -23654L
        ), nrow = 3, byrow = TRUE),
        m2_name = m2_name(mats$Q),
        m2_class = "m2_matrix",
        m2_meta = list(
          ring = coefring_as_ring("ZZ")
        )
      )
    )

    expect_equal(mats, expected_output)

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
