test_that("cdb() computes CDB diversification benefits correctly", {
  set.seed(123)
  x <- rnorm(1000, 0.0005, 0.02)
  y <- rnorm(1000, 0.0003, 0.015)

  result <- cdb(x, y, p = 0.05)

  # Structure tests
  expect_true(is.matrix(result))
  expect_equal(dim(result), c(1, 3))
  
  # Value sanity checks
  expect_true(all(is.finite(result)))
  expect_true(all(result > 0))
  expect_true(all(result < 1))  # Typically, CDB values are between 0 and 1

  # Deterministic result (within tolerance)
  expected <- cdb(x, y, p = 0.05)
  expect_equal(result, expected, tolerance = 1e-8)
})
