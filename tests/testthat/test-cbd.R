# tests/testthat/test-cdb.R

test_that("cdb() returns a scalar numeric for a given weight", {
  set.seed(123)
  x <- rnorm(1000, 0.0005, 0.02)
  y <- rnorm(1000, 0.0003, 0.015)

  w <- 0.30
  result <- cdb(x, y, p = 0.05, w = w)

  # Structure
  expect_type(result, "double")
  expect_length(result, 1L)
  expect_true(is.finite(result))

  # Typical range (CDB is usually in (0,1), but allow tiny numerical wiggle)
  expect_gt(result, 0)
  expect_lt(result, 1)

  # Deterministic for same inputs
  expected <- cdb(x, y, p = 0.05, w = w)
  expect_equal(result, expected, tolerance = 1e-12)
})

test_that("cdb() satisfies edge cases: w=0 and w=1 yield CDB=0", {
  set.seed(42)
  x <- rnorm(1000, 0.0005, 0.02)
  y <- rnorm(1000, 0.0003, 0.015)

  res_w0 <- cdb(x, y, p = 0.05, w = 0)
  res_w1 <- cdb(x, y, p = 0.05, w = 1)

  expect_equal(res_w0, 0, tolerance = 1e-12)
  expect_equal(res_w1, 0, tolerance = 1e-12)
})

test_that("cdb() input validation works", {
  x <- rnorm(10)
  y <- rnorm(10)

  # Missing or wrong types
  expect_error(cdb("not numeric", y, p = 0.05, w = 0.2), "`x` and `y` must be numeric")
  expect_error(cdb(x, y, p = -0.1, w = 0.2), "`p` must be a scalar in \\(0,1\\)")
  expect_error(cdb(x, y, p = 1.1, w = 0.2),  "`p` must be a scalar in \\(0,1\\)")
  expect_error(cdb(x, y, p = c(0.05, 0.1), w = 0.2), "`p` must be a scalar in \\(0,1\\)")

  # Weight validations
  expect_error(cdb(x, y, p = 0.05, w = -0.01), "`w` must be a scalar in \\[0,1\\]")
  expect_error(cdb(x, y, p = 0.05, w = 1.01),  "`w` must be a scalar in \\[0,1\\]")
  expect_error(cdb(x, y, p = 0.05, w = c(0.2, 0.3)), "`w` must be a scalar in \\[0,1\\]")
})
