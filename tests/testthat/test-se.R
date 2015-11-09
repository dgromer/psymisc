context("se")

# Example data from Pfister, R., & Janczyk, M. (2013). Confidence intervals for
# two sample means: Calculation, interpretation, and a few simple rules.
# Advances in Cognitive Psychology, 9(2), 74.

test_that("Calculation of the standard error of the mean", {
  expect_equal(se(c(7, 3, 4, 2, 5)), .86, tolerance = .01)
  expect_equal(se(c(8, 5, 6, 5, 7)), .58, tolerance = .01)
  expect_equal(se(c(8, 5, 6, 5, 7, NA)), NA_integer_)
  expect_equal(se(c(8, 5, 6, 5, 7, NA), na.rm = TRUE), .58, tolerance = .01)
})
