context("ci")

# Example data from Pfister, R., & Janczyk, M. (2013). Confidence intervals for
# two sample means: Calculation, interpretation, and a few simple rules.
# Advances in Cognitive Psychology, 9(2), 74.

test_that("Calculation of the confidence interval around the mean", {
  expect_equal(ci(c(7, 3, 4, 2, 5)),
               list(lower = 4.2 - 2.39, upper = 4.2 + 2.39),
               tolerance = .001)
  expect_equal(ci(c(8, 5, 6, 5, 7)),
               list(lower = 6.2 - 1.62, upper = 6.2 + 1.62),
               tolerance = .001)
  expect_equal(ci(c(8, 5, 6, 5, 7, NA)),
               list(lower = NA_integer_, upper = NA_integer_))
  expect_equal(ci(c(8, 5, 6, 5, 7, NA), na.rm = TRUE),
               list(lower = 6.2 - 1.62, upper = 6.2 + 1.62),
               tolerance = .001)
})

test_that("Calculatin of the margin of error", {
  expect_equal(moe(c(7, 3, 4, 2, 5)), 2.39, tolerance = .001)
  expect_equal(moe(c(8, 5, 6, 5, 7)), 1.62, tolerance = .001)
  expect_equal(moe(c(8, 5, 6, 5, 7, NA)), NA_integer_)
  expect_equal(moe(c(8, 5, 6, 5, 7, NA), na.rm = TRUE), 1.62, tolerance = .001)
})
