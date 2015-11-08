context("routlier")

data <- data.frame(x = 1:5)

test_that("routlier using absolute values", {
  expect_equal(routlier(data, x, action = "remove", absolute = c(2, NA)),
               data[-1, , drop = FALSE])
  expect_equal(routlier(data, x, action = "remove", absolute = c(NA, 4)),
               data[-5, , drop = FALSE])
  expect_equal(routlier(data, x, action = "remove", absolute = c(2, 4)),
               data[c(-1, -5), , drop = FALSE])
})
