context("routlier")

data <- data.frame(x = 1:5, y = 6:10)

test_that("set outliers to NA using absolute values", {
  expect_equal(routlier(data, x, absolute = c(2, NA)),
               data.frame(x = c(NA, 2:5), y = 6:10))
  expect_equal(routlier(data, x, absolute = c(NA, 4)),
               data.frame(x = c(1:4, NA), y = 6:10))
  expect_equal(routlier(data, x, absolute = c(2, 4)),
               data.frame(x = c(NA, 2:4, NA), y = 6:10))
  expect_equal(routlier(data, x, y, absolute = c(2, 9)),
               data.frame(x = c(NA, 2:5), y = c(6:9, NA)))
})

test_that("flag outliers using absolute values", {
  expect_equal(routlier(data, x, action = "flag", absolute = c(2, NA)),
               cbind(data, outlier = c(TRUE, rep(FALSE, 4))))
  expect_equal(routlier(data, x, action = "flag", absolute = c(NA, 4)),
               cbind(data, outlier = c(rep(FALSE, 4), TRUE)))
  expect_equal(routlier(data, x, action = "flag", absolute = c(2, 4)),
               cbind(data, outlier = c(TRUE, rep(FALSE, 3), TRUE)))
  expect_equal(routlier(data, x, y, action = "flag", absolute = c(2, 9)),
               cbind(data, outlier = c(TRUE, rep(FALSE, 3), TRUE)))
})

test_that("remove outliers using absolute values", {
  expect_equal(routlier(data, x, action = "remove", absolute = c(2, NA)),
               data[-1, ])
  expect_equal(routlier(data, x, action = "remove", absolute = c(NA, 4)),
               data[-5, ])
  expect_equal(routlier(data, x, action = "remove", absolute = c(2, 4)),
               data[c(-1, -5), ])
  expect_equal(routlier(data, x, y, action = "remove", absolute = c(2, 9)),
               data[c(-1, -5), ])
})

data <- data.frame(x = c(rep(1, 15), 10), y = c(1, -10, rep(1, 14)))

test_that("set outliers to NA using z-scores", {
  expect_equal(routlier(data, x),
               data.frame(x = c(rep(1, 15), NA), y = c(1, -10, rep(1, 14))))
  expect_equal(routlier(data, x, y),
               data.frame(x = c(rep(1, 15), NA), y = c(1, NA, rep(1, 14))))
})

test_that("flag outliers to NA using z-scores", {
  expect_equal(routlier(data, x, action = "flag"),
               cbind(data, outlier = c(rep(FALSE, 15), TRUE)))
  expect_equal(routlier(data, x, y, action = "flag"),
               cbind(data, outlier = c(FALSE, TRUE, rep(FALSE, 13), TRUE)))
})

test_that("remove outliers using z-scores", {
  expect_equal(routlier(data, x, action = "remove"), data[-16, ])
  expect_equal(routlier(data, x, y, action = "remove"), data[c(-2, -16), ])
})
