test_that("t_test equals to t.test", {
  expect_equal(t.test(1:10, y = c(7:20))[], t_test(1:10, y = c(7:20))[-10])
})
