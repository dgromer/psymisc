context("recode")

test_that("string recoding", {
  expect_equal(recode(c("female", "male", "male", "female"), "female" = "f",
                      "male" = "m"),
               c("f", "m", "m", "f"))
})

test_that("interval recoding", {
  expect_equal(recode(1:10, "1:5" = "a", "6:10" = "b"),
               c(rep("a", 5), rep("b", 5)))
  expect_equal(recode(1:20, "1:10" = 5, "11:20" = 15),
               c(rep(5, 10), rep(15, 10)))
})

test_that("vector recoding", {
  expect_equal(recode(letters[1:6], "c('a', 'b', 'c')" = "abc",
                      "c('d', 'e', 'f')" = "def"),
               c(rep("abc", 3), rep("def", 3)))
})

test_that("recode with default value", {
  expect_equal(recode(c(1, 5, 8, 3), "1" = "one", "3" = "three", "5" = "five",
                      default = "no idea"),
               c("one", "five", "no idea", "three"))
  expect_equal(recode(c(1, 5, 8, 3), "1:4" = "x", "6:9" = "y",
                      default = "default"),
               c("x", "default", "y", "x"))
})

test_that("recode with coerce argument", {
  expect_equal(recode(1:4, "4" = "1", "3" = "2", "2" = "3", "1" = "4",
                      coerce = as.numeric),
               4:1)
})
