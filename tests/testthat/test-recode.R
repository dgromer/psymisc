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

test_that("logical recoding", {

  # Smaller than
  expect_equal(recode(1:5, "< 3" = 0), c(rep(0, 2), 3:5))
  expect_equal(recode(c(.3, 1.5, .8), "< .9" = 0), c(0, 1.5, 0))

  # Smaller than or equal to
  expect_equal(recode(1:5, "<= 3" = 0), c(rep(0, 3), 4:5))
  expect_equal(recode(c(.123, .8, .34), "<= .34" = 0), c(0, .8, 0))

  # Greater than
  expect_equal(recode(1:20, "> 15" = 15), c(1:15, rep(15, 5)))
  expect_equal(recode(c(.9, .231, .71, .48), "> 0.703" = 1), c(1, .231, 1, .48))

  # Equal to or greater
  expect_equal(recode(1:5, ">= 4" = 5), c(1:3, rep(5, 2)))
  expect_equal(recode(c(.8, .3, .5), ">= .5" = 1.2), c(1.2, .3, 1.2))

  # With logical and
  expect_equal(recode(1:5, "<= 2" = 1, "> 2 & <= 3" = 5, "> 3" = 10),
               c(rep(1, 2), 5, rep(10, 2)))
  expect_equal(recode(1:6, "< 3" = "small", ">= 3 & <= 5" = "medium",
                      "> 5" = "large"),
               c(rep("small", 2), rep("medium", 3), "large"))
  expect_equal(recode(c(.3, .7, .1, .6, .9), "> .2 & < .8" = .5),
               c(rep(.5, 2), .1, .5, .9))

  # With logical or
  expect_equal(recode(1:5, "< 2 | > 4" = "outlier", default = "normal"),
               c("outlier", rep("normal", 3), "outlier"))
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
