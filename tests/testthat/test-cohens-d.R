# Example data from Lakens, D. (2013). Calculating and reporting effect sizes to
# facilitate cumulative science: a practical primer for t-tests and ANOVAs. 
# Frontiers in Psychology, 4, 863. doi:10.3389/fpsyg.2013.00863

df <- data.frame(movie_1 = c(9, 7, 8, 9, 8, 9, 9, 10, 9, 9),
                 movie_2 = c(9, 6, 7, 8, 7, 9, 8, 8, 8, 7))

test_that("Between group cohen's d", {
  expect_equal(round(cohens_d(df$movie_1, df$movie_2), 2), 1.13)
})

test_that("Between group cohen's d, hedges correction", {
  expect_equal(round(cohens_d(df$movie_1, df$movie_2, corr = "hedges_g"), 2),
               1.08)
})

test_that("Within group cohen's d", {
  expect_equal(round(cohens_d(df$movie_1, df$movie_2, paired = TRUE), 2), 1.5)
})
