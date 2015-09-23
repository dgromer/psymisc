# Example data from Lakens, D. (2013). Calculating and reporting effect sizes to
# facilitate cumulative science: a practical primer for t-tests and ANOVAs. 
# Frontiers in Psychology, 4, 863. doi:10.3389/fpsyg.2013.00863

df <- data.frame(movie_1 = c(9, 7, 8, 9, 8, 9, 9, 10, 9, 9),
                 movie_2 = c(9, 6, 7, 8, 7, 9, 8, 8, 8, 7))

test_that("Output for t_apa() between subject", {
  expect_match(t_apa(t_test(df$movie_1, df$movie_2, var.equal = TRUE)),
               "t\\(18\\) = 2\\.52, p = \\.022, d = 1\\.13")
})

test_that("Output for t_apa() within subject", {
  expect_match(t_apa(t_test(df$movie_1, df$movie_2, paired = TRUE)),
               "t\\(9\\) = 4\\.74, p = \\.001, d = 1\\.50")
})