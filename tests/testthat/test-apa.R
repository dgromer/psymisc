# Example data from Lakens, D. (2013). Calculating and reporting effect sizes to
# facilitate cumulative science: a practical primer for t-tests and ANOVAs. 
# Frontiers in Psychology, 4, 863. doi:10.3389/fpsyg.2013.00863

df <- data.frame(movie_1 = c(9, 7, 8, 9, 8, 9, 9, 10, 9, 9),
                 movie_2 = c(9, 6, 7, 8, 7, 9, 8, 8, 8, 7))

test_that("Output for t_apa between subject", {
  expect_match(t_apa(t_test(df$movie_1, df$movie_2, var.equal = TRUE),
                     print = FALSE),
               "t\\(18\\) = 2\\.52, p = \\.022, d = 1\\.13")
})

test_that("Output for t_apa within subject", {
  expect_match(t_apa(t_test(df$movie_1, df$movie_2, paired = TRUE),
                     print = FALSE),
               "t\\(9\\) = 4\\.74, p = \\.001, d = 1\\.50")
})

test_that("Formal structure of t_apa output)", {
  expect_match(t_apa(t_test(df$movie_1, df$movie_2, var.equal = TRUE),
                     print = FALSE),
               paste0("t\\([[:digit:]]+\\) [=<] [[:digit:]]+\\.[[:digit:]]{2},",
                      " p [=<] \\.[[:digit:]]{3}, d [=<] [[:digit:]]+\\.",
                      "[[:digit:]]{2}"))
  expect_match(t_apa(t_test(df$movie_1, df$movie_2), print = FALSE),
               paste0("t\\([[:digit:]]+\\.[[:digit:]]{2}\\) [=<] ",
                      "[[:digit:]]+\\.[[:digit:]]{2}, p [=<] \\.[[:digit:]]{3}",
                      ", d [=<] [[:digit:]]+\\.[[:digit:]]{2}"))
})

# Example data from Hollander, M. & Wolfe, D. A. (1973). Nonparametric
# Statistical Methods. New York: John Wiley & Sons. Pages 185–194.

x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
y <- c( 2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)

test_that("Output for cor_apa", {
  expect_match(cor_apa(cor.test(x, y, alternative = "greater"), print = FALSE),
               "r\\(7)\\ = \\.57, p = \\.054")
  expect_match(cor_apa(cor.test(x, y, method = "kendall",
                                alternative = "greater"), print = FALSE),
               "r_tau = \\.44, p = \\.060")
  expect_match(cor_apa(cor.test(x, y, method = "spearman",
                                alternative = "greater"), print = FALSE),
               "r_s = \\.60, p = \\.048")
})

test_that("Formal structure of cor_apa output", {
  expect_match(cor_apa(cor.test(x, y), print = FALSE),
               paste0("r\\([[:digit:]]+\\) [=<] \\.[[:digit:]]{2}, p [=<] \\.",
                      "[[:digit:]]{3}"))
})

# Example data from Agresti, A. (2007) An Introduction to Categorical Data
# Analysis, 2nd ed., New York: John Wiley & Sons. Page 38.

m <- matrix(c(762, 327, 468, 484, 239, 477), nrow = 2)
dimnames(m) <- list(gender = c("F", "M"),
                    party = c("Democrat","Independent", "Republican"))

test_that("Output for chisq_apa", {
  expect_match(chisq_apa(chisq.test(m), print = FALSE),
               "chi\\^2\\(2\\) = 242\\.30, p < \\.001")
  expect_match(chisq_apa(chisq.test(m), print_n = TRUE, print = FALSE),
               "chi\\^2\\(2, n = 2757\\) = 242\\.30, p < \\.001")
})

test_that("Formal structure for chisq_apa output", {
  expect_match(chisq_apa(chisq.test(m), print = FALSE),
               paste0("chi\\^2\\([[:digit:]]+\\) = [[:digit:]]+\\.",
                      "[[:digit:]]{2}, p [=<] \\.[[:digit:]]{3}"))
})

library(ez)
data(ANT)

library(dplyr, warn.conflicts = FALSE)

data <-
  ANT %>%
  filter(error == 0) %>%
  group_by(subnum, group, cue, flank) %>%
  summarise(rt = mean(rt)) %>%
  as.data.frame # ezANOVA does not support tbl_df

anova <- anova_apa(
  ezANOVA(data, dv = rt, wid = subnum, within = c(cue, flank),
          between = group, detailed = TRUE),
  print = FALSE
)

# test_that("Output for anova_apa", {
#   
# })

test_that("Formal structure for anova_apa output", {
  expect_equal(nrow(anova), 3 + 3 + 1 + 1)
})
