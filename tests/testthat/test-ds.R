context("ds")

library(dplyr, warn.conflicts = FALSE)

test_that("ds is a wrapper for group_by and summarise", {
  expect_equal(ds(sleep, extra ~ group),
               sleep %>% group_by(group) %>% summarise(mean = mean(extra),
                                                       se = se(extra)))
})

# Example data from Pfister, R., & Janczyk, M. (2013). Confidence intervals for
# two sample means: Calculation, interpretation, and a few simple rules.
# Advances in Cognitive Psychology, 9(2), 74.

data <- data.frame(group = c(rep("control", 5), rep("pheromones", 5)),
                   affection = c(7, 3, 4, 2, 5, 8, 5, 6, 5, 7))

res_ds <- ds(data, affection ~ group, funs = c("mean", "sd"))
res_ds$sd <- round(res_ds$sd, 2) # Round standard deviation

test_that("ds for mean and sd", {
  expect_equal(res_ds,
               data_frame(group = factor(c("control", "pheromones")),
                          mean = c(4.2, 6.2), sd = c(1.92, 1.3)))
})
