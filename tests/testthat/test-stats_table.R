context("stats_table")

test_that("stats_table: two groups between", {

  library(dplyr)

  df <- data_frame(Variable = c("age", "trait_anx", "acrophobia", "sens_seek"),
                   M_control = c("30.60", "20.00", "14.00", "60.60"),
                   SD_control = c("6.91", "3.39", "4.30", "10.31"),
                   `M_height anx` = c("32.20", "20.60", "62.60", "43.40"),
                   `SD_height anx` = c("8.70", "5.03", "11.30", "13.03"),
                   t = c("-0.32", "-0.22", "-8.98", "2.31"),
                   p = c(".756", ".831", "< .001", ".051"),
                   d = c("-0.20", "-0.14", "-5.68", "1.46"))

  df <- as.data.frame(df)

  expect_identical(stats_table(hquest, iv = group, dvs = age:sens_seek), df)
})

test_that("stats_table: three groups between", {

  library(dplyr)

  df <- data_frame(Variable = "anxiety", M_cave = "34.33", SD_cave = "20.34",
                   M_hmd = "27.67", SD_hmd = "18.21", M_monitor = "19.00",
                   SD_monitor = "15.38", `F` = "32.24", p = "< .001",
                   petasq = ".84")

  df <- as.data.frame(df)

  expect_identical(stats_table(height, iv = group, dvs = anxiety), df)
})

test_that("stats_table: two groups within", {

  # Example data from Lakens, D. (2013). Calculating and reporting effect sizes to
  # facilitate cumulative science: a practical primer for t-tests and ANOVAs.
  # Frontiers in Psychology, 4, 863. doi:10.3389/fpsyg.2013.00863

  data <- data.frame(
    movie = rep(1:2, each = 10),
    rating = c(9, 7, 8, 9, 8, 9, 9, 10, 9, 9, 9, 6, 7, 8, 7, 9, 8, 8, 8, 7)
  )

  library(dplyr)

  df <- data_frame(Variable = "rating", M_1 = "8.70", SD_1 = "0.82",
                   M_2 = "7.70", SD_2 = "0.95", t = "2.52", p = ".022",
                   d = "1.13")

  df <- as.data.frame(df)

  expect_identical(stats_table(data, iv = movie, dvs = rating), df)
})

test_that("stats_table: three groups within", {

  library(dplyr)

  df <- data_frame(Variable = "anxiety", M_anx_lvl1 = "8.67",
                   SD_anx_lvl1 = "3.99", M_anx_lvl2 = "24.00",
                   SD_anx_lvl2 = "11.53", M_anx_lvl3 = "48.33",
                   SD_anx_lvl3 = "10.12", `F` = "118.90", p = "< .001",
                   petasq = ".89")

  df <- as.data.frame(df)

  expect_identical(stats_table(height, iv = level, dvs = anxiety,
                               paired = TRUE), df)
})

