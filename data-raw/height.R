set.seed(1337)

height <- dplyr::data_frame(
  id = 1:50,
  group = factor(rep(c("treatment", "control"), each = 25)),
  sex = factor(sample(c("m", "f"), 50, replace = TRUE)),
  age = round(rnorm(50, 23, 3)),
  stai_trait = round(rnorm(50, 37, 3)),
  stai_state_pre = stai_trait + round(rnorm(50)),
  stai_state_post = stai_state_pre + round(c(rnorm(25, 3), rnorm(25, 5))),
  aq_anx = round(rnorm(50, 60, 5)),
  aq_avoi = round(rnorm(50, 18, 2)),
  anx_lvl1 = round(c(rnorm(25, 20, 3), rnorm(25, 23, 3))),
  anx_lvl2 = round(c(rnorm(25, 40, 4), rnorm(25, 42, 4))),
  anx_lvl3 = round(c(rnorm(25, 60, 5), rnorm(25, 64, 5)))
)

height <- as.data.frame(height)

set.seed(NULL)
