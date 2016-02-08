height <- data.frame(
  id = 1:15,
  group = rep(c("monitor", "hmd", "cave"), each = 5),
  anx_lvl1 = c( 5,  5, 10, 10,  5, 10,  5, 15,  5,  5, 10, 15, 15,  5, 10),
  anx_lvl2 = c(15,  5, 15, 10, 10, 25, 20, 30, 25, 25, 30, 40, 35, 30, 45),
  anx_lvl3 = c(35, 35, 50, 40, 35, 50, 45, 50, 50, 55, 65, 40, 60, 65, 50)
)

height <- tidyr::gather(height, "level", "anxiety", anx_lvl1:anx_lvl3)
