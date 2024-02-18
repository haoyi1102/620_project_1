rm(list = ls())
gc()

haoyi_data <- read.csv("haoyi_XX_XY.csv")
chenggg_data <- read.csv("chenggg_XX_XY.csv")
zihaohan_data <- read.csv("zihaohan_XX_XY.csv")

extract_and_aggregate <- function(data_list) {
  total_XX <- 0
  total_XY <- 0
  
  for(data in data_list) {
    XX <- as.matrix(data[, -ncol(data)])
    XY <- as.matrix(data[, ncol(data)], ncol = 1)
    
    total_XX <- total_XX + XX
    total_XY <- total_XY + XY
  }
  
  total_beta <- solve(total_XX) %*% total_XY
  
  return(total_beta)
}

data_list <- list(haoyi_data, chenggg_data, zihaohan_data)
total_beta <- extract_and_aggregate(data_list)

beta_names <- c("(Intercept)", "Total.ST.min", "Social.ST.min", "Pickups", "prop_ST","duration")
names(total_beta) <- beta_names

beta_estimates <- as.numeric(total_beta)

beta_names <- attr(total_beta, "names")

total_beta_df <- data.frame(Coefficient = beta_names, Estimate = beta_estimates)

print(total_beta_df)