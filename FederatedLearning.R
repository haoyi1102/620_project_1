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
  result = data.frame(total_beta = total_beta,
                      total_XX = total_XX,
                      total_XY = total_XY)
  return(result)
}

data_list <- list(haoyi_data, chenggg_data, zihaohan_data)
result = extract_and_aggregate(data_list)
total_beta = result[,1]
total_XY = result[,ncol(result)]
total_XX = result[,3:ncol(result)-1]
beta_names <- c("(Intercept)", "Total.ST.min", "Social.ST.min", "Pickups", "duration","is_weekday")
names(total_beta) <- beta_names

beta_estimates <- as.numeric(total_beta)

beta_names <- attr(total_beta, "names")

total_beta_df <- data.frame(Coefficient = beta_names, Estimate = beta_estimates)

print(total_beta_df)

# calculate the variance
YY_n_zihao = read.csv("zihaohan_YY_n.csv")
YY_n_chenggg = read.csv("chenggg_YY_n.csv")
YY_n_haoyi = read.csv("haoyi_YY_n.csv")


total_YY = YY_n_chenggg$YY+YY_n_haoyi$YY+YY_n_zihao$YY
total_n = YY_n_chenggg$n + YY_n_haoyi$n + YY_n_zihao$n
p = 6
beta_matrix = as.matrix(beta_estimates)

epsilon.2 = total_YY - 2 * t(beta_matrix) %*% total_XY + t(beta_matrix) %*% as.matrix(total_XX) %*% beta_matrix

sigma.square = epsilon.2/(total_n-p)

se.beta = sqrt(abs(as.numeric(sigma.square) * solve(total_XX))) 
  
se.beta = diag(se.beta)

t.value = beta_estimates/se.beta
p_values <- 2 * pt(-abs(t.value), df = total_n - p)

total_beta_df <- data.frame(Coefficient = beta_names, Estimate = beta_estimates, 
                            statistics = t.value,P.value = p_values)
write.csv(total_beta_df,"federalLearning_beta.csv",row.names = FALSE)

