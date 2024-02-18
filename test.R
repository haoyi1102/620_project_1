rm(list = ls())
gc()
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

convert_to_minutes <- function(time) {
  if (!grepl("h", time)) {
    return(as.numeric(sub("m", "", time)))
  }
  if (!grepl("m", time)) {
    return(60*as.numeric(sub("h", "", time)))
  }
  parts <- strsplit(time, "h|m")[[1]]
  as.numeric(parts[1]) * 60 + as.numeric(parts[2])
}

# chenggg
df_chenggg <- read_excel("ScreenTime_chenggg.xlsx")
df_chenggg <- df_chenggg[c(1:31), ]
df_chenggg$Pickup.1st_EST <- format(as.POSIXct(df_chenggg$Pickup.1st_PST, format = "%H:%M", tz = "America/Los_Angeles"), "%H:%M", tz = "America/New_York")
df_chenggg <- df_chenggg%>% select(-"Pickup.1st_PST")
df_chenggg$Total.ST.min <- sapply(df_chenggg$Total.ST, convert_to_minutes)
df_chenggg$Social.ST.min <- sapply(df_chenggg$Social.ST, convert_to_minutes)
df_chenggg$prop_ST <- df_chenggg$Social.ST.min / df_chenggg$Total.ST.min
df_chenggg$duration_per_use <- df_chenggg$Total.ST.min / df_chenggg$Pickups
df_chenggg$is_weekday <- ifelse(wday(df_chenggg$Date) %in% 2:6, 1, 0)
# haoyi

df_haoyi <- read_excel("ScreenTime_haoyi.xlsx")
df_haoyi <- df_haoyi[c(1:31), ]
df_haoyi$Total.ST.min <- sapply(df_haoyi$Total.ST, convert_to_minutes)
df_haoyi$Social.ST.min <- sapply(df_haoyi$Social.ST, convert_to_minutes)
df_haoyi$prop_ST <- df_haoyi$Social.ST.min / df_haoyi$Total.ST.min
df_haoyi$duration_per_use <- df_haoyi$Total.ST.min / df_haoyi$Pickups
df_haoyi$is_weekday <- ifelse(wday(df_haoyi$Date) %in% 2:6, 1, 0)

#zihaohan
df_zihaohan <- read_excel("ScreenTimeZihaoHan.xlsx")
df_zihaohan$Total.ST.min <- sapply(df_zihaohan$Total.ST, convert_to_minutes)
df_zihaohan$Social.ST.min <- sapply(df_zihaohan$Social.ST, convert_to_minutes)
df_zihaohan$prop_ST <- df_zihaohan$Social.ST.min / df_zihaohan$Total.ST.min
df_zihaohan$duration_per_use <- df_zihaohan$Total.ST.min / df_zihaohan$Pickups
df_zihaohan$is_weekday <- ifelse(df_zihaohan$Date < as.Date("2024-01-10"), 0,
                                 ifelse(wday(df_zihaohan$Date) %in% 2:6, 1, 0))
df_combined <- rbind(df_chenggg, df_haoyi, df_zihaohan)

linear_model <- lm(prop_ST ~ Total.ST.min + Social.ST.min + Pickups + duration_per_use + is_weekday, data = df_combined)
summary(linear_model)

X <- as.matrix(cbind(1, df_combined$Total.ST.min, df_combined$Social.ST.min, 
                     df_combined$Pickups, df_combined$duration_per_use,
                     df_combined$is_weekday))
XX = t(X)%*%X
Y <- as.matrix(df_combined$prop_ST)
XY = t(X)%*%Y
beta = solve(XX) %*% XY


X_1 =  as.matrix(cbind(1, df_zihaohan$Total.ST.min, df_zihaohan$Social.ST.min, 
                           df_zihaohan$Pickups, df_zihaohan$duration_per_use,
                           df_zihaohan$is_weekday))
X_2 =  as.matrix(cbind(1, df_chenggg$Total.ST.min, df_chenggg$Social.ST.min, 
                           df_chenggg$Pickups, df_chenggg$duration_per_use,
                           df_chenggg$is_weekday))
X_3 = as.matrix(cbind(1, df_haoyi$Total.ST.min, df_haoyi$Social.ST.min, 
                      df_haoyi$Pickups, df_haoyi$duration_per_use,
                      df_haoyi$is_weekday))
XX_1 = t(X_1) %*% X_1
XX_2 = t(X_2) %*% X_2
XX_3 = t(X_3) %*% X_3
XX_fed = XX_1 + XX_2 + XX_3

X_1_fed =  as.matrix(cbind(1, df_zihaohan$Total.ST.min, df_zihaohan$Social.ST.min, 
                       df_zihaohan$Pickups, df_zihaohan$duration_per_use,
                       df_zihaohan$is_weekday))



