rm(list = ls())
gc()
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(circular)
library(scales)
time_to_degrees <- function(time) {
  # Convert time to hours and minutes
  hours <- as.numeric(substr(time, 1, 2))
  minutes <- as.numeric(substr(time, 4, 5))
  # Calculate the total hours since midnight
  total_hours <- hours + minutes / 60
  # Calculate the angle
  angle <- total_hours / 24 * 360
  return(angle)
}
# df_chenggg
df_chenggg <- read_excel("ScreenTime_chenggg.xlsx")

df_chenggg <- df_chenggg[c(1:31), ]
df_chenggg$Pickup.1st_EST <- format(as.POSIXct(df_chenggg$Pickup.1st_PST, format = "%H:%M", tz = "America/Los_Angeles"), "%H:%M", tz = "America/New_York")
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
df_chenggg$Pickup.1st_EST <- format(as.POSIXct(df_chenggg$Pickup.1st_PST, 
                                               format = "%H:%M", 
                                               tz = "America/Los_Angeles"),
                                    "%H:%M", tz = "America/New_York")
df_chenggg <- df_chenggg%>% select(-"Pickup.1st_PST")
df_chenggg$Pickup.1st.angular = data.frame(sapply(df_chenggg$Pickup.1st_EST, time_to_degrees))[,1]
df_chenggg$Total.ST.min <- sapply(df_chenggg$Total.ST, convert_to_minutes)
df_chenggg$Social.ST.min <- sapply(df_chenggg$Social.ST, convert_to_minutes)
df_chenggg$prop_ST <- df_chenggg$Social.ST.min / df_chenggg$Total.ST.min
df_chenggg$duration_per_use <- df_chenggg$Total.ST.min / df_chenggg$Pickups
df_chenggg$is_weekday <- ifelse(wday(df_chenggg$Date) %in% 2:6, 1, 0)
#df_haoyi

df_haoyi <- read_excel("ScreenTime_haoyi.xlsx")
df_haoyi <- df_haoyi[c(1:31), ]
# df_haoyi$Pickup.1st_EST <- format(as.POSIXct(df_haoyi$Pickup.1st_EST, format = "%H:%M", tz = "America/Los_Angeles"), "%H:%M", tz = "America/New_York")

convert_to_minutes <- function(time) {
  if (!grepl("h", time)) {
    return(as.numeric(sub("m", "", time)))
  }
  parts <- strsplit(time, "h|m")[[1]]
  as.numeric(parts[1]) * 60 + as.numeric(parts[2])
}

df_haoyi$Pickup.1st.angular = (hour(df_haoyi$Pickup.1st_EST)*60 + 
                                 minute(df_haoyi$Pickup.1st_EST))/(24*60)*360
df_haoyi$Total.ST.min <- sapply(df_haoyi$Total.ST, convert_to_minutes)
df_haoyi$Social.ST.min <- sapply(df_haoyi$Social.ST, convert_to_minutes)
df_haoyi$prop_ST <- df_haoyi$Social.ST.min / df_haoyi$Total.ST.min
df_haoyi$duration_per_use <- df_haoyi$Total.ST.min / df_haoyi$Pickups
df_haoyi$is_weekday <- ifelse(wday(df_haoyi$Date) %in% 2:6, 1, 0)

#df_zihaohan
df_zihaohan <- read_excel("ScreenTimeZihaoHan.xlsx")

convert_to_minutes <- function(time) {
  if (!grepl("h", time)) {
    return(as.numeric(sub("m", "", time)))
  }
  parts <- strsplit(time, "h|m")[[1]]
  as.numeric(parts[1]) * 60 + as.numeric(parts[2])
}
df_zihaohan$Pickup.1st.angular = (hour(df_zihaohan$Pickup.1st_EST)*60 + 
                                    minute(df_zihaohan$Pickup.1st_EST))/(24*60)*360
df_zihaohan$Total.ST.min <- sapply(df_zihaohan$Total.ST, convert_to_minutes)
df_zihaohan$Social.ST.min <- sapply(df_zihaohan$Social.ST, convert_to_minutes)
df_zihaohan$prop_ST <- df_zihaohan$Social.ST.min / df_zihaohan$Total.ST.min
df_zihaohan$duration_per_use <- df_zihaohan$Total.ST.min / df_zihaohan$Pickups

df_zihaohan$is_weekday <- ifelse(df_zihaohan$Date < as.Date("2024-01-10"), 0,
                        ifelse(wday(df_zihaohan$Date) %in% 2:6, 1, 0))
df_zihaohan$course_hours = rep(13)

# Add a new column to each dataframe to indicate the source of the data
df_chenggg$Source <- 'chenggg'
df_haoyi$Source <- 'haoyi'
df_zihaohan$Source <- 'zihaohan'

# Combine the three dataframes into one by appending them row-wise
df_combined <- rbind(df_chenggg, df_haoyi, df_zihaohan)
df_combined$Y = log(df_combined$prop_ST/(1-df_combined$prop_ST))

linear_model <- lm(Y ~ Total.ST.min + Social.ST.min + 
                     Pickups + duration_per_use + is_weekday
                   + Pickup.1st.angular + procrastination + BMI, 
                   data = df_combined)

# View the summary of the linear regression model
summary(linear_model)

 