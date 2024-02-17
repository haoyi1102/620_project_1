setwd("C:/Users/21492/Desktop/BIOSTAT620/620_project_1")
rm(list = ls())
gc()
library(readxl)
library(dplyr)
df <- read_excel("ScreenTime_chenggg.xlsx")
df <- df[c(1:31), ]
df$Pickup.1st_EST <- format(as.POSIXct(df$Pickup.1st_PST, format = "%H:%M", tz = "America/Los_Angeles"), "%H:%M", tz = "America/New_York")

convert_to_minutes <- function(time) {
  if (!grepl("h", time)) {
    return(as.numeric(sub("m", "", time)))
  }
  parts <- strsplit(time, "h|m")[[1]]
  as.numeric(parts[1]) * 60 + as.numeric(parts[2])
}
df$Total.ST.min <- sapply(df$Total.ST, convert_to_minutes)
df$Social.ST.min <- sapply(df$Social.ST, convert_to_minutes)
df$prop_ST <- df$Social.ST.min / df$Total.ST.min
df$duration_per_use <- df$Total.ST.min / df$Pickups

# Function to calculate summary statistics and return a data frame
calculate_summary <- function(data, variable) {
  summary_df <- data %>% 
    summarise(
      Mean = mean(get(variable), na.rm = TRUE),
      SD = sd(get(variable), na.rm = TRUE),
      Min = min(get(variable), na.rm = TRUE),
      Max = max(get(variable), na.rm = TRUE),
      Median = median(get(variable), na.rm = TRUE),
      Q1 = quantile(get(variable), 0.25, na.rm = TRUE),
      Q3 = quantile(get(variable), 0.75, na.rm = TRUE)
    )
  return(summary_df)
}

# Apply the function to each numeric variable
numerical_variables <- sapply(df, is.numeric)

summary_statistics_df <- do.call(rbind, lapply(names(df)[numerical_variables], calculate_summary, data = df))
rownames(summary_statistics_df) <- names(df)[numerical_variables]
