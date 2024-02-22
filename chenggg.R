#setwd("C:/Users/21492/Desktop/BIOSTAT620/620_project_1")
#Sys.setlocale("LC_TIME", "en_US.UTF-8")
rm(list = ls())
gc()
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(ggplot2)
library(scales)
df <- read_excel("ScreenTime_chenggg.xlsx")
df <- df[c(1:31), ]
df$Pickup.1st_EST <- format(as.POSIXct(df$Pickup.1st_PST, format = "%H:%M", tz = "America/Los_Angeles"), "%H:%M", tz = "America/New_York")
df <- df %>% select(-"Pickup.1st_PST")
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
df$Total.ST.min <- sapply(df$Total.ST, convert_to_minutes)
df$Social.ST.min <- sapply(df$Social.ST, convert_to_minutes)
df$prop_ST <- df$Social.ST.min / df$Total.ST.min
df$duration_per_use <- df$Total.ST.min / df$Pickups
df$is_weekday <- ifelse(wday(df$Date) %in% 2:6, 1, 0)

### calculate XX XY

X <- as.matrix(cbind(1, df$Total.ST.min, df$Social.ST.min, df$Pickups, df$duration_per_use,df$is_weekday))
Y <- df$prop_ST

XX <- t(X) %*% X  
XY <- t(X) %*% Y

XY_matrix <- matrix(XY, ncol = 1)

combined_matrix <- cbind(XX, XY = XY_matrix)

combined_df <- as.data.frame(combined_matrix)

write.csv(combined_df, file = "chenggg_XX_XY.csv", row.names = FALSE)

result_summary <- df %>%
  group_by(is_weekday) %>%
  summarise(
    mean_Total_ST_min = mean(Total.ST.min),
    sd_Total_ST_min = sd(Total.ST.min),
    q1_Total_ST_min = quantile(Total.ST.min, 0.25),
    q3_Total_ST_min = quantile(Total.ST.min, 0.75),
    min_Total_ST_min = min(Total.ST.min),
    max_Total_ST_min = max(Total.ST.min),
    
    mean_Social_ST_min = mean(Social.ST.min),
    sd_Social_ST_min = sd(Social.ST.min),
    q1_Social_ST_min = quantile(Social.ST.min, 0.25),
    q3_Social_ST_min = quantile(Social.ST.min, 0.75),
    min_Social_ST_min = min(Social.ST.min),
    max_Social_ST_min = max(Social.ST.min),
    
    mean_Pickups = mean(Pickups),
    sd_Pickups = sd(Pickups),
    q1_Pickups = quantile(Pickups, 0.25),
    q3_Pickups = quantile(Pickups, 0.75),
    min_Pickups = min(Pickups),
    max_Pickups = max(Pickups),
    
    mean_prop_ST = mean(prop_ST),
    sd_prop_ST = sd(prop_ST),
    q1_prop_ST = quantile(prop_ST, 0.25),
    q3_prop_ST = quantile(prop_ST, 0.75),
    min_prop_ST = min(prop_ST),
    max_prop_ST = max(prop_ST),
    
    mean_duration_per_use = mean(duration_per_use),
    sd_duration_per_use = sd(duration_per_use),
    q1_duration_per_use = quantile(duration_per_use, 0.25),
    q3_duration_per_use = quantile(duration_per_use, 0.75),
    min_duration_per_use = min(duration_per_use),
    max_duration_per_use = max(duration_per_use)
  )

statistical_summary <- result_summary %>%
  pivot_longer(cols = -is_weekday) %>%
  separate(name, into = c("variable", "stat"), sep = "_") %>%
  pivot_wider(names_from = stat, values_from = value)

write.csv(statistical_summary, file = "SummaryStatchenggg.csv", row.names = TRUE)

df$Date <- as.Date(df$Date)
df$is_weekday <- factor(df$is_weekday)
proportion.plot = ggplot(df, aes(x = Date, y = prop_ST, 
                                 color = is_weekday, group = 1)) +
  geom_line(color = "steelblue") +
  geom_point() +
  xlab("Date") +
  ylab("Proportion of Social Screen Time") +
  ylim(0,max(df$prop_ST)+0.1) +
  scale_color_manual(labels = c("No Course Today","Have Course Today"), 
                     values = c("black", "red")) +
  scale_x_date(labels = date_format("%m/%d")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        legend.title = element_blank())
ggsave("./Figure/line_plot_chenggg.png", plot = proportion.plot, width = 10, height = 6, units = "in")

boxplot_plot <- ggplot(df, aes(x = is_weekday, y = prop_ST, fill = is_weekday)) +
  geom_boxplot() +
  xlab("Weekday") +
  ylab("Proportion of Social Screen Time") +
  #scale_fill_manual(values = c("black", "red")) +  # Adjust fill colors as needed
  theme_minimal()

ggsave("./Figure/box_plot_chenggg.png", plot = proportion.plot, width = 10, height = 6, units = "in")

