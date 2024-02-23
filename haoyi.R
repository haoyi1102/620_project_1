rm(list = ls())
gc()

library(readxl)
library(dplyr)
library(lubridate)
library(tidyverse)
library(gtsummary)
library(ggplot2)
library(scales)

df <- read_excel("ScreenTime_haoyi.xlsx")
df <- df[c(1:31), ]
# df$Pickup.1st_EST <- format(as.POSIXct(df$Pickup.1st_EST, format = "%H:%M", tz = "America/Los_Angeles"), "%H:%M", tz = "America/New_York")

convert_to_minutes <- function(time) {
  if (!grepl("h", time)) {
    return(as.numeric(sub("m", "", time)))
  }
  parts <- strsplit(time, "h|m")[[1]]
  as.numeric(parts[1]) * 60 + as.numeric(parts[2])
}

Pickup.1st.angular = (hour(df$Pickup.1st_EST)*60 + minute(df$Pickup.1st_EST))/(24*60)*360

df$Total.ST.min <- sapply(df$Total.ST, convert_to_minutes)
df$Social.ST.min <- sapply(df$Social.ST, convert_to_minutes)
df$prop_ST <- df$Social.ST.min / df$Total.ST.min
df$duration_per_use <- df$Total.ST.min / df$Pickups
df$is_weekday <- ifelse(wday(df$Date) %in% 2:6, 1, 0)

### calculate XX XY

X <- as.matrix(cbind(1, df$Total.ST.min, df$Social.ST.min, 
                     df$Pickups, df$duration_per_use,df$is_weekday,
                     Pickup.1st.angular,df$procrastination,df$BMI))
Y <- log(df$prop_ST/(1-df$prop_ST))

XX <- t(X) %*% X  
XY <- t(X) %*% Y

XY_matrix <- matrix(XY, ncol = 1)

combined_matrix <- cbind(XX, XY = XY_matrix)

combined_df <- as.data.frame(combined_matrix)

write.csv(combined_df, "haoyi_XX_XY.csv", row.names = FALSE)

### calculate YY and n

YY = t(Y) %*% Y
n = nrow(df)
data_to_save <- data.frame(YY = c(YY[,1]), n = n)
write.csv(data_to_save, file = "haoyi_YY_n.csv", row.names = FALSE)
###
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
    max_duration_per_use = max(duration_per_use),
    
    mean_procrastination = mean(procrastination),
    sd_procrastination = sd(procrastination),
    q1_procrastination = quantile(procrastination, 0.25),
    q3_procrastination = quantile(procrastination, 0.75),
    min_procrastination = min(procrastination),
    max_procrastination = max(procrastination),
    
    mean_BMI = mean(BMI),
    sd_BMI = sd(BMI),
    q1_BMI = quantile(BMI, 0.25),
    q3_BMI = quantile(BMI, 0.75),
    min_BMI = min(BMI),
    max_BMI = max(BMI),
    
    mean_course_hours = mean(course_hours),
    sd_course_hours = sd(course_hours),
    q1_course_hours = quantile(course_hours, 0.25),
    q3_course_hours = quantile(course_hours, 0.75),
    min_course_hours = min(course_hours),
    max_course_hours = max(course_hours),
    
    mean_Pickup.1st.angular = mean(Pickup.1st.angular),
    sd_Pickup.1st.angular = sd(Pickup.1st.angular),
    q1_Pickup.1st.angular = quantile(Pickup.1st.angular, 0.25),
    q3_Pickup.1st.angular = quantile(Pickup.1st.angular, 0.75),
    min_Pickup.1st.angular = min(Pickup.1st.angular),
    max_Pickup.1st.angular = max(Pickup.1st.angular),
    
  )

statistical_summary <- result_summary %>%
  pivot_longer(cols = -is_weekday) %>%
  separate(name, into = c("variable", "stat"), sep = "_") %>%
  pivot_wider(names_from = stat, values_from = value)


###
write.csv(statistical_summary, file = "SummaryStathaoyi.csv", row.names = TRUE)
### plot

statistical_summary <- statistical_summary[c(1,7),]

library(gt)
library(dplyr)

statistical_summary <- statistical_summary %>%
  mutate(
    is_weekday = case_when(
      is_weekday == 0 ~ "Weekends",
      is_weekday == 1 ~ "Weekday"
    )
  )

visual_table_1 <- statistical_summary %>%
  select(-variable) %>%
  gt() %>%
  tab_header(
    title = "Descriptive Statistics by Weekday",
    subtitle = "Presenting Mean Values"
  ) %>%
  cols_label(
    is_weekday = "Is Weekday",
    Total = "Total",
    Social = "Social",
    Pickups = "Pickups",
    prop = "Proportion",
    duration = "Duration",
    procrastination = "Procrastination",
    BMI = "BMI",
    course = "Course",
    Pickup.1st.angular = "Pickup.1st.Angular"
  ) %>%
  fmt_number(
    columns = vars(Total, Social, Pickups, prop, duration, procrastination, BMI, course, Pickup.1st.angular),
    decimals = 2
  )

gtsave(visual_table_1, filename = "./Figure/statistical_summary_haoyi.html")

### boxplot
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
ggsave("./Figure/line_plot_haoyi.png", plot = proportion.plot, width = 10, height = 6, units = "in")

boxplot_plot <- ggplot(df, aes(x = is_weekday, y = prop_ST, fill = is_weekday)) +
  geom_boxplot() +
  xlab("Weekday") +
  ylab("Proportion of Social Screen Time") +
  #scale_fill_manual(values = c("black", "red")) +  # Adjust fill colors as needed
  theme_minimal()

ggsave("./Figure/box_plot_haoyi.png", plot = proportion.plot, width = 10, height = 6, units = "in")


