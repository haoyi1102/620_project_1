library(readxl)
library(dplyr)
setwd("/Users/hanzihao/Desktop/biostat620")
st = read_excel("ScreenTimeZihaoHan.xlsx")
hm_to_min = function(text){
  split = strsplit(text,"h|m")
  hours = as.numeric(split[[1]][1])
  minutes = as.numeric(split[[1]][2])
  convert_minutes = hours * 60 + minutes
  return(convert_minutes)
}
st$Total.ST.min = sapply(st$Total.ST,hm_to_min)
st$Social.ST.min = sapply(st$Social.ST,hm_to_min)
st <- st %>%
  rename(Pickup.1st_EST=  Pickup.1st)
write.csv(st, file = "", row.names = FALSE)
