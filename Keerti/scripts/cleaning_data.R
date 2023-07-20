library(dplyr)
library(ggplot2)
library(tidyverse)
library(imputeTS)

data <- read.csv("data/chs2020_public.csv")
# View(data)

data <- na_mean(data)

data$toohighblsugar <- as.numeric(gsub("\\00:00:0","",data$toohighblsugar))

data$toohighblsugar[is.na(data$toohighblsugar)]<-mean(data$toohighblsugar,na.rm=TRUE)
data$toohighblsugar

round_df <- function(data, digits) {
  data$toohighblsugar <- sapply(data, mode) == 'numeric'
  data[data$toohighblsugar] <-  round(data[toohighblsugar], digits)
  data
}

round_df(data, 1)


data$toohighblsugar <- print(data$toohighblsugar, digits = 1)
data$toohighblsugar.round()

data_round2 <- data %>%                   # Using dplyr functions
  mutate_if(is.numeric,
            round,
            digits = 1)
data_round2














# # data <- na.omit(data)
# 
# df_names <- names(data)
# 
# for (col in df_names){
#   print(col)
#   #print(mean(data$X,na.rm=TRUE))
# }
# 
# for(i in 1:ncol(data)) {                                   # Replace NA in all columns
#   data[ , i][is.na(data[ , i])] <- mean(data[ , i], na.rm = TRUE)
# }

# max_mood1 <- names(which.max(table(data$mood1)))
# 
# data %>% 
#   mutate(mood1 = if_else(is.na(mood1), 
#                          max_mood1, 
#                          mood1))




  
