data <- read.csv("data/chs2020_public.csv")
getwd()
head(chs2020_public)
library(dplyr)
library(tidymodels)
library(reshape2)
library(rsample)
library(parsnip)
library(yardstick)
library(ggplot2)
library(imputeTS)

## Cleaning Data ###
healthdata <- chs2020_public

# 1. Removing NAs
healthdata2 <- na_mean(data)
View(healthdata2)

data$toohighblsugar <- as.numeric(gsub("\\00:00:0","",data$toohighblsugar))

data$toohighblsugar[is.na(data$toohighblsugar)]<-mean(data$toohighblsugar,na.rm=TRUE)

data$toohighblsugar <- round(data$toohighblsugar, digits = 1)

## Grouping Data ###
Povrate <- healthdata$imputed_neighpovgroup4_1519

healthdataCors <- healthdata |> 
  cor() |>
  melt() |>
  as.data.frame()

ggplot(irisCors, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "gray", high = "purple", mid = "lavender",
                       midpoint = 0)