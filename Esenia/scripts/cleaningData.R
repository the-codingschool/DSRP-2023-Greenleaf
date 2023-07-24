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
smokingdata <- select(data, imputed_povertygroup, imputed_povgroup3, 
                      imputed_pov200, cpd20a, everydaycpda, heavysmoker20a, smokecat, 
                      sourcelastcig, cost20cigarettes, cigpurchase20, smokeecig12m20_q1,
                      smokeecig30days20_q1, smokehookah12m_q1)
alcoholdata <- select(data, imputed_povertygroup, drinker, daysalc30, averagedrink20, heavydrink20, bingenew)
data_suicide <- select(data, imputed_povertygroup,
                       ipvphy, insultipv, lowinchousing20, delaypayrent, rodentsstreet)
mentalhealthdata <- select(data, imputed_povertygroup, imputed_povgroup3, 
                           imputed_pov200, mood1, mood2, mood3, mood4, mood5,
                           mood6, mood8, mood9, mood11)
dataCors <- mentalhealthdata |> 
  cor() |>
  melt() |>
  as.data.frame()

ggplot(dataCors, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "lightgray", high = "royalblue", mid = "lightblue",
                       midpoint = 0)
