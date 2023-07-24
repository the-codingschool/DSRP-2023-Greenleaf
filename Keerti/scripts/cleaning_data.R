library(tidymodels)
library(dplyr)
library(reshape2)
library(rsample)
library(ggplot2)
library(tidyverse)
library(imputeTS)

data <- read.csv("data/chs2020_public.csv")

data <- na_mean(data)
data$toohighblsugar

# changing time-date format to integer format for toohighblsugar column
data$toohighblsugar <- as.numeric(gsub("\\00:00:0","",data$toohighblsugar))
data$toohighblsugar[is.na(data$toohighblsugar)] <- mean(data$toohighblsugar,na.rm=TRUE)
data$toohighblsugar <- round(data$toohighblsugar, digits = 1)

# age, poverty, insurance, access to healthcare, immunizations
data_immunizations <- select(data, agegroup, agegroup5, agegroup6, age21up, age25up, age40new,
                             age45up, age50up, age18_64, imputed_neighpovgroup4_1519,
                             imputed_povertygroup, imputed_povgroup3, imputed_pov200,
                             generalhealth, insuredgateway20, insured, insure5, pcp20,
                             medplace, didntgetcare20, regularrx, skiprxcost,toldprescription20,
                             takingmeds20, fluvaccineshot, whereflu20)

# tile plot
dataCors <- data_immunizations |>
  cor() |>
  melt() |>
  as.data.frame()

plots <- ggplot(dataCors, aes(x = Var1, y = Var2, fill = value)) +
   geom_tile() +
   scale_fill_gradient2(low = "red", high = "blue", mid = "white",
                        midpoint = 0)

plots + theme(axis.text.x = element_text(angle=90, hjust=1))
