data <- read.csv("data/chs2020_public.csv")

View(data)
library(tidyr)
library(janitor)
library(dplyr)
library(tidymodels)
library(dplyr)
library(reshape2)
library(rsample)
library(Metrics)
library(parsnip)
library(yardstick)
library(ggplot2)
library(tidyverse)
library(imputeTS)


install.packages("imputeTS")
clean_HS20 <- na_mean(data)
View(clean_HS20) 

data <- na_mean(data)
data$toohighblsugar

##Replace "toohighblsugar" with mean

data$toohighblsugar <- as.numeric(gsub("\\00:00:0","",data$toohighblsugar))
data$toohighblsugar[is.na(data$toohighblsugar)]<-mean(data$toohighblsugar,na.rm=TRUE)
data$toohighblsugar <- round(data$toohighblsugar, digits = 1)

HealthSurvey20 <- c(clean_HS20,data$toohighblsugar) 
View(HealthSurvey20)

HealthSurvey20$insure20r


## Explore data with plots

ggplot(as.data.frame(HealthSurvey20), aes(x=HealthSurvey20$didntgetcare20)) +
  geom_histogram(fill= "yellow",bins = 5, stat="count")+
  labs(title="Health Survey by Access to Healthcare")

ggplot(as.data.frame(HealthSurvey20), aes(x=wt21_dual_q1, y=didntgetcare20))+
  geom_violin() + geom_boxplot(width=0.2, color="purple")+
  labs(title="Health Survey by Survey Weight and Access to Healthcare", xlab= "wt21_dual_q1", ylab= "didntgetcare20")


##Narrow Dataset

insured_diabcomu <- select(as.data.frame(HealthSurvey20), imputed_neighpovgroup4_1519, insuredgateway20, insure5, 
                        insure20r, diabetes20, diabcntrlmeds, ageatdiabetes, skiprxcost, toohighblsugar)
view(insured_diabcomu)
insured_diabcomu$insuredgateway20

HOP <- ggplot(as.data.frame(HealthSurvey20), aes(x=HealthSurvey20$didntgetcare20)) +
  geom_histogram(fill= "yellow",bins = 5, stat="count")+
  labs(title="Health Survey by Access to Healthcare")


##Tile Plots

insured_diabcomuCors <- insured_diabcomu |>
  cor() |>
  melt() |>
  as.data.frame()

ggplot(insured_diabcomuCors, aes(x = Var1, y= Var2, fill = value)) +
  geom_tile()+
  scale_fill_gradient2(low = "red", high = "blue", mid = "white",
                     midpoint = 0)
ggplot(data = insured_diabcomu, aes(x = insuredgateway20, y = diabcntrlmeds))+
         geom_point()+
  theme_minimal()
ggplot(data = insured_diabcomu, aes(x= imputed_neighpovgroup4_1519))+
  geom_density()

DiabetesHS20Cors <- DiabetesHS.20 |>
  cor() |>
  melt() |>
  as.data.frame()

DiabetesHS20 <- filter(insured_diabcomu, ageatdiabetes == "3", diabetes20 == "1") 
view(DiabetesHS20)

## Filtering on Insurance Type
                       
DiabetesHS.20 <- filter(insured_diabcomu, insured_diabcomu$insure20r==1 |
                         insured_diabcomu$insure20r==2 | insured_diabcomu$insure20r==3 | insured_diabcomu$insure20r==4 |
                         insured_diabcomu$insure20r==5 | insured_diabcomu$insure20r==6 |
                       insured_diabcomu$ageatdiabetes == 1 | insured_diabcomu$ageatdiabetes==2| insured_diabcomu$ageatdiabetes==3, diabetes20 == "1" )

## Filter on Age at Diabetes 
view(DiabetesHS.20)
Diabeticage <- filter(HealthSurvey20, HealthSurvey20$ageatdiabetes==1 | HealthSurvey20$ageatdiabetes==2 |HealthSurvey20$ageatdiabetes==3)
Dibage <- filter(insured_diabcomu, insured_diabcomu$ageatdiabetes==1 | insured_diabcomu$ageatdiabetes==2 | insured_diabcomu==3)
DigAge<- insured_diabcomu[(insured_diabcomu$ageatdiabetes==1 | insured_diabcomu$ageatdiabetes==2 | insured_diabcomu==3)]
ggplot(DiabetesHS20Cors, aes(x = Var1, y= Var2, fill = value)) +
  geom_tile()+
  scale_fill_gradient2(low = "red", high = "blue", mid = "white",
                       midpoint = 0)
