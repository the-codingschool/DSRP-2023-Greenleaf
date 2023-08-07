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
library(xgboost)
library(ranger)

ggplot(DiabetesHS.20, aes(x = imputed_neighpovgroup4_1519, y = insure20r))+
  geom_line(stat = "summary",
                     fun = "mean")

ggplot(DiabetesHS.20, aes(x = insure20r, y = diabcntrlmeds )) +
  geom_violin()

ggplot(DiabetesHS.20, aes(x = imputed_neighpovgroup4_1519 , y = diabcntrlmeds)) +
  geom_line()

ggplot(DiabetesHS.20, aes(x = skiprxcost , y = diabcntrlmeds)) +
  geom_point()+
  geom_smooth()
DHS20.Accuracy <- table(takingmed, takingmedF, Tin, neighpov)
view(DHS20.Accuracy)
ggplot(DiabetesHS.20, aes(x = DHS20.Accuracy)) +
  geom_histogram()

## Violin Insurance vs Poverty Level
 
 ggplot(insured_diabcomu, aes(x = insure5, y = imputed_neighpovgroup4_1519)) +
   geom_violin(fill = "purple")+
   labs(title="Type of Insurance by Poverty Level")+
   theme_classic()+
   scale_x_continuous(labels = c('private', 'Medicare', 'Medicaid', 'Other', 'Uninsured'))+
   scale_y_continuous(labels = c('<10%', '<20%', '<30%', '<100%'))
 
 ggplot(CorrectAge, aes(x = ageatdiabetes)) +
   geom_bar()
 CorrectAge <- filter(DiabetesHS.20, DiabetesHS.20$ageatdiabetes == "1"|DiabetesHS.20$ageatdiabetes == "2" | DiabetesHS.20$ageatdiabetes == "3")
view(CorrectAge)

b <- data.frame(Ages=c("<18","18-40", ">40"), Percentage=c(3,28,69), frame=rep('b',3)) 
ggplot(b, aes(x=Ages, y= Percentage, fill=Ages))  + 
  geom_bar(stat='identity')+
  labs(title="Age Groups Diagnosed With Diabetes")+
  theme_minimal()

ggplot(NoMonMed, aes(x = diabcntrlmeds, y = insure5, fill= "blue")) +
   geom_line(stat = "summary",
             fun = "mean", colour="blue")

##Diabetic Medication taken by Insurance and Poverty Level
ggplot(NoMonMed1, aes(x = insure5 , y = imputed_neighpovgroup4_1519, color = diabcntrlmeds)) +
   geom_point()+
  theme_minimal()+
  labs(title="Diabetic Medication taken by Insurance and Poverty", x = "Type of Insurance", y = "Poverty Level")+
  scale_x_continuous(labels = c('private', 'Medicare', 'Medicaid', 'Other', 'Uninsured'))+
  scale_y_continuous(labels = c('<10%', '<20%', '<30%', '<100%'))
  

##Diabetic Medication taken by Insurance and Poverty Level Jittered
ggplot(NoMonMed1, aes(x = insure5 , y = imputed_neighpovgroup4_1519, color = diabcntrlmeds)) +
  geom_jitter()+
  theme_minimal()+
  labs(title="Diabetic Medication taken by Insurance and Poverty", x = "Type of Insurance", y = "Poverty Level")
  
##Diabetic Medication taken by Blood Sugar Control and Poverty Level
ggplot(insured_diabcomu, aes(x = toohighblsugar , y = imputed_neighpovgroup4_1519, color = diabcntrlmeds)) +
  geom_point()+
  theme_minimal()+
  labs(title="Medication Taken by Blood Sugar Control & Poverty Level", x = "Can't Control High Blood Sugar", y = "Poverty Level")+
  scale_x_continuous(labels = c('None', 'Some', 'Most', 'All', 'NA'))+
  scale_y_continuous(labels = c('<10%', '<20%', '<30%', '<100%'))

##Diabetic Medication taken by Insurance and Poverty Level Jittered
ggplot(insured_diabcomu, aes(x = toohighblsugar , y = imputed_neighpovgroup4_1519, color = diabcntrlmeds)) +
  geom_jitter()+
  theme_minimal()+
  labs(title="Medication Taken by Blood Sugar Control & Poverty Level", x = "Can't Control High Blood Sugar", y = "Poverty Level")


ggplot(NoMonMed, aes(x = insure5 , y = imputed_neighpovgroup4_1519, color = diabcntrlmeds)) +
  geom_jitter()+
  theme_classic()


ggplot(NoMonMed, aes(x = insure5 , y = imputed_neighpovgroup4_1519, color = diabcntrlmeds)) +
  geom_count()+
  theme_minimal()

ggplot(NoMonMed, aes(x = skiprxcost, y = toohighblsugar, color = diabcntrlmeds)) +
  geom_count()
 

NoMonMed <- filter(DiabetesHS.20, DiabetesHS.20$skiprxcost == "1", DiabetesHS.20$skiprxcost== "2", DiabetesHS.20$insure5 == "1", DiabetesHS.20$insure5 == "2"
                   , DiabetesHS.20$insure5 == "3" , DiabetesHS.20$insure5 == "4" , DiabetesHS.20$insure5 == "5"))
view(NoMonMed)

NoMonMed1 <- DiabetesHS.20[(DiabetesHS.20$insure5 == "1"| DiabetesHS.20$insure5 == "2"
                            | DiabetesHS.20$insure5 == "3" | DiabetesHS.20$insure5 == "4" | DiabetesHS.20$insure5 == "5"),]

view(NoMonMed1)


Diabeticage <- filter(HealthSurvey20, HealthSurvey20$ageatdiabetes==1 | HealthSurvey20$ageatdiabetes==2 |HealthSurvey20$ageatdiabetes==3)
                                
a <- data.frame(Variables=c("diabcntrlmeds ","insure5", "neighpov1519", "insure20r "), Percentage=c(80,91,67, 83), frame=rep('a',4)) 
ggplot(a, aes(x=Variables, y= Percentage, fill=Variables))  + 
  geom_bar(stat='identity')+
  
  labs(title="Boost Testing")+
  
## Scatter Plot num vs categorical 

  
##Line Plot 
ggplot(insured_diabcomu, aes(x = diabcntrlmeds , y = toohighblsugar)) +
  geom_line(stat = "summary",
            fun = "mean")
ggplot(insured_diabcomu, aes(x = diabcntrlmeds , y = ageatdiabetes)) +
  geom_line(stat = "summary",
            fun = "mean")