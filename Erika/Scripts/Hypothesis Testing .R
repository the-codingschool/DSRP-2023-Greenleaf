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
install.packages("corrplot")
library(corrplot)
## Research Question: How is access to health insurance affecting diabetic people living in marginalized communities?
## Null Hypothesis:  There is no significance between insurance, poverty, and diabetic medication
##Alternative Hypothesis:There is significance between insurance, poverty, and diabetic medication
view(insured_diabcomu)

##Anova 
anova_results <- aov(insure20r ~ diabcntrlmeds, DiabetesHS.20)
summary(anova_results)
TukeyHSD(anova_results)


##Chi-Squared Test

table1 <- table(DiabetesHS.20$insuredgateway20, DiabetesHS.20$diabcntrlmeds)
chis_result <- chisq.test(table1)
chis_result$p.value

table2 <- table(DiabetesHS.20$imputed_neighpovgroup4_1519, DiabetesHS.20$diabetes20)
Result2 <- chisq.test(table2)
Result2$p.value

table3 <- table(DiabetesHS.20$insure5, DiabetesHS.20$diabetes20)
Result3 <- chisq.test(table3)
Result3$p.value

T4 <- table(DiabetesHS.20$insure5, DiabetesHS.20$diabcntrlmeds)
Result4 <- chisq.test(T4)
Result4$p.value

T5 <- table(DiabetesHS.20$insure5, DiabetesHS.20$insure20r)
Result5 <- chisq.test(T5)
Result5$p.value

T6 <- table(DiabetesHS.20$insure20r, DiabetesHS.20$diabcntrlmeds)
Result6 <- chisq.test(T6)
Result6$p.value

tablex<- table(insured_diabcomu$insure5, insured_diabcomu$imputed_neighpovgroup4_1519)
chis_result <- chisq.test(tablex)
chis_result$p.value
chis_result$residuals
corrplot(chis_result$residuals, is.corr = F)
view()
