library(tidymodels)
library(dplyr)
library(reshape2)
library(rsample)
library(ggplot2)
library(tidyverse)
library(imputeTS)
library(janitor)
library(tidyr)
library(corrplot)


# age, poverty, insurance, access to healthcare, immunizations
data_flu <- select(data, agegroup6, imputed_povertygroup, 
                   generalhealth, insuredgateway20, insured, insure5, pcp20,
                   medplace, didntgetcare20, regularrx, 
                   toldhighbp20, mhtreat20_all,
                   smokecat, employment20, difficultdailyact, assistdevice,
                   fluvaccineshot)


data_check <- select(data, imputed_povertygroup, emp3)
ggplot(data = data_check, aes(x = imputed_povertygroup, y = emp3)) +
  geom_jitter()
# higher the imputed_povertygroup category number the less impoverished

data_flu$fluvaccineshot





# Hypothesis Testing ####
# Null Hypothesis - Age, poverty level, and healthcare access do not affect whether an NYC resident received the flu vaccine in 2020.
# Alternative Hypothesis - Age, poverty level, and healthcare access affect whether an NYC resident received the flu vaccine in 2020.

## Chi-Squared Test ####

# age: p value = 4.334091e-67
t1 <- table(new_data$agegroup6, new_data$fluvaccineshot)
chisq_result <- chisq.test(t1)
chisq_result$p.value
chisq_result
# SIGNIFICANT


# imputed_povertygroup: p value = 0.0001980559
t2 <- table(new_data$imputed_povertygroup, new_data$fluvaccineshot)
chisq_result2 <- chisq.test(t2)
chisq_result2$p.value
# SIGNIFICANT


# generalhealth: p value = 2.912482e-08
t3 <- table(new_data$generalhealth, new_data$fluvaccineshot)
chisq_result3 <- chisq.test(t3)
chisq_result3$p.value
# SIGNIFICANT


# insuredgateway20: p value = 2.171387e-28
t4 <- table(data$insuredgateway20, data$fluvaccineshot)
chisq_result4 <- chisq.test(t4)
chisq_result4$p.value
# SIGNIFICANT


# insured: p value = 1.361778e-30
t5 <- table(data$insured, data$fluvaccineshot)
chisq_result5 <- chisq.test(t5)
chisq_result5$p.value
# SIGNIFICANT


# insure5: p value = 1.704919e-48
t6 <- table(data$insure5, data$fluvaccineshot)
chisq_result6 <- chisq.test(t6)
chisq_result6$p.value
# SIGNIFICANT


# pcp20: p value = 3.54575e-79
t7 <- table(data$pcp20, data$fluvaccineshot)
chisq_result7 <- chisq.test(t7)
chisq_result7$p.value
# SIGNIFICANT

# medplace: p value = 3.279108e-30
t8 <- table(data$medplace, data$fluvaccineshot)
chisq_result8 <- chisq.test(t8)
chisq_result8$p.value
# SIGNIFICANT

# didntgetcare20: p value = 0.2095622
t9 <- table(data$didntgetcare20, data$fluvaccineshot)
chisq_result9 <- chisq.test(t9)
chisq_result9$p.value
# NOT SIGNIFICANT

# regularrx: p value = 3.043354e-105
t10 <- table(data$regularrx, data$fluvaccineshot)
chisq_result10 <- chisq.test(t10)
chisq_result10$p.value
# SIGNIFICANT

# toldhighbp20: p value = 9.00902e-38
t11 <- table(data$toldhighbp20, data$fluvaccineshot)
chisq_result11 <- chisq.test(t11)
chisq_result11$p.value
# SIGNIFICANT

# mhtreat20_all: p value = 2.322162e-10
t12 <- table(data$mhtreat20_all, data$fluvaccineshot)
chisq_result12 <- chisq.test(t12)
chisq_result12$p.value
# SIGNIFICANT


# smokecat: p value = 1.987408e-13
t13 <- table(data$smokecat, data$fluvaccineshot)
chisq_result13 <- chisq.test(t13)
chisq_result13$p.value
# SIGNIFICANT


# employment20: p value = 5.198959e-47
t14 <- table(data$employment20, data$fluvaccineshot)
chisq_result14 <- chisq.test(t14)
chisq_result14$p.value
# SIGNIFICANT


# difficultdailyact: p value = 4.548509e-11
t15 <- table(data$difficultdailyact, data$fluvaccineshot)
chisq_result15 <- chisq.test(t15)
chisq_result15$p.value
# SIGNIFICANT


# assistdevice: p value = 6.070845e-17
t16 <- table(data$assistdevice, data$fluvaccineshot)
chisq_result16 <- chisq.test(t16)
chisq_result16$p.value
# SIGNIFICANT





