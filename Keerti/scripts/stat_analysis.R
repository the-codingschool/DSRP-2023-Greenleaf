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
                             medplace, didntgetcare20, regularrx, fluvaccineshot)


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
t1 <- table(final_data$agegroup6, final_data$fluvaccineshot)
chisq_result <- chisq.test(t1)
chisq_result$p.value
# SIGNIFICANT


# poverty: p value = 0.0001980559
t2 <- table(final_data$imputed_povertygroup, final_data$fluvaccineshot)
chisq_result2 <- chisq.test(t2)
chisq_result2$p.value
# SIGNIFICANT


# generalhealth: p value = 1.200925e-07
t3 <- table(final_data$generalhealth, final_data$fluvaccineshot)
chisq_result3 <- chisq.test(t3)
fisher_test <- fisher.test(t3)

fisher_test$p.value
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



