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
library(RColorBrewer)
library(parsnip)
library(xgboost)
library(ranger)
library(plotrix)
library(ggpubr)
data <- read.csv("data/chs2020_public.csv")

#Variables
insurance_asthma <- select(data, agegroup6, birthsex, newrace6_b, usborn,
                           maritalstatus20, education, child, employment20,
                           emp3, imputed_povertygroup, imputed_povgroup3, 
                           imputed_neighpovgroup4_1519, everasthma,
                           currentasthma20, stillasthmaall, rodentsstreet, 
                           helpneighbors20_q1, discussissues, helpcommproj,
                           trustkeys, insured, insure5, pcp20, didntgetcare20, 
                           regularrx, medplace)

clean_insurance_asthma <- na.omit(insurance_asthma)

still_asthma <- clean_insurance_asthma|> filter(stillasthmaall == "1")

yes_asthma <- clean_insurance_asthma|> filter(everasthma == "1")

never_asthma <- clean_insurance_asthma|> filter(everasthma == "2")

current_asthma <- clean_insurance_asthma|> filter(currentasthma20 == "1")

still_asthma <- clean_insurance_asthma|> filter(stillasthmaall == "1")


#ignore####

#(1) Testing distribution of people who have been diagnosed with asthma before, no insurance, and wheater or not they got care

#Results - Of the people with asthma and no insurance (16 ppl) 15 ppl said they got care and 1 person said they didn’t
yesasthma_noinsure <- yes_asthma|> filter(insured == "2")
yesasthma_noinsure_yescare <- yesasthma_noinsure|> filter(didntgetcare20 == c("1", "2"))
yesasthma_noinsure_yescare 
select(yesasthma_noinsure_yescare, everasthma, insured, didntgetcare20)



#(2) Testing distribution of people who had an asthma attack within the past 12 months, NO insurance and and wheater or not they got care
#Results - Of the people who had an attack, no insurance and were/wernt able to get care 
#3 people were able to get care whereas 3 people weren't (50/50)
currentasthma_noinsure <- current_asthma|> filter(insured == "2")
currentasthma_noinsure_care <- currentasthma_noinsure|> filter(didntgetcare20 == c("1", "2"))
select(currentasthma_noinsure_care, currentasthma20, insured, didntgetcare20)

#Further testing
#Results - When testing the FPL, I found that 5 people were medium pov and 1 person was high poverty
currentasthma_noinsure_care2 <- currentasthma_noinsure|> filter(didntgetcare20 == "1")
ll <- currentasthma_noinsure_care2|> filter(imputed_neighpovgroup4_1519 == "2")

lk <-select(ll, currentasthma20, insured, didntgetcare20, imputed_neighpovgroup4_1519)

view(lk)



#(4) Testing distribution of people who had an asthma attack within the past 12 months, insurance and and wheater or not they got care
#Results - Of the people who had an attack, insurance and were/wernt able to get care (056 ppl)
#305 ppl got care and 51 ppl didnt
stillasthma_insure <- still_asthma|> filter(insured == "1")
results <- select(stillasthma_insure, stillasthmaall, insured, didntgetcare20)

results |> 
  group_by(didntgetcare20) |> 
  summarize(count = n())


#Further testing
#Results - When testing the FPL, I found that for the ppl that said they DIDNT get care (52 ppl) 
#(7 ppl were low pov, 22 were med pov, 12 were high pov and 11 were very high pov)
stillasthma_insure_care2 <- stillasthma_insure|> filter(didntgetcare20 == "1")

povchart <- select(stillasthma_insure_care2, imputed_neighpovgroup4_1519)

povchart |> 
  group_by(imputed_neighpovgroup4_1519) |> 
  summarize(count = n())





#insurance testing####


#(2a) Testing distribution of people who had an asthma attack within the past 12 months, HAVE 
#insurance and and wheater or not they got care as well as what medical place they usually go to see 
#Results - Of the people who had an attack, HAVE insurance and were/werent able to get care 34 didnt get care and 127 got care
#results - 98 go to a private doctor, 14 community health center, 32 hospital outpatient clinic, 9 urgent care center, 2 hospital ED, 0 retail clinic, 5 some other place, 1 no usual place
current_asthma <- clean_insurance_asthma|> filter(currentasthma20 == "1")
current_asthma2 <- select(current_asthma, currentasthma20, insured, didntgetcare20, medplace, imputed_neighpovgroup4_1519, imputed_povertygroup)
currentasthma_insure <- current_asthma2|> filter(insured == "1")

currentasthma_insure |> 
  group_by(didntgetcare20) |> 
  summarize(count = n())

currentasthma_insure |> 
  group_by(medplace) |> 
  summarize(count = n())


#further testing

currentasthma_insure_privatecare <- currentasthma_insure|> filter(medplace == 1 )

sum(currentasthma_insure_privatecare$medplace)
ik <- select(currentasthma_insure_privatecare, imputed_neighpovgroup4_1519, imputed_povertygroup)

ik |> 
  group_by(imputed_neighpovgroup4_1519) |> 
  summarize(count = n())

ik |> 
  group_by(imputed_povertygroup) |> 
  summarize(count = n())




#Second part of project (environment) ####


#(5) Testing distribution of people who still have asthma and their environment
#Results4 - (imputed_neighpovgroup4_1519) Of the people who still have asthma, 67 ppl were low pov, 147 med pov, 101 high pov, 70 very high pov
#why so much people in med pov? most people fall under the category of medium poverty. especially living in a place like nyc, someone whos nyc might make a decent wage but to nyc standards its not a liviable amount to be considered low poverty

#Results5 - (imputed_povertygroup) Of the people who still have asthma, 123 ppl were low pov, 67 ppl were med-high pov, 51 ppl were med pov, 69 were high pov, 75 very high pov

# 1 = yes
still_asthma <- clean_insurance_asthma|> filter(stillasthmaall == "1")

# 1,2,3,4 are all placements on the poverty line
#stillasthma_neigh <- still_asthma|> filter(imputed_neighpovgroup4_1519 == c("1", "2", "3", "4"))

sum(still_asthma$stillasthmaall)

stillneigh <- select(still_asthma, imputed_neighpovgroup4_1519, imputed_povertygroup)


stillneigh |> 
  group_by(imputed_neighpovgroup4_1519) |> 
  summarize(count = n())


stillneigh |> 
  group_by(imputed_povertygroup) |> 
  summarize(count = n())



#(5a) Testing distribution of people who used to have have asthma and their environment
#Results4a - (imputed_neighpovgroup4_1519) Of the people who used to have asthma, 728 ppl were low pov, 1451 med pov, 692 high pov, 417 very high pov

#Results5a - (imputed_povertygroup) Of the people who still have asthma, 677 ppl were low pov, 641 ppl were med-high pov, 590 ppl were med pov, 561 were high pov, 819 very high pov
nostill_asthma <- clean_insurance_asthma|> filter(stillasthmaall == "2")

sum(nostill_asthma$stillasthmaall)

nostillneigh <- select(nostill_asthma, imputed_neighpovgroup4_1519, imputed_povertygroup)

nostillneigh |> 
  group_by(imputed_neighpovgroup4_1519) |> 
  summarize(count = n())


nostillneigh |> 
  group_by(imputed_povertygroup) |> 
  summarize(count = n())



#(6) Testing distribution of people who had an asthma attack within the past 12 months and their environment
#Results6 - (imputed_neighpovgroup4_1519) Of the people who had asthma attacks, 21 ppl were low pov, 64 ppl were med pov, 57 ppl were high pov, 31 ppl were very high pov

#Results7 - (imputed_povertygroup) Of the people who still have asthma, 47 ppl were low pov, 31 ppl were med-high pov, 25 ppl were med pov, 36 were high pov, 34 very high pov

current_asthma <- clean_insurance_asthma|> filter(currentasthma20 == "1")

currentneigh <- select(current_asthma, imputed_neighpovgroup4_1519, imputed_povertygroup)


current <- currentneigh |> 
  group_by(imputed_neighpovgroup4_1519) |> 
  summarize(count = n())

View(current)

currentneigh |> 
  group_by(imputed_povertygroup) |> 
  summarize(count = n())


ggplot(current, aes(x = imputed_neighpovgroup4_1519, y = count)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_minimal() +
  labs(
    title = "Distribution of People Who Expereinced Asthma Attacks
    and their Placement on the Federal Poverty Line
    1 = Low Pov, 2 = Medium Pov, 3 = High Pov, Very-High Pov",
    x = "Federal Poverty Line Group",
    y = "Number of people")



ggplot(current, aes(x = imputed_neighpovgroup4_1519, y = count, fill = imputed_neighpovgroup4_1519)) +
  geom_bar(stat = "summary",
           fun = "mean") +
  theme_minimal() +
  labs(
    title = "Distribution of People Who Expereinced Asthma Attacks
    and their Placement on the Federal Poverty Line
    1 = Low Pov, 2 = Medium Pov, 3 = High Pov, Very-High Pov",
    x = "Federal Poverty Line Group",
    y = "Number of people"
  ) +
  scale_color_brewer(palette = "Pastel1")
  
  



#(6a) Testing distribution of people who havent had an asthma attack within the past 12 months and their enviorment
#Results6a - (imputed_neighpovgroup4_1519) Of the people who havent had asthma attacks, 774 ppl were low pov, 1534 ppl were med pov, 736 ppl were high pov, 456 ppl were very high pov

#Results6a - (imputed_povertygroup) Of the people who havent had asthma attacks, 753 ppl were low pov, 677 ppl were med-high pov, 616 ppl were med pov, 594 were high pov, 860 very high pov
nocurrent_asthma <- clean_insurance_asthma|> filter(currentasthma20 == "2")

nocurrentneigh <- select(nocurrent_asthma, imputed_neighpovgroup4_1519, imputed_povertygroup)


nocurrentneigh |> 
  group_by(imputed_neighpovgroup4_1519) |> 
  summarize(count = n())


nocurrentneigh |> 
  group_by(imputed_povertygroup) |> 
  summarize(count = n())




#pt2 of Second part of Project (socioeconomic) ####


#null: The the socioeconomic status of a New York citizen does not significantly determine 
#whether or not a New Yorker has asthma (education, FPL, Occupation)

#alternative hypothesis: The the socioeconomic status of a New York citizen does significantly 
#determine whether or not a New Yorker has asthma (education, FPL, Occupation)



#(7) Testing distribution of people who still have asthma and their education level
#Results7 - (education) Of the people who still have asthma, 71 ppl less than hs, 80 hs, 89 some college or technical school, 155 very college graduate

still_asthma <- clean_envi_asthma|> filter(stillasthmaall == "1")
sum(still_asthma$stillasthmaall)

stilledu <- select(still_asthma, stillasthmaall, education)

stilledu |> 
  group_by(education) |> 
  summarize(count = n())



#(8) Testing distribution of people who have asthma attacks and their education level
#Results8 - (education) Of the people who still have asthma, 25 ppl less than hs, 28 hs, 46 some college or technical school, 74 very college graduate

current_asthma <- clean_insurance_asthma|> filter(currentasthma20 == "1")

currentedu <- select(current_asthma, education)

currentedu |> 
  group_by(education) |> 
  summarize(count = n())



#(9) Testing distribution of people who still have asthma and their occupation status 
#Results9 - (emp3) Of the people who still have asthma, 172 ppl employed, 56 unemployed, 167 not in labor force

still_asthma <- clean_envi_asthma|> filter(stillasthmaall == "1")
sum(still_asthma$stillasthmaall)

stillemp <- select(still_asthma, stillasthmaall, emp3)

stillemp |> 
  group_by(emp3) |> 
  summarize(count = n())



#(9) Testing distribution of people who still have asthma and their occupation status 
#Results9 - (emp3) Of the people who still have asthma, 88 ppl employed, 20 unemployed, 65 not in labor force

current_asthma <- clean_insurance_asthma|> filter(currentasthma20 == "1")

currentemp3 <- select(current_asthma, emp3)

currentemp3 |> 
  group_by(emp3) |> 
  summarize(count = n())








#Practice####
results5data <- data.frame(Poverty = c("Less than high school" , "High school Graduate", "Some college/technical school", "College Graduate"),
                           count = c("70", "76", "88", "151"))




library(data.table)

tableresults5 <- table(results5data)
tableresults5 <- as.data.table(results5data)

tableresults4$count
as.numeric()


stilledu |> 
  group_by(education) |> 
  summarize(count = n())



tableresults4

ggplot(tableresults5, aes(x = Poverty, y = count, fill = Poverty)) +
  geom_bar(stat = "summary",
           fun = "mean") +
  theme_minimal() +
  labs(
    title = "Distribution of people who still have asthma
    and their placement on the Federal Poverty Line",
    x = "Education",
    y = "Number of people"
  ) +
  scale_fill_manual(values = c("darkblue", "blue", "deepskyblue", "lightblue"))












#experiemental

results4data <- data.frame(Poverty = c("Low poverty" , "Medium Poverty", "High Poverty", "Very High Poverty"),
                           count = c("22", "30", "27", "13"))

tableresults4 <- table(results4data)
library(data.table)
tableresults4 <- as.data.table(results4data)

tableresults4$count
as.numeric()
row.names(tableresults4) <- c("Low Poverty", "Medium Poverty", "High Poverty", "Very High Poverty")
tableresults4_distribution <- round(as.numeric(tableresults4$count)/92*100)
names <- paste(names(tableresults4), "-", count, "%", sep = " ")





pie3D(tableresults4_distribution, main = "Testing distribution of people who still have asthma and their enviorment.", 
      labels = names, col = c("lightpink","cornsilk"), border = "white", radius = 1 )


tableresults4 <- table(results4data)

tableresults4

ggplot(tableresults4, aes(x = Poverty, y = count, fill = Poverty)) +
  geom_bar(stat = "summary",
           fun = "mean") +
  theme_minimal() +
  labs(
    title = "Distribution of people who still have asthma
    and their placement on the Federal Poverty Line",
    x = "Federal Poverty Line Placement",
    y = "Number of people"
  ) +
  scale_fill_manual(values = c("darkblue", "blue", "deepskyblue", "lightblue"))





