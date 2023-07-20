data <- read.csv("data/chs2020_public.csv")
getwd()
head(chs2020_public)
library(dplyr)

## Cleaning Data ###
healthdata <- chs2020_public

# 1. Removing NAs

healthdata_new <- filter(chs2020_public, wt21_dual_q1 != "NA", strata_q1 != "NA", 
                      age40new != "NA", age45up != "NA", age50up != "NA",
                      age18_64 != "NA", skiprxcost != "NA", 
                      toldprescription20 != "NA", takingmeds20 != "NA", 
                      checkedbp20_q1 != "NA", ageatdiabetes != "NA",
                      diabcntrlmeds != "NA", toohighblsugar != "NA", 
                      firsttoldasthma != "NA", workingac_q1 != "NA", 
                      helpneighbors20_q1 != "NA", everyday != "NA",
                      numberperdaya != "NA", cpd20a != "NA", 
                      heavysmoker20a != "NA", everydaycpda != "NA",
                      mentholcigs20 != "NA", sourcelastcig != "NA", 
                      cost20cigarettes != "NA", cigpurchase20 != "NA",
                      cigarillo20_q1 != "NA", smokeecig12m20_q1 != "NA", 
                      smokeecig30days20_q1 != "NA", likedecigsflavs_q1 != "NA",
                      smokehookah12m_q1 != "NA", smellcigsmoke20_q1 != "NA", 
                      sexualid20 != "NA", bmi != "NA", whereflu20 != "NA", 
                      swim != "NA", evercolon20 != "NA", colonoscopy10yr20 != "NA",
                      condom20 != "NA", analsex != "NA", analsexcondomuse20 != "NA",
                      sexbehav_active20 != "NA", wsw != "NA", wswexclusive != "NA",
                      everheardofprep != "NA", everusedprep20 != "NA", msm != "NA",
                      msmexclusive != "NA", bthcontrollastsex20_q1 != "NA",
                      condomusetrend != "NA", insultipv != "NA") 
head(healthdata_new)
View(healthdata_new)

data$strata_q1[is.na(data$strata_q1)]<-mean(data$strata_q1,na.rm=TRUE)
data$wt21_dual_q1[is.na(data$wt21_dual_q1)]<-mean(data$wt21_dual_q1,na.rm=TRUE)
data$nutrition1[is.na(data$nutrition1)]<-mean(data$nutrition1,na.rm=TRUE)
data$age40new[is.na(data$age40new)]<-mean(data$age40new,na.rm=TRUE)

install.packages("imputeTS")
library(imputeTS)
healthdata2 <- na_mean(data)
View(healthdata2)
