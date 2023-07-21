# Load libraries
library(dplyr)  # For data manipulation
library(tidyr)  # For data tidying


## Viewing Dataset####
data <- read.csv("data/chs2020_public.csv")
data <- healthdata
str(healthdata)

# View summary statistics
summary(healthdata)

# View the first few rows of the dataset
head(healthdata)


## Cleaning Data####

healthdata <- filter(healthdata, wt21_dual_q1 != "NA", strata_q1 != "NA",
                     nutrition1 != "NA", age40new != "NA", age45up != "NA", age50up != "NA",
                     age18_64 != "NA", skiprxcost != "NA", toldprescription20 != "NA",
                     takingmeds20 != "NA", checkedbp20_q1 != "NA", ageatdiabetes != "NA", 
                     diabcntrlmeds != "NA", insured != "NA", insure5 != "NA")
View(healthdata)

healthdata$toohighblsugar

healthdata$toohighblsugar <- as.numeric(gsub("\\00:00:0", "", healthdata$toohighblsugar))
healthdata$toohighblsugar[is.na(healthdata$toohighblsugar)] <- mean(healthdata$toohighblsugar, na.rm = TRUE)
healthdata$toohighblsugar <- round(healthdata$toohighblsugar, digits = 1)

healthdata
