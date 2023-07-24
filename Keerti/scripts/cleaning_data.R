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

data <- read.csv("data/chs2020_public.csv")

data <- na_mean(data)
data$toohighblsugar

# changing time-date format to integer format for toohighblsugar column
data$toohighblsugar <- as.numeric(gsub("\\00:00:0","",data$toohighblsugar))
data$toohighblsugar[is.na(data$toohighblsugar)] <- mean(data$toohighblsugar,na.rm=TRUE)
data$toohighblsugar <- round(data$toohighblsugar, digits = 1)

# exploring data correlations
# data_nutrition <- select(data, nutrition1, fruitveg20, avgsodaperday20, twoplussoda,
#                    nsugardrinkperday20, avgsugarperday20, nsodasugarperday20,
#                    avgsodasugarperday20, ssb, checkedbp20_q1, diabetes20, ageatdiabetes,
#                    diabcntrlmeds, toohighblsugar)
# 
# data_other <- select(data, colonoscopy10yr_45, checkedbp20_q1, diabetes20, ageatdiabetes,
#                      diabcntrlmeds, toohighblsugar, everasthma, currentasthma20,
#                      firsttoldasthma, stillasthmaall)
# 
# data_smoking <- select(data, cpd20a, everydaycpda, heavysmoker20a,
#                        smokecat,sourcelastcig, 
#                        cost20cigarettes, cigpurchase20) # mb not
# data_alcohol <- select(data, drinker, daysalc30, averagedrink20,
#                        heavydrink20, bingenew)
# 
# data_suicide <- select(data, thinksuicide, triedsuicide, ipvphy, insultipv,
#                        lowinchousing20, delaypayrent, rodentsstreet)
# 
# data_cancer <- select(data, evercolon20, colonoscopy10yr20, evercolon20_45,
#                       colonoscopy10yr_45,
#                       cpd20a, everydaycpda, heavysmoker20a,
#                       smokecat,sourcelastcig, 
#                       cost20cigarettes, cigpurchase20,
#                       smokeecig12m20_q1, smokeecig30days20_q1,
#                       smokehookah12m_q1)
# 
# data_immune <- select(data, fluvaccineshot, whereflu20, avgsodaperday20, 
#                       insuredgateway20, insure20r, insured, insure5,
#                       pcp20, medplace, didntgetcare20, regularrx,
#                       skiprxcost)

# data_alcohol <- select(data, agegroup, agegroup5, agegroup6, age21up, age25up, age40new,                    
#                        age45up, age50up, age18_64, newrace, newrace6, imputed_neighpovgroup4_1519, 
#                        imputed_povertygroup, imputed_povgroup3, imputed_pov200,
#                        drinker, daysalc30, averagedrink20, heavydrink20, bingenew)
# 
# data_diabetes <- select(data, agegroup, agegroup5, agegroup6, age21up, age25up, age40new,                    
#                         age45up, age50up, age18_64, bmi, weightall, weight20in4, weight20in5,
#                         fruitveg20, avgsodaperday20, twoplussoda, nsugardrinkperday20, avgsugarperday20,
#                         nsodasugarperday20, avgsodasugarperday20, ssb, exercise20,
#                         cyclingfreq, cycling20, swim, imputed_neighpovgroup4_1519, 
#                         imputed_povertygroup, imputed_povgroup3, imputed_pov200,
#                         toldhighbp20, toldprescription20, takingmeds20, checkedbp20_q1, diabetes20,
#                         ageatdiabetes, diabcntrlmeds, toohighblsugar)
# 
# data_smoker <- select(data, agegroup, agegroup5, agegroup6, age21up, age25up, age40new,                    
#                       age45up, age50up, age18_64, bmi, weightall, weight20in4, weight20in5,
#                       fruitveg20, avgsodaperday20, twoplussoda, nsugardrinkperday20, avgsugarperday20,
#                       nsodasugarperday20, avgsodasugarperday20, ssb, exercise20,
#                       cyclingfreq, cycling20, swim, imputed_neighpovgroup4_1519, 
#                       imputed_povertygroup, imputed_povgroup3, imputed_pov200,
#                       mood1, mood2, mood3, mood4, mood5, mood6, mood9, mood8, mood11)


data_immunizations <- select(data, agegroup, agegroup5, agegroup6, age21up, age25up, age40new,
                             age45up, age50up, age18_64, imputed_neighpovgroup4_1519,
                             imputed_povertygroup, imputed_povgroup3, imputed_pov200,
                             generalhealth, insuredgateway20, insured, insure5, pcp20,
                             medplace, didntgetcare20, regularrx, skiprxcost,toldprescription20,
                             takingmeds20, fluvaccineshot, whereflu20)

# data_insurance <- select(data, agegroup, agegroup5, agegroup6, age21up, age25up, age40new,                    
#                          age45up, age50up, age18_64, imputed_neighpovgroup4_1519, 
#                          imputed_povertygroup, imputed_povgroup3, imputed_pov200,
#                          generalhealth, insuredgateway20, insured, insure5, pcp20, 
#                          medplace, didntgetcare20, regularrx, skiprxcost,toldprescription20, takingmeds20,
#                          difficultdailyact, assistdevice)


names(data)

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
