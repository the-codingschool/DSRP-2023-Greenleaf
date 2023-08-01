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
dataCors <- df_immunizations |>
  cor() |>
  melt() |>
  as.data.frame()

plots <- ggplot(dataCors, aes(x = Var1, y = Var2, fill = value)) +
   geom_tile() +
   scale_fill_gradient2(low = "red", high = "blue", mid = "white",
                        midpoint = 0) +
  labs(x="Variables",y="Variables",title="Correlation between variables") 

plots + theme(axis.text.x = element_text(angle=90, hjust=1))




# age, poverty, insurance, access to healthcare, immunizations
df1 <- select(data, agegroup6, imputed_povertygroup, 
              generalhealth, insuredgateway20, insured, insure5, pcp20,
              medplace, didntgetcare20, regularrx, 
              toldhighbp20, mhtreat20_all,
              smokecat, employment20, difficultdailyact, assistdevice,
              fluvaccineshot)

df2<-df1[(df1$fluvaccineshot==1 | df1$fluvaccineshot==2),]

df2<-df2[(df2$agegroup6==1 | df2$agegroup6==2 | df2$agegroup6==3 | 
            df2$agegroup6==4 | df2$agegroup6==5 | df2$agegroup6==6),]

df3<-df2[(df2$generalhealth==1 | df2$generalhealth==2 | df2$generalhealth==3 | 
            df2$generalhealth==4 | df2$generalhealth==5),]

df3<-df3[(df3$insuredgateway20==1 | df3$insuredgateway20==2),]

df4<-df3[(df3$insured==1 | df3$insured==2),]

df4<-df4[(df4$insure5==1 | df4$insure5==2 | df4$insure5==3 | 
            df4$insure5==4 | df4$insure5==5),]

df5<-df4[(df4$pcp20==1 | df4$pcp20==2),]

df5<-df5[(df5$medplace==1 | df5$medplace==2 | df5$medplace==3 | 
            df5$medplace==4 | df5$medplace==5 | df5$medplace==6
            | df5$medplace==7),]

df6<-df5[(df5$didntgetcare20==1 | df5$didntgetcare20==2),]

df6<-df6[(df6$regularrx==1 | df6$regularrx==2),]

df7<-df6[(df6$toldhighbp20==1 | df6$toldhighbp20==2),]

df7<-df7[(df7$mhtreat20_all==1 | df7$mhtreat20_all==2),]

df8<-df7[(df7$smokecat==1 | df7$smokecat==2 | df7$smokecat==3
          | df7$smokecat==4),]

df8<-df8[(df8$employment20==1 | df8$employment20==2 | df8$employment20==3 | 
            df8$employment20==4 | df8$employment20==5 | df8$employment20==6
          | df8$employment20==7 | df8$employment20==8),]

df9<-df8[(df8$difficultdailyact==1 | df8$difficultdailyact==2),]

df9<-df9[(df9$assistdevice==1 | df9$assistdevice==2),]

new_data <- df9
