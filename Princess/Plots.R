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



#TILE PLOTS####

#tile plot for Access to health care
AccessCor <- cor(clean_data2) |>
cor() |>
  melt() |>
  as.data.frame()

ggplot(AccessCor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "lightblue", high = "blue",
                       midpoint = 0)


#tile plot for demographics

DemoCor <- cor(clean_demo) |>
  cor() |>
  melt() |>
  as.data.frame()

ggplot(DemoCor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "lightgreen", high = "blue",
                       midpoint = 0)


#tile plot for the highly correlated demographics variables

highdemoCor <- cor(highdemo) |>
  cor() |>
  melt() |>
  as.data.frame()

ggplot(highdemoCor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "darkred", high = "darkblue",
                       midpoint = 0)


#adding asthma variables to the main demographics tile chart to see how asthma correlates to all variables

#adding asthma variables to the highly correlated demographics variables to see how asthma is effected 

demo_asthmaCor <- cor(clean_demo_asthma) |>
  cor() |>
  melt() |>
  as.data.frame()

ggplot(demo_asthmaCor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "darkred", high = "darkblue",
                       midpoint = 0)


#tile plot for the highly correlated demographics and asthma variables
highdemo_asthma <- select(clean_demo_asthma, birthsex, newrace6_b, usborn,
                          education, imputed_povertygroup, imputed_povgroup3,
                          imputed_neighpovgroup4_1519, everasthma, currentasthma20, 
                          stillasthmaall)

highdemo_asthmaCor <- cor(highdemo_asthma) |>
  cor() |>
  melt() |>
  as.data.frame()

#found little to no correlations between the asthma and demographic variables 
ggplot(highdemo_asthmaCor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "orange", high = "darkblue",
                       midpoint = 0)




envi_asthma2 <- select(data, emp3, imputed_neighpovgroup4_1519,
                      everasthma, currentasthma20, stillasthmaall, rodentsstreet, 
                      helpneighbors20_q1, discussissues, helpcommproj, trustkeys)

clean_envi_asthma2 <- na.omit(envi_asthma2)


enviCor <- cor(clean_envi_asthma2) |>
  cor() |>
  melt() |>
  as.data.frame()

ggplot(enviCor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "darkred", high = "darkblue",
                       midpoint = 0)



ggplot(data = Spotify_Youtube, aes(x = Album_type, y = Stream, fill=Album_type)) + 
  geom_bar(stat = "summary",
           fun = "mean") +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "m")) +
  theme_minimal() +
  labs(
    title = "Top Artists Discography Type v.s Streams",
    x = "Discography",
    y = "Streams"
  ) +
  scale_fill_manual(values = c("darkblue", "blue", "lightblue"))










#replace with the mean of the value not cancel the na

#Selecting and choosing variables
names(data)
View(everasthma, currentasthma20, data$stillasthmaall)
asthma_data <- select(data, everasthma, currentasthma20, stillasthmaall)

#removing NA's
clean_asthma <- na.omit(asthma_data)

#creating data frame
data.frame(clean_asthma)
write.csv(clean_asthma, "clean_asthma.csv")
View(clean_asthma)

#view the distribution to compare with the codebook
# 2 = No, 1 = Yes, NA = Don't know/Refused/Missing/Not asked/

ever_types <- data |>
  summarize(.by = everasthma,
            count = sum(!is.na(everasthma)))

current_types <- data |>
  summarize(.by = currentasthma20,
            count = sum(!is.na(currentasthma20)))

still_types <- data |>
  summarize(.by = stillasthmaall,
            count = sum(!is.na(stillasthmaall)))




ggplot(clean_envi_asthma, aes(x = stillasthmaall, y = education)) +
  geom_point() +
  theme_minimal()






















#experiemental####

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










































#years plot
insurance_asthma <- select(data, agegroup6, birthsex, newrace6_b, usborn,
                           maritalstatus20, education, child, employment20,
                           emp3, imputed_povertygroup, imputed_povgroup3, 
                           imputed_neighpovgroup4_1519, everasthma,
                           currentasthma20, stillasthmaall, rodentsstreet, 
                           helpneighbors20_q1, discussissues, helpcommproj,
                           trustkeys, insured, insure5, pcp20, didntgetcare20, 
                           regularrx, medplace)

clean_insurance_asthma <- na.omit(insurance_asthma)

clean_insurance_asthma$everasthma
everasthma <- table(clean_insurance_asthma$everasthma)

row.names(everasthma) <- c("Yes", "No")

rbind(everasthma, row.names(everasthma))

ik <- row.names(everasthma)

as.data.frame(ik)


as.data.frame(everasthma)

everasthma$var1
View(everasthma)





ever <- data.frame(Answer = c("Yes" , "No"),
                           count = c("559", "3092"))



ggplot(ever, aes(x = Answer, y = count)) +
  geom_bar(stat = "summary",
           fun = "mean") +
  theme_minimal() +
  labs(
    title = "Distribution of people who have asthma",
    x = "Answer",
    y = "Number of people")










