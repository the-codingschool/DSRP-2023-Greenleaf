#to read in data

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
data <- read.csv("data/chs2020_public.csv")

data <- read_csv("C:/Users/Cheryl/OneDrive/Desktop/DSRP-2023-Greenleaf/DSRP-2023-Greenleaf/data/chs2020_public.csv")

View(data)

names(data)

str(data)

sum(is.na(data))

colSums(is.na(data))

filter(data, is.na(age25up))


#demographic variables
demographics <- select(data, agegroup6, birthsex, newrace6_b, usborn,
                       maritalstatus20, education, child, employment20,
                       emp3, imputed_povertygroup, imputed_povgroup3, imputed_neighpovgroup4_1519)

clean_demo <- na.omit(demographics)

#selection of the highly correlated demographic variables
highdemo <- select(clean_demo, agegroup6, employment20, emp3,
                   imputed_neighpovgroup4_1519, newrace6_b, education,
                   imputed_povertygroup, imputed_povgroup3, usborn)


#adding asthma variables to the highly correlated demographics variables to see how asthma is effected 
demo_asthma <- select(data, agegroup6, birthsex, newrace6_b, usborn,
                      maritalstatus20, education, child, employment20,
                      emp3, imputed_povertygroup, imputed_povgroup3, imputed_neighpovgroup4_1519,
                      everasthma, currentasthma20, stillasthmaall)

clean_demo_asthma <- na.omit(demo_asthma)






#ACCESS/HEALTH INSURANCE
insured_data <- na_mean(data$insured)
Rinsured_data <- round(insured_data, digits = 1)
Int_insured_data = as.integer(Rinsured_data)

insure5_data = na_mean(data$insure5)
Rinsure5_data = round(insured_data, digits = 1)
Int_insure5_data = as.integer(Rinsure5_data)

pcp_data = na_mean(data$pcp20)
Rpcp_data = round(insured_data, digits = 1)
Int_pcp_data = as.integer(Rpcp_data)

medplace_data = na_mean(data$medplace)
Rmedplace_data = round(insured_data, digits = 1)
Int_medplace_data = as.integer(Rmedplace_data)

didntget20_data = na_mean(data$didntgetcare20)
Rdidntget20_data = round(insured_data, digits = 1)
Int_didntget20_data = as.integer(Rdidntget20_data)

regx_data = na_mean(data$regularrx)
Rregx_data = round(insured_data, digits = 1)
Int_regx_data = as.integer(Rregx_data)



Access_data <- select(data, (all_of(Int_insured_data)), (all_of(Int_insure5_data)), 
                      (all_of(Int_pcp_data)), (all_of(Int_medplace_data)), 
                      (all_of(Int_didntget20_data)), (all_of(Int_regx_data)))

Access_data2 = select(data, insured, pcp20, didntgetcare20, regularrx)
clean_data2 <- na.omit(Access_data2)


AccessCor <- cor(clean_data2) |>
  cor() |>
  melt() |>
  as.data.frame()

ggplot(AccessCor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "lightblue", high = "blue",
                       midpoint = 0)



ggplot(Access_data, aes(x = Int_insured_data, y = Int_didntget20_data)) +
  geom_point() +
  theme_minimal()


AccessCor <- cor(Access_data) |>
  cor() |>
  melt() |>
  as.data.frame()

ggplot(AccessCor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white",
                       midpoint = 0)


                         
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


#correlation plots

AsthmaCor <- cor(clean_asthma) |>
  cor() |>
  melt() |>
  as.data.frame()

ggplot(AsthmaCor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "pink", high = "darkred",
                       midpoint = 0)


#Pie Chart

sum(is.na(data))

?pie

Tableever <- table(clean_asthma$everasthma)
row.names(Tableever) <- c("Yes", "No")
ever_distribution <- round(Tableever/8781*100)
names <- paste(names(Tableever), "-", ever_distribution, "%", sep = " ")


?col

pie(Tableever, main = "Have you ever been told by a
doctor, nurse or other health
professional that you had asthma?", col = c(4,2), labels = names)































#testing





#Step 5: Seperate Data into Testing and Training sets
#Choose 70-85% of all dtat to train on
library(tidymodels)
library(rsample)

#Regression dataset splits

reg_split <- initial_split(clean_asthma, prop = .75)

#use the split to form testing and training sets
reg_train <- training(reg_split)
reg_test <- testing(reg_split)


##set a seed for producability, gives us the same results every time
set.seed(71723)




#Liner regression

#Findings
#null: there is no relationship between the dependent variable and the independent variables
#alternative: there is a relationship between the dependent variable and the independent variables
#(p-value: < 2.2e-16) Reject the null

lm_fit <- linear_reg() |>
  set_engine("lm") |>
  set_mode("regression") |>
  fit(everasthma ~ stillasthmaall + currentasthma20, 
      data = reg_train)

cor(clean_asthma)

#stillasthmaall is the most significantly related to everasthma
lm_fit$fit
summary(lm_fit$fit)


##boosted decision trees
boost_reg_fit <- boost_tree() |>
  set_engine("xgboost") |>
  set_mode("regression") |>
  fit(everasthma ~., data = reg_train)

boost_reg_fit$fit

#Reveals that the error is decreasing over time
boost_reg_fit$fit$evaluation_log



#random forest
#regression

forest_reg_fit <- rand_forest() |>
  set_engine("ranger") |>
  set_mode("regression") |>
  fit(everasthma ~ stillasthmaall, data = reg_train)

forest_reg_fit$fit




data$insure20r







## Step 8: Evaluate Model Preformance on Test Set
#Calculate errors for regression
library(yardstick)
#Regression models: lm_fit, boost_reg_fit, forest_reg_fit

predict(lm_fit, reg_test)

reg_results <- reg_test

#tells us how much error is between the trues and the predictions
reg_results$lm_pred <- predict(lm_fit, reg_test)$.pred
reg_results$boost_pred <- predict(boost_reg_fit, reg_test)$.pred
reg_results$forest_pred <- predict(forest_reg_fit, reg_test)$.pred

#truth is speal length false in Lm_pred
mae(reg_results, Sepal.Length, lm_pred)



yardstick::mae(reg_results, Sepal.Length, lm_pred)
yardstick::mae(reg_results, Sepal.Length, boost_pred)
yardstick::mae(reg_results, Sepal.Length, forest_pred)

yardstick::rmse(reg_results, Sepal.Length, lm_pred)
yardstick::rmse(reg_results, Sepal.Length, boost_pred)
yardstick::rmse(reg_results, Sepal.Length, forest_pred)


#calculate accuracy for classification models
install.packages("MLmetrics")
library(MLmetrics)
class_results <- class_test

class_results$lm_pred <- predict(log_fit, class_test)$.pred_class
class_results$boost_pred <- predict(log_fit, class_test)$.pred_class
class_results$forest_pred <- predict(log_fit, class_test)$.pred_class


F1_Score(class_results$Species, class_results$log_pred)
F1_Score(class_results$Species, class_results$boost_pred)
F1_Score(class_results$Species, class_results$forest_pred)

























