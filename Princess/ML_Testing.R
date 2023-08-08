#Step 5: Seperate Data into Testing and Training sets
#Choose 70-85% of all dtat to train on
library(leaps)
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
library(faraway)
library(Stat2Data)
library(skimr)
library(lmSubsets)
library(mosaic)
library(infer)

#Testing Variables####
Tdemo_asthma <- select(data, agegroup6, birthsex, newrace6_b, usborn,
                       maritalstatus20, education, child, employment20,
                       emp3, imputed_povertygroup, imputed_povgroup3, imputed_neighpovgroup4_1519,
                       everasthma, currentasthma20, stillasthmaall)

clean_Tdemo_asthma <- na.omit(Tdemo_asthma)


skim(clean_Tdemo_asthma)

best <- regsubsets(agegroup6~., data = clean_Tdemo_asthma, nbest = 2, method = "exhaustive")

with(summary(best), data.frame(rsq, adjr2, cp, rss, outmat))



#which model is best based on $adjR^2$? -- 8 (1) (first model of size 8)

#which model is best based on $cp$? -- 8 (2) (first model of size 8)

#what variables are in the best models of size 8? -- birthsex, newrace_6, maritalstatus20, education, child, employment20, emp3, imputed_povertygroup

#now, run the best model of size 8 and report the AIC value by using the 'AIC()' command on my model (AIC shows how well a model fits the data it was generated from)


m1 <- lm(agegroup6 ~ birthsex+newrace6_b+maritalstatus20+education+child+employment20+emp3+imputed_povertygroup, data = clean_Tdemo_asthma)
summary(m1)
AIC(m1)


#backward elimination
#nbest is the one best model of each size. nvmax is the maximum model size that I want
backward <- regsubsets(agegroup6~., data = clean_Tdemo_asthma, nbest = 1, nvmax = 6, method = "backward")
summary(backward)
with(summary(backward), data.frame(cp, outmat))

#which model is best based on cp? - we want the smallest cp so the best model would be the model of size 6

#Coefficient test based on the variables that has "*" in the backwards elimination
m2 <- lm(agegroup6 ~ birthsex+newrace6_b+maritalstatus20+child+employment20+emp3, data = clean_Tdemo_asthma)
summary(m2)
AIC(m2)


#all the Coefficients are significant as they have at least 3 "***"

#plot to show is the assumptions met
#results - the conditions look okay
plot(m2, which = 1)
plot(m2, which = 2)


#forward eleimination
forward <- regsubsets(agegroup6~., data = clean_Tdemo_asthma, nbest = 1, 
                      nvmax = 6, method = "forward")
summary(forward)
with(summary(forward), data.frame(cp, outmat))

#which model is the best based on $cp? - model of size 6 

#What are the variables of the best model? - birthsex, newrace_6, maritalstatus20, child, employment20, emp3
#the only difference is that education and imputed_povertygroup aren't included like in the backwards elemination


summary(m1)
summary(m2)

AIC(m1)
AIC(m2)



#Training and testing####

set.seed(42)

which_train <- sample(1:8493, size = 4247, replace = FALSE)

training <- clean_Tdemo_asthma |>
  slice(which_train)

#keeps the other rows that are not in the training dataset
testing <- clean_Tdemo_asthma |>
  slice(-which_train)

#Now run the model

#use the model that I liked from variable testing (m2)

#the testing and training datasets are subsets of clean_Tdemo_asthma which is why the coefficants are a bit diffrent

m2 <- lm(agegroup6 ~ birthsex+newrace6_b+maritalstatus20+child+employment20+emp3, data = training)
summary(m2)

#pred_age = the predicted age

testing <- testing |>
  mutate(pred_age = predict(m2, newdata = testing))

#testing for residuals
testing <- testing |>
  mutate(residuals = agegroup6 - pred_age)


testing |>
  summarize(cor = cor(agegroup6, pred_age))

View(testing)

#taking this value and squaring it is similar to R^2 based on the testing dataset
testing |>
  summarize(cor = cor(agegroup6, pred_age)) |>
  mutate(R2 = cor^2)


#now find the difference between the r-squared for the training dataset and the r-squared for the testing dataset


#residual plots for the testing data
ggplot(testing, aes(x = pred_age, y = residuals)) +
  geom_point() +
  geom_smooth(se = FALSE)



ggplot(clean_Tdemo_asthma, aes(x = everasthma, y = imputed_povertygroup)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)



ggplot(clean_Tdemo_asthma, aes(x = agegroup6, y = jitter(everasthma), color = imputed_neighpovgroup4_1519)) +
  geom_point(alpha = 0.5)


ever_m1 <- lm(everasthma ~ usborn + imputed_neighpovgroup4_1519, data = clean_Tdemo_asthma)

summary(ever_m1)







#Regression dataset splits

reg_split <- initial_split(clean_Tdemo_asthma, prop = .75)

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

#what finding can  pull from age groupings?
lm_fit <- linear_reg() |>
  set_engine("lm") |>
  set_mode("regression") |>
  fit(everasthma ~ usborn + imputed_neighpovgroup4_1519, data = reg_train)


lm_fit$fit
summary(lm_fit)































Tdemo_asthma <- select(data, agegroup6, birthsex, newrace6_b, usborn,
                      maritalstatus20, education, child, employment20,
                      emp3, imputed_povertygroup, imputed_povgroup3, imputed_neighpovgroup4_1519,
                      everasthma, currentasthma20, stillasthmaall)

clean_Tdemo_asthma <- na.omit(Tdemo_asthma)



#Regression dataset splits

reg_split <- initial_split(clean_Tdemo_asthma, prop = .75)

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

#what finding can  pull from age groupings?
lm_fit <- linear_reg() |>
  set_engine("lm") |>
  set_mode("regression") |>
  fit(clean_Tdemo_asthma, data = reg_train)



























#birthsex, employment20, emp3, child, imputed_povgroup3, imputed_povertygroup, newrace6_b and maritalstatus20are the most significant to agegroup

lm_fit$fit
summary(lm_fit)


##boosted decision trees
boost_reg_fit <- boost_tree() |>
  set_engine("xgboost") |>
  set_mode("regression") |>
  fit(agegroup6 ~., data = reg_train)

boost_reg_fit$fit

#Reveals that the error is decreasing over time
boost_reg_fit$fit$evaluation_log


#random forest
#regression

forest_reg_fit <- rand_forest() |>
  set_engine("ranger") |>
  set_mode("regression") |>
  fit(agegroup6 ~., data = reg_train)

forest_reg_fit$fit



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
mae(reg_results, agegroup6, lm_pred)



yardstick::mae(reg_results, agegroup6, lm_pred)
yardstick::mae(reg_results, agegroup6, boost_pred)
yardstick::mae(reg_results, agegroup6, forest_pred)

yardstick::rmse(reg_results, agegroup6, lm_pred)
yardstick::rmse(reg_results, agegroup6, boost_pred)
yardstick::rmse(reg_results, agegroup6, forest_pred)










