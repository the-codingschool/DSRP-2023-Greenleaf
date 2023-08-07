library(tidyr)
library(janitor)
library(dplyr)
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
library(xgboost)
library(ranger)

Diabmeds <-DiabetesHS20[(DiabetesHS20$diabcntrlmeds==1 | DiabetesHS20$diabcntrlmeds==2),]
view(Diabmeds)
view(insured_diabcomu)
intInsu <- mutate(Diabmeds, diabcntrlmeds = as.numeric(diabcntrlmeds))
intInsu

pcas <- prcomp(intInsu, scale. = F)
summary(pcas)
pcas$rotation
pcas$rotation^2

pca_vals <- as.data.frame(pcas$x)
pca_vals$diabcntrlmeds <- intInsu$diabcntrlmeds
ggplot(pca_vals, aes(PC1, PC2, color =  intInsu$diabcntrlmeds))+
  geom_point()+
  theme_minimal()

class_results$diabcntrlmeds

##Classification dataset splits (DiabetesHS20)

class_split <- initial_split(DiabetesHS20, prop = 0.75)
class_train <- training(class_split)               
class_test <- testing(class_split)

##Boost
boost_class_fitHS <- boost_tree()|>
  set_engine("xgboost")|>
  set_mode("classification")|>
  fit(as.factor(diabcntrlmeds) ~ ., data = Diabmeds)
boost_class_fitHS$evaluation_log
boost_class_fitHS$fit$evaluation_log

class_results <- class_test
class_results$boost_pred <- predict(boost_class_fitHS, class_test)$.pred_class
class_results$boost_pred
takingmed <- f1(class_results$diabcntrlmeds, class_results$boost_pred)
takingmed
class_results$diabcntrlmeds
##Random
forest_class_fit_Idc <- rand_forest()|>
  set_engine("ranger")|>
  set_mode("classification")|>
  fit(as.factor(diabcntrlmeds) ~ ., data = Diabmeds ) 

class_results$forest_pred <- predict(forest_class_fit_Idc, class_test)
takingmedF <- f1(class_results$diabcntrlmeds, class_results$forest_pred)
takingmedF
##Boost for Type of Insurance 
TypeofInsu <- DiabetesHS20[(DiabetesHS20$insure5==1 | DiabetesHS20$insure5==2 |DiabetesHS20$insure5==3 |
                            DiabetesHS20$insure5==4 | DiabetesHS20$insure5==5),]

class_split <- initial_split(DiabetesHS20, prop = 0.75)
class_train <- training(class_split)               
class_test <- testing(class_split)

boost_class_fitTI <- boost_tree()|>
  set_engine("xgboost")|>
  set_mode("classification")|>
  fit(as.factor(insure5) ~ ., data = TypeofInsu)
boost_class_fitTI$evaluation_log

boost_class_fitTI$fit$evaluation_log

class_results <- class_test
class_results$boost_pred <- predict(boost_class_fitTI, class_test)$.pred_class
Tin <- f1(class_results$insure5, class_results$boost_pred)
Tin
## Boost for neighborhood income Classification
DHSinc20 <- mutate(DiabetesHS.20, 
                       imputed_neighpovgroup4_1519 = as.factor(imputed_neighpovgroup4_1519))

boost.class.fit.in <- boost_tree()|>
  set_engine("xgboost")|>
  set_mode("classification")|>
  fit(as.factor(imputed_neighpovgroup4_1519) ~ ., data = DiabetesHS.20)

boost.class.fit.in$fit$evaluation_log
class_results$boost_pred <- predict(boost.class.fit.in, class_test)$.pred_class
neighpov <- f1(class_results$imputed_neighpovgroup4_1519, class_results$boost_pred)
neighpov

##Linear Regression

DiabHS20AllNumeric <- mutate(DiabetesHS.20, 
                         insure5  = as.integer(insure5))
reg_split <- initial_split(DiabetesHS20, prop =.75)  
reg_train <- training(reg_split)
reg_test <- testing(reg_split)


lm_fit1 <- linear_reg() |>
  set_engine("lm") |>
  set_mode("regression")|>
  fit(insure5  ~ insuredgateway20 + imputed_neighpovgroup4_1519+ insure20r +
  diabetes20 + diabcntrlmeds + ageatdiabetes, 
   data = reg_train)

lm_fit1$fit
summary(lm_fit1$fit)

reg_results <- reg_test

reg_results$lm_pred <- predict(lm_fit1, reg_test)$.pred
yardstick::mae(reg_results,insure5, lm_pred)
yardstick::rmse(reg_results,insure5 , lm_pred)


## Boost Regression for place of insurance attainment
DHS20Numeric <- mutate(DiabetesHS.20, 
                       insure20r = as.integer(insure20r))

boost.reg.fit.In20 <- boost_tree()|>
  set_engine("xgboost")|>
  set_mode("regression")|>
  fit(insure20r ~ ., data = reg_train)

boost.reg.fit.In20$fit$evaluation_log
reg_results$boost_pred <- predict(boost.reg.fit.In20, reg_test)$.pred
yardstick::mae(reg_results, insure20r , boost_pred)
yardstick::rmse(reg_results,  insure20r , boost_pred)

## Boost for Insurance 20
DHSin20 <- mutate(DiabetesHS.20, 
                   insure20r = as.factor(insure20r))

boost.class.fit.in20 <- boost_tree()|>
  set_engine("xgboost")|>
  set_mode("classification")|>
  fit(as.factor(insure20r) ~ ., data = DiabetesHS.20)

boost.class.fit.in$fit$evaluation_log
class_results$boost_pred <- predict(boost.class.fit.in20, class_test)$.pred_class
in20res <- f1(class_results$insure20r, class_results$boost_pred)
in20res
