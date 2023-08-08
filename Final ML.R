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

#currentasthma20 ~ rodentsstreet + helpcommproj + helpneighbors20_q1 + trustkeys + discussissues

#Regression dataset splits

reg_split <- initial_split(clean_envi_asthma, prop = .75)

#use the split to form testing and training sets
reg_train <- training(reg_split)
reg_test <- testing(reg_split)


##set a seed for producability, gives us the same results every time
set.seed(71723)

#Liner regression

#Findings
#null: there is no relationship between the dependent variable and the independent variables
#null: The enviornmental living conditions of a New York citizen does not significantlly determine wheather or not a New Yorker has asthma (mice, neighborhood interactions)
#alternative: there is a relationship between the dependent variable and the independent variables
#alternative hypothesis: The enviornmental living conditions of a New York citizen does significantlly determine wheather or not a New Yorker has asthma (mice, neighborhood interactions)

#(p-value: < 2.2e-16) Reject the null

#what finding can  pull from age groupings?
lm_fit <- linear_reg() |>
  set_engine("lm") |>
  set_mode("regression") |>
  fit(currentasthma20 ~ rodentsstreet + helpneighbors20_q1 + discussissues + helpcommproj + trustkeys, data = reg_train)


lm_fit$fit
summary(lm_fit$fit)


##boosted decision trees
boost_reg_fit <- boost_tree() |>
  set_engine("xgboost") |>
  set_mode("regression") |>
  fit(currentasthma20 ~ rodentsstreet + helpneighbors20_q1 + discussissues + helpcommproj + trustkeys, data = reg_train)

boost_reg_fit$fit

#Reveals that the error is decreasing over time
boost_reg_fit$fit$evaluation_log


#random forest
#regression

forest_reg_fit <- rand_forest() |>
  set_engine("ranger") |>
  set_mode("regression") |>
  fit(currentasthma20 ~ rodentsstreet + helpneighbors20_q1 + discussissues + helpcommproj + trustkeys, data = reg_train)

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



#truth is currentasthma20, false in Lm_pred
#RESULTS: Lm_Pred has the lowest error for both mae(0.0925) and rmse(0.222)
yardstick::mae(reg_results, currentasthma20, lm_pred)
yardstick::mae(reg_results, currentasthma20, boost_pred)
yardstick::mae(reg_results, currentasthma20, forest_pred)

yardstick::rmse(reg_results, currentasthma20, lm_pred)
yardstick::rmse(reg_results, currentasthma20, boost_pred)
yardstick::rmse(reg_results, currentasthma20, forest_pred)


library(Metrics)

errors <- data.frame(mae = yardstick::mae(reg_results, currentasthma20, lm_pred),
                    rmse = yardstick::rmse(reg_results, currentasthma20, lm_pred),
                    modelType = "lm_pred")


errors2 <- data.frame(mae = yardstick::mae(reg_results, currentasthma20, boost_pred),
                     rmse = yardstick::rmse(reg_results, currentasthma20, boost_pred),
                     modelType = "Boosted Tree")


errors3 <- data.frame(mae = yardstick::mae(reg_results, currentasthma20, forest_pred),
                     rmse = yardstick::rmse(reg_results, currentasthma20, forest_pred),
                     modelType = "Random Forest")


combinederrors <- rbind(errors,
                errors2,
                data.frame(mae = yardstick::mae(reg_results, currentasthma20, forest_pred),
                           rmse = yardstick::rmse(reg_results, currentasthma20, forest_pred),
                           modelType = "Random Forest"))


combinederrors <- data.frame(combinederrors)




#Plots

#plot of errors

library(ggplot2)

combinederrors$mae..estimate


ggplot(combinederrors, aes(x = modelType, y = mae..estimate, fill = modelType)) +
  geom_bar(stat = "summary",
           fun = "mean") +
  theme_minimal() +
  labs(
    title = "Distribution of Errors (mae)",
    x = "Model Type",
    y = "mae estimate"
  ) +
  scale_fill_manual(values = c("lightgreen", "lightpink", "lightblue"))


ggplot(combinederrors, aes(x = modelType, y = rmse..estimate, fill = modelType)) +
  geom_bar(stat = "summary",
           fun = "mean") +
  theme_minimal() +
  labs(
    title = "Distribution of Errors (rmse)",
    x = "Model Type",
    y = "rmse estimate"
  ) +
  scale_fill_manual(values = c("lightgreen", "lightpink", "lightblue"))




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








actual <- reg_results$currentasthma20
predicted <- reg_results$lm_pred
predicted2 <- reg_results$forest_pred

values <- data.frame(actual = actual,
                     predicted = predicted,
                     predicted2 = predicted2)



ggplot(values, aes(x = actual, y = predicted)) +
  geom_abline(lty = "dashed", color = "black") +
  geom_point() +
  theme_minimal() +
  annotate(geom = "text", label = paste("rmse =", round(rmse(actual, predicted),2)),
           x = 6, y = 1.8)















