library(tidymodels)
library(dplyr)
library(reshape2)
library(rsample)
library(ggplot2)
library(tidyverse)
library(imputeTS)
library(janitor)
library(tidyr)
library(corrplot)
library(parsnip)
library(Metrics)
library(caret)

# removing not significant variables
final_data <- select(new_data, -didntgetcare20)


# PCA ####
pcas <- prcomp(final_data)
pcas

pcas <- prcomp(final_data, scale. = T)
summary(pcas)
# pc1 and pc2 make up ~44 percent of variance
pcas$rotation

## get the x values of PCAs and make it a data frame
pca_vals <- as.data.frame((pcas$x))
pca_vals$fluvaccineshot <- final_data$fluvaccineshot

ggplot(pca_vals, aes(PC1, PC2, color = fluvaccineshot)) +
  geom_point() +
  theme_minimal()

str(final_data)

# Train/Test Split ####

final_data <- mutate(final_data,
                     fluvaccineshot = as.factor(fluvaccineshot))

set.seed(72623)

split <- initial_split(final_data, prop = .75)
train <- training(split)
test <- testing(split)


# Logistic Reg ####
log_fit <- logistic_reg() |>
  set_engine("glm") |>
  set_mode("classification") |>
  fit(fluvaccineshot ~ agegroup6 + imputed_povertygroup + ., data = train)

log_fit$fit
summary(log_fit$fit)


# Boosted Tree ####
boost_class_fit <- boost_tree() |>
  set_engine("xgboost") |>
  set_mode("classification") |>
  fit(fluvaccineshot ~ ., data = train)

summary(boost_class_fit$fit)


# Random Forest #####
forest_class_fit <- rand_forest() |>
  set_engine("ranger") |>
  set_mode("classification") |>
  fit(fluvaccineshot ~ ., data = train)

forest_class_fit$fit


# Error Evaluation ####

results <- test

results$log_pred <- predict(log_fit,test)$.pred_class
results$boost_pred <- predict(boost_class_fit, test)$.pred_class
results$forest_pred <- predict(forest_class_fit, test)$.pred_class


## Logistic Regression F1: 0.6425 ####
results$actual <- factor(results$fluvaccineshot, levels = c("1","2"))
results$pred_lp <- factor(results$log_pred, levels = c("1","2"))
confusionMatrix(results$pred_lp, results$actual, mode = "everything", positive="1")


## Boosted Trees F1: 0.6333 ####
results$actual <- factor(results$fluvaccineshot, levels = c("1","2"))
results$pred_bp <- factor(results$boost_pred, levels = c("1","2"))
confusionMatrix(results$pred_bp, results$actual, mode = "everything", positive="1")


## Random Forest F1: 0.6256 ####
results$actual <- factor(results$fluvaccineshot, levels = c("1","2"))
results$pred_fp <- factor(results$forest_pred, levels = c("1","2"))
confusionMatrix(results$pred_fp, results$actual, mode = "everything", positive="1")

# install.packages("MLmetrics")
library("MLmetrics")


F1_Score(as.numeric(results$actual), as.numeric(results$pred_lp))
results$actual
results$pred_lp
F1_Score(as.numeric(results$actual), as.numeric(results$pred_bp))
F1_Score(as.numeric(results$actual), as.numeric(results$pred_fp))

mae(as.numeric(actual), as.numeric(pred_bp))
results$pred_lp

actual <- results$actual
pred_lp <- results$pred_lp
pred_bp <- results$pred_bp
pred_fp <- results$pred_fp

errors <- data.frame(mae = c(mae(as.numeric(actual), as.numeric(pred_lp)),
                             mae(as.numeric(actual), as.numeric(pred_bp)), 
                             mae(as.numeric(actual), as.numeric(pred_fp))),
                     rmse = c(rmse(as.numeric(actual), as.numeric(pred_lp)), 
                              rmse(as.numeric(actual), as.numeric(pred_bp)),
                              rmse(as.numeric(actual), as.numeric(pred_fp))),
                     modelType = c("logRegression", "boostTree", "randForest"))
errors

