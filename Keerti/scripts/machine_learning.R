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
library(MLmetrics)
library(class)


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


# K-Nearest Neighbours ####
target_category <- train[,15]
test_category <- test[,15]
knn_model <- knn(train,test,cl=target_category,k=13)


# Support Vector Machine ####


# Error Evaluation ####

results <- test

results$log_pred <- predict(log_fit,test)$.pred_class
results$boost_pred <- predict(boost_class_fit, test)$.pred_class
results$forest_pred <- predict(forest_class_fit, test)$.pred_class

actual <- results$actual
pred_lp <- results$pred_lp
pred_bp <- results$pred_bp
pred_fp <- results$pred_fp
fluvaccineshot <- results$fluvaccineshot


## Logistic Regression F1: 0.6721 ####
actual <- factor(fluvaccineshot, levels = c("1","2"))
pred_lp <- factor(results$log_pred, levels = c("1","2"))
confusionMatrix(pred_lp, actual, mode = "everything", positive="1")


## Boosted Trees F1: 0.6595 ####
actual <- factor(fluvaccineshot, levels = c("1","2"))
pred_bp <- factor(results$boost_pred, levels = c("1","2"))
confusionMatrix(pred_bp, actual, mode = "everything", positive="1")


## Random Forest F1: 0.6446 ####
actual <- factor(fluvaccineshot, levels = c("1","2"))
pred_fp <- factor(results$forest_pred, levels = c("1","2"))
confusionMatrix(pred_fp, actual, mode = "everything", positive="1")


errors <- data.frame(mae = c(mae(as.numeric(actual), as.numeric(pred_lp)),
                             mae(as.numeric(actual), as.numeric(pred_bp)), 
                             mae(as.numeric(actual), as.numeric(pred_fp))),
                     rmse = c(rmse(as.numeric(actual), as.numeric(pred_lp)), 
                              rmse(as.numeric(actual), as.numeric(pred_bp)),
                              rmse(as.numeric(actual), as.numeric(pred_fp))),
                     modelType = c("logRegression", "boostTree", "randForest"))
errors

#      mae      rmse       modelType
# 1 0.3607532 0.6006274 logRegression
# 2 0.3785927 0.6152988     boostTree
# 3 0.3885035 0.6233005    randForest


## KNN Accuracy: 95.63925% ####
conf_matrix <- table(knn_model,test_category)
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(conf_matrix)





