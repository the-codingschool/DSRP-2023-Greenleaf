## Clean Data
filter(healthdata2, mood1 == "1", mood1 == "2", mood2 == "1",
       mood2 == "2", mood3 == "1", mood3 == "2", mood4 == "1",
       mood5 == "1", mood5 == "2", mood6 == "1", mood6 == "2",
       imputed_povertygroup == "3", imputed_povertygroup == "4")
mentalhealthdata <- select(data, imputed_povertygroup, mood1, mood2, 
                           mood3, mood4, mood5, mood6)

## Hypothesis Testing ###
# Compare the mental health of high poverty vs low poverty
# Null Hypothesis: Average mental health rates is the same for high poverty and low poverty
# Alternative Hypothesis: Rate of mental health is different for high and low poverty

lowpov <- healthdata2 |> filter(imputed_povertygroup == "1")
highpov <- healthdata2 |> filter(imputed_povertygroup == "4")

## T-Test
t.test(lowpov$mood2, highpov$mood2, paired = F, alternative = "two.sided")

## PCA ###
pcas <- prcomp(mentalhealthdata, scale. = T)
summary(pcas)

pca_vals <- as.data.frame(pcas$x)

ggplot(pca_vals, aes(PC1, PC2)) +
  geom_point() +
  theme_minimal()

## Supervised Machine Learning ###
library(dplyr)

# High Correlation 
ggplot(mentalhealthdata, aes(x = imputed_povertygroup, y = mood1)) +
  geom_point() +
  theme_minimal()

# Low Correlation 
ggplot(mentalhealthdata, aes(x = imputed_povertygroup, y = mood2)) +
  geom_point() +
  theme_minimal()

library(rsample)
set.seed(72723)

## Training & Testing Data
reg_split <- initial_split(mentalhealthdata, prop = .75)

reg_train <- training (reg_split)
reg_test <- testing(reg_split)

class_split <- initial_split(data, prop = .75)

class_train <- training(class_split)
class_test <- testing(class_split)

class_test

library(parsnip)

## Classification Model ###
library(xgboost)
boost_reg_fit <- boost_tree() |>
  set_engine("xgboost") |>
  set_mode("regression") |>
  fit(imputed_povertygroup ~ ., data = reg_train)

boost_reg_fit$fit$evaluation_log

## Random Forest Tree
library(ranger)
forest_reg_fit <- rand_forest() |>
  set_engine("ranger") |>
  set_mode("regression") |>
  fit(imputed_povertygroup ~ ., data = reg_train)

forest_reg_fit$fit

## Evaluate Performance
library(yardstick)

reg_results <- reg_test

reg_results$boost_pred <- predict(boost_reg_fit, reg_test)$.pred
reg_results$forest_pred <- predict(forest_reg_fit, reg_test)$.pred

yardstick::mae(reg_results, imputed_povertygroup, boost_pred)
yardstick::mae(reg_results, imputed_povertygroup, forest_pred)
