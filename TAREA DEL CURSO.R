path<-"D:/R coursera"
setwd(path)
library(caret)
library(tidyverse)
library(caret);library(kernlab)

## Importing Data TRAIN
## Feature Extraction
df_training <- read.table(file="pml-training.csv", header=T, sep=",")
df_testing <- read.table(file="pml-testing.csv", header=T, sep=",")
rm(training)
str(df_training)
table(df_training$classe)

## Feature Extraction
df_training <-
  df_training %>%
  mutate(
    datetime = lubridate::dmy_hms(df_training$cvtd_timestamp),
    across(where(is.logical), as.numeric),
    new_window = str_replace(new_window, "no", "FALSE"),
    new_window = str_replace(new_window, "yes", "TRUE"),
    new_window = as.logical(new_window)
  ) %>%
  select(-c(raw_timestamp_part_1, raw_timestamp_part_2, cvtd_timestamp))

df_testing <-
  df_testing %>%
  mutate(
    datetime = lubridate::dmy_hms(df_testing$cvtd_timestamp),
    across(where(is.logical), as.numeric),
    new_window = str_replace(new_window, "no", "FALSE"),
    new_window = str_replace(new_window, "yes", "TRUE"),
    new_window = as.logical(new_window)
  ) %>%
  select(-c(raw_timestamp_part_1, raw_timestamp_part_2, cvtd_timestamp))


###Generate 10 folds for cross-validation, 
##stratified by `classe`. For a dataset of this size,
###10-fold cross-validation should be sufficient to optimise bias-variance trade_off.

##{r 10foldcv}
library(tidymodels)
tidymodels_prefer()
wear_folds <-
  vfold_cv(df_training, v = 10, strata = classe)

## Build Models
##Remove predictors with at least 90% of data missing (most models do not deal well with missing data)
##Remove predictors with near zero variance (these will not provide useful information but bloat the , model)
##Remove highly correlated predictors (as above)
##Remove predictors related to time or the test subject (I might reconsider putting these back in if the model performs poorly)
##Normalise numerical predictors (to improve the fitting algorithms)

wear_rec <-
  recipe(classe ~ ., data = df_training) %>%
  update_role("...1", new_role = "id") %>%
  step_filter_missing(all_predictors(), threshold = 0.9) %>%
  step_nzv(all_numeric()) %>%
  step_corr(all_numeric()) %>%
  step_rm(c("...1", "user_name", "new_window", "num_window", "datetime")) %>%
  step_normalize(all_predictors()) %>%
  prep()

corr(df_training)

#### Model logistic regresion - exercise
model <- nnet::multinom(classe ~raw_timestamp_part_1+raw_timestamp_part_2+roll_belt, data = df_training)
summary(model)
#### Model final final-Random forest
modelFit <- train(classe~raw_timestamp_part_1+raw_timestamp_part_2+roll_belt
                    ,data = df_training,method="rf")
modelFit

modelFit$finalModel
plot(modelFit)
print(modelFit$finalModel)

## 
rpart.plot(modelFit$finalModel)
############################
predictions <- predict(modelFit,newdata=df_testing)
View(predictions)
