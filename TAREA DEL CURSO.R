path<-"D:/R coursera"
setwd(path)
library(caret)
library(tidyverse)
library(caret);library(kernlab)
df_training <- read.table(file="pml-training.csv", header=T, sep=",")
df_testing <- read.table(file="pml-testing.csv", header=T, sep=",")
rm(training)
str(df_training)
table(df_training$classe)

modelFit <- train(classe~raw_timestamp_part_1+raw_timestamp_part_2+roll_belt
                    ,data = df_training,method="rf")
modelFit

modelFit$finalModel
############################
predictions <- predict(modelFit,newdata=df_testing)
predictions

confusionMatrix(predictions,df_testing$classe)