---
title: "Course8_Week4_Assignment"
author: "Joyce Lo"
date: "February 20, 2018"
output: html_document
---
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE, fig.path = "./figures/")
```

# Intro
## Objective

The purpose of this study is to create a model that effectively predicts *how well* the subjects perform certain exercises based on selective covariates and relevant data collected by devices such as Jawbone Up, Nike FuelBand, and Fitbit.  The outcome variable is "classe".

## Methodology

Applying learnings from the Coursera class, the approach is as follows: define question --> Input data --> Create features --> algorithm --> parameters --> evaluations.

## Key findings

I found that using *random forest* as algorithm and using covariates like "roll_belt", "yaw_belt","pitch_forearm", "magnet_dumbell_z"  were most effective as it yields an accuracy of 99% in the testing set.  

# Analysis

Let's download the data.  *Note that I commented out the downloading part* because it only has to be done once.
```{r downloadData} 
#fileURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
#if(!file.exists("data")){dir.create("data")}
#download.file(fileURL, destfile = "./data/training.csv")
training <- read.csv("./data/training.csv")

#fileURL1 <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
#download.file(fileURL1, destfile = "./data/validation.csv")
validation <- read.csv("./data/validation.csv")
```

# Exploratory Research

This step is arguably the most important because as suggested by the adage: "junk in, junk out".  Let's explore the data sets!
Note: to save space, most of the outputs will be in Appendix.

Output in Appendix A1)
``` {r headings, results='hide'} 
summary(training)
head(training,5)
# make sure validation has same attributes
head(validation,5)
```

##Checking for attributes with lots of missing values
```{r missingValues, results='hide'}
sapply(training, function(x) sum(is.na(x)))
```

there are many columns with over 98% of missing value - I can't fill in the missing value with any confidence; therefore, I will remove those attributes as covariates.  Also, the first 7 columns also don't appear to have anything with the predicted outcome - I'll remove them as well.

```{r trimDataFrame}
training1 <- training[-c(1:7, 18:19, 21:22, 24:25, 27:36, 50:59, 75:83, 93:94, 96:97, 99:100, 103:112, 131:132, 134:135, 137:138, 141:150)]
validation1 <- validation[-c(1:7, 18:19, 21:22, 24:25, 27:36, 50:59, 75:83, 93:94, 96:97, 99:100, 103:112, 131:132, 134:135, 137:138, 141:150)]
```

##Checking for factor variables. Output in Appendix A2)
```{r FactorVar, results='hide'}
str(training1)
```

There are few factor variables with 2 to 4 levels that consist of either error terms or a value that does not vary between observations. i.e. they are useless in predicting the outcome.  Let's remove them too.

```{r trimDataFrame2}
training2 <- subset(training1, select=-c(kurtosis_yaw_belt, skewness_yaw_belt, amplitude_yaw_belt, kurtosis_yaw_dumbbell, skewness_yaw_dumbbell, amplitude_yaw_dumbbell, kurtosis_yaw_forearm, skewness_yaw_forearm, amplitude_yaw_forearm))
validation2 <- subset(validation1, select=-c(kurtosis_yaw_belt, skewness_yaw_belt, amplitude_yaw_belt, kurtosis_yaw_dumbbell, skewness_yaw_dumbbell, amplitude_yaw_dumbbell, kurtosis_yaw_forearm, skewness_yaw_forearm, amplitude_yaw_forearm))
```

##Let's check for Near Zero Variables.  Output in Appendix A3)
```{r NZV, results='hide'}
library(caret)
nzv <- nearZeroVar(training2, saveMetrics = TRUE)
nzv
```

Let's remove them from the training and validation sets too.
```{r removeNZV}
nzv_true <- which(nzv$nzv ==T)
training3 <- training2[,-(nzv_true)]
validation3 <- validation2[,-(nzv_true)]
```

I tried doing a Feature Plot but it took too much processing power.  


# Data Splitting

Let's split training3 into training and testing set:
```{r dataSplitting}
set.seed(1234)
inTrain <- createDataPartition(y=training3$classe, p=0.6, list=FALSE)
trainset <- training3[inTrain,]
testset <- training3[-inTrain,]
```

# Algorithm selection

## Classification Tree

Because the predicted outcome (y) variable is a factor variable, classification tree may be a viable option.  Let's try to fit it:
```{r Tree}
library(rpart)
mod_tree <- rpart(classe ~., data=trainset, method="class")
predict_tree <- predict(mod_tree, testset, type="class")
confusionMatrix(predict_tree, testset$classe)
library(rattle)
fancyRpartPlot(mod_tree)
```

75% accuracy using classification tree.  Let's try random forest.

## Random forest
``` {r modrf}
library(randomForest)
mod_rf <- randomForest(classe ~., data=trainset, method="class")
pred_rf <- predict(mod_rf, testset, type="class")
confusionMatrix(pred_rf, testset$classe)#$overall[1]
```

*99% using random forest.*  That's way better!

Out of curiosity, let's see which features were the most useful in predicting classe:
``` {r VarImp}
order(varImpPlot(mod_rf, decreasing=T))
```


# Quiz answers
I'm quite pleased with the effectiveness of the rf model.  I will use it to answer questions in the quiz
``` {r quiz}
valid_pred_rf <- predict(mod_rf, validation3 )
valid_pred_rf
```
I submitted these answers and got perfect score.  YAY!


# Appendices
A1) Exploratory Research
``` {r headings2, echo=FALSE} 
summary(training)
head(training,5)
# make sure validation has same attributes
head(validation,5)
```

A2) Check for factor variables
```{r FactorVar2, echo=FALSE}
str(training1)
```

A3) Check for Near Zero Variables
```{r NZV2, echo=FALSE}
nzv <- nearZeroVar(training2, saveMetrics = TRUE)
nzv
```
