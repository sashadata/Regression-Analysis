#################### Project 6: Regression analysis
#### Predict number of cases a client likely to open based on collected demographics
#### Apply multiple regression to examine how multiple predictor variables can be used to model a response variable
####################

### Load and explore the dataset

data<-read.csv("demo.csv")

str(data)

### The following data was collected on 1187 clients.
### This is a very rich dataset with the following columns: 
### Client _id - id of client assigned at the stage of enrollment 
### Children - number of children in the household
### Income - income of client
### NofCases - number of cases opened by clients during program
### Age - age of client
### Score - assessment of survey completed by client
### Days - total number of days being enrolled in the program
### Services - total number of services received by clients


#### All of the variables are quantitative
#### On average client has opened 3.6 cases. The range is 7, min is 2, and max is 4. 

library(tidyverse)
library(dplyr)
summary(data)

### The first column is id. Not useful for analysis, will be removed

data<-subset(data, select = Children:Services) 


### In the following pairs plot of the other 7 quantitative values, there is a slight    
### relationship between number of cases and age, number of cases and services, and quite 
### strong relationship between number of cases and number of children in the 
### household
pairs(data)

### Run correlation analysis to tell us the relationship between variables.
### Notice that most of the correlations are at or below 0.20. 
### The highest is between nofcases and children 0.81
### Then, age and nofcaaes is correlated at .21 
### Services and nofcases is .22

round(cor(data),2)

#### Create multiple linear regression model to number of cases from the rest of the explanatory variables

library(MASS)
options(scipen=999) 
model<-lm(NofCases ~.,data=data)
summary(model)

#### Observations from the model:
#### The p𝑝-value for each coefficient tells whether it’s a significant predictor 
#### of number of cases given the other explanatory variables in the model.
#### The p𝑝-values for children, age, services and score are significant.

#### This model gives us a multiple R2 of 0.6427 and an adjusted R2of 0.6827. 
#### In other words, 68.27% of variation in "number of cases" can be explained 
#### by the linear regression model by four variables.

#### For example, as children variable increases by 1 unit, the number of cases will increase by 1.014
### Plugging numbers in the formula to answer question: how many cases will a client open with the following characteristics 2 children in the family;
### 39 years old, with 50 services and 165 score for the survey assessment, 90 days in program, and
### whoes income is 20K
### Number of Cases=1.59+ 1.014*2 (Children) + 0.011*39(Age) +0.0005*50 (Services) -0.0013*165(Score)-0.0003*90(Days)+
### -0.000001 *20000(Income)
### The answer is ~ 4 


#### Use leaps package which has tuning parameter nvmax (number of predictors to consider
### in the model
library(leaps)
step<-stepAIC(model, direction ="both", trace=FALSE)
summary(step)

### As the data set contains only 5 predictors, the nvmax function will identify 5 best models 
### The models excluded the income varaible; however, the combination of all other variables is allowed.  
### This makes sense as income was not significant in multiple regression formular used above

models <- regsubsets(NofCases~., data = data, nvmax = 5, method = "seqrep")
summary(models)

library(caret)
set.seed(12345)

### Using 10-fold cross-validation to estimate the average prediction error (RMSE) of each of the 5 models
train.control <-trainControl(method ="cv", number = 10) 

### train the model
step.model <- train(NofCases ~., data = data,
                    method = "leapBackward", 
                    tuneGrid = data.frame(nvmax = 1:5),
                    trControl = train.control)

### It can be seen that the model with 3 variables (nvmax = 3) is the one that has the lowest RMSE. 
### This indicates that the best model is the one with nvmax = 3 variables.

step.model$results
step.model$bestTune

### It can be seen that the best 3-variables model containes Children, Score, Age
### In other words, these 3 variables are the main predictors on how many cases will be opened 
summary(step.model$finalModel)


