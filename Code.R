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

model<-lm(NofCases ~.,data=data)
summary(model)

#### Observations from the model:
#### The p𝑝-value for each coefficient tells whether it’s a significant predictor 
#### of number of cases given the other explanatory variables in the model.
#### The p𝑝-values for children, age, services and score are significant.

#### This model gives us a multiple R2 of 0.6427 and an adjusted R2of 0.6827. 
#### In other words, 68.27% of variation in "number of cases" can be explained 
#### by the linear regression model by four variables.


