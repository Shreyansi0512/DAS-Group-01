##Load the data DataSet1
DataSet1 <- read.csv("~/Desktop/Project 2/dataset1.csv")

library(tidyverse) 
library(car)
library(kableExtra)
library(gridExtra)
library(skimr)
library(knitr)
library(broom)

# Check the structure of the data
str(DataSet1)

# Summary statistics summary(DataSet1)
summary(DataSet1)

#Find the nonnumerical columns
dataset1$Electricity <- factor(dataset1$Electricity)
dataset1$Type.of.Household <- factor(dataset1$Type.of.Household)
dataset1$Household.Head.Sex <- factor(dataset1$Household.Head.Sex)
df <- data.frame(DataSet1)
df_num <- select_if(df, is.numeric)
summary(df_num)

# Fit the generalized linear model

model <- glm(Total.Number.of.Family.members ~ Total.Household.Income +
               Total.Food.Expenditure + Household.Head.Age + Type.of.Household +
               House.Floor.Area + House.Age + Number.of.bedrooms + Electricity, data =
               DataSet1, family = "poisson")

# Get the model summary
summary(model)

