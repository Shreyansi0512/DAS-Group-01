# Load the dataset
data <- read.csv("dataset.csv")

# Fit a GLM model
model <- glm(Total.Number.of.Family.members ~ Total.Household.Income + Total.Food.Expenditure + Household.Head.Age + House.Floor.Area + House.Age + Number.of.bedrooms, data = dataset)

# View the summary of the model
summary(model)

#Calculate relative importance of predictor variables
#Two types of analysis are used here, LMG and Pratt.
library(relaimpo)
rel_imp <- calc.relimp(model, type = "pratt")
rel_imp

#The above output is from a GLM model which has  Total.Number.of.Family.members as the response variable. 
#The model is based on 1725 observations with 6 regressors
#The proportion of variance explained by the model is 24.38%.

importance <- rel_imp$pratt
importance
#the relative importance of each predictor variable
barplot(importance, main = "Relative Importance of Predictor Variables",
        xlab = "Predictor Variables", ylab = "Relative Importance",
        col = "steelblue", ylim = c(0,1))
