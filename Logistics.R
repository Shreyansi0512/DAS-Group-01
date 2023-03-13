dt = read.csv("dataset.csv")
train_sub = sample(nrow(dt),7/10*nrow(dt))
train_data = dt[train_sub,]
test_data = dt[-train_sub,]

dt_logistic <- glm(Total.Number.of.Family.members ~ Total.Household.Income+Total.Food.Expenditure+Household.Head.Age+House.Floor.Area+House.Age+Number.of.bedrooms, data = train_data)

income_logistic <- glm(Total.Number.of.Family.members ~ Total.Household.Income, 
                   data = train_data)
food_logistic <- glm(Total.Number.of.Family.members ~ Total.Food.Expenditure, 
                       data = train_data)
age_logistic <- glm(Total.Number.of.Family.members ~ Household.Head.Age, 
                     data = train_data)
area_logistic <- glm(Total.Number.of.Family.members ~ House.Floor.Area, 
                     data = train_data)
bed_logistic <- glm(Total.Number.of.Family.members ~ Number.of.bedrooms, 
                     data = train_data)
house_age_logistic <- glm(Total.Number.of.Family.members ~ House.Age, 
                    data = train_data)

summary(dt_logistic)
summary(income_logistic)   
summary(food_logistic)
summary(age_logistic)
summary(area_logistic)
summary(bed_logistic)
summary(house_age_logistic)

pre_logistic<-predict(dt_logistic,newdata=test_data,type="response")
print(pre_logistic)
obs_p_logistic = data.frame(prob=pre_logistic,obs=test_data$Total.Number.of.Family.members)
table(test_data$Total.Number.of.Family.members,pre_logistic,dnn=c("real","pred"))

# Create a binary response variable by collapsing some levels
test_data$binary_response <- ifelse(test_data$Total.Number.of.Family.members > 2, "Positive", "Negative")

# Generate predicted probabilities using the logistic regression model
pre_logistic <- predict(dt_logistic, newdata = test_data, type = "response")

# Calculate the ROC curve using the binary response variable and predicted probabilities
logistic_roc <- roc(test_data$binary_response, pre_logistic)

# Plot the ROC curve
plot(logistic_roc)

# we collapse the original response variable test_data$Total.Number.of.Family.members into a binary variable with two levels
# where "Positive" indicates the response variable is greater than 2, and "Negative" indicates the response variable is less than or equal to 2
# We then generate predicted probabilities using the logistic regression model dt_logistic, and use the roc function to calculate the ROC curve 
# using the new binary response variable and predicted probabilities. Finally, we plot the resulting ROC curve using the plot function
