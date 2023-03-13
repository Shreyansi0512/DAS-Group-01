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
logistic_roc <- roc(test_data$Total.Number.of.Family.members,pre_logistic)
