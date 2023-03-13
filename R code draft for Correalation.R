##Exploratory Data Analysis 
##Load the data DataSet1
DataSet1 <- read.csv("\~/Desktop/Project 2/dataset1.csv")

library(tidyverse) 
library(car)
library(kableExtra)
library(gridExtra)
library(skimr)
library(ggplot2)
library(GGally)
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


skim <- skim_with(numeric = sfl(hist = NULL), 
                  base = sfl(n = length))
skim(df_num) %>%
  transmute(Variable=skim_variable, n = n, Mean=numeric.mean, SD=numeric.sd,
            Min=numeric.p0, Median=numeric.p50,  Max=numeric.p100,
            IQR = numeric.p75-numeric.p50) %>%
  kable(caption = '\\label{tab: Summary Statistics} Asian Health Data Summary Statistics for 2015.', digits=2) %>%
  kable_styling(font_size = 10, latex_options = "hold_position")

##Compute the correlation matrix for the variables

ggpairs(DataSet1[,-c(2, 4, 6, 11)])


# Plot histograms of the variables

DataSet1 %>% select(Total.Household.Income, Total.Food.Expenditure,
Household.Head.Age, Total.Number.of.Family.members, House.Floor.Area,
House.Age, Number.of.bedrooms) %>% gather() %>% ggplot(aes(x=value)) +
geom_histogram(bins = 50) + facet_wrap(~key, scales="free")

# Plot scatter plots of the variables

ggplot(DataSet1, aes(x = Total.Household.Income, y = Total.Number.of.Family.members)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Total Household Income vs. Number of Family Members")


ggplot(DataSet1, aes(x = Total.Food.Expenditure, y = Total.Number.of.Family.members)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Total Food Expenditure vs. Number of Family Members")

ggplot(DataSet1, aes(x = Household.Head.Age, y = Total.Number.of.Family.members)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Household Head Age vs. Number of Family Members")

ggplot(DataSet1, aes(x = House.Floor.Area, y = Total.Number.of.Family.members)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("House Floor Area vs. Number of Family Members")

ggplot(DataSet1, aes(x = House.Age, y = Total.Number.of.Family.members)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("House Age vs. Number of Family Members")

ggplot(DataSet1, aes(x = Number.of.bedrooms, y = Total.Number.of.Family.members)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Number of Bedrooms vs. Number of Family Members")

ggplot(data = DataSet1, aes(x = Type.of.Household, y = Total.Number.of.Family.members)) +
  geom_boxplot() +
  ggtitle("Type of Household vs. Number of Family Members")

ggplot(data = DataSet1, aes(x = Total.Food.Expenditure, y = Total.Number.of.Family.members, group = Electricity)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Total Food Expenditure vs. Number of Family Members by Electricity") +
  labs(x = "Total Food Expenditure (in Philippine peso)", y = "Total Number of Family Members") +
  theme(plot.title = element_text(hjust = 0.5))
