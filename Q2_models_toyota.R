### Research question 2 : What are the factors that influence the duration of a listing for a used car? 

library(tidyverse)
library(corrplot)
library(Hmisc)
library(car)
setwd("~/Downloads/Data_science-main/cleaned-data")
data <- read.csv("toyota_cleaned.csv")
toyota_cleaned <- read.csv("toyota_cleaned.csv")
attach(data)

# Our independant variable will be listing age
# First we want to build a model with our numerical variables (vehicle.age, kilometers, power, consumption, price), to try to explain the impact on the listing age. 
# We start by creating a correlation matrix with the numeric columns

 
data_numeric <- select(data, c("price", "vehicle.age", "kilometers", "power", "consumption", "listing.age"))
data_numeric
cor_matrix <- cor(data_numeric, use = "complete.obs")

cor_matrix
corrplot(cor_matrix)

# Based on the correlation matrix, there is a negative correlation between price and listing age, indicating that older listings tend to be associated with lower-priced vehicles. Kilometers appear to have no significant influence on listing age, whereas power and consumption seem to explain the listing age to a moderate extent. 
# We will make simple linear models for these 4 variables and look at their respective plots.
# We will set a significance level of α = 0.01.

summary(lm(listing.age ~ consumption))
ggplot(data, aes(consumption, listing.age)) + 
  geom_point() +
  geom_smooth(method = lm) +
  ylim(0, max(data$listing.age))

# Consumption is not statistically significant. we will not include this variable in our model. 
#While there is a positive trend indicated by the model, the weak fit suggests that consumption alone is not a strong predictor of the listing age, and other variables or a different type of model may be needed to better explain the variation in the listing age of used car listings.
#Trend Indication: The slope of the line is positive, suggesting that there is a positive relationship between consumption and listing age. This means that as the consumption of the vehicle increases, the listing tends to be older. However, the relationship does not appear to be very strong, as indicated by the wide confidence interval band (the grey area surrounding the line) and the spread of the points.

summary(lm(listing.age ~ vehicle.age))
ggplot(data, aes(vehicle.age, listing.age)) + 
  geom_point() +
  geom_smooth(method = lm) +
  ylim(0, max(data$listing.age))
# vehicle age is not statistically significant. We will not include this variable in our model 
# we can conclude that there is no relation between the age of the vehicle and the age of the listing

ggplot(lm(listing.age ~ vehicle.age), aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  geom_smooth()

#In conclusion, the residuals plot suggests that there may be issues with linearity, and the model could be improved. 

# We will try to use the LOESS method instead of a linear model

ggplot(data, aes(vehicle.age,listing.age)) + 
  geom_point() +
  geom_smooth(method = loess) +
  ylim(0, max(data$listing.age))


# We can try to fix this using a second degree variable. Let's make this model below:

data$vehicle.age.squared <- (vehicle.age ^ 2)
attach(data)
summary(lm(listing.age ~ vehicle.age + vehicle.age.squared))

# Let's check the residuals vs fitted plot

ggplot(lm(listing.age ~ vehicle.age + vehicle.age.squared), aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  geom_smooth()

# We prefer to use this model. Unfortunately we can observe heteroskedasticity in the errors.
#same problem for kilometers 

summary(lm(listing.age ~ kilometers))
ggplot(data, aes(kilometers, listing.age)) + 
  geom_point() +
  geom_smooth(method = lm) +
  ylim(0, max(data$listing.age))

ggplot(lm(listing.age ~ kilometers), aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  geom_smooth()

# we can observe some heteroskedasticity and the blue line indcates that the model is not prefectly linear 
