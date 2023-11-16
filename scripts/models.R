### Research question 1: Which physical factors have the biggest impact on used car prices?

library(tidyverse)
library(corrplot)
library(Hmisc)
setwd("~/GitHub/Data_science/cleaned-data")
data <- read.csv("golf2_cleaned.csv")
golf2_cleaned <- read.csv("golf2_cleaned.csv")
attach(data)

# We will start our analysis with the VW Golf data set.
# Our dependent variable y will be price.
# Firstly we will make a model with our numerical variables (vehicle.age, kilometers, power, consumption) to try and explain price.
# We start by creating a correlation matrix with the numeric columns.

data_numeric <- select(data, c("price", "vehicle.age", "kilometers", "power", "consumption"))
data_numeric
cor_matrix <- cor(data_numeric, use = "complete.obs")
cor_matrix
corrplot(cor_matrix)

# By looking at the numbers and the plot, we can already see that kilometers and vehicle.age are negatively correlated with price, and power is positively correlated. Consumption seems to have no effect on price, we will probably not include this variable.
# We will make simple linear models for these 4 variables and look at their respective plots.
# We will set a significance level of α = 0.01.

summary(lm(price ~ consumption))
ggplot(data, aes(consumption, price)) + 
  geom_point() +
  geom_smooth(method = lm) +
  ylim(0, max(data$price))

# Consumption is not statistically significant at α = 0.01. We will not include this variable in our model.

summary(lm(price ~ vehicle.age))
ggplot(data, aes(vehicle.age, price)) + 
  geom_point() +
  geom_smooth(method = lm) +
  ylim(0, max(data$price))

# The relationship between price and vehicle age is obviously not linear. We can check this with a residuals vs fitted plot, which should show us a horizontal line if the data is linear.

ggplot(lm(price ~ vehicle.age), aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  geom_smooth()

# We will try to use the LOESS method instead of a linear model

ggplot(data, aes(vehicle.age, price)) + 
  geom_point() +
  geom_smooth(method = loess) +
  ylim(0, max(data$price))

# In the graph above, we can see that the average price goes up after a vehicle age of approx. 9000 days (= 24 years, 7 months, 21 days) which is counter-intuitive.
# One hypothesis would be that these cars could be considered as vintage or collector cars, and therefore won't appeal to the average buyer, who is a daily driver. These types of cars have usually been very well maintained and have low mileage, which could also explain this overvaluation.
# We also notice that the curve is steeper and therefore cars seem to depreciate faster in their first 2500 days (6 years, 10 months, 2 days) than in their next 7500 (20 years, 6 months, 12 days).
# We can try to fix this using second and third degree variables. Let's make these models below:

data$vehicle.age.squared <- (vehicle.age ^ 2)
data$vehicle.age.cubed <- (vehicle.age ^ 3)
attach(data)
summary(lm(price ~ vehicle.age + vehicle.age.squared + vehicle.age.cubed))

# Let's check the residuals vs fitted plot

ggplot(lm(price ~ vehicle.age + vehicle.age.squared + vehicle.age.cubed), aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  geom_smooth()

# It looks much better. Now we will try with only vehicle.age.squared.

ggplot(lm(price ~ vehicle.age + vehicle.age.squared), aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  geom_smooth()

# The result is very similar. We will prefer to use this model in order to limit the number of variables used in the final model.
# We notice the same problem to a lesser extent with kilometers:

summary(lm(price ~ kilometers))
ggplot(data, aes(kilometers, price)) + 
  geom_point() +
  geom_smooth(method = lm) +
  ylim(0, max(data$price))

ggplot(lm(price ~ kilometers), aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  geom_smooth()

# We add a second degree variable kilometers.squared

data$kilometers.squared <- (kilometers ^ 2)
attach(data)
summary(lm(price ~ kilometers + kilometers.squared))

# Let's check the residuals vs fitted plot

ggplot(lm(price ~ kilometers + kilometers.squared), aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  geom_smooth()

# This is a better solution.
# We see that power is more or less linear so there is no need to add a second degree variable.

summary(lm(price ~ power))
ggplot(data, aes(power, price)) + 
  geom_point() +
  geom_smooth(method = lm) +
  ylim(0, max(data$price))

# Separately they are all statistically significant. Let's make a linear model with these 5 variables together.

model1 <- lm(price ~ vehicle.age + vehicle.age.squared + kilometers + kilometers.squared + power)
summary(model1)
plot(model1)

# We observe that every variable is statistically significant if we use them together in model1
# Let's check the residuals vs fitted plot

ggplot(model1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  geom_smooth()

### We will now make simple linear models to test our logical variables (logit regression)
# We first need to transform some columns into logicals


#############NOTES:
# - check multicollinearity