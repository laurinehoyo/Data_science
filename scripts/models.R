### Research question 1: Which physical factors have the biggest impact on used car prices?
library(tidyverse)
library(corrplot)
install.packages("Hmisc")
library(Hmisc)
setwd("~/GitHub/Data_science/cleaned-data")
data <- read.csv("golf2_cleaned.csv")
golf2_cleaned <- read.csv("golf2_cleaned.csv")
attach(data)

# We will start our analysis with the VW Golf data set.
# We start by creating a correlation matrix with the numeric columns

data_numeric <- select(data, c("price", "kilometers", "power", "consumption", "vehicle.age"))
data_numeric
cor_matrix <- cor(data_numeric, use = "complete.obs")
cor_matrix
corrplot(cor_matrix)

# By looking at the numbers and the plot, we can already see that kilometers and vehicle.age are negatively correlated with price, and power is positively correlated. Consumption seems to have no effect on price.
# We will make simple linear models for these 3 variables and look at their respective plots.

summary(lm(price ~ kilometers))
ggplot(data, aes(kilometers, price)) + 
  geom_point() +
  geom_smooth(method = lm) +
  ylim(0, max(data$price))

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

# We can try to fix this using second and third degree variables. Let's make these models below:

data$vehicle.age.squared <- (vehicle.age ^ 2)
data$vehicle.age.cubed <- (vehicle.age ^ 3)
summary(lm(price ~ vehicle.age + vehicle.age.squared + vehicle.age.cubed))

# Let's check the residuals vs fitted plot

ggplot(lm(price ~ vehicle.age + vehicle.age.squared + vehicle.age.cubed), aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  geom_smooth()

#It looks much better. Now we will try with only vehicle.age.squared.

summary(lm(price ~ power))
ggplot(data, aes(power, price)) + 
  geom_point() +
  geom_smooth(method = lm) +
  ylim(0, max(data$price))

# Seperately they are all statistically significant. Let's make a linear model with these 3 variables together.

model1 <- lm(price ~ kilometers + vehicle.age + power)
summary(model1)
plot(model1)

ggplot(data, aes(kilometers, price)) + 
  geom_point() +
  geom_smooth(method = lm) +
  ylim(0, max(data$price))
  