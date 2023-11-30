### Research question 1: Which physical factors have the biggest impact on used car prices?

library(tidyverse)
library(corrplot)
library(Hmisc)
library(car)
setwd("~/GitHub/Data_science/cleaned-data")
data <- read.csv("volvo2_cleaned.csv")
volvo2_cleaned <- read.csv("volvo2_cleaned.csv")
attach(data)

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

# Consumption is statistically significant at α = 0.01.

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

# We can try to fix this using a second degree variable. Let's make this model below:

data$vehicle.age.squared <- (vehicle.age ^ 2)
attach(data)
summary(lm(price ~ vehicle.age + vehicle.age.squared))

# Let's check the residuals vs fitted plot

ggplot(lm(price ~ vehicle.age + vehicle.age.squared), aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  geom_smooth()

# We prefer to use this model. Unfortunately we can observe heteroskedasticity in the errors.
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

model1 <- lm(price ~ vehicle.age + vehicle.age.squared + kilometers + kilometers.squared + power + consumption)
summary(model1)

# We observe that every variable is statistically significant if we use them together in model1
# Let's check the residuals vs fitted plot

ggplot(model1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  geom_smooth()

# We will now remove defective vehicles and run the model again, this will hopefully stop the model from undervaluing cars.
# Listings flagged as defective account for under 5% of our observations, so removing them won't have a significant impact on our data.

table(data$defective)[2]/nrow(data)
data_no_def <- filter(data, !defective)
model2 <- lm(data_no_def$price ~ data_no_def$vehicle.age + data_no_def$vehicle.age.squared + data_no_def$kilometers + data_no_def$kilometers.squared + data_no_def$power + data_no_def$consumption)

# Significance test

summary(model2)

# Residuals vs fitted plot

ggplot(model2, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  geom_smooth()

# We don't notice much difference in the statistical tests or plots between the 2 models. The standard deviation of the residuals has decreased a small amount, but we can't consider this to be significant.

sqrt(anova(model1)$"Mean Sq")[7] # Sd model1
sqrt(anova(model2)$"Mean Sq")[7] # Sd model2

### We will now run simple linear regressions for our logical variables. We first need to transform some of our columns into logical values.

# For body.type, we can see that the majority of cars are "SUV / Tout-terrain", we have a very small amount that are other body types. These are errors, the XC60 only comes as an SUV.

table(drivetrain)

# We will create a logical column "diesel" which will be set to TRUE where fuel.type == "Diesel" is TRUE. We do the same for column "hybrid". When both are FALSE, the fuel.type will be petrol.

data$diesel <- fuel.type == "Diesel"
data$hybrid <- ifelse(fuel.type == "Hybride léger essence/électrique" | 
                        fuel.type == "Hybride rechargeable essence/électrique" |
                        fuel.type == "Hybride léger diesel/électrique", 
                      TRUE, FALSE)

# We repeat the process for transmission. We will only have one variable "manual". We consider "Boîte manuelle automatisée" to be automatic because it is very similar to an automatic transmission.

data$manual <- ifelse(transmission == "Automatique" |
                        transmission == "Boîte manuelle automatisée" |
                        transmission == "Boîte automatique variable", 
                      FALSE, TRUE)

# We repeat the process for drivetrain. We will create one column "fwd".

data$fwd <- drivetrain == "Traction avant"

# Now we run the simple linear regressions for expertise, warranty, wagon, cabriolet, diesel, hybrid, natural.gas, electric, manual and awd. We note which ones are statistically significant at α = 0.01.

attach(data)
summary(lm(price ~ expertise)) # Significant
summary(lm(price ~ warranty)) # Significant
summary(lm(price ~ diesel)) # Significant
summary(lm(price ~ hybrid)) # Significant
summary(lm(price ~ manual)) # Significant
summary(lm(price ~ fwd)) # Significant

# Let's add all the significant variables to a model and see if they are still significant.

model3 <- lm(price ~ expertise + warranty + diesel + hybrid + manual + fwd)
summary(model3)
anova(model3)

# They are all significant except expertise and fwd. Let's add them to our original model.

model4 <- lm(price ~ vehicle.age + vehicle.age.squared + kilometers + kilometers.squared + power + consumption + expertise + warranty + diesel + hybrid + manual + fwd)
summary(model4)

# We remove expertise, warranty, diesel, manual and fwd as they are not significant.

model4 <- lm(price ~ vehicle.age + vehicle.age.squared + kilometers + kilometers.squared + power + consumption + hybrid)
summary(model4)

# All our variables are now significant. Our adjusted R-squared is 0.9239, which is the proportion of the variance of the price that is explained by our model. We consider this to be a good score. Let's check the residuals vs fitted plot and the standard deviation of the residuals.

ggplot(model4, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  geom_smooth()

sqrt(anova(model4)$"Mean Sq")[8]

# The plot seems very similar to model2 and the standard deviation has decreased a small amount, which is an improvement. We still have a problem with heteroskedasticity in the errors. 

anova(model4)
model4_data <- select(data, c("price", "vehicle.age", "vehicle.age.squared", "kilometers", "kilometers.squared", "power", "consumption", "hybrid"))
cor_matrix_2 <- cor(model4_data, use = "complete.obs")
cor_matrix_2
corrplot(cor_matrix_2)

# Model with cars more expensive than 60'000 CHF removed. We do this because the heteroskedasticity observed was mainly when fitted values > 60000. 

data_no_expensive <- filter(data, price < 60000)
nrow(data_no_expensive)/nrow(data)

model5 <- lm(data_no_expensive$price ~ data_no_expensive$vehicle.age + data_no_expensive$vehicle.age.squared + data_no_expensive$kilometers + data_no_expensive$kilometers.squared + data_no_expensive$power + data_no_expensive$consumption + data_no_expensive$hybrid)
summary(model5)
sqrt(anova(model5)$"Mean Sq")[8]

# The standard deviation of the errors has improved significantly. Let's look at the residuals vs fitted plot.

ggplot(model5, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  geom_smooth()

# This looks worse than model4's plot. We may have not enough data in this case.

# Model4 without defectives (model6)

data_no_def <- filter(data, !defective)
model6 <- lm(data_no_def$price ~ data_no_def$vehicle.age + data_no_def$vehicle.age.squared + data_no_def$kilometers + data_no_def$kilometers.squared + data_no_def$power + data_no_def$consumption + data_no_def$hybrid)
summary(model6)
anova(model6)
sqrt(anova(model6)$"Mean Sq")[8]

ggplot(model6, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  geom_smooth()

# We will test model6 for multicollinearity

model6_data <- select(data_no_def, c("price", "vehicle.age", "vehicle.age.squared", "kilometers", "kilometers.squared", "power", "consumption", "hybrid"))
cor_matrix_3 <- cor(model6_data, use = "complete.obs")
cor_matrix_3
corrplot(cor_matrix_3)

vif(model6)
model7 <- lm(data_no_def$price ~ data_no_def$kilometers + data_no_def$kilometers.squared + data_no_def$power + data_no_def$consumption + data_no_def$hybrid)
vif(model7)
summary(model7)
model8 <- lm(data_no_def$price ~ data_no_def$vehicle.age + data_no_def$vehicle.age.squared + data_no_def$power + data_no_def$consumption + data_no_def$hybrid)
vif(model8)
summary(model8)

#Our VIF values are much smaller when we remove either kilometers or vehicle.age, but remain higher than 10 for kilometers and vehicle age.
#The multicollinearity seems to be severe with the volvos.

# Let's add a residuals column to the data, which will be the residuals calculated by model6. We remove all rows with missing values in data_no_def first.

data_no_def <- data_no_def[complete.cases(data_no_def[,c("price", "vehicle.age", "vehicle.age.squared", "kilometers", "kilometers.squared", "power", "consumption", "hybrid")]),]
data_no_def$residuals <- model6$residuals

data_no_def <- data_no_def |>
  mutate(count = row_number()) |>
  select(count, everything())

#Save file
write.csv(data, "volvo_models_data.csv")