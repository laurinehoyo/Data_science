### Research question 1: Which physical factors have the biggest impact on used car prices?

library(tidyverse)
library(corrplot)
library(Hmisc)
library(car)
setwd("~/GitHub/Data_science/cleaned-data")
data <- read.csv("skoda2_cleaned.csv")
skoda2_cleaned <- read.csv("skoda2_cleaned.csv")
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

model1 <- lm(price ~ vehicle.age + vehicle.age.squared + kilometers + kilometers.squared + power)
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
model2 <- lm(data_no_def$price ~ data_no_def$vehicle.age + data_no_def$vehicle.age.squared + data_no_def$kilometers + data_no_def$kilometers.squared + data_no_def$power)

# Significance test

summary(model2)

# Residuals vs fitted plot

ggplot(model2, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  geom_smooth()

# We don't notice much difference in the statistical tests or plots between the 2 models. The standard deviation of the residuals has decreased a small amount, but we can't consider this to be significant.

sqrt(anova(model1)$"Mean Sq")[6] # Sd model1
sqrt(anova(model2)$"Mean Sq")[6] # Sd model2

### We will now run simple linear regressions for our logical variables. We first need to transform some of our columns into logical values.

# For body.type, we can see that the majority of cars are "Break".
# We will then create a logical column "cabriolet" which will be set to TRUE where body.type == "cabriolet" is TRUE. We do the same for column "suv" and "sedan". When the 3 are FALSE, the body.type will be Break.

table(body.type)

data$cabriolet <- body.type == "Cabriolet"
data$suv <- body.type == "SUV / Tout-terrain"
data$sedan <- body.type == "Limousine"

# We will repeat the process for fuel.type. Columns will be "diesel", "hybrid", and "natural.gas".

data$diesel <- fuel.type == "Diesel"
data$hybrid <- ifelse(fuel.type == "Hybride léger essence/électrique" | 
                        fuel.type == "Hybride rechargeable essence/électrique" |
                        fuel.type == "Hybride léger diesel/électrique", 
                      TRUE, FALSE)
data$natural.gas <- fuel.type == "Gaz naturel (CNG) / Essence"

# We repeat the process for transmission. We will only have one variable "manual". We consider "Boîte manuelle automatisée" to be automatic because it is very similar to an automatic transmission.

data$manual <- ifelse(transmission == "Automatique" |
                        transmission == "Boîte manuelle automatisée" |
                        transmission == "Boîte automatique variable", 
                      FALSE, TRUE)

# We repeat the process for drivetrain. We will create one column "awd".

data$awd <- drivetrain == "4 roues motrices"

# Now we run the simple linear regressions for expertise, warranty, cabriolet, suv, diesel, hybrid, natural.gas, manual and awd. We note which ones are statistically significant at α = 0.01.

attach(data)
summary(lm(price ~ expertise)) # Significant
summary(lm(price ~ warranty)) # Significant
summary(lm(price ~ suv)) # NOT Significant
summary(lm(price ~ sedan)) # Significant
summary(lm(price ~ cabriolet)) # NOT Significant
summary(lm(price ~ diesel)) # Significant
summary(lm(price ~ hybrid)) # Significant
summary(lm(price ~ natural.gas)) # NOT Significant
summary(lm(price ~ manual)) # Significant
summary(lm(price ~ awd)) # Significant

# Let's add all the significant variables to a model and see if they are still significant.

model3 <- lm(price ~ expertise + warranty + sedan + diesel + hybrid + manual + awd)
summary(model3)
anova(model3)

# They are all significant except sedan. Let's add them to our original model.

model4 <- lm(price ~ vehicle.age + vehicle.age.squared + kilometers + kilometers.squared + power + expertise + warranty + sedan + diesel + hybrid + manual + awd)
summary(model4)

# We remove sedan and awd, they are not significant.

model4 <- lm(price ~ vehicle.age + vehicle.age.squared + kilometers + kilometers.squared + power + expertise + warranty + diesel + hybrid + manual)
summary(model4)

# All our variables are now significant. Our adjusted R-squared is 0.9346, which is the proportion of the variance of the price that is explained by our model. We consider this to be a good score. Let's check the residuals vs fitted plot and the standard deviation of the residuals.

ggplot(model4, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  geom_smooth()

sqrt(anova(model4)$"Mean Sq")[11]

# The plot seems very similar to model2 and the standard deviation has decreased a small amount, which is an improvement. We still have a problem with heteroskedasticity in the errors. 

anova(model4)
model4_data <- select(data, c("price", "vehicle.age", "vehicle.age.squared", "kilometers", "kilometers.squared", "power", "expertise", "warranty", "diesel", "hybrid", "manual"))
cor_matrix_2 <- cor(model4_data, use = "complete.obs")
cor_matrix_2
corrplot(cor_matrix_2)

# Model with cars more expensive than 40'000 CHF removed. We do this because the heteroskedasticity observed was mainly when fitted values > 40000. 

data_no_expensive <- filter(data, price < 40000)
nrow(data_no_expensive)/nrow(data)

model5 <- lm(data_no_expensive$price ~ data_no_expensive$vehicle.age + data_no_expensive$vehicle.age.squared + data_no_expensive$kilometers + data_no_expensive$kilometers.squared + data_no_expensive$power + data_no_expensive$expertise + data_no_expensive$warranty + data_no_expensive$diesel + data_no_expensive$hybrid + data_no_expensive$manual)
summary(model5)

# We remove hybrid because it is no longer significant.

model5 <- lm(data_no_expensive$price ~ data_no_expensive$vehicle.age + data_no_expensive$vehicle.age.squared + data_no_expensive$kilometers + data_no_expensive$kilometers.squared + data_no_expensive$power + data_no_expensive$expertise + data_no_expensive$warranty + data_no_expensive$diesel + data_no_expensive$manual)
summary(model5)
sqrt(anova(model5)$"Mean Sq")[10]

# The standard deviation of the errors has improved significantly. Let's look at the residuals vs fitted plot.

ggplot(model5, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  geom_smooth()

# This looks better than model4's plot.

# Model4 without defectives (model6)

data_no_def <- filter(data, !defective)
model6 <- lm(data_no_def$price ~ data_no_def$vehicle.age + data_no_def$vehicle.age.squared + data_no_def$kilometers + data_no_def$kilometers.squared + data_no_def$power + data_no_def$expertise + data_no_def$warranty + data_no_def$diesel + data_no_def$hybrid + data_no_def$manual)
summary(model6)
anova(model6)
sqrt(anova(model6)$"Mean Sq")[11]

ggplot(model6, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  geom_smooth()

# We will test model6 for multicollinearity

model6_data <- select(data_no_def, c("price", "vehicle.age", "vehicle.age.squared", "kilometers", "kilometers.squared", "power", "expertise", "warranty", "diesel", "hybrid", "manual"))
cor_matrix_3 <- cor(model6_data, use = "complete.obs")
cor_matrix_3
corrplot(cor_matrix_3)

vif(model6)
model7 <- lm(data_no_def$price ~ data_no_def$kilometers + data_no_def$kilometers.squared + data_no_def$power + data_no_def$expertise + data_no_def$warranty + data_no_def$diesel + data_no_def$hybrid + data_no_def$manual)
vif(model7)
summary(model7)
model8 <- lm(data_no_def$price ~ data_no_def$vehicle.age + data_no_def$vehicle.age.squared + data_no_def$power + data_no_def$expertise + data_no_def$warranty + data_no_def$diesel + data_no_def$hybrid + data_no_def$manual)
vif(model8)
summary(model8)

#Our VIF values are much smaller when we remove either kilometers or vehicle.age, but the VIFs for those variables remain at around 10.

# Let's add a residuals column to the data, which will be the residuals calculated by model6. We remove all rows with missing values in data_no_def first.

data_no_def <- data_no_def[complete.cases(data_no_def[,c("price", "vehicle.age", "vehicle.age.squared", "kilometers", "kilometers.squared", "power", "expertise", "warranty", "diesel", "hybrid", "manual")]),]
data_no_def$residuals <- model6$residuals

data_no_def <- data_no_def |>
  mutate(count = row_number()) |>
  select(count, everything())

#Save file
write.csv(data, "skoda_models_data.csv")