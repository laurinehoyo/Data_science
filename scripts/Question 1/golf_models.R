### Research question 1: Which physical factors have the biggest impact on used car prices?

library(tidyverse)
library(corrplot)
library(Hmisc)
library(car)
library(MASS)
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

# We don't notice much difference in the statistical tests or plots between the 2 models. The standard deviation of the residuals has decreased a small amount (~ 0.5%), but we can't consider this to be significant.

sqrt(anova(model1)$"Mean Sq")[6] # Sd model1
sqrt(anova(model2)$"Mean Sq")[6] # Sd model2

### We will now run simple linear regressions for our logical variables. We first need to transform some of our columns into logical values.

# For body.type, we can see that the majority of cars are either "Limousine", "Break" or "Cabriolet".
# We will then create a logical column "wagon" which will be set to TRUE where body.type == "Break" is TRUE (Note: "break" is a reserved word). We do the same for columns "cabriolet", "small.car" and "coupe". When all 4 are FALSE, the body.type will be Limousine.

table(body.type)

data$wagon <- body.type == "Break"
data$cabriolet <- body.type == "Cabriolet"
data$small.car <- body.type == "Petite voiture"
data$coupe <- body.type == "Coupé"

# We will repeat the process for fuel.type. Columns will be "diesel", "hybrid", "natural.gas" and "electric"

data$diesel <- fuel.type == "Diesel"
data$hybrid <- ifelse(fuel.type == "Hybride léger essence/électrique" | 
                      fuel.type == "Hybride rechargeable essence/électrique", 
                    TRUE, FALSE)
data$natural.gas <- fuel.type == "Gaz naturel (CNG) / Essence"
data$electric <- fuel.type == "Électrique"

# We repeat the process for transmission. We will only have one variable "manual". We consider "Boîte manuelle automatisée" to be automatic because it is very similar to an automatic transmission.

data$manual <- ifelse(transmission == "Automatique" |
                        transmission == "Boîte manuelle automatisée" |
                        transmission == "Boîte automatique variable", 
                      FALSE, TRUE)

# We repeat the process for drivetrain. We will create one column "awd". We will neglect "Propulsion" and leave it as false, the same as for "Traction avant" because there are only 2 observations.

data$awd <- drivetrain == "4 roues motrices"

# Now we run the simple linear regressions for expertise, warranty, wagon, cabriolet, diesel, hybrid, natural.gas, electric, manual and awd. We note which ones are statistically significant at α = 0.01.

attach(data)
summary(lm(price ~ expertise)) # Significant
summary(lm(price ~ warranty)) # Significant
summary(lm(price ~ wagon)) # Significant
summary(lm(price ~ cabriolet)) # Significant
summary(lm(price ~ small.car)) # NOT Significant
summary(lm(price ~ coupe)) # NOT Significant
summary(lm(price ~ diesel)) # Significant
summary(lm(price ~ hybrid)) # Significant
summary(lm(price ~ natural.gas)) # NOT Significant
summary(lm(price ~ electric)) # NOT Significant
summary(lm(price ~ manual)) # Significant
summary(lm(price ~ awd)) # Significant

############################################################################# Updated regression (stepwise)
# We will make a regression model that includes all of our variables then do a stepwise regression (backward) to remove non significant variables.
# Notes: in car_cleaning files we have kept only non defective vehicles. there is no difference between data and data_no_def, we can delete models made with data_no_def that are a copy of models made with data

model1 <- lm(price ~ kilometers + kilometers.squared + vehicle.age + vehicle.age.squared + power + consumption + expertise + warranty + wagon + cabriolet + small.car + coupe + diesel + hybrid + natural.gas + electric + manual + awd)
summary(model1)
model2 <- stepAIC(model1, direction = "backward")
summary(model2)
# cabriolet and consumption are still not significant at an alpha of 0.01, we remove them
# we subset the data first to exclude observations that contain NA
data <- data[complete.cases(data[, c("kilometers", "kilometers.squared", "vehicle.age", "vehicle.age.squared", "power", "expertise", "warranty", "wagon", "diesel", "manual", "awd")]), ]
model3 <- lm(formula = price ~ kilometers + kilometers.squared + vehicle.age + vehicle.age.squared + power + expertise + warranty + wagon + diesel + manual + awd)
summary(model3)
# All of our variables are now significant. This will be our final model

data$price.residuals <- model3$residuals
data$pred.price <- model3$fitted.values

#####################################################################


# Let's add all the significant variables to a model and see if they are still significant.

model3 <- lm(price ~ expertise + warranty + wagon + cabriolet + diesel + hybrid + manual + awd)
summary(model3)
anova(model3)

# They are all significant. Let's add them to our original model.

model4 <- lm(price ~ vehicle.age + vehicle.age.squared + kilometers + kilometers.squared + power + expertise + warranty + wagon + cabriolet + diesel + hybrid + manual + awd)
summary(model4)

# We remove cabriolet as it is no longer significant.

model4 <- lm(price ~ vehicle.age + vehicle.age.squared + kilometers + kilometers.squared + power + expertise + warranty + wagon + diesel + hybrid + manual + awd)
summary(model4)

# All our variables are now significant. Our adjusted R-squared is 0.9015, which is the proportion of the variance of the price that is explained by our model. We consider this to be a good score. Let's check the residuals vs fitted plot and the standard deviation of the residuals.

ggplot(model4, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  geom_smooth()

sqrt(anova(model4)$"Mean Sq")[13]

# The plot seems very similar to model2 and the standard deviation has decreased a small amount, which is an improvement. 

anova(model4)
model4_data <- select(data, c("price", "vehicle.age", "vehicle.age.squared", "kilometers", "kilometers.squared", "power", "expertise", "warranty", "wagon", "diesel", "hybrid", "manual", "awd"))
cor_matrix_2 <- cor(model4_data, use = "complete.obs")
cor_matrix_2
corrplot(cor_matrix_2)

# Model with cars older than 9000 days removed (before March 1999)

data_no_vintage <- filter(data, vehicle.age < 9000)
ggplot(data_no_vintage, aes(vehicle.age, price)) + 
  geom_point() +
  geom_smooth(method = lm) +
  ylim(0, max(data$price))
model5 <- lm(data_no_vintage$price ~ data_no_vintage$vehicle.age + data_no_vintage$vehicle.age.squared + data_no_vintage$kilometers + data_no_vintage$kilometers.squared + data_no_vintage$power + data_no_vintage$expertise + data_no_vintage$warranty + data_no_vintage$wagon + data_no_vintage$diesel + data_no_vintage$manual + data_no_vintage$awd)

# We remove hybrid since it is no longer significant

summary(model5)
anova(model5)
sqrt(anova(model5)$"Mean Sq")[12] # Standard deviation of residuals has decreased significantly

# Model4 without defectives (model6)

data_no_def <- filter(data, !defective)
model6 <- lm(data_no_def$price ~ data_no_def$vehicle.age + data_no_def$vehicle.age.squared + data_no_def$kilometers + data_no_def$kilometers.squared + data_no_def$power + data_no_def$expertise + data_no_def$warranty + data_no_def$wagon + data_no_def$diesel + data_no_def$hybrid + data_no_def$manual + data_no_def$awd)
summary(model6)
anova(model6)
sqrt(anova(model6)$"Mean Sq")[13]

ggplot(model6, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  geom_smooth()

# We will test model6 for multicollinearity

model6_data <- select(data_no_def, c("price", "vehicle.age", "vehicle.age.squared", "kilometers", "kilometers.squared", "power", "expertise", "warranty", "wagon", "diesel", "hybrid", "manual", "awd"))
cor_matrix_3 <- cor(model6_data, use = "complete.obs")
cor_matrix_3
corrplot(cor_matrix_3)

# The highest absolute value of a correlation coefficient in the matrix (except for the diagonal and the correlations between kilometers and vehicle.age and their respective squared values) is price/kilometers at 0.755. We will check our VIF values.

vif(model6)
model7 <- lm(data_no_def$price ~ data_no_def$kilometers + data_no_def$kilometers.squared + data_no_def$power + data_no_def$expertise + data_no_def$warranty + data_no_def$wagon + data_no_def$diesel + data_no_def$hybrid + data_no_def$manual + data_no_def$awd)
vif(model7)
summary(model7)
model8 <- lm(data_no_def$price ~ data_no_def$vehicle.age + data_no_def$vehicle.age.squared + data_no_def$power + data_no_def$expertise + data_no_def$warranty + data_no_def$wagon + data_no_def$diesel + data_no_def$hybrid + data_no_def$manual + data_no_def$awd)
vif(model8)
summary(model8)

#Our VIF values are much smaller when we remove either kilometers or vehicle.age, but the VIFs for those variables remain at around 10.

# Model 6 formula
25510 + (-4.861)*2147 + (0.0003431)*(2147^2) + (-0.1207)*83100 + (0.0000002382)*(83100^2) + 64.82*150 + 1364*1 + 851.8*1 + (-594.3)*1 + 1036*1 + 982.2*0 + (-560.8)*0 + 2056*1

# Let's add a residuals column to the data, which will be the residuals calculated by model6. We remove all rows with missing values in data_no_def first.

data_no_def <- data_no_def[complete.cases(data_no_def[,c("price", "vehicle.age", "vehicle.age.squared", "kilometers", "kilometers.squared", "power", "expertise", "warranty", "wagon", "diesel", "hybrid", "manual", "awd")]),]
data_no_def$residuals <- model6$residuals

data_no_def <- data_no_def |>
  mutate(count = row_number()) |>
  select(count, everything())

#Save file
write.csv(data, "golf_models_data.csv")

#############NOTES:
# - check multicollinearity , make VIF