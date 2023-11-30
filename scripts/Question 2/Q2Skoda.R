### Research question 2: What are the factors that influence the duration of a listing for a used car? 

library(tidyverse)
library(corrplot)
library(Hmisc)
library(car)
setwd("~/GitHub/Data_science/cleaned-data")
data <- read.csv("skoda2_cleaned.csv")
skoda2_cleaned <- read.csv("skoda2_cleaned.csv")
attach(data)

# Firstly we will make a model with our numerical variables to try and explain listing.age, our dependent variable y
# We start by creating a correlation matrix with the numeric columns.

data_numeric <- select(data, c("listing.age", "price", "vehicle.age", "kilometers", "power", "consumption"))
data_numeric
cor_matrix <- cor(data_numeric, use = "complete.obs")
cor_matrix
corrplot(cor_matrix)

# We will make simple linear models for these 4 variables and look at their respective plots.
# We will set a significance level of α = 0.01.

summary(lm(listing.age ~ consumption))
ggplot(data, aes(consumption, listing.age)) + 
  geom_point() +
  geom_smooth(method = lm) +
  ylim(0, max(data$listing.age))

# Consumption is statistically significant at α = 0.01. Relationship not linear. We can check this with a residuals vs fitted plot, which should show us a horizontal line if the data is linear.

ggplot(lm(listing.age ~ consumption), aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  geom_smooth()

ggplot(lm(listing.age ~ consumption), aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  geom_smooth()

# We will try to use the LOESS method instead of a linear model

ggplot(data, aes(consumption, listing.age)) + 
  geom_point() +
  geom_smooth(method = loess) +
  ylim(0, max(data$listing.age))

# Let's make these models below:

data$consumption.squared <- (consumption ^ 2)
data$consumption.cubed <- (consumption ^ 3)
attach(data)
summary(lm(listing.age ~ consumption + consumption.cubed + consumption.squared))

# Let's check the residuals vs fitted plot

ggplot(lm(listing.age ~ consumption + consumption.cubed + consumption.squared), aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  geom_smooth()

ggplot(lm(price ~ consumption + consumption.squared), aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  geom_smooth()


# Vehicle.age is not stat significant

summary(lm(listing.age ~ vehicle.age))
ggplot(data, aes(vehicle.age, listing.age)) + 
  geom_point() +
  geom_smooth(method = lm) +
  ylim(0, max(data$listing.age))


# Kilometers is not stat significant 

summary(lm(listing.age ~ kilometers))
ggplot(data, aes(kilometers, listing.age)) + 
  geom_point() +
  geom_smooth(method = lm) +
  ylim(0, max(data$listing.age))

# Power is not stat significant.

summary(lm(listing.age ~ power))
ggplot(data, aes(power, listing.age)) + 
  geom_point() +
  geom_smooth(method = lm) +
  ylim(0, max(data$listing.age))


# Model 1 : regression with consumption

model1 <- lm(listing.age ~ consumption)
summary(model1)

# Let's check the residuals vs fitted plot

ggplot(model1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  geom_smooth()

# We will now remove defective vehicles and run the model again, this will hopefully stop the model from undervaluing cars.
# Listings flagged as defective account for under 5% of our observations, so removing them won't have a significant impact on our data.

table(data$defective)[2]/nrow(data)
data_no_def <- filter(data, !defective)
model2 <- lm(data_no_def$price ~ data_no_def$consumption + data_no_def$consumption.squared + data_no_def$consumption.cubed)

# Significance test

summary(model2)

# Residuals vs fitted plot

ggplot(model2, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  geom_smooth()

sqrt(anova(model1)$"Mean Sq")#[6] # Sd model1
sqrt(anova(model2)$"Mean Sq")#[6] # Sd model2

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
summary(lm(listing.age ~ expertise)) # Significant
summary(lm(listing.age ~ warranty)) # not Significant
summary(lm(listing.age ~ suv)) # NOT Significant
summary(lm(listing.age ~ sedan)) # Significant
summary(lm(listing.age ~ cabriolet)) # NOT Significant
summary(lm(listing.age ~ diesel)) # Significant
summary(lm(listing.age ~ hybrid)) # not Significant
summary(lm(listing.age ~ natural.gas)) # NOT Significant
summary(lm(listing.age ~ manual)) # not Significant
summary(lm(listing.age ~ awd)) # not Significant

# Let's add all the significant variables to a model and see if they are still significant.

model3 <- lm(listing.age ~ expertise + sedan + diesel)
summary(model3)
anova(model3)

# They are all significant. Let's add them to our original model.

model4 <- lm(listing.age ~ consumption + consumption.squared + consumption.cubed + expertise + sedan + diesel)
summary(model4)

# We remove consumption (^2 and ^3 too) and diesel, they are not significant.

model4 <- lm(listing.age ~ + expertise + sedan)
summary(model4)

# All our variables are now significant. Our adjusted R-squared is 0.004988, which is the proportion of the variance of the price that is explained by our model. We consider this to be a bad score. Let's check the residuals vs fitted plot and the standard deviation of the residuals.

ggplot(model4, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  geom_smooth()

sqrt(anova(model4)$"Mean Sq")#[11]

anova(model4)
model4_data <- select(data, c("listing.age", "expertise", "sedan"))
cor_matrix_2 <- cor(model4_data, use = "complete.obs")
cor_matrix_2
corrplot(cor_matrix_2)

# Model with cars more expensive than 40'000 CHF removed. We do this because the heteroskedasticity observed was mainly when fitted values > 40000. 

data_no_expensive <- filter(data, price < 40000)
nrow(data_no_expensive)/nrow(data)

model5 <- lm(data_no_expensive$listing.age ~ data_no_expensive$expertise + data_no_expensive$sedan)
summary(model5)

sqrt(anova(model5)$"Mean Sq")#[10]

ggplot(model5, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  geom_smooth()

# Model4 without defectives (model6)

data_no_def <- filter(data, !defective)
model6 <- lm(data_no_def$listing.age ~ data_no_def$expertise + data_no_def$sedan)
summary(model6)
anova(model6)
sqrt(anova(model6)$"Mean Sq")#[11]

ggplot(model6, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  geom_smooth()

# We will test model6 for multicollinearity

model6_data <- select(data_no_def, c("listing.age", "expertise", "sedan"))
cor_matrix_3 <- cor(model6_data, use = "complete.obs")
cor_matrix_3
corrplot(cor_matrix_3)

vif(model6)
model7 <- lm(data_no_def$listing.age ~ data_no_def$expertise + data_no_def$sedan)
vif(model7)

# Let's add a residuals column to the data, which will be the residuals calculated by model6. We remove all rows with missing values in data_no_def first.

data_no_def <- data_no_def[complete.cases(data_no_def[,c("listing.age", "expertise", "sedan")]),]
data_no_def$residuals <- model6$residuals

data_no_def <- data_no_def |>
  mutate(count = row_number()) |>
  select(count, everything())
