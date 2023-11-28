### Research question 2: What are the factors that influence the duration of a listing for a used car? 

library(tidyverse)
library(corrplot)
library(Hmisc)
library(car)
setwd("~/GitHub/Data_science/cleaned-data")
data <- read.csv("audi2_cleaned.csv")
audi2_cleaned <- read.csv("audi2_cleaned.csv")
attach(data)

# We will start our analysis with the Audi A3 data set.
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


# Vehicle.age is not statistically significant. Remove from model.

summary(lm(listing.age ~ vehicle.age))
ggplot(data, aes(vehicle.age, listing.age)) + 
  geom_point() +
  geom_smooth(method = lm) +
  ylim(0, max(data$listing.age))

# None of these graphs make sense??


# Kilometers is not statistically significant. Remove from model.

summary(lm(listing.age ~ kilometers))
ggplot(data, aes(kilometers, listing.age)) + 
  geom_point() +
  geom_smooth(method = lm) +
  ylim(0, max(data$listing.age))


# Power is not statistically significant. Remove from model 

summary(lm(listing.age ~ power))
ggplot(data, aes(power, listing.age)) + 
  geom_point() +
  geom_smooth(method = lm) +
  ylim(0, max(data$listing.age))

# Model 1 is the regression between listing.age and consumption

model1 <- lm(listing.age ~ consumption)
summary(model1)

# Let's check the residuals vs fitted plot

ggplot(model1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  geom_smooth()

# We will now remove defective vehicles and run the model again, this will hopefully stop the model from undervaluing cars.
# Listings flagged as defective account for under 5% of our observations, so removing them won't have a significant impact on our data.

table(data$defective)[2]/table(data$defective)[1]
data_no_def <- filter(data, !defective)
model2 <- lm(data_no_def$price ~ data_no_def$consumption + data_no_def$consumption.squared + data_no_def$consumption.cubed)

# Significance test

summary(model2)

# Residuals vs fitted plot

ggplot(model2, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  geom_smooth()

# The adjusted R^2 has changed between M1 and M2 (from 0.01102 to 0.05217 -> 0.05 difference, not significant)

sqrt(anova(model1)$"Mean Sq") # Sd model1
sqrt(anova(model2)$"Mean Sq") # Sd model2

### We will now run simple linear regressions for our logical variables. We first need to transform some of our columns into logical values.

# For body.type, we can see that the majority of cars are either "Limousine", "Break" or "Cabriolet".
# We will then create a logical column "wagon" which will be set to TRUE where body.type == "Break" is TRUE (Note: "break" is a reserved word). We do the same for columns "cabriolet", "small.car" and "coupe". When all 4 are FALSE, the body.type will be Limousine.

table(body.type)
data_logical <- data |>
  filter(body.type != "Petite voiture") |>
  filter(body.type != "Coupé")

data$wagon <- body.type == "Break"
data$cabriolet <- body.type == "Cabriolet"
data$small.car <- body.type == "Petite voiture"
data$coupe <- body.type == "Coupé"

data_logical <- data |>
  filter(body.type != "Petite voiture") |>
  filter(body.type != "Coupé")

table(body.type)

data$wagon <- body.type == "Break"
data$cabriolet <- body.type == "Cabriolet"
data$small.car <- body.type == "Petite voiture"
data$coupe <- body.type == "Coupé"

# We will repeat the process for fuel.type. Columns will be "diesel", "hybrid", "natural.gas" and "electric"

data$diesel <- fuel.type == "Diesel"
data$hybrid <- ifelse(fuel.type == "Hybride léger essence/électrique" | 
                        fuel.type == "Hybride rechargeable essence/électrique" |
                        fuel.type == "Hybride léger diesel/électrique", 
                      TRUE, FALSE)
data$natural.gas <- fuel.type == "Gaz naturel (CNG) / Essence"
data$electric <- fuel.type == "Électrique"

# We repeat the process for transmission. We will only have one variable "manual". We consider "Boîte manuelle automatisée" to be automatic because it is very similar to an automatic transmission.

data$manual <- ifelse(transmission == "Automatique" |
                        transmission == "Boîte manuelle automatisée" |
                        transmission == "Boîte automatique variable", 
                      FALSE, TRUE)

# We repeat the process for drivetrain. We will create one column "awd". 

data$awd <- drivetrain == "4 roues motrices"

# Now we run the simple linear regressions for expertise, warranty, wagon, cabriolet, diesel, hybrid, natural.gas, electric, manual and awd. We note which ones are statistically significant at α = 0.01.

attach(data)
summary(lm(listing.age ~ expertise)) # not significant
summary(lm(listing.age ~ warranty)) # Significant
summary(lm(listing.age ~ wagon)) # not significant
summary(lm(listing.age ~ cabriolet)) # Significant
summary(lm(listing.age ~ small.car)) # NOT Significant
summary(lm(listing.age ~ coupe)) # NOT Significant
summary(lm(listing.age ~ diesel)) # Significant
summary(lm(listing.age ~ hybrid)) # not significant
summary(lm(listing.age ~ natural.gas)) # NOT Significant
summary(lm(listing.age ~ electric)) # NA 
summary(lm(listing.age ~ manual)) # not significant
summary(lm(listing.age ~ awd)) # Significant

# Let's add all the significant variables to a model and see if they are still significant.

model3 <- lm(listing.age ~ warranty + cabriolet + diesel + awd)
summary(model3)
anova(model3)

# They are all significant. Let's add them to our original model.

model4 <- lm(listing.age ~ consumption + consumption.squared + consumption.cubed + warranty + cabriolet + diesel + awd)
summary(model4)

# We remove consumption.cubed as it is no longer significant.

model4 <- lm(listing.age ~ consumption + consumption.squared + warranty + cabriolet + diesel + awd)
summary(model4)

# All our variables are now significant. Our adjusted R-squared is 0.02853, which is the proportion of the variance of the duration of the listing that is explained by our model. We consider this to be a bad score. Let's check the residuals vs fitted plot and the standard deviation of the residuals.

ggplot(model4, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  geom_smooth()

sqrt(anova(model4)$"Mean Sq") [13] # NA si je laisse [13]


anova(model4)
model4_data <- select(data, c("listing.age", "consumption", "consumption.squared", "warranty", "cabriolet", "diesel", "awd"))
cor_matrix_2 <- cor(model4_data, use = "complete.obs")
cor_matrix_2
corrplot(cor_matrix_2)

# Model with cars older than 9000 days removed (before March 1999)

#data_no_vintage <- filter(data, vehicle.age < 9000)
#ggplot(data_no_vintage, aes(vehicle.age, listing.age)) + 
 # geom_point() +
  #geom_smooth(method = lm) +
  #ylim(0, max(data$listing.age))
#model5 <- lm(data_no_vintage$price ~ data_no_vintage$vehicle.age + data_no_vintage$vehicle.age.squared + data_no_vintage$kilometers + data_no_vintage$kilometers.squared + data_no_vintage$power + data_no_vintage$expertise + data_no_vintage$warranty + data_no_vintage$wagon + data_no_vintage$diesel + data_no_vintage$manual + data_no_vintage$awd)


# Model4 without defectives (model6)

data_no_def <- filter(data, !defective)
model6 <- lm(data_no_def$listing.age ~ data_no_def$consumption + data_no_def$consumption.squared  + data_no_def$warranty + data_no_def$cabriolet + data_no_def$diesel +  data_no_def$awd)
summary(model6)
anova(model6)
sqrt(anova(model6)$"Mean Sq")[13]

ggplot(model6, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  geom_smooth()

# Après ça j'ai arrêté
# We will test model6 for multicollinearity

model6_data <- select(data_no_def, c("listing.age", "consumption", "consumption.squared", "warranty", "cabriolet", "diesel", "awd"))
cor_matrix_3 <- cor(model6_data, use = "complete.obs")
cor_matrix_3
corrplot(cor_matrix_3)

vif(model6)

# Model 6 formula
# 25510 + (-4.861)*2147 + (0.0003431)*(2147^2) + (-0.1207)*83100 + (0.0000002382)*(83100^2) + 64.82*150 + 1364*1 + 851.8*1 + (-594.3)*1 + 1036*1 + 982.2*0 + (-560.8)*0 + 2056*1

# Let's add a residuals column to the data, which will be the residuals calculated by model6. We remove all rows with missing values in data_no_def first.

data_no_def <- data_no_def[complete.cases(data_no_def[,c("listing.age", "consumption", "consumption.squared", "warranty", "cabriolet", "diesel", "awd")]),]
data_no_def$residuals <- model6$residuals

data_no_def <- data_no_def |>
  mutate(count = row_number()) |>
  select(count, everything())
