# Research question 3: How do different car features affect depreciation over time?

library(tidyverse)
library(corrplot)
setwd("~/GitHub/Data_science/cleaned-data")

# To answer this question, we will use the observations in our data where we have values for "new.price"

data <- read.csv("golf_models_data.csv")
attach(data)
data <- data |> filter(!is.na(new.price))
attach(data)
data$depreciation <- new.price - price
attach(data)

# Plot of new prices (red) and prices (blue). The difference between these lines is how much the car has depreciated.

ggplot(data[order(data$new.price),]) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(new.price), y = new.price), color = "red", se = FALSE) +
  ylim(0, max(new.price)) +
  xlim(0, nrow(data)) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(new.price), y = price), color = "blue", se = FALSE) +
  geom_ribbon(aes(x = seq(new.price),
                  ymin = predict(loess(price ~ seq(price), span = 0.1)),
                  ymax = predict(loess(new.price ~ seq(price), span = 0.1))),
              fill = "grey", alpha = 0.5) +
  labs(x = "Observations sorted by new price", y = "New price | Price") +
  ggtitle("Comparison of listing prices and new prices")

# Let's make simple linear models to test if our variables can explain the depreciation

# new price
summary(lm(depreciation ~ new.price))
ggplot(data, mapping = aes(depreciation, new.price)) +
  geom_point() +
  geom_smooth()

# price
summary(lm(depreciation ~ price))
ggplot(data, mapping = aes(depreciation, price)) +
  geom_point() +
  geom_smooth()

# vehicle age
summary(lm(depreciation ~ vehicle.age))
ggplot(data, mapping = aes(depreciation, vehicle.age)) +
  geom_point() +
  geom_smooth()

# kilometers
summary(lm(depreciation ~ kilometers))
ggplot(data, mapping = aes(depreciation, kilometers)) +
  geom_point() +
  geom_smooth()

# power
summary(lm(depreciation ~ power))
ggplot(data, mapping = aes(depreciation, power)) +
  geom_point() +
  geom_smooth()

# consumption
summary(lm(depreciation ~ consumption))
ggplot(data, mapping = aes(depreciation, consumption)) +
  geom_point() +
  geom_smooth()

# expertise
summary(lm(depreciation ~ expertise))
ggplot(data, mapping = aes(depreciation, expertise)) +
  geom_point()

# warranty
summary(lm(depreciation ~ warranty))
ggplot(data, mapping = aes(depreciation, warranty)) +
  geom_point()

# hybrid
table(fuel.type)
summary(lm(depreciation ~ hybrid))
ggplot(data, mapping = aes(depreciation, hybrid)) +
  geom_point() 

# diesel
table(fuel.type)
summary(lm(depreciation ~ diesel))
ggplot(data, mapping = aes(depreciation, diesel)) +
  geom_point() 

# We don't have enough data to make a regression with fuel type natural.gas (2 obs.) or electric (28 obs.)

# wagon
table(body.type)
summary(lm(depreciation ~ wagon))
ggplot(data, mapping = aes(depreciation, wagon)) +
  geom_point() 

# We don't have enough data to make a regression with body type cabriolet (5 obs.)

# transmission
summary(lm(depreciation ~ manual))
ggplot(data, mapping = aes(depreciation, manual)) +
  geom_point() 

# drivetrain
summary(lm(depreciation ~ awd))
ggplot(data, mapping = aes(depreciation, awd)) +
  geom_point() 

# We will test all variables together

model1 <- lm(depreciation ~ price + new.price + kilometers + vehicle.age + power + consumption + expertise + warranty + wagon + diesel + hybrid + manual + awd)
summary(model1)

# We test only variables that don't change over a vehicle's life.

model2 <- lm(depreciation ~ power + new.price + consumption + wagon + diesel + hybrid + manual + awd)
summary(model2)

# Our variable depreciation will be a lot greater when the new price of the vehicle is large, which could bias our tests. To resolve this, we will create a new variable rel_dep which is relative depreciation and re-run our regressions.

data$rel_dep <- 1 - price/new.price
attach(data)

# new price
summary(lm(rel_dep ~ new.price))
ggplot(data, mapping = aes(rel_dep, new.price)) +
  geom_point() +
  geom_smooth()

# price
summary(lm(rel_dep ~ price))
ggplot(data, mapping = aes(rel_dep, price)) +
  geom_point() +
  geom_smooth()

# vehicle age
summary(lm(rel_dep ~ vehicle.age))
ggplot(data, mapping = aes(rel_dep, vehicle.age)) +
  geom_point() +
  geom_smooth()

# kilometers
summary(lm(rel_dep ~ kilometers))
ggplot(data, mapping = aes(rel_dep, kilometers)) +
  geom_point() +
  geom_smooth()

# power
summary(lm(rel_dep ~ power))
ggplot(data, mapping = aes(rel_dep, power)) +
  geom_point() +
  geom_smooth()

# consumption
summary(lm(rel_dep ~ consumption))
ggplot(data, mapping = aes(rel_dep, consumption)) +
  geom_point() +
  geom_smooth()

# expertise
summary(lm(rel_dep ~ expertise))
ggplot(data, mapping = aes(rel_dep, expertise)) +
  geom_point()

# warranty
summary(lm(rel_dep ~ warranty))
ggplot(data, mapping = aes(rel_dep, warranty)) +
  geom_point()

# hybrid
table(fuel.type)
summary(lm(rel_dep ~ hybrid))
ggplot(data, mapping = aes(rel_dep, hybrid)) +
  geom_point() 

# diesel
table(fuel.type)
summary(lm(rel_dep ~ diesel))
ggplot(data, mapping = aes(rel_dep, diesel)) +
  geom_point() 

# We don't have enough data to make a regression with fuel type natural.gas (2 obs.) or electric (28 obs.)

# wagon
table(body.type)
summary(lm(rel_dep ~ wagon))
ggplot(data, mapping = aes(rel_dep, wagon)) +
  geom_point() 

# We don't have enough data to make a regression with body type cabriolet (5 obs.)

# transmission
summary(lm(rel_dep ~ manual))
ggplot(data, mapping = aes(rel_dep, manual)) +
  geom_point() 

# drivetrain
summary(lm(rel_dep ~ awd))
ggplot(data, mapping = aes(rel_dep, awd)) +
  geom_point()


# We make our models again, this time for rel_dep instead of depreciation

model3 <- lm(rel_dep ~ price + new.price + kilometers + vehicle.age + power + consumption + expertise + warranty + wagon + diesel + hybrid + manual + awd) # equivalent to model1
summary(model3)

model4 <- lm(depreciation ~ power + new.price + consumption + wagon + diesel + hybrid + manual + awd) # equivalent to model2
summary(model4)

# We will make a correlation matrix to check which variables are correlated.

cor_matrix <- cor(select(data, c("depreciation", "rel_dep", "new.price", "price", "kilometers", "vehicle.age", "power", "consumption", "expertise", "warranty", "wagon", "diesel", "hybrid", "manual", "awd")), use = "complete.obs")
cor_matrix
corrplot(cor_matrix)
select(data, c("new.price", "price", "kilometers", "vehicle.age", "power", "consumption", "expertise", "warranty", "wagon", "diesel", "hybrid", "manual", "awd"))

# Relative depreciation is clearly strongly negatively correlated with price, but surprisingly is weakly correlated with new price.
# Obviously relative depreciation is strongly dependent on vehicle age and kilometers, since these are the main factors that determine a vehicle's depreciation.

summary(lm(rel_dep ~ vehicle.age + kilometers))

# 78% of rel_dep's variance can be explained by just vehicle.age and kilometers (adjusted R squared).
# Let's investigate if any immovable factors have a significant effect on relative depreciation.

cor_matrix[2, order(-abs(cor_matrix[2,]))] # We order correlations with rel_dep by descending order (absolute values)

# hybrid is moderately negatively correlated rel_dep, so this could mean hybrid cars depreciate at a slower rate than non-hybrid cars, assuming that hybrid cars are independent from from vehicle.age and kilometers.
# We suspect that the average age and kilometers of hybrid cars is significantly less than the averages of the population since this fuel type has been developed recently, and hybrid cars are much more common nowadays than 10+ years ago.
# We will test this now with a Student's t-Test.

t.test(vehicle.age, vehicle.age[hybrid == TRUE]) # p-value < 2.2e-16
t.test(kilometers, kilometers[hybrid == TRUE]) # p-value < 2.2e-16

# Clearly, the difference in means for vehicle age and kilometers between hybrid cars and the population is not negligible.

# We make some plots with rel_dep

# New prices vs depreciation
ggplot(data[order(data$new.price),]) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(new.price), y = new.price), color = "red", se = FALSE) +
  ylim(0, 100000) +
  xlim(0, nrow(data)) +
  geom_point(mapping = aes(x = seq(new.price), y = rel_dep*100000), size = 1, alpha = 0.5, stroke = 0) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(new.price), y = rel_dep*100000), color = "blue", se = FALSE) +
  labs(x = "Observations sorted by new price", y = "New price | Relative depreciation * 100'000") +
  ggtitle("Comparison of new prices (red) and relative depreciation (blue)")

# Prices vs depreciation
ggplot(data[order(data$price),]) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(price), y = price), color = "red", se = FALSE) +
  ylim(0, 100000) +
  xlim(0, nrow(data)) +
  geom_point(mapping = aes(x = seq(price), y = rel_dep*100000), size = 1, alpha = 0.5, stroke = 0) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(price), y = rel_dep*100000), color = "blue", se = FALSE) +
  labs(x = "Observations sorted by price", y = "Price | Relative depreciation * 100'000") +
  ggtitle("Comparison of prices (red) and relative depreciation (blue)")

summary(rel_dep)
sd(rel_dep)

# By looking at the plot above, we can confirm that 

###### NOTES. 
# do all variables together
#for some variables like wagon, maybe a lot of golfs used to be wagons before and now not so much, which is why the depreciation is greater for wagons (they are older and vehicle age is correlated to depreciation) (hybrid, diesel, etc -> plot diesel vs vehicle age and see if evenly distributed, do a test to see if there is a difference)
# should we only check for depreciation with factors that don't change during a vehicle's life (power, fuel.type, body.type, transmission, ) 
# maybe we should control for variables that change over a car's lifespan and that directly affect how much it has depreciated (kilometers, vehicle.age, expertise, warranty, defective) (or remove defective)
# we should model relative depreciation instead of depreciation

# how can we control for vehicle age and kilometers ? can we take data from the population where age and kilometers are the same as the average hybrid?

# control variable : hybrid*vehicle.age
# prioriser multiple regression > simple regression, faire tests vif