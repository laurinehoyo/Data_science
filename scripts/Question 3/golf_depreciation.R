# Research question 3: How do different car features affect depreciation over time?

library(tidyverse)
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
  ylim(c(0, max(new.price))) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(new.price), y = price), color = "blue", se = FALSE) +
  geom_ribbon(aes(x = seq(new.price),
                  ymin = predict(loess(price ~ seq(price), span = 0.1)),
                  ymax = predict(loess(new.price ~ seq(price), span = 0.1))),
              fill = "grey", alpha = 0.5) +
  labs(x = "Observations sorted by new price", y = "Price") +
  ggtitle("Comparison of listing prices and new prices")

# Let's make simple linear models to test if our variables can explain the depreciation

# price

summary(lm(depreciation ~ price))

ggplot(data) +
  geom_point(mapping = aes(depreciation, price)) +
  geom_smooth(mapping = aes(depreciation, price))

ggplot(lm(depreciation ~ price), aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
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

# new price

summary(lm(depreciation ~ new.price))

ggplot(data, mapping = aes(depreciation, new.price)) +
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

# fuel type

table(fuel.type)
summary(lm(depreciation ~ hybrid))

ggplot(data, mapping = aes(depreciation, hybrid)) +
  geom_point() 

# body.type

table(body.type)
summary(lm(depreciation ~ wagon))

ggplot(data, mapping = aes(depreciation, wagon)) +
  geom_point() 

# transmission

summary(lm(depreciation ~ manual))

ggplot(data, mapping = aes(depreciation, manual)) +
  geom_point() 

# drivetrain

summary(lm(depreciation ~ awd))

ggplot(data, mapping = aes(depreciation, awd)) +
  geom_point() 

###### NOTES. 
# do all variables together
#for some variables like wagon, maybe a lot of golfs used to be wagons before and now not so much, which is why the depreciation is greater for wagons (they are older and vehicle age is correlated to depreciation)