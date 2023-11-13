### Graphs

library(tidyverse)
setwd("~/GitHub/Data_science/cleaned-data")

data <- read.csv("golf2_cleaned.csv")
attach(data)

### vehicle.age - price plots

ggplot(data = data) +
  geom_point(mapping = aes(x = vehicle.age, y = price, color = warranty)) +
  geom_smooth(method = loess, mapping = aes(vehicle.age, price), color = "red")

# In the graph above, thanks to the conditional means smoothing curve, we can see that the average price goes up after a vehicle age of approx. 9000 days (= 24 years, 7 months, 21 days) which is counter intuitive.
# One hypothesis would be that these cars could be considered as vintage or collector cars, and therefore won't appeal to the average buyer, who is a daily driver. These types of cars have usually been very well maintained and have low mileage, which could also explain this overvaluation.
# We also notice that the curve is steeper and therefore cars seem to depreciate faster in their first 2500 days (6 years, 10 months, 2 days) than in their next 7500 (20 years, 6 months, 12 days).
# Cars that are under 4000 days old are more likely to come with a warranty than those older than 4000 days (colors).

ggplot(data = data) +
  geom_point(mapping = aes(x = vehicle.age, y = price, size = power, color = consumption), alpha = 0.5)

### kilometers - price plots

ggplot(data = data) +
  geom_point(mapping = aes(x = kilometers, y = price)) +
  geom_smooth(mapping = aes(kilometers, price))

### kilometers - vehicle age plots

ggplot(data = data) +
  geom_point(mapping = aes(kilometers, vehicle.age)) +
  geom_rug(mapping = aes(x = kilometers, y = vehicle.age))

ggplot(data = data) +
  geom_point(mapping = aes(kilometers, vehicle.age)) +
  geom_quantile(mapping = aes(x = kilometers, y = vehicle.age)) +
  geom_smooth(method = lm, mapping = aes(kilometers, vehicle.age), color = "green")
