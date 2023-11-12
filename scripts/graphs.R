### Graphs

library(tidyverse)
setwd("~/GitHub/Data_science/cleaned-data")

data <- read.csv("golf2_cleaned.csv")
attach(data)

### vehicle.age - price plots

ggplot(data = data) +
  geom_point(mapping = aes(x = vehicle.age, y = price, color = warranty))

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
