# EDA Graphs & Matrices

# Question 3: How do different vehicle features affect depreciation ?
library(patchwork)
library(ggtext)
library(plotly)
library(tidyverse)
library(corrplot)
setwd("~/GitHub/Data_science/cleaned-data")

golf <- read.csv("golf_models_data.csv")
audi <- read.csv("audi_models_data.csv")
skoda <- read.csv("skoda_models_data.csv")
volvo <- read.csv("volvo_models_data.csv")
toyota <- read.csv("toyota_models_data.csv")

# Depreciation variables
# We filter to remove NAs from new price for this part

golf <- golf |> filter(!is.na(new.price))

golf$depreciation = golf$new.price - golf$price
golf$rel_dep = golf$depreciation / golf$new.price

# Depreciation plots

# New prices vs listing prices

a1 <- ggplot(golf[order(golf$new.price),]) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(new.price), y = new.price), color = "royalblue") +
  ylim(0, max(golf$new.price)) +
  xlim(0, nrow(golf)) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(new.price), y = price), color = "orangered", se = FALSE) +
  geom_ribbon(aes(x = seq(new.price),
                  ymin = predict(loess(price ~ seq(price), span = 0.1)),
                  ymax = predict(loess(new.price ~ seq(price), span = 0.1))),
              fill = "grey", alpha = 0.5) +
  labs(x = "Observations sorted by new price", y = "Price", title = "Comparison of listing prices and new prices", subtitle = "<span style='color:royalblue;'>&#9899; New prices</span><br/><span style='color:orangered;'>&#9899; Actual prices</span>") +
  ggtitle("Comparison of listing prices and new prices") +
  theme_light() +
  theme(plot.subtitle = element_markdown())

# New prices vs depreciation

b1 <- ggplot(golf[order(golf$new.price),]) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(new.price), y = new.price), color = "royalblue", se = FALSE) +
  ylim(0, 100000) +
  xlim(0, nrow(golf)) +
  geom_point(mapping = aes(x = seq(new.price), y = rel_dep*100000), size = 1, alpha = 0.5, stroke = 0) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(new.price), y = rel_dep*100000), color = "orangered", se = FALSE) +
  labs(x = "Observations sorted by new price", y = "Price") +
  scale_y_continuous(sec.axis = sec_axis(~./100000, name = "Relative depreciation")) +
  theme_light() +
  theme(plot.subtitle = element_markdown())

# Prices vs depreciation

c1 <- ggplot(golf[order(golf$price),]) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(price), y = price), color = "royalblue", se = FALSE) +
  ylim(0, 100000) +
  xlim(0, nrow(golf)) +
  geom_point(mapping = aes(x = seq(price), y = rel_dep*100000), size = 1, alpha = 0.5, stroke = 0) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(price), y = rel_dep*100000), color = "orangered", se = FALSE) +
  labs(x = "Observations sorted by price", y = "Price", title = "Comparison of prices and relative depreciation", subtitle = "<span style='color:royalblue;'>&#9899; Prices</span><br/><span style='color:orangered;'>&#9899; Relative depreciation</span>") +
  scale_y_continuous(sec.axis = sec_axis(~./100000, name = "Relative depreciation")) +
  theme_light() +
  theme(plot.subtitle = element_markdown())

# Vehicle age vs depreciation

d1 <- ggplot(golf[order(golf$vehicle.age),]) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(vehicle.age), y = vehicle.age/365), color = "royalblue", se = FALSE) +
  ylim(0, max(golf$vehicle.age)/365) +
  xlim(0, nrow(golf)) +
  geom_point(mapping = aes(x = seq(vehicle.age), y = rel_dep*(max(golf$vehicle.age)/365)), size = 1, alpha = 0.5, stroke = 0) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(price), y = rel_dep*(max(golf$vehicle.age)/365)), color = "orangered", se = FALSE) +
  labs(x = "Observations sorted by vehicle age", y = "Vehicle age (years)", subtitle = "VW Golf") +
  scale_y_continuous(sec.axis = sec_axis(~./(max(golf$vehicle.age)/365), name = "Relative depreciation")) +
  theme_light() +
  theme(plot.subtitle = element_markdown())

# Kilometers vs depreciation

e1 <- ggplot(golf[order(golf$kilometers),]) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(kilometers), y = kilometers), color = "royalblue", se = FALSE) +
  ylim(0, max(golf$kilometers)) +
  xlim(0, nrow(golf)) +
  geom_point(mapping = aes(x = seq(kilometers), y = rel_dep*max(golf$kilometers)), size = 1, alpha = 0.5, stroke = 0) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(price), y = rel_dep*max(golf$kilometers)), color = "orangered", se = FALSE) +
  labs(x = "Observations sorted by kilometers", y = "Kilometers", title = "Comparison of kilometers and relative depreciation", subtitle = "<span style='color:royalblue;'>&#9899; Kilometers</span><br/><span style='color:orangered;'>&#9899; Relative depreciation</span>") +
  scale_y_continuous(sec.axis = sec_axis(~./max(golf$kilometers), name = "Relative depreciation")) +
  theme_light() +
  theme(plot.subtitle = element_markdown())

# Correlation matrices

cor_matrix_golf <- cor(select(golf, c("depreciation", "rel_dep", "new.price", "price", "kilometers", "vehicle.age", "power", "consumption", "expertise", "warranty", "wagon", "diesel", "hybrid", "manual", "awd")), use = "complete.obs")
cor_matrix_golf
corrplot(cor_matrix_golf)

# Test these plots for the other makes and models

# audi
audi <- audi |> filter(!is.na(new.price))
audi$depreciation = audi$new.price - audi$price
audi$rel_dep = audi$depreciation / audi$new.price

# skoda
skoda <- skoda |> filter(!is.na(new.price))
skoda$depreciation = skoda$new.price - skoda$price
skoda$rel_dep = skoda$depreciation / skoda$new.price

#remove NAs from skoda
skoda <- skoda[!is.na(skoda$vehicle.age),]

# volvo
volvo <- volvo |> filter(!is.na(new.price))
volvo$depreciation = volvo$new.price - volvo$price
volvo$rel_dep = volvo$depreciation / volvo$new.price

# toyota
toyota <- toyota |> filter(!is.na(new.price))
toyota$depreciation = toyota$new.price - toyota$price
toyota$rel_dep = toyota$depreciation / toyota$new.price

# New prices vs listing prices ----
# New prices vs listing prices audi

a2 <- ggplot(audi[order(audi$new.price),]) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(new.price), y = new.price), color = "royalblue") +
  ylim(0, max(audi$new.price)) +
  xlim(0, nrow(audi)) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(new.price), y = price), color = "orangered", se = FALSE) +
  geom_ribbon(aes(x = seq(new.price),
                  ymin = predict(loess(price ~ seq(price), span = 0.1)),
                  ymax = predict(loess(new.price ~ seq(price), span = 0.1))),
              fill = "grey", alpha = 0.5) +
  labs(x = "Observations sorted by new price", y = "Price", title = "Comparison of listing prices and new prices", subtitle = "<span style='color:royalblue;'>&#9899; New prices</span><br/><span style='color:orangered;'>&#9899; Actual prices</span>") +
  ggtitle("Comparison of listing prices and new prices") +
  theme_light() +
  theme(plot.subtitle = element_markdown())

# New prices vs listing prices skoda

a3 <- ggplot(skoda[order(skoda$new.price),]) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(new.price), y = new.price), color = "royalblue") +
  ylim(0, max(skoda$new.price)) +
  xlim(0, nrow(skoda)) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(new.price), y = price), color = "orangered", se = FALSE) +
  geom_ribbon(aes(x = seq(new.price),
                  ymin = predict(loess(price ~ seq(price), span = 0.1)),
                  ymax = predict(loess(new.price ~ seq(price), span = 0.1))),
              fill = "grey", alpha = 0.5) +
  labs(x = "Observations sorted by new price", y = "Price", title = "Comparison of listing prices and new prices", subtitle = "<span style='color:royalblue;'>&#9899; New prices</span><br/><span style='color:orangered;'>&#9899; Actual prices</span>") +
  ggtitle("Comparison of listing prices and new prices") +
  theme_light() +
  theme(plot.subtitle = element_markdown())

# New prices vs listing prices volvo

a4 <- ggplot(volvo[order(volvo$new.price),]) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(new.price), y = new.price), color = "royalblue") +
  ylim(0, max(volvo$new.price)) +
  xlim(0, nrow(volvo)) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(new.price), y = price), color = "orangered", se = FALSE) +
  geom_ribbon(aes(x = seq(new.price),
                  ymin = predict(loess(price ~ seq(price), span = 0.1)),
                  ymax = predict(loess(new.price ~ seq(price), span = 0.1))),
              fill = "grey", alpha = 0.5) +
  labs(x = "Observations sorted by new price", y = "Price", title = "Comparison of listing prices and new prices", subtitle = "<span style='color:royalblue;'>&#9899; New prices</span><br/><span style='color:orangered;'>&#9899; Actual prices</span>") +
  ggtitle("Comparison of listing prices and new prices") +
  theme_light() +
  theme(plot.subtitle = element_markdown())

# New prices vs listing prices toyota

a5 <- ggplot(toyota[order(toyota$new.price),]) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(new.price), y = new.price), color = "royalblue") +
  ylim(0, max(toyota$new.price)) +
  xlim(0, nrow(toyota)) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(new.price), y = price), color = "orangered", se = FALSE) +
  geom_ribbon(aes(x = seq(new.price),
                  ymin = predict(loess(price ~ seq(price), span = 0.1)),
                  ymax = predict(loess(new.price ~ seq(price), span = 0.1))),
              fill = "grey", alpha = 0.5) +
  labs(x = "Observations sorted by new price", y = "Price", title = "Comparison of listing prices and new prices", subtitle = "<span style='color:royalblue;'>&#9899; New prices</span><br/><span style='color:orangered;'>&#9899; Actual prices</span>") +
  ggtitle("Comparison of listing prices and new prices") +
  theme_light() +
  theme(plot.subtitle = element_markdown())

# Patchwork

(a1 | a2) /
  (a3 | a4)

# New prices vs depreciation ----
# New prices vs depreciation audi

b2 <- ggplot(audi[order(audi$new.price),]) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(new.price), y = new.price), color = "royalblue", se = FALSE) +
  ylim(0, 100000) +
  xlim(0, nrow(audi)) +
  geom_point(mapping = aes(x = seq(new.price), y = rel_dep*100000), size = 1, alpha = 0.5, stroke = 0) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(new.price), y = rel_dep*100000), color = "orangered", se = FALSE) +
  labs(x = "Observations sorted by new price", y = "Price", subtitle = "<span style='color:royalblue;'>&#9899; New prices</span><br/><span style='color:orangered;'>&#9899; Relative depreciation</span>") +
  scale_y_continuous(sec.axis = sec_axis(~./100000, name = "Relative depreciation")) +
  theme_light() +
  theme(plot.subtitle = element_markdown())

# New prices vs depreciation skoda

b3 <- ggplot(skoda[order(skoda$new.price),]) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(new.price), y = new.price), color = "royalblue", se = FALSE) +
  ylim(0, 100000) +
  xlim(0, nrow(skoda)) +
  geom_point(mapping = aes(x = seq(new.price), y = rel_dep*100000), size = 1, alpha = 0.5, stroke = 0) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(new.price), y = rel_dep*100000), color = "orangered", se = FALSE) +
  labs(x = "Observations sorted by new price", y = "Price") +
  scale_y_continuous(sec.axis = sec_axis(~./100000, name = "Relative depreciation")) +
  theme_light() +
  theme(plot.subtitle = element_markdown())

# New prices vs depreciation volvo

b4 <- ggplot(volvo[order(volvo$new.price),]) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(new.price), y = new.price), color = "royalblue", se = FALSE) +
  ylim(0, 100000) +
  xlim(0, nrow(volvo)) +
  geom_point(mapping = aes(x = seq(new.price), y = rel_dep*100000), size = 1, alpha = 0.5, stroke = 0) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(new.price), y = rel_dep*100000), color = "orangered", se = FALSE) +
  labs(x = "Observations sorted by new price", y = "Price") +
  scale_y_continuous(sec.axis = sec_axis(~./100000, name = "Relative depreciation")) +
  theme_light() +
  theme(plot.subtitle = element_markdown())


# New prices vs depreciation toyota

b5 <- ggplot(toyota[order(toyota$new.price),]) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(new.price), y = new.price), color = "royalblue", se = FALSE) +
  ylim(0, 100000) +
  xlim(0, nrow(toyota)) +
  geom_point(mapping = aes(x = seq(new.price), y = rel_dep*100000), size = 1, alpha = 0.5, stroke = 0) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(new.price), y = rel_dep*100000), color = "orangered", se = FALSE) +
  labs(x = "Observations sorted by new price", y = "Price", title = "Comparison of new prices and relative depreciation", subtitle = "<span style='color:royalblue;'>&#9899; New prices</span><br/><span style='color:orangered;'>&#9899; Relative depreciation</span>") +
  scale_y_continuous(sec.axis = sec_axis(~./100000, name = "Relative depreciation")) +
  theme_light() +
  theme(plot.subtitle = element_markdown())

# Patchwork

(b1 | b2) /
  (b3 | b4) +
  plot_annotation(
    title = "Comparison of new prices and relative depreciation"
  )

# Prices vs depreciation ----
# Prices vs depreciation audi

ggplot(audi[order(audi$price),]) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(price), y = price), color = "royalblue", se = FALSE) +
  ylim(0, 100000) +
  xlim(0, nrow(audi)) +
  geom_point(mapping = aes(x = seq(price), y = rel_dep*100000), size = 1, alpha = 0.5, stroke = 0) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(price), y = rel_dep*100000), color = "orangered", se = FALSE) +
  labs(x = "Observations sorted by price", y = "Price", title = "Comparison of prices and relative depreciation", subtitle = "<span style='color:royalblue;'>&#9899; Prices</span><br/><span style='color:orangered;'>&#9899; Relative depreciation</span>") +
  scale_y_continuous(sec.axis = sec_axis(~./100000, name = "Relative depreciation")) +
  theme_light() +
  theme(plot.subtitle = element_markdown())

# Prices vs depreciation skoda

ggplot(skoda[order(skoda$price),]) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(price), y = price), color = "royalblue", se = FALSE) +
  ylim(0, 100000) +
  xlim(0, nrow(skoda)) +
  geom_point(mapping = aes(x = seq(price), y = rel_dep*100000), size = 1, alpha = 0.5, stroke = 0) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(price), y = rel_dep*100000), color = "orangered", se = FALSE) +
  labs(x = "Observations sorted by price", y = "Price", title = "Comparison of prices and relative depreciation", subtitle = "<span style='color:royalblue;'>&#9899; Prices</span><br/><span style='color:orangered;'>&#9899; Relative depreciation</span>") +
  scale_y_continuous(sec.axis = sec_axis(~./100000, name = "Relative depreciation")) +
  theme_light() +
  theme(plot.subtitle = element_markdown())

# Prices vs depreciation volvo

ggplot(volvo[order(volvo$price),]) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(price), y = price), color = "royalblue", se = FALSE) +
  ylim(0, 100000) +
  xlim(0, nrow(volvo)) +
  geom_point(mapping = aes(x = seq(price), y = rel_dep*100000), size = 1, alpha = 0.5, stroke = 0) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(price), y = rel_dep*100000), color = "orangered", se = FALSE) +
  labs(x = "Observations sorted by price", y = "Price", title = "Comparison of prices and relative depreciation", subtitle = "<span style='color:royalblue;'>&#9899; Prices</span><br/><span style='color:orangered;'>&#9899; Relative depreciation</span>") +
  scale_y_continuous(sec.axis = sec_axis(~./100000, name = "Relative depreciation")) +
  theme_light() +
  theme(plot.subtitle = element_markdown())

# Prices vs depreciation toyota

ggplot(toyota[order(toyota$price),]) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(price), y = price), color = "royalblue", se = FALSE) +
  ylim(0, 100000) +
  xlim(0, nrow(toyota)) +
  geom_point(mapping = aes(x = seq(price), y = rel_dep*100000), size = 1, alpha = 0.5, stroke = 0) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(price), y = rel_dep*100000), color = "orangered", se = FALSE) +
  labs(x = "Observations sorted by price", y = "Price", title = "Comparison of prices and relative depreciation", subtitle = "<span style='color:royalblue;'>&#9899; Prices</span><br/><span style='color:orangered;'>&#9899; Relative depreciation</span>") +
  scale_y_continuous(sec.axis = sec_axis(~./100000, name = "Relative depreciation")) +
  theme_light() +
  theme(plot.subtitle = element_markdown())


# Vehicle age vs depreciation ----
# Vehicle age vs depreciation audi

d2 <- ggplot(audi[order(audi$vehicle.age),]) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(vehicle.age), y = vehicle.age/365), color = "royalblue", se = FALSE) +
  ylim(0, max(audi$vehicle.age)/365) +
  xlim(0, nrow(audi)) +
  geom_point(mapping = aes(x = seq(vehicle.age), y = rel_dep*(max(audi$vehicle.age)/365)), size = 1, alpha = 0.5, stroke = 0) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(price), y = rel_dep*(max(audi$vehicle.age)/365)), color = "orangered", se = FALSE) +
  labs(x = "Observations sorted by vehicle age", y = "Vehicle age (years)", subtitle = "Audi A3", title = "<span style='color:royalblue;'>&#9899; Vehicle age (years)</span><br/><span style='color:orangered;'>&#9899; Relative depreciation</span>") +
  scale_y_continuous(sec.axis = sec_axis(~./(max(audi$vehicle.age)/365), name = "Relative depreciation")) +
  theme_light() +
  theme(plot.title = element_markdown())

# Vehicle age vs depreciation skoda

d3 <- ggplot(skoda[order(skoda$vehicle.age),]) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(vehicle.age), y = vehicle.age/365), color = "royalblue", se = FALSE) +
  ylim(0, max(skoda$vehicle.age)/365) +
  xlim(0, nrow(skoda)) +
  geom_point(mapping = aes(x = seq(vehicle.age), y = rel_dep*(max(skoda$vehicle.age)/365)), size = 1, alpha = 0.5, stroke = 0) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(price), y = rel_dep*(max(skoda$vehicle.age)/365)), color = "orangered", se = FALSE) +
  labs(x = "Observations sorted by vehicle age", y = "Vehicle age (years)", subtitle = "Skoda Octavia") +
  scale_y_continuous(sec.axis = sec_axis(~./(max(skoda$vehicle.age)/365), name = "Relative depreciation")) +
  theme_light() +
  theme(plot.subtitle = element_markdown())

# Vehicle age vs depreciation volvo

d4 <- ggplot(volvo[order(volvo$vehicle.age),]) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(vehicle.age), y = vehicle.age/365), color = "royalblue", se = FALSE) +
  ylim(0, max(volvo$vehicle.age)/365) +
  xlim(0, nrow(volvo)) +
  geom_point(mapping = aes(x = seq(vehicle.age), y = rel_dep*(max(volvo$vehicle.age)/365)), size = 1, alpha = 0.5, stroke = 0) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(price), y = rel_dep*(max(volvo$vehicle.age)/365)), color = "orangered", se = FALSE) +
  labs(x = "Observations sorted by vehicle age", y = "Vehicle age (years)", subtitle = "Volvo XC60") +
  scale_y_continuous(sec.axis = sec_axis(~./(max(volvo$vehicle.age)/365), name = "Relative depreciation")) +
  theme_light() +
  theme(plot.subtitle = element_markdown())

# Vehicle age vs depreciation toyota

d5 <- ggplot(toyota[order(toyota$vehicle.age),]) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(vehicle.age), y = vehicle.age/365), color = "royalblue", se = FALSE) +
  ylim(0, max(toyota$vehicle.age)/365) +
  xlim(0, nrow(toyota)) +
  geom_point(mapping = aes(x = seq(vehicle.age), y = rel_dep*(max(toyota$vehicle.age)/365)), size = 1, alpha = 0.5, stroke = 0) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(price), y = rel_dep*(max(toyota$vehicle.age)/365)), color = "orangered", se = FALSE) +
  labs(x = "Observations sorted by vehicle age", y = "Vehicle age (years)", title = "Comparison of vehicle age and relative depreciation", subtitle = "<span style='color:royalblue;'>&#9899; Vehicle age (years)</span><br/><span style='color:orangered;'>&#9899; Relative depreciation</span>") +
  scale_y_continuous(sec.axis = sec_axis(~./(max(toyota$vehicle.age)/365), name = "Relative depreciation")) +
  theme_light() +
  theme(plot.subtitle = element_markdown())

# Patchwork

(d1 | d2) /
  (d3 | d4) +
  plot_annotation(
    title = "Comparison of new prices and relative depreciation"
  )

# Kilometers vs depreciation ----
# Kilometers vs depreciation audi

ggplot(audi[order(audi$kilometers),]) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(kilometers), y = kilometers), color = "royalblue", se = FALSE) +
  ylim(0, max(audi$kilometers)) +
  xlim(0, nrow(audi)) +
  geom_point(mapping = aes(x = seq(kilometers), y = rel_dep*max(audi$kilometers)), size = 1, alpha = 0.5, stroke = 0) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(price), y = rel_dep*max(audi$kilometers)), color = "orangered", se = FALSE) +
  labs(x = "Observations sorted by kilometers", y = "Kilometers", title = "Comparison of kilometers and relative depreciation", subtitle = "<span style='color:royalblue;'>&#9899; Kilometers</span><br/><span style='color:orangered;'>&#9899; Relative depreciation</span>") +
  scale_y_continuous(sec.axis = sec_axis(~./max(audi$kilometers), name = "Relative depreciation")) +
  theme_light() +
  theme(plot.subtitle = element_markdown())

# Kilometers vs depreciation skoda

ggplot(skoda[order(skoda$kilometers),]) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(kilometers), y = kilometers), color = "royalblue", se = FALSE) +
  ylim(0, max(skoda$kilometers)) +
  xlim(0, nrow(skoda)) +
  geom_point(mapping = aes(x = seq(kilometers), y = rel_dep*max(skoda$kilometers)), size = 1, alpha = 0.5, stroke = 0) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(price), y = rel_dep*max(skoda$kilometers)), color = "orangered", se = FALSE) +
  labs(x = "Observations sorted by kilometers", y = "Kilometers", title = "Comparison of kilometers and relative depreciation", subtitle = "<span style='color:royalblue;'>&#9899; Kilometers</span><br/><span style='color:orangered;'>&#9899; Relative depreciation</span>") +
  scale_y_continuous(sec.axis = sec_axis(~./max(skoda$kilometers), name = "Relative depreciation")) +
  theme_light() +
  theme(plot.subtitle = element_markdown())

# Kilometers vs depreciation volvo

ggplot(volvo[order(volvo$kilometers),]) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(kilometers), y = kilometers), color = "royalblue", se = FALSE) +
  ylim(0, max(volvo$kilometers)) +
  xlim(0, nrow(volvo)) +
  geom_point(mapping = aes(x = seq(kilometers), y = rel_dep*max(volvo$kilometers)), size = 1, alpha = 0.5, stroke = 0) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(price), y = rel_dep*max(volvo$kilometers)), color = "orangered", se = FALSE) +
  labs(x = "Observations sorted by kilometers", y = "Kilometers", title = "Comparison of kilometers and relative depreciation", subtitle = "<span style='color:royalblue;'>&#9899; Kilometers</span><br/><span style='color:orangered;'>&#9899; Relative depreciation</span>") +
  scale_y_continuous(sec.axis = sec_axis(~./max(volvo$kilometers), name = "Relative depreciation")) +
  theme_light() +
  theme(plot.subtitle = element_markdown())

# Kilometers vs depreciation toyota

ggplot(toyota[order(toyota$kilometers),]) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(kilometers), y = kilometers), color = "royalblue", se = FALSE) +
  ylim(0, max(toyota$kilometers)) +
  xlim(0, nrow(toyota)) +
  geom_point(mapping = aes(x = seq(kilometers), y = rel_dep*max(toyota$kilometers)), size = 1, alpha = 0.5, stroke = 0) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(price), y = rel_dep*max(toyota$kilometers)), color = "orangered", se = FALSE) +
  labs(x = "Observations sorted by kilometers", y = "Kilometers", title = "Comparison of kilometers and relative depreciation", subtitle = "<span style='color:royalblue;'>&#9899; Kilometers</span><br/><span style='color:orangered;'>&#9899; Relative depreciation</span>") +
  scale_y_continuous(sec.axis = sec_axis(~./max(toyota$kilometers), name = "Relative depreciation")) +
  theme_light() +
  theme(plot.subtitle = element_markdown())
# Correlation matrices ----

# Audi

cor_matrix_audi <- 