# EDA Graphs & Matrices

# Question 3: How do different vehicle features affect depreciation ?
library(patchwork)
library(ggtext)
library(tidyverse)
library(corrplot)
library(kableExtra)
library(shiny)
setwd("~/GitHub/Data_science/cleaned-data")

golf <- read.csv("golf_models_data.csv")
audi <- read.csv("audi_models_data.csv")
skoda <- read.csv("skoda_models_data.csv")
volvo <- read.csv("volvo_models_data.csv")
toyota <- read.csv("toyota_models_data.csv")

# Table for reductions in sample size

sample_sizes <- data.frame(
  "VW Golf" = nrow(golf),
  "Audi A3" = nrow(audi),
  "Skoda Octavia" = nrow(skoda),
  "Volvo XC60" = nrow(volvo),
  "Toyota Yaris" = nrow(toyota)
)
sample_sizes

# Depreciation variables
# We filter to remove NAs from new price for this part

# golf
golf <- golf |> filter(!is.na(new.price))
golf$depreciation = golf$new.price - golf$price
golf$rel_dep = golf$depreciation / golf$new.price

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

# Table for reductions in sample size continued

sample_sizes_2 <- data.frame(
  "VW Golf" = nrow(golf),
  "Audi A3" = nrow(audi),
  "Skoda Octavia" = nrow(skoda),
  "Volvo XC60" = nrow(volvo),
  "Toyota Yaris" = nrow(toyota)
)

sample_sizes_change <- ((sample_sizes - sample_sizes_2) / sample_sizes) * 100
sample_sizes <- rbind(sample_sizes, sample_sizes_2)
sample_sizes <- rbind(sample_sizes, sample_sizes_change)
sample_sizes <- round(sample_sizes, digits = 0)
sample_sizes[3,] <- paste(sample_sizes[3,], "%", sep = " ")
rownames(sample_sizes) <- c("Original", "Filtered", "Reduction")

sample_sizes |>
  kable() |>
  kable_styling()

# Depreciation plots
# New prices vs listing prices ----

# New prices vs listing prices golf

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
  ggtitle("VW Golf") +
  theme_light() +
  theme(plot.subtitle = element_markdown())

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
  ggtitle("Audi A3") +
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
  ggtitle("Skoda Octavia") +
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
  ggtitle("Volvo XC60") +
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
  ggtitle("Toyota Yaris") +
  theme_light() +
  theme(plot.subtitle = element_markdown())

# Patchwork

a1
(a2 | a3) /
(a4 | a5)

# shiny interactive plots

ui <- fluidPage(
  fluidRow(
    column(3, actionButton("btn_a1", "Plot A1")),
    column(3, actionButton("btn_a2", "Plot A2")),
    column(3, actionButton("btn_a3", "Plot A3")),
    column(3, actionButton("btn_a4", "Plot A4")),
    column(3, actionButton("btn_a5", "Plot A5")),
  ),
  plotOutput("plot")
)

server <- function(input, output, session) {
  observeEvent(input$btn_a1, {
    output$plot <- renderPlot({ a1 })
  })
  
  observeEvent(input$btn_a2, {
    output$plot <- renderPlot({ a2 })
  })
  
  observeEvent(input$btn_a3, {
    output$plot <- renderPlot({ a3 })
  })
  
  observeEvent(input$btn_a4, {
    output$plot <- renderPlot({ a4 })
  })
  
  observeEvent(input$btn_a5, {
    output$plot <- renderPlot({ a5 })
  })
}

shinyApp(ui, server)

# New prices vs depreciation ----

# New prices vs depreciation golf

b1 <- ggplot(golf[order(golf$new.price),]) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(new.price), y = new.price), color = "royalblue", se = FALSE) +
  ylim(0, 100000) +
  xlim(0, nrow(golf)) +
  geom_point(mapping = aes(x = seq(new.price), y = rel_dep*100000), size = 1, alpha = 0.5, stroke = 0) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(new.price), y = rel_dep*100000), color = "orangered", se = FALSE) +
  labs(x = "Observations sorted by new price", y = "Price", subtitle = "<span style='color:royalblue;'>&#9899; New prices</span><br/><span style='color:orangered;'>&#9899; Relative depreciation</span>") +
  scale_y_continuous(sec.axis = sec_axis(~./100000, name = "Relative depreciation")) +
  theme_light() +
  theme(plot.subtitle = element_markdown())

# New prices vs depreciation audi

b2 <- ggplot(audi[order(audi$new.price),]) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(new.price), y = new.price), color = "royalblue", se = FALSE) +
  ylim(0, 100000) +
  xlim(0, nrow(audi)) +
  geom_point(mapping = aes(x = seq(new.price), y = rel_dep*100000), size = 1, alpha = 0.5, stroke = 0) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(new.price), y = rel_dep*100000), color = "orangered", se = FALSE) +
  labs(x = "Observations sorted by new price", y = "Price") +
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
  labs(x = "Observations sorted by new price", y = "Price") +
  scale_y_continuous(sec.axis = sec_axis(~./100000, name = "Relative depreciation")) +
  theme_light() +
  theme(plot.subtitle = element_markdown())

# Patchwork

b1 +
  plot_annotation(title = "Comparison of new prices and relative depreciation")
b2
b3
b4
b5

(b1 | b2) /
  (b3 | b4) +
  plot_annotation(
    title = "Comparison of new prices and relative depreciation"
  )

# Prices vs depreciation ----

# Prices vs depreciation golf

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
c1

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

# Vehicle age vs depreciation golf

d1 <- ggplot(golf[order(golf$vehicle.age),]) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(vehicle.age), y = vehicle.age/365), color = "royalblue", se = FALSE) +
  ylim(0, max(golf$vehicle.age)/365) +
  xlim(0, nrow(golf)) +
  geom_point(mapping = aes(x = seq(vehicle.age), y = rel_dep*(max(golf$vehicle.age)/365)), size = 1, alpha = 0.5, stroke = 0) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(price), y = rel_dep*(max(golf$vehicle.age)/365)), color = "orangered", se = FALSE) +
  labs(x = "Observations sorted by vehicle age", y = "Vehicle age (years)", title = "Comparison of vehicle age and relative depreciation", subtitle = "VW Golf") +
  scale_y_continuous(sec.axis = sec_axis(~./(max(golf$vehicle.age)/365), name = "Relative depreciation")) +
  theme_light() +
  theme(plot.subtitle = element_markdown())
d1

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

(d2 | d3) /
  (d4 | d5) +
  plot_annotation(
    title = "Comparison of vehicle age and relative depreciation"
  )

# Kilometers vs depreciation ----

# Kilometers vs depreciation golf

e1 <- ggplot(golf[order(golf$kilometers),]) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(kilometers), y = kilometers), color = "royalblue", se = FALSE) +
  ylim(0, max(golf$kilometers)) +
  xlim(0, nrow(golf)) +
  geom_point(mapping = aes(x = seq(kilometers), y = rel_dep*max(golf$kilometers)), size = 1, alpha = 0.5, stroke = 0) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(price), y = rel_dep*max(golf$kilometers)), color = "orangered", se = FALSE) +
  labs(x = "Observations sorted by kilometers", y = "Kilometers", title = "Comparison of kilometers and relative depreciation", subtitle = "VW Golf") +
  scale_y_continuous(sec.axis = sec_axis(~./max(golf$kilometers), name = "Relative depreciation")) +
  theme_light() +
  theme(plot.subtitle = element_markdown())
e1

# Kilometers vs depreciation audi

e2 <- ggplot(audi[order(audi$kilometers),]) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(kilometers), y = kilometers), color = "royalblue", se = FALSE) +
  ylim(0, max(audi$kilometers)) +
  xlim(0, nrow(audi)) +
  geom_point(mapping = aes(x = seq(kilometers), y = rel_dep*max(audi$kilometers)), size = 1, alpha = 0.5, stroke = 0) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(price), y = rel_dep*max(audi$kilometers)), color = "orangered", se = FALSE) +
  labs(x = "Observations sorted by kilometers", y = "Kilometers", subtitle = "Audi A3", title = "<span style='color:royalblue;'>&#9899; Kilometers</span><br/><span style='color:orangered;'>&#9899; Relative depreciation</span>") +
  scale_y_continuous(sec.axis = sec_axis(~./max(audi$kilometers), name = "Relative depreciation")) +
  theme_light() +
  theme(plot.title = element_markdown())

# Kilometers vs depreciation skoda

e3 <- ggplot(skoda[order(skoda$kilometers),]) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(kilometers), y = kilometers), color = "royalblue", se = FALSE) +
  ylim(0, max(skoda$kilometers)) +
  xlim(0, nrow(skoda)) +
  geom_point(mapping = aes(x = seq(kilometers), y = rel_dep*max(skoda$kilometers)), size = 1, alpha = 0.5, stroke = 0) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(price), y = rel_dep*max(skoda$kilometers)), color = "orangered", se = FALSE) +
  labs(x = "Observations sorted by kilometers", y = "Kilometers", subtitle = "Skoda Octavia") +
  scale_y_continuous(sec.axis = sec_axis(~./max(skoda$kilometers), name = "Relative depreciation")) +
  theme_light() +
  theme(plot.subtitle = element_markdown())

# Kilometers vs depreciation volvo

e4 <- ggplot(volvo[order(volvo$kilometers),]) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(kilometers), y = kilometers), color = "royalblue", se = FALSE) +
  ylim(0, max(volvo$kilometers)) +
  xlim(0, nrow(volvo)) +
  geom_point(mapping = aes(x = seq(kilometers), y = rel_dep*max(volvo$kilometers)), size = 1, alpha = 0.5, stroke = 0) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(price), y = rel_dep*max(volvo$kilometers)), color = "orangered", se = FALSE) +
  labs(x = "Observations sorted by kilometers", y = "Kilometers", subtitle = "Volvo XC60") +
  scale_y_continuous(sec.axis = sec_axis(~./max(volvo$kilometers), name = "Relative depreciation")) +
  theme_light() +
  theme(plot.subtitle = element_markdown())

# Kilometers vs depreciation toyota

e5 <- ggplot(toyota[order(toyota$kilometers),]) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(kilometers), y = kilometers), color = "royalblue", se = FALSE) +
  ylim(0, max(toyota$kilometers)) +
  xlim(0, nrow(toyota)) +
  geom_point(mapping = aes(x = seq(kilometers), y = rel_dep*max(toyota$kilometers)), size = 1, alpha = 0.5, stroke = 0) +
  geom_smooth(span = 0.1, mapping = aes(x = seq(price), y = rel_dep*max(toyota$kilometers)), color = "orangered", se = FALSE) +
  labs(x = "Observations sorted by kilometers", y = "Kilometers", subtitle = "Toyota Yaris") +
  scale_y_continuous(sec.axis = sec_axis(~./max(toyota$kilometers), name = "Relative depreciation")) +
  theme_light() +
  theme(plot.subtitle = element_markdown())

# Patchwork

(e2 | e3) /
  (e4 | e5) +
  plot_annotation(
    title = "Comparison of kilometers and relative depreciation"
  )

# Correlation matrices

cor_matrix_golf <- cor(select(golf, c("depreciation", "rel_dep", "new.price", "price", "kilometers", "vehicle.age", "power", "consumption", "expertise", "warranty", "wagon", "diesel", "hybrid", "manual", "awd")), use = "complete.obs")
cor_matrix_golf
corrplot(cor_matrix_golf)


# Correlation matrices ----

# golf
cor_matrix_golf <- cor(select(golf, c("depreciation", "rel_dep", "new.price", "price", "kilometers", "vehicle.age", "power", "consumption", "expertise", "warranty", "wagon", "diesel", "hybrid", "electric", "natural.gas", "manual", "awd")), use = "complete.obs")
# We don't include natural gas vehicles because there are only 2 observations, and such a small sample size may lead to unreliable estimates of correlations.
cor_matrix_golf
corrplot(cor_matrix_golf)
# When we run the correlation matrix with variable electric included, we get NAs for correlations between electric and every other variable. We explicitly remove all rows where there is a NA (use = "complete.obs"), so this may be the reason. Upon further inspection, we notice that consumption is NA for all electric vehicles.
# We still want to check the correlations between electric and all other variables, so we will set consumption for electric vehicles to an arbitrary value, say -999, for our matrix to function correctly. We make a copy of the dataframe for this.
# For the other vehicles, we will simply omit electric because the counts are too low.
golf2 <- golf
golf2$consumption[golf2$electric == TRUE] <- -999
cor_matrix_golf2 <- cor(select(golf2, c("depreciation", "rel_dep", "new.price", "price", "kilometers", "vehicle.age", "power", "consumption", "expertise", "warranty", "wagon", "diesel", "hybrid", "electric", "natural.gas", "manual", "awd")), use = "complete.obs")
cor_matrix_golf2
corrplot(cor_matrix_golf2)

# audi
cor_matrix_audi <- cor(select(audi, c("depreciation", "rel_dep", "new.price", "price", "kilometers", "vehicle.age", "power", "consumption", "expertise", "warranty", "cabriolet", "diesel", "hybrid", "manual", "awd")), use = "complete.obs")
# We omit natural gas vehicles (2 observations) and coupe (1 observation)
cor_matrix_audi
corrplot(cor_matrix_audi)

# skoda
cor_matrix_skoda <- cor(select(skoda, c("depreciation", "rel_dep", "new.price", "price", "kilometers", "vehicle.age", "power", "consumption", "expertise", "warranty", "sedan", "diesel", "hybrid", "natural.gas", "manual", "awd")), use = "complete.obs")
cor_matrix_skoda
corrplot(cor_matrix_skoda)

# volvo
cor_matrix_volvo <- cor(select(volvo, c("depreciation", "rel_dep", "new.price", "price", "kilometers", "vehicle.age", "power", "consumption", "expertise", "warranty", "diesel", "hybrid", "manual")), use = "complete.obs")
cor_matrix_volvo
corrplot(cor_matrix_volvo)

# toyota
cor_matrix_toyota <- cor(select(toyota, c("depreciation", "rel_dep", "new.price", "price", "kilometers", "vehicle.age", "power", "consumption", "expertise", "warranty", "sedan", "hybrid", "manual", "awd")), use = "complete.obs")
cor_matrix_toyota
corrplot(cor_matrix_toyota)

