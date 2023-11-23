# Research question 3: How do different car features affect depreciation over time?

library(tidyverse)
setwd("~/GitHub/Data_science/cleaned-data")

# To answer this question, we will use the observations in our data where we have values for "new.price"

data <- read.csv("golf2_cleaned.csv")
attach(data)
data <- data |> filter(!is.na(new.price))
attach(data)
data$depreciation <- new.price - price
