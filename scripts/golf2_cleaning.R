### We load the necessary libraries to clean our data

library(tidyverse)
library(lubridate)

### We set our working directory to the data folder and load our dataset golf2

setwd("~/GitHub/Data_science/data")
data <- read.csv("golf2.csv")
golf2 <- read.csv("golf2.csv")

### We will start by keeping only the numbers in the columns "price", "kilometers" and "listing.id"

data$price <- parse_number(price)
data$kilometers <- parse_number(kilometers, locale = locale(grouping_mark = "'"))
data$listing.id <- parse_number(listing.id)

### We keep only the first number in "consumption" and get rid of "l/100km" and set "-" to "NA"

data$consumption <- na_if(data$consumption, "-")
data$consumption <- parse_number(data$consumption)

### We see that there are some outliers that shouldn't be here. The values jump from 11.6 l/100km to 160 l/100km. These are probably errors when scraping the data, the scraping software has maybe taken another field's value when the consumption field was blank.

data$consumption[order(data$consumption, decreasing = TRUE)]
data$consumption[data$consumption > 12] <- NA
unique(data$consumption[order(data$consumption, decreasing = TRUE)])

data$consumption <- golf2$consumption
data$consumption
