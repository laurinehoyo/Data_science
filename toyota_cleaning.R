### We load the necessary libraries to clean our data

library(tidyverse)
library(lubridate)

### We set our working directory to the data folder and load our dataset toyota

setwd("~/Documents/GitHub/Data_science/data")
data <- read.csv("toyota.csv")
toyota <- read.csv("toyota.csv")

### We will start by keeping only the numbers in the columns "price", "kilometers" and "listing.id"

data$price <- parse_number(data$price, locale = locale(grouping_mark = "'"))
data$kilometers <- parse_number(data$kilometers, locale = locale(grouping_mark = "'"))
data$listing.id <- parse_number(data$listing.id)

### We keep only the first number in "consumption" and get rid of "l/100km" and set "-" to "NA"

data$consumption <- na_if(data$consumption, "-")
data$consumption <- parse_number(data$consumption)

### Now let's do the same for column "power". We will keep the unit of measurement horsepower instead of kW.

data$power <- na_if(data$power, "-")
data$power <- parse_number(data$power)

### We will now change the "Oui"/"Non" values in the column "expertise" to logical values TRUE/FALSE

data$expertise[data$expertise == "Non"] <- FALSE
data$expertise[data$expertise == "Oui"] <- TRUE
data$expertise <- as.logical(data$expertise)

### Let's do the same for columns "accident" and "warranty".

data$accident[data$accident == "Non"] <- FALSE
data$accident[data$accident == "Oui"] <- TRUE
data$accident <- as.logical(data$accident)

data$warranty[data$warranty == "Non"] <- FALSE
data$warranty[data$warranty == "Oui"] <- TRUE
data$warranty <- as.logical(data$warranty)

### We will now fix the column "new.price" by setting values "Oui"/"Non" (Errors when scraping) to NA, then we will parse numbers from the remaining columns.

data$new.price <- na_if(data$new.price, "Oui")
rlang::last_trace()
rlang::last_trace(drop = FALSE)

data$new.price <- na_if(data$new.price, "Non")
data$new.price <- parse_number(data$new.price, locale = locale(grouping_mark = "'"))

