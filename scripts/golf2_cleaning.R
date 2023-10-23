### We load the necessary libraries to clean our data

library(tidyverse)
library(lubridate)

### We set our working directory to the data folder and load our dataset golf2

setwd("~/GitHub/Data_science/data")
data <- read.csv("golf2.csv")
golf2 <- read.csv("golf2.csv")

### We will start by keeping only the numbers in the columns "price", "kilometers" and "listing.id"

data$price <- parse_number(data$price, locale = locale(grouping_mark = "'"))
data$kilometers <- parse_number(data$kilometers, locale = locale(grouping_mark = "'"))
data$listing.id <- parse_number(data$listing.id)

### We keep only the first number in "consumption" and get rid of "l/100km" and set "-" to "NA"

data$consumption <- na_if(data$consumption, "-")
data$consumption <- parse_number(data$consumption)

### We see that there are some outliers that shouldn't be here. The values jump from 11.6 l/100km to 160 l/100km. These are probably errors when scraping the data, the scraping software has maybe taken another field's value when the consumption field was blank.

unique(data$consumption[order(data$consumption)])
data$consumption[data$consumption > 12] <- NA

# We can easily see all the unique values and the duplicate count with this code:

#as.tibble(data$consumption) |>
#  group_by_all() |>
#  count() |>
#  print(n = 77)

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
data$new.price <- na_if(data$new.price, "Non")
data$new.price <- parse_number(data$new.price, locale = locale(grouping_mark = "'"))

### If we look at the unique values we can see some unrealistic new car prices, notably 1, 54 and 4309. We will assume these are errors and remove them.

unique(data$new.price[order(data$new.price)])
data$new.price[data$new.price < 5000] <- NA

### Let's drop some columns that have meaningless information, and that are a result of scraping the data. We will drop columns "web.scraper.order", "web.scraper.start.url", "page" and "annonce.link".

data <- data[-c(1:4)]

### Now we want to set the "date" column to type date. To do this we need to first format the values as dd.mm.yyyy, they are currently at mm.yyyy.

#Let's look at the data first

unique(data$date[order(data$date)])

#We remove one outlier (error)

data$date[2802] <- NA

#We will add the string "01." to the beginning of every value.

rm(test) 
test <- data$date
test <- as.character(data$date)
test <- dmy(paste0("01.", data$date))
test
rm(test)
test <- c("01.2.1999", "01.7.2004", "01.11.2019")
as.Date(test)
install.packages("parsedate")
library(parsedate)
parse_date(test)
as.Date(parse_date(test))


###assistant
test <- format(data$date, format = "%m.%Y")
test
as.Date(test)
class(test)
typeof(test)
test <- paste0("01.", test)
parse_date(test)
