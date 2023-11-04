### We load the necessary libraries to clean our data

library(tidyverse)
library(lubridate)

### We set our working directory to the data folder and load our dataset golf2

setwd("~/Downloads/Data_science-main/data")
data <- read.csv("audi2.csv")
audi2 <- read.csv("audi2.csv")

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

### Now we want to set the "date" column to class "Date" to simplify plotting. To do this we need to format the values as yyyy.mm.dd, they are currently at mm.yyyy.

# Let's look at the data first

unique(data$date[order(data$date)])

# We remove one visible error

#data$date[2802] <- NA

# Now we set the format to mm.yyyy (the vector needs to be a character vector for the parse_date function to work) then parse date. The final format is yyyy-mm-dd.

data$date <- data$date |>
  format(format = "%m.%Y") |>
  parse_date(format = "%m.%Y")

### We set expertise.date and created.date to date classes as well

data$created.date <- parse_date(data$created.date)

data$expertise.date <- data$expertise.date |>
  format(format = "%d.%m.%Y") |>
  parse_date(format = "%d.%m.%Y")



### We will now add a column to the data where values will be TRUE where the vehicle is potentially defective or in need of repairs, and FALSE if not. 
### We will need to remove these defective vehicles when creating our model because they could bias it, making it underestimate prices for functioning vehicles.
### We plan to identify defective vehicles in two ways. The first way being the "accident" column, which is TRUE if the seller has flagged the vehicle as having been in an accident in the past, and FALSE if not.
### The second way will be to look for keywords in columns "title", "subtitle" and "description.text" which are common in listings with defective vehicles, such as "defekt", "motor startet nicht" etc.
### We will create a column where values will be TRUE if one of these keywords or key phrases is detected, and FALSE if not.
### If either of these two identifiers are TRUE, the vehicle will be deemed defective and not eligible to serve as data for creating our model.

# Create a vector of defective keywords for multiple languages
defective_keywords <- c(
  "defekt", "defect", "startet nicht", "starttet nicht", "gebrochen", "lampe leuchtet", "für export", 
  "gerausche", "geräusche", "gerräusche", "angebrochen", "angeschlagen", "beschädigt", "fehlerhaft", 
  "lädiert", "nicht in Ordnung", "nicht mehr funktionierend", "schadhaft", "zerbrochen", "zerrissen", 
  "kaputt", "témoin", "bruit", "défaut", "ne démarre pas", "cassé", "endommagé", "endommager", 
  "endommage", "défectueux", "defectueux", "pas en bon état", "ne fonctionne plus", "spia", "rumore", 
  "difetto", "non parte", "non si avvia", "rotto", "per l'esportazione", "danneggiato", "danneggiare", 
  "danneggia", "difettoso", "non in buone condizioni", "non funziona più", "defective", "does not start", 
  "won't start", "broken", "light on", "for export", "noise", "faulty", "not working", "no longer working"
)

# Define a function to check for defective keywords
contains_defective_keywords <- function(text, keywords) {
  sapply(text, function(x) any(str_detect(x, regex(paste(keywords, collapse = "|"), ignore_case = TRUE))))
}

# Apply the function to the relevant columns and create a new 'defective' column
data$defective <- reduce(
  list(
    data$accident,
    contains_defective_keywords(data$title, defective_keywords),
    contains_defective_keywords(data$subtitle, defective_keywords),
    contains_defective_keywords(data$description.text, defective_keywords)
  ),
  `|`
)

# Finally, we can remove the defective vehicles from our dataset
clean_data <- data %>% filter(!defective)
