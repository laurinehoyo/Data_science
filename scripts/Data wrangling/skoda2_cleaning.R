### We load the necessary libraries to clean our data

library(tidyverse)
library(lubridate)

### We set our working directory to the data folder and load our dataset golf2

setwd("~/GitHub/Data_science/data")
data <- read.csv("skoda2.csv")
skoda2 <- read.csv("skoda2.csv")

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

data <- data %>% select(-web.scraper.order, -web.scraper.start.url, -page, -annonce.link)

### Now we want to set the "date" column to class "Date" to simplify plotting. To do this we need to format the values as yyyy.mm.dd, they are currently at mm.yyyy.

# Let's look at the data first

unique(data$date[order(data$date)])

# We remove one visible error

data$date[1584] <- NA

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

# First we create a vector of characters which are the defective keywords.

defective_keywords_de <- c("defekt", "defect", "defeckt", "startet nicht", "starttet nicht", "gebrochen", "lampe leuchtet", "für export", "fur export", "gerausche", "geräusche", "gerräusche", "angebrochen", "angeschlagen", "beschädigt", "fehlerhaft", "lädiert", "nicht in Ordnung", "nicht mehr funktionierend", "nicht mehr gut", "schadhaft", "zerbrochen", "zerrissen", "kaputt" )

# We create a second list where the keywords are likely to have negation before them, so as to not falsely flag vehicles as defective. 
# We will then create an "ok" keywords vector that overpower the second defective keywords vector.

defective_keywords_2_de <- c("schäden", "problem", "schade", "probleme", "schaden")
ok_keywords_de <- c(paste("ohne", defective_keywords_2_de, sep = " "), paste("kein", defective_keywords_2_de, sep = " "), paste("keine", defective_keywords_2_de, sep = " "))

#We repeat these steps for other languages that listings could be in (French, Italian and English).

defective_keywords_fr <- c("témoin", "bruit", "défaut", "ne démarre pas", "cassé", "pour export", "pour l'export", "endommagé", "endommager", "endommage", "défectueux", "defectueux", "pas en bon état", "ne fonctionne plus", "cassé")
defective_keywords_2_fr <- c("dégâts", "dégats", "degâts", "degats", "problème", "probleme", "dommages")
ok_keywords_fr <- c(paste("sans", defective_keywords_2_fr, sep = " "), paste("pas de", defective_keywords_2_fr, sep = " "), paste("aucun", defective_keywords_2_fr, sep = " "))

defective_keywords_it <- c("spia", "rumore", "difetto", "non parte", "non si avvia", "rotto", "per l'esportazione", "per export", "danneggiato", "danneggiare", "danneggia", "difettoso", "non in buone condizioni", "non funziona più")
defective_keywords_2_it <- c("danni", "problemi", "problema")
ok_keywords_it <- c(paste("nessun", defective_keywords_2_it, sep = " "), paste("senza", defective_keywords_2_it, sep = " "))

defective_keywords_en <- c("defective", "does not start", "won't start", "broken", "light on", "for export", "noise", "faulty", "not working", "no longer working")
defective_keywords_2_en <- c("damage", "damaged", "problem", "problems")
ok_keywords_en <- c(paste("no", defective_keywords_2_en, sep = " "), paste("without", defective_keywords_2_en, sep = " "), paste("not", defective_keywords_2_en, sep = " "))

# We create vectors for the keywords

defective_keywords <- c(defective_keywords_de, defective_keywords_fr, defective_keywords_it, defective_keywords_en)
defective_keywords_2 <- c(defective_keywords_2_de, defective_keywords_2_fr, defective_keywords_2_it, defective_keywords_2_en)
ok_keywords <- c(ok_keywords_de, ok_keywords_fr, ok_keywords_it, ok_keywords_en)

# We add the column "defective".

data$defective <- logical(nrow(data))

# We set to true if any keyword from defective_keywords_2 (susceptible to be a false positive) is in the subtitle column or description.text column.

for (keyword in defective_keywords_2) {
  data$defective <- data$defective | grepl(keyword, data$description.text, ignore.case = TRUE)
}

for (keyword in defective_keywords_2) {
  data$defective <- data$defective | grepl(keyword, data$subtitle, ignore.case = TRUE)
}

# We set to FALSE all the TRUE values that contain a keyword in ok_keywords.

for (keyword in ok_keywords) {
  data$defective <- data$defective & !grepl(keyword, data$description.text, ignore.case = TRUE)
}

for (keyword in ok_keywords) {
  data$defective <- data$defective & !grepl(keyword, data$subtitle, ignore.case = TRUE)
}

# We set to TRUE if any keyword from defective_keywords are in the subtitle or description.text columns.

for (keyword in defective_keywords) {
  data$defective <- data$defective | grepl(keyword, data$description.text, ignore.case = TRUE)
}

for (keyword in defective_keywords) {
  data$defective <- data$defective | grepl(keyword, data$subtitle, ignore.case = TRUE)
}

# We set to defective to TRUE where the column accident is TRUE.

data$defective[data$accident] <- TRUE

### We set to NA if values are "-" in columns fuel.type, transmission and drivetrain.

data$fuel.type <- na_if(data$fuel.type, "-")
data$transmission <- na_if(data$transmission, "-")
data$drivetrain <- na_if(data$drivetrain, "-")

### We will create the columns vehicle.age and listing.age to be able to model without using dates. The unit of measurement will be days.

# We define scraping_date which is the date of the most recently created listing (we assume this corresponds to the scraping date since there are many listings posted each day for our data sets).

scraping_date <- data$created.date[order(data$created.date, decreasing = TRUE)[1]]

data$vehicle.age <- scraping_date - data$date
data$listing.age <- scraping_date - data$created.date

# We remove the defective vehicles from our cleaned data

data <- data[data$defective == FALSE,]

### Finally we export the data to our cleaned-data folder

write.csv(data, file = "~/GitHub/Data_science/cleaned-data/skoda2_cleaned.csv", row.names = FALSE)
