#install.packages("janitor")
library(janitor)
library(dplyr)
mydata <- read.csv("cleaned-data/toyota.csv")

class(mydata$price)# verifier si la colonne price est bien de ype "caractere"


# Enlever les CHF dans la colonne "price"
mydata$price <- gsub("CHF", "", mydata$price)


# Supprimer les caractères spéciaux dans la colonne "title"
#mydata$title <- gsub("[^[:alnum:][:space:]]", "", mydata$title)

# Convertir la colonne "title" en minuscules
mydata$title <- tolower(mydata$title)

# Supprimer les informations redondantes dans la colonne "title"
mydata$title <- gsub("toyota yaris", "", mydata$title)

# Supprimer les caractères spéciaux

# Remplacer les valeurs manquantes par une valeur par défaut
mydata$subtitle[is.na(mydata$subtitle)] <- "Inconnu"

# Convertir la colonne "title" en minuscules
mydata$subtitle <- tolower(mydata$subtitle)

library(stringr)




# Supprimer les espaces blancs en début et fin de chaîne
mydata$body.type <- trimws(mydata$body.type)

# Convertir les valeurs en minuscules
mydata$body.type <- tolower(mydata$body.type)

# Remplacer les valeurs erronées par des valeurs correctes
mydata$body.type[mydata$body.type == "hatchback"] <- "berline"
mydata$body.type[mydata$body.type == "sedan"] <- "berline"
mydata$body.type[mydata$body.type == "coupe"] <- "coupé"
mydata$body.type[mydata$body.type == "convertible"] <- "cabriolet"

# Nettoyer la colonne "kilométrage"
mydata$kilometers <- gsub("[^0-9]", "", mydata$kilometers)

# Convertir la colonne en numérique
mydata$kilometers <- as.numeric(mydata$kilometers)

