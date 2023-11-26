### Research question 2 : What are the factors that influence the duration of a listing for a used car? 

library(tidyverse)
library(corrplot)
library(Hmisc)
library(car)

setwd("~/Downloads/Data_science-main/cleaned-data")
data <- read.csv("toyota_cleaned.csv")
toyota_cleaned <- read.csv("toyota_cleaned.csv")
data <- read.csv("toyota_cleaned.csv")
data2 <- subset(data, created.date != "2022-11-14")
# nous avons supprimé les lignes avec cette date car elle rendait les listing.date irealiste. 

attach(data2)

# Our independant variable will be listing age
#Le terme "listing age" se réfère généralement à la durée depuis laquelle un article, une annonce ou une inscription est actif ou a été posté.
# First we want to build a model with our numerical variables (vehicle.age, kilometers, power, consumption, price), to try to explain the impact on the listing age. 
# We start by creating a correlation matrix with the numeric columns

data_numeric <- select(data2, c("price", "vehicle.age", "kilometers", "power", "consumption", "listing.age"))
data_numeric
cor_matrix <- cor(data_numeric, use = "complete.obs")

cor_matrix
corrplot(cor_matrix)

# nous remarquons que la correlation entre listing.age et price est positive mais relativement faible => ce qui signifie qu'il existe très peu de relation lineaire entre ces dernieres, cependant le fait que ce soit positive peut illustrer une certaine relation entre les deux variables 
# Les correlations entre listing.age et le reste des variables est quasiment 0, il n'y a pas de relation entre ces variables. Il faut ajouter residuals à la matrice.

model0 <- lm(listing.age ~ price + vehicle.age + kilometers + power + consumption, data = data2, na.action = na.exclude)
data2$residuals_model0 <- resid(model0)
summary(data2$residuals_model0)
residuals_model0 <- resid(model0)
data2$residuals_model0 <- residuals_model0
data_numeric_2 <- select(data2, c("price", "vehicle.age", "kilometers", "power", "consumption", "listing.age", "residuals_model0"))
data_numeric_2


# Calcul de la nouvelle matrice de corrélation
cor_matrix_with_residuals <- cor(data_numeric_2, use = "complete.obs")

cor_matrix_with_residuals

# Affichage de la nouvelle matrice de corrélation
corrplot(cor_matrix_with_residuals)

# conclusion : la corrélation entre les residuals et le listing age est de 0.99. Cette corrélation élevé peut suggérer que d'autres facteurs peuvent influencer les listing.age mais que ces derniere ne sont pas présente dans le modèle actuel
# c'est que nous analyserons dans un second temps avec le model1. 

# We will make simple linear models for these 5 variables and look at their respective plots.
# We will set a significance level of α = 0.01.

# 1. Influence du prix sur le listing age 

summary(lm(listing.age ~ price))
ggplot(data2, aes(price, listing.age)) + 
  geom_point() +
  geom_smooth(method = lm) +
  ylim(0, max(data2$listing.age))

# p value : 0.144 > 0.01  Donc price n'est pas significant.
# le prix n'a donc pas d'inlfuence sur la durée de l'annonce bien que la pente soit légèrement positive 


# 2. Influence de la consommation sur le listing age
summary(lm(listing.age ~ consumption))
ggplot(data2, aes(consumption, listing.age)) + 
  geom_point()+
  ylim(0, max(data2$listing.age))


# Consumption is not statistically significant (pvalue < 0.01). we will not include this variable in our model. 
# la répartition des points dans ce graphe illustre bien qu'il n'y a aucune relation entre les deux variables 


#3. Influence vehicle age sur le listing age 
summary(lm(listing.age ~ vehicle.age))
ggplot(data2, aes(vehicle.age, listing.age)) + 
  geom_point() +
  ylim(0, max(data2$listing.age))

# vehicle age is not statistically significant. We will not include this variable in our model 
# la répartition des points dans ce graphe illustre bien qu'il n'y a aucune relation entre les deux variables 
# we can conclude that there is no relation between the age of the vehicle and the age of the listing


# 4. Influence kilometers sur le listing age. 


summary(lm(listing.age ~ kilometers))
ggplot(data2, aes(kilometers, listing.age)) + 
  geom_point() +
  geom_smooth(method = lm) +
  ylim(0, max(data2$listing.age))

# pas significatif. Ce qui signifique que le nombre de kilomètre n'a pas d'influence sur le listing age. 
# la ligne est horizontal ce qui signifie qu'il n'existe pas de relation entre le listing age et les kilomètres 


#5.Influence power sur le listing age 

summary(lm(listing.age ~ power))
ggplot(data2, aes(power, listing.age)) + 
  geom_point() +
  geom_smooth(method = lm) +
  ylim(0, max(data2$listing.age))

# p value < 0.01: ce n'est significatif, ce qui signifie que la puissance de la voiture n'influence pas la durée d'une annonce. 
# la répartition des points dans ce graphe illustre bien qu'il n'y a aucune relation entre les deux variables 


# CONCLUSION : Séparemment, aucunes variables n'a d'influence direct sur la durée de l'annonce 

# For body.type, we can see that the majority of cars are either "Limousine" or "Petite voiture".
# We will create a logical column "sedan" which will be set to TRUE where body.type == "Limousine" is TRUE. We do the same for column "coupe". When both are FALSE, the body.type will be "Petite voiture".

table(fuel.type)

data2$sedan <- body.type == "Limousine"
data2$coupe <- body.type == "Coupé"

# We will repeat the process for fuel.type. Columns will be "diesel" and "hybrid".

data2$diesel <- fuel.type == "Diesel"
data2$hybrid <- ifelse(fuel.type == "Hybride léger essence/électrique" | 
                         fuel.type == "Hybride rechargeable essence/électrique" |
                         fuel.type == "Hybride léger diesel/électrique", 
                       TRUE, FALSE)

# We repeat the process for transmission. We will only have one variable "manual". We consider "Boîte manuelle automatisée" to be automatic because it is very similar to an automatic transmission.

data2$manual <- ifelse(transmission == "Automatique" |
                         transmission == "Boîte manuelle automatisée" |
                         transmission == "Boîte automatique variable", 
                       FALSE, TRUE)

# We repeat the process for drivetrain. We will create one column "awd".

data2$awd <- drivetrain == "4 roues motrices"
# Now we run the simple linear regressions for expertise, warranty, wagon, cabriolet, diesel, hybrid, natural.gas, electric, manual and awd. We note which ones are statistically significant at α = 0.01.

attach(data2)
summary(lm(listing.age ~ expertise)) # not Significant
summary(lm(listing.age ~ warranty)) # not Significant
summary(lm(listing.age ~ coupe)) # NOT Significant
summary(lm(listing.age ~ sedan)) # not Significant
summary(lm(listing.age ~ diesel)) # not Significant
summary(lm(listing.age ~ hybrid)) # NOT Significant
summary(lm(listing.age ~ manual)) # NOT Significant
summary(lm(listing.age ~ awd)) # not Significant



## la partie en dessous est fausse. Il faut que tu fasse la régression avec price comme variable dépendante (exactement comme celle de la première question de recherche). Tu peux nommer le modèle model1 car il n'y a pas d'autres modèles dans ton code et ça prête à confusion.

# ajouter une colonne avec les residuals du model 4 
model1 <- lm(price ~ vehicle.age + listing.age + kilometers  + power + consumption + expertise + warranty + sedan + diesel + awd)
summary(model)

# Diagnostics du modèle
par(mfrow=c(2,2)) # peut etre utile 
plot(model1)
# j'ai un doute sur ce model, j'ai l'impression que ca n'explique pas directement l'influence sur le listing.age

# nous allons analyser si d'autres facteurs peuvent influencer le listing age donc ici cela concerne le body type
model1_data <- select(data2, c("price", "vehicle.age", "kilometers", "power", "consumption", "listing.age", "expertise","sedan", "diesel", "hybrid","manual", "awd"))
cor_matrix_2 <- cor(model1_data, use = "complete.obs")
cor_matrix_2
corrplot(cor_matrix_2)

# CONCLUSION : aucun body type n'a de corrélation significative avec le listing age. On en deduit donc qu'ils n'ont pas d'influence sur la durée de l'annonce 

# Calcul des résidus du modèle
data2$residuals_model1 <- resid(model1)

# Affichage du résumé des résidus
summary(data2$residuals_model1)

# Sélection des variables pour la matrice de corrélation, incluant les résidus
data_numeric_3 <- select(data2, c("price", "vehicle.age", "kilometers", "power", "consumption", "listing.age", "expertise", "sedan", "diesel", "hybrid", "manual", "awd", "residuals_model1"))

# Calcul de la nouvelle matrice de corrélation avec les résidus
cor_matrix_with_residuals1 <- cor(data_numeric_3, use = "complete.obs")

# Affichage de la matrice de corrélation
corrplot(cor_matrix_with_residuals1)


# CONCLUSION : aucune corrélation significative avec listing age, donc aucun de ces facteurs n'a d'inlfuence sur la durée de l'annonce 
# difference entre ce que le model a prédit et le prix de la voiture, donc le plus i est négatif, plus l'annonce sera une bonne offre et si le résiduel est positif ou tres grand, c'est donc une mauvaise offre 
# est ce que les voitures qui on un prix beaucoup plus bas se vendent plus lentement ? 


