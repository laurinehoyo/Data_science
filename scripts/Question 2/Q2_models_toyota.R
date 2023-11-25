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
# Based on the correlation matrix, there is a positive correlation between price and listing age, indicating that older listings tend to be associated with lower-priced vehicles. Kilometers appear to have no significant influence on listing age, whereas power and consumption seem to explain the listing age to a moderate extent. 

## les correlations entre listing.age et le reste des variables est quasiment 0, il n'y a pas de relation entre ces variables. Il faut ajouter residuals à la matrice.

# We will make simple linear models for these 5 variables and look at their respective plots.
# We will set a significance level of α = 0.01.

# 1. Influence du prix sur le listing age 

summary(lm(listing.age ~ price))
ggplot(data2, aes(price, listing.age)) + 
  geom_point() +
  geom_smooth(method = lm) +
  ylim(0, max(data2$listing.age))

# p value : 0.144 > 0.01  Donc price n'est pas significant.
# pente légerement positive, ce qui indique une augmentation du prix a une influence sur la durée de l'annonce, donc plus le prix est élevé plus l'annonce restera longtemps.
## le commentaire que tu as fait juste en dessus est faux, on ne peut pas dire que le prix a une influence sur listing.age car tu viens de le tester et de voir qu'il n'est pas significatif.

# 2. Influence de la consommation sur le listing age
summary(lm(listing.age ~ consumption))
ggplot(data2, aes(consumption, listing.age)) + 
  geom_point() +
  geom_smooth(method = lm) +
  ylim(0, max(data2$listing.age))


# Consumption is not statistically significant (pvalue < 0.01). we will not include this variable in our model. 
#While there is a positive trend indicated by the model, the weak fit suggests that consumption alone is not a strong predictor of the listing age, and other variables or a different type of model may be needed to better explain the variation in the listing age of used car listings.
#Trend Indication: The slope of the line is positive, suggesting that there is a positive relationship between consumption and listing age. This means that as the consumption of the vehicle increases, the listing tends to be older. However, the relationship does not appear to be very strong, as indicated by the wide confidence interval band (the grey area surrounding the line) and the spread of the points.
## tu nas pas besoin de dire que il y a une relation entre les variables indépendentes (consumption dans ce cas) et listing.age quand ce n'est pas significatif. La ligne geom_smooth ne veut rien dire dans ce cas. essaie de plot sans la ligne avec juste les points et tu verras que il n'y a pas de relation du tout

#3. Influence vehicle age sur le listing age 
summary(lm(listing.age ~ vehicle.age))
ggplot(data2, aes(vehicle.age, listing.age)) + 
  geom_point() +
  geom_smooth(method = lm) +
  ylim(0, max(data2$listing.age))

# vehicle age is not statistically significant. We will not include this variable in our model 
# la pente est tres légèrement négative ( pretiquement plate), ce qui rejoint le fait que la correlation est extremement faible et donc que l'age du véhicule n'ura pas d'effet sur la durée de l'annonce 
# we can conclude that there is no relation between the age of the vehicle and the age of the listing
##pareil que mon commentaire pour consumption

ggplot(lm(listing.age ~ vehicle.age), aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  geom_smooth()

# the residuals plot montre qu'il n'y a pas de problemes au niveau de la linéarité, la ligne bleue est a peu pret egale à la ligne rouge. Aucun residu ne semble fournir des valeurs aberrantes. 
#In conclusion, the residuals plot suggests that there may be issues with linearity, and the model could be improved. 
## le residuals vs fitted plot sert à vérifier que il n'y a pas trop de variance. vehicle.age n'est pas significatif donc ca sert à rien de le mettre ici, tu peux l'enlever.

# 4. Influence kilometers sur le listing age. 
#same problem for kilometers 

summary(lm(listing.age ~ kilometers))
ggplot(data2, aes(kilometers, listing.age)) + 
  geom_point() +
  geom_smooth(method = lm) +
  ylim(0, max(data2$listing.age))

# pas significatif. Ce qui signifique que le nombre de kilomètre n'a pas d'influence sur le listing age. 
# la ligne est horizontal ce qui signifie qu'il n'existe pas de relation entre le listing age et les kilomètres 
##c'est juste

#5.Influence power sur le listing age 

summary(lm(listing.age ~ power))
ggplot(data2, aes(power, listing.age)) + 
  geom_point() +
  geom_smooth(method = lm) +
  ylim(0, max(data2$listing.age))

# p value < 0.01: ce n'est significatif, ce qui signifie que la puissance de la voiture n'influence pas la durée d'une annonce. 
# la relation est légèrement positive 

ggplot(lm(listing.age ~ power), aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  geom_smooth()

## tu n'as pas besoin de dire que la relation est positive. On n'a pas besoin du resids vs fitted plot.

# nous voulons nous assurer de la linéarité entre lsiting age te power 
# bien que la variable power soit statistiquement significative dans la prédiction de l'âge de l'annonce.
# nous observons de l'hétéroskedasticité car la variance des résidus augmente avec les valeurs ajustées
# conclusion il est necessaire d'améliorer ce model (à refléchir)

# Séparemment, aucunes variables n'a d'influence direct sur la durée de l'annonce 

# For body.type, we can see that the majority of cars are either "Limousine" or "Petite voiture".
# We will create a logical column "sedan" which will be set to TRUE where body.type == "Limousine" is TRUE. We do the same for column "coupe". When both are FALSE, the body.type will be "Petite voiture".

table(drivetrain)

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
#summary(lm(listing.age ~ coupe)) # NOT Significant
summary(lm(listing.age ~ sedan)) # not Significant
summary(lm(listing.age ~ diesel)) # not Significant
summary(lm(listing.age ~ hybrid)) # NOT Significant
summary(lm(listing.age ~ manual)) # NOT Significant
summary(lm(listing.age ~ awd)) # not Significant





# ajouter une colonne avec les residuals du model 4 

model4 <- lm(listing.age ~ price + vehicle.age + kilometers + power + consumption + expertise  + sedan + diesel + hybrid + manual + awd)
summary(model4)

# Residual 
model4 <- lm(listing.age ~ price + vehicle.age + kilometers + power + consumption + expertise+ warranty +sedan +diesel+hybrid+ manual + awd, data = data2, na.action = na.exclude)
data2$residuals_model4 <- resid(model4)
summary(data2$residuals_model4)
residuals_model4 <- resid(model4)
data2$residuals_model4 <- residuals_model4

# difference netre ce que le model a prédit et le prix de la voiture, donc le plus i est négatif, plus l'annonce sera une bone offre et si le résiduel est positif ou tres grand, c'est donc une mauvaise offre 
# est ce que les voitures qui on un prix beaucoup plus bas se vendent plus lentement ? 

correlation <- cor(data2$residuals_model4, data2$listing.age, use = "complete.obs")
print(correlation)

#la corrélation est positive et significative, cela suggère que les voitures dont le prix est plus élevé que prévu tendent à rester plus longtemps dans les annonces. 
ggplot(data2, aes(x = residuals_model4, y = listing.age)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  ylim(0, max(data2$listing.age, na.rm = TRUE)) +
  xlab("Residuals (Difference between Predicted and Actual Price)") +
  ylab("Listing Age") +
  ggtitle("Scatterplot of Residuals vs Listing Age with Regression Line")


#ce graphique suggère que les voitures dont le prix est plus élevé que ce qui est prédit par le modèle tendent à être sur les annonces plus longtemps, ce qui pourrait indiquer que les voitures surévaluées prennent plus de temps à se vendre

# etudions la correlation des body type avec le listing age 
# problème avec la coupé, affichage NA ?? 
model4_data <- select(data2, c("price", "vehicle.age", "kilometers", "power", "consumption", "listing.age", "expertise","sedan", "diesel", "hybrid","manual", "awd"))
cor_matrix_2 <- cor(model4_data, use = "complete.obs")
cor_matrix_2
corrplot(cor_matrix_2)
cor_matrix
