### Research question 2 : What are the factors that influence the duration of a listing for a used car? 

library(tidyverse)
library(corrplot)
library(Hmisc)
library(car)

setwd("~/Downloads/Data_science-main/cleaned-data")
data <- read.csv("golf2_cleaned.csv")
toyota_cleaned <- read.csv("golf2_cleaned.csv")
data <- read.csv("golf2_cleaned.csv")

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

# nous remarquons que la correlation entre listing.age et price est positive mais faible => ce qui signifie qu'il existe très peu de relation lineaire entre ces dernieres, cependant le fait que ce soit positive peut illustrer une certaine relation entre les deux variables 
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

# conclusion : la corrélation entre les residuals et le listing age est de 0.98. Cette corrélation élevé peut suggérer que d'autres facteurs peuvent influencer les listing.age mais que ces derniere ne sont pas présente dans le modèle actuel
# c'est que nous analyserons dans un second temps avec le model1. 

# We will make simple linear models for these 5 variables and look at their respective plots.
# We will set a significance level of α = 0.01.

# 1. Influence du prix sur le listing age 

summary(lm(listing.age ~ price))
ggplot(data2, aes(price, listing.age)) + 
  geom_point() +
  geom_smooth(method = lm) +
  ylim(0, max(data2$listing.age))

# p value : < 0.01  Donc price est significatif. nous pouvons l'inclure dans notre model. 
# le prix peut donc avoir une influence sur le listing age. 
# la pente est positive, ce qui illustre bien la relation linéaire entre price et listing age. Ce qui signifie que quand le prix agemente, le listing age a tendance à augmenter également et donc que la voiture reste plus longtemps sur le site. 

# nous allons vérifier qu'il n'y a pas trop de variance.
ggplot(lm(listing.age ~ price), aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  geom_smooth()

# teste de breush and pagan pour l'hétéroskedasticité 

library(lmtest)

# Réalisation du modèle de régression linéaire
model <- lm(listing.age ~ price, data = data2)

# Résumé du modèle pour voir les statistiques
summary(model)

# Test de Breusch-Pagan pour l'hétéroscédasticité
bptest(model)

# le p est inferieur à 1%, ce qui signifie qu'il existe un problème d'hétéroskedasticité dans ce model


# 2. Influence de la consommation sur le listing age
summary(lm(listing.age ~ consumption))
ggplot(data2, aes(consumption, listing.age)) +
  geom_smooth(method = lm) +
  geom_point()+
  ylim(0, max(data2$listing.age))


# Consumption is  not statistically significant (pvalue > 0.01). we will not include this variable in our model. 
#le repartition des points montre bien qu'il n'y a pas de relation linéaire. 


#3. Influence vehicle age sur le listing age 
summary(lm(listing.age ~ vehicle.age))
ggplot(data2, aes(vehicle.age, listing.age)) + 
  geom_smooth(method = lm)+
  geom_point() +
  ylim(0, max(data2$listing.age))

# vehicle age is not statistically significant. la ligne est pratiquement horrizontal, ce qui signifie qu'il n'existe pas de relation linéaire entre l'age du véhicule et le lsiting age.
# la répartition des points dans ce graphe illustre bien qu'il n'y a aucune relation entre les deux variables 
#nous n'allons pas l'inclure dans notre modèle


# 4. Influence kilometers sur le listing age. 


summary(lm(listing.age ~ kilometers))
ggplot(data2, aes(kilometers, listing.age)) + 
  geom_point() +
  geom_smooth(method = lm) +
  ylim(0, max(data2$listing.age))

# significatif. Ce qui signifie que le  nombre de kilometre d'une voiture peut avoir une influence sur le listing age. 
#la ligne est légerement negative a mesure que les kilometres augementent, ce qui signifie que la relation est negative donc quand les kilometre augementent le listing age diminue. 

# nous allons vérifier qu'il n'y a pas trop de variance.
ggplot(lm(listing.age ~ kilometers), aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  geom_smooth()

# teste de breush and pagan pour l'hétéroskedasticité 

library(lmtest)

# Test de Breusch-Pagan pour l'hétéroscédasticité
bptest(lm(listing.age ~ kilometers))

#on note l'existence d'hétéroskédasticité 

# We can try to fix this using a second degree variable. Let's make this model below:

data2$kilometers.squared <- (kilometers^2)
attach(data2)
summary(lm(listing.age ~ kilometers + kilometers.squared))

# Let's check the residuals vs fitted plot

ggplot(lm(listing.age ~ kilometers + kilometers.squared), aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  geom_smooth()

# kilometers squared n'est pas significatif on garde l'autre modele. 

#5.Influence power sur le listing age 

summary(lm(listing.age ~ power))
ggplot(data2, aes(power, listing.age)) + 
  geom_point() +
  geom_smooth(method = lm) +
  ylim(0, max(data2$listing.age))

# p value < 0.01: ce n'est significatif, ce qui signifie que la puissance de la voiture n'influence pas la durée d'une annonce. 
# la répartition des points dans ce graphe illustre bien qu'il n'y a aucune relation entre les deux variables 


# CONCLUSION : price, consumption, vehicle age  et kilometres sont statistiquement significatif. Faisons une regression linéaire avec ces variables

model1 <- lm(listing.age ~ price + vehicle.age  + consumption + kilometers)
summary(model1)

#we observed that all the variables are significant excepted kilometers. 
# Let's check the residuals vs fitted plot

ggplot(model1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  geom_smooth()


# We will now remove defective vehicles and run the model again, this will hopefully stop the model from undervaluing cars.
# Listings flagged as defective account for under 5% of our observations, so removing them won't have a significant impact on our data.

table(data$defective)[2]/nrow(data)
data_no_def <- filter(data, !defective)
model2 <- lm(data_no_def$listing.age ~ data_no_def$vehicle.age + data_no_def$price + data_no_def$kilometers + data_no_def$consumption + data_no_def$power)

# Significance test

summary(model2)

# Residuals vs fitted plot

ggplot(model2, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  geom_smooth()

# We don't notice much difference in the statistical tests or plots between the 2 models. 

### We will now run simple linear regressions for our logical variables. We first need to transform some of our columns into logical values.

# For body.type, we can see that the majority of cars are either "Limousine" or "Petite voiture".
# We will create a logical column "sedan" which will be set to TRUE where body.type == "Limousine" is TRUE. We do the same for column "coupe". When both are FALSE, the body.type will be "Petite voiture".

table(body.type)

data2$wagon <- body.type == "Break"
data2$cabriolet <- body.type == "Cabriolet"
data2$small.car <- body.type == "Petite voiture"
data2$coupe <- body.type == "Coupé"

# We will repeat the process for fuel.type. Columns will be "diesel", "hybrid", "natural.gas" and "electric"

data2$diesel <- fuel.type == "Diesel"
data2$hybrid <- ifelse(fuel.type == "Hybride léger essence/électrique" | 
                        fuel.type == "Hybride rechargeable essence/électrique", 
                      TRUE, FALSE)
data2$natural.gas <- fuel.type == "Gaz naturel (CNG) / Essence"
data2$electric <- fuel.type == "Électrique"

# We repeat the process for transmission. We will only have one variable "manual". We consider "Boîte manuelle automatisée" to be automatic because it is very similar to an automatic transmission.

data2$manual <- ifelse(transmission == "Automatique" |
                        transmission == "Boîte manuelle automatisée" |
                        transmission == "Boîte automatique variable", 
                      FALSE, TRUE)

# We repeat the process for drivetrain. We will create one column "awd". We will neglect "Propulsion" and leave it as false, the same as for "Traction avant" because there are only 2 observations.

data2$awd <- drivetrain == "4 roues motrices"

# Now we run the simple linear regressions for expertise, warranty, wagon, cabriolet, diesel, hybrid, natural.gas, electric, manual and awd. We note which ones are statistically significant at α = 0.01.

attach(data2)
summary(lm(listing.age ~ expertise)) # Significant
summary(lm(listing.age ~ warranty)) # Significant
summary(lm(listing.age ~ wagon)) # not Significant
summary(lm(listing.age ~ cabriolet)) # not Significant
summary(lm(listing.age~ small.car)) # NOT Significant
summary(lm(listing.age ~ coupe)) # NOT Significant
summary(lm(listing.age ~ diesel)) # not Significant
summary(lm(listing.age ~ hybrid)) # Significant
summary(lm(listing.age ~ natural.gas)) # NOT Significant
summary(lm(listing.age ~ electric)) # NOT Significant
summary(lm(listing.age ~ manual)) # not Significant
summary(lm(listing.age ~ awd)) # Significant

# Let's add all the significant variables to a model and see if they are still significant.

model3 <- lm(listing.age ~ expertise + warranty+  hybrid + awd)
summary(model3)


#Warranty  n'est plus significant dans ce model. Ajoutons ce qui sont significant dans notre modele initial 
model4 <- lm(listing.age ~ price + vehicle.age  + consumption + kilometers +expertise +  hybrid + awd)
summary(model4)
anova(model4)

# kilometres, awd et hybrid ne sont pas significant nous pouvons donc les retirer de notre modèle. 

model5 <- lm(listing.age ~ price + vehicle.age  + consumption +expertise  )
summary(model5)



# All our variables are now significant. Our adjusted R-squared is 0.03234, which is the proportion of the variance of the price that is explained by our model. 
# Cela suggère que seulement 3% de la variance dans la variable dépendante est expliquée par le modèle. 
#Let's check the residuals vs fitted plot and the standard deviation of the residuals.

ggplot(model5, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  geom_smooth()

sqrt(anova(model5)$"Mean Sq")[14]

anova(model5)
model5_data2 <- select(data2, c("price", "vehicle.age", "listing.age", "kilometers", "power", "expertise", "warranty", "wagon", "diesel", "hybrid", "manual", "awd"))
cor_matrix_2 <- cor(model5_data2, use = "complete.obs")
cor_matrix_2
corrplot(cor_matrix_2)

# Model with cars older than 9000 days removed (before March 1999)

data_no_vintage <- filter(data2, vehicle.age < 9000)
ggplot(data_no_vintage, aes(vehicle.age, listing.age)) + 
  geom_point() +
  geom_smooth(method = lm) +
  ylim(0, max(data2$listing.age))
model6 <- lm(data_no_vintage$listing.age ~ data_no_vintage$vehicle.age + data_no_vintage$price + data_no_vintage$kilometers + data_no_vintage$power + data_no_vintage$expertise + data_no_vintage$warranty + data_no_vintage$wagon + data_no_vintage$diesel + data_no_vintage$manual + data_no_vintage$awd)
summary(model6)

# we can remove vehicle age, kilometers, warranty  power, wagon, diesel,manual et awd because they are not significant. 
model6 <- lm(data_no_vintage$listing.age ~  data_no_vintage$expertise + data_no_vintage$price + data_no_vintage$vehicle.age)
summary(model6)

summary(model6)
anova(model6)
sqrt(anova(model6)$"Mean Sq")[13] # Standard deviation of residuals has decreased significantly

# Model4 without defectives (model7)

data_no_def <- filter(data2, !defective)
model7 <- lm(data_no_def$listing.age ~ data_no_def$price+ data_no_def$vehicle.age  + data_no_def$kilometers + data_no_def$consumption + data_no_def$hybrid + data_no_def$awd)
summary(model7)
anova(model7)
sqrt(anova(model7)$"Mean Sq")[14]

ggplot(model7, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  geom_smooth()

# We will test model6 for multicollinearity

model7_data <- select(data_no_def, c("listing.age", "price", "vehicle.age", "consumption", "kilometers", "expertise", "hybrid", "awd"))
cor_matrix_3 <- cor(model7_data, use = "complete.obs")
cor_matrix_3
corrplot(cor_matrix_3)
