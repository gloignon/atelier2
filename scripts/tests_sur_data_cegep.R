# tests_sur_data_cegep.R
# Démos, test du chi2, test t et anova sur données simulées cégep
#
# Par G Loignon
# guillaume.loignon@umontreal.ca
# Dernière révision: février 2019

library(tidyverse)
library("data.table")

load("data/dt_synth2.rda")
#---- Bon à savoir ----

# voir les niveaux d'une variable catégorielle:
#levels(data$col)

# éliminer les niveaux non-utilisés
#data$col <- droplevels(data$col)

#écrire seulement dans certaines rangées
#data[data$col == "blabla"] <- x
#data[data$col == "blabla"] <- data[data$col == "blabla"]


#---- Sélection de données ----
# Consigne: créez un jeu de données pre.u contenant seulement les programmes "Sc humaines" et "Sc nature",
#           et seulement le cours "philo101". Assurez-vous que les niveaux non-utilisés de la variable
#           Programme ont été éliminés (la fonction droplevel() sera utile pour ce faire.)

# Solution avec R de base [rangées, colonnes]
pre.u <- dt.synth2[dt.synth2$Programme %in% c("Sc humaines", "Sc nature") & dt.synth2$Cours == "philo101"]
pre.u$Programme <- droplevels(pre.u$Programme)
levels(pre.u$Programme)

# Solution avec R de base (subset)
pre.u <- subset(dt.synth2, Programme %in% c("Sc humaines", "Sc nature") & dt.synth2$Cours == "philo101") 
#Note: subset peut causer des problèmes. À éviter.
pre.u$Programme <- droplevels(pre.u$Programme)
levels(pre.u$Programme)


#Solution avec data.table
is.data.table(dt.synth2) #la jeu de données est déjà de type data.table
pre.u <- dt.synth2[Programme %in% c("Sc humaines", "Sc nature")  & Cours == "philo101"]
pre.u[, Programme := droplevels(Programme)]
levels(pre.u$Programme)

# Solution avec le tidyverse
pre.u <- dt.synth2 %>% 
  filter(Programme %in% c("Sc humaines", "Sc nature") & Cours == "philo101") %>% 
  droplevels() %>%
  as_factor()
levels(pre.u$Programme)

#---- Chi carré ----

# Question: Le cégep considère souhaite envoyer une trousse de motivation à tous les élèves de sciences
#           humaines et sciences pures ayant une MGS < 75. Alors que vous faites les envois postaux, 
#           vous avez l'impression que les élèves de sciences humaines sont sur-représentés.
#           Vérifiez cette intuition à l'aide d'un test du chi carré.

sommaire <- table(pre.u$MGS < 75, pre.u$Programme)
sommaire
chisq.test(sommaire)

#---- Test t ----

# Question: La moyenne au secondaire (MGS) des deux groupes est-elle comparable?
t.test(data=pre.u, MGS ~ Programme)

#---- ANOVA ----
#Consigne: Dans dt.synth2, convertissez la colonne Cours en facteur, puis faites une ANOVA
#          visant à expliquer la noteMax en fonction du cours et du programme d'études.
#          Comparez les modèles avec et sans interaction.

dt.synth2$cours <- factor(dt.synth2$Cours)
anova.inter <- aov(data=dt.synth2, noteMax ~ Cours * Programme)
anova.pas.inter <- aov(data=dt.synth2, noteMax ~ Cours + Programme)

anova(anova.inter, anova.pas.inter)