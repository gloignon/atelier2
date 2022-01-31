# fonctions.R
# Survol des fonctions perso en R
#
# Par G Loignon
# guillaume.loignon@umontreal.ca
# Mars 2019

# Conseil: les recommandations de style en R suggèrent de faire débuter le nom de la
#          fonction par une majuscule, et d'utiliser un verbe comme nom.

library(data.table)
library(tidyverse)

# Bon à savoir: on peut compiler les fonctions avec la librairie compiler pour
#               accélérer le traitement. Voir:
#                 https://csgillespie.github.io/efficientR/7-4-the-byte-compiler.html#example-the-mean-function

CalculerMoyTronq <- function(vecteur) {
# Accepte un vecteur et retourne la moyenne "tronquée" en ignorant les NA
  n <- length(vecteur)
  debut <- round(n / 10) + 1
  fin <- n - round(n / 10) 
  vecteur.ord <- sort(vecteur)
  vecteur.trim <- vecteur.ord[debut:fin]
  moy <- mean(vecteur.trim, na.rm = TRUE)
  return(moy)
}
  
CalculerMoyTronqV2 <- function(vecteur, coupe = 10) {
  # Accepte un vecteur et un pourcentage à couper en tête et en queue,
  # Retourne la moyenne "tronquée" en ignorant les NA
  coupe <- coupe / 100
  n <- length(vecteur)
  debut <- round(n * coupe) + 1
  fin <- n - round(n * coupe) 
  vecteur.ord <- sort(vecteur)
  vecteur.trim <- vecteur.ord[debut:fin]
  moy <- mean(vecteur.trim, na.rm = TRUE)
  return(moy)
}

mean(gss_cat$tvhours)  # NA
mean(gss_cat$tvhours, na.rm = T)  # 2.98
CalculerMoyTronq(gss_cat$tvhours)  # 3.52
CalculerMoyTronqV2(gss_cat$tvhours)  # 3.52
CalculerMoyTronqV2(gss_cat$tvhours, 10)  # 3.52
CalculerMoyTronqV2(gss_cat$tvhours, 15)  # 3.83

# Faire rouler une fonction sur un jeu de données
  # ex: je veux la moyenne tronquée par affiliation politique

# Méthode data.table
dt.gss <- data.table(gss_cat)
dt.afiliations <- dt.gss[, .(
  moyTronqEcouteTV = CalculerMoyTronq(tvhours)
      # on pourrait mettre d'autres variables calculées ici
), by="partyid"]  # j'ai exclu les groupes ayant trop peu de répondants

dt.afiliations

# Méthode tidyverse
tbl.gss <- as_tibble(gss_cat) %>%
  group_by(partyid) %>%
  summarise(moyTronqEcouteTV = CalculerMoyTronq(tvhours))
head(tbl.gss)

  # Note: il y a d'autres moyens avec tidyverse, notamment la famille
  #       des apply() que nous n'auront pas le temps de voir maintenant.

# À VOTRE TOUR
# 
#  - Créez une fonction CalculerMAD qui accepte un vecteur et retourne
#    la moyenne des valeurs absolues des écarts à la médiane 
#    (median absolute deviation).
#
#  - Calculez la MAD de gss_cat$tvhours
#
#  - Calculez la MAD de tvhours par groupe, pour chaque statut marital
#
# Truc: pour la valeur absolue: abs()
# Autre truc: ceci n'est qu'un exercice! Il existe déjà une fonction mad() qui fait exactement
# ce que vous souhaitez. :D
# Vous pouvez utiliser mad() pour contre-vérifier votre fonction personnalisée.











# solution plus bas
















CalculerMAD <- function(vecteur) {
  med <- median(vecteur, na.rm=T)
  abs.dev <- abs(med - vecteur)
  abs.dev.moy <- mean(abs.dev, na.rm=T)
  return(abs.dev.moy)
}
CalculerMAD(gss_cat$tvhours)

tbl.gss <- as_tibble(gss_cat) %>%
  group_by(marital) %>%
  summarise(moyTronqEcouteTV = CalculerMoyTronq(tvhours))
head(tbl.gss)
