# fonctions.R
# Survol des fonctions perso en R
#
# Par G Loignon
# guillaume.loignon@umontreal.ca
# Mars 2019

# Conseil: les recommandations de style en R suggèrent de faire débuter le nom de la
#          fonction par une majuscule, et d'utiliser un verbe comme nom.

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

# À VOTRE TOUR
# 
# - Créez une fonction CalculerMAD qui accepte un vecteur et retourne
#   la moyenne des valeurs absolues des écarts à la médiane 
#   (median absolute deviation).
# 
#  - Calculez la MAD de gss_cat$tvhours
#
# Truc: pour la valeur absolue: abs()











# solution plus bas
















CalculerMAD <- function(vecteur) {
  med <- median(vecteur, na.rm=T)
  abs.dev <- abs(med - vecteur)
  abs.dev.moy <- mean(abs.dev, na.rm=T)
  return(abs.dev.moy)
}
CalculerMAD(gss_cat$tvhours)
